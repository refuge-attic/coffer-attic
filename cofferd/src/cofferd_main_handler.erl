-module(cofferd_main_handler).

-export([init/3, handle/2, terminate/2]).

-define(MAX_BODY_SIZE, 4294967296).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {HasBody, Req3} = cowboy_req:has_body(Req2),
    {ok, Req4} = maybe_process_it(Method, HasBody, Req3),
    {ok, Req4, State}.

maybe_process_it(<<"GET">>, false, Req0) ->
    {[ContentId], Req} = cowboy_req:path_info(Req0),
    case cofferd_manager:exists(ContentId) of
        true  ->
            {ok, Req2} = cowboy_req:chunked_reply(200, Req),
            {ok, Token} = cofferd_manager:get_blob_init(ContentId),
            {ok, Req3} = iterate_over_reading_chunks(Req2, Token),
            {ok, Req3};
        false ->
            cowboy_req:reply(404, [], <<"Doesn't exist">>, Req)
    end;

maybe_process_it(<<"POST">>, true, Req0) ->
    {[ContentId], Req} = cowboy_req:path_info(Req0),
    case cowboy_req:has_body(Req) of
        {true, Req2} ->
            {ok, Token} = cofferd_manager:store_blob_init(ContentId),
            case iterate_over_writing_chunks(Req2, Token, 0) of
                {ok, Req3} ->
                    cowboy_req:reply(201, [], <<"Done">>, Req3);
                _Other ->
                    cowboy_req:reply(400, [], <<"Error...">>, Req2)
            end;
        {false, Req2} ->
            cowboy_req:reply(400, [], <<"Bad request">>, Req2)
    end;

maybe_process_it(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"bad request, check input data">>, Req);

maybe_process_it(<<"DELETE">>, false, Req0) ->
    {[ContentId], Req} = cowboy_req:path_info(Req0),
    case cofferd_manager:exists(ContentId) of
        true  ->
            cowboy_req:reply(200, [], <<"OK">>, Req);
        false ->
            cowboy_req:reply(404, [], <<"Doesn't exist">>, Req)
    end;

maybe_process_it(<<"OPTIONS">>, false, Req) ->
    RespHeaders = [{<<"Allow">>, <<"OPTIONS, GET, POST, DELETE">>}],
    cowboy_req:reply(200, RespHeaders, <<"">>, Req);

maybe_process_it(_, _, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Req, _State) ->
    ok.

%%

iterate_over_reading_chunks(Req, Token) ->
    case cofferd_manager:get_blob(Token) of
        {ok, Data} ->
            ok = cowboy_req:chunk(Data, Req),
            iterate_over_reading_chunks(Req, Token);
        eof ->
            cofferd_manager:get_blob_end(Token),
            {ok, Req}
    end.

iterate_over_writing_chunks(Req, Token, FinalSize) ->
    case cowboy_req:stream_body(Req) of
        {ok, Data, Req2} ->
            %io:format("FinalSize: ~p~n", [FinalSize]),
            %%io:format("DATA: {~p}~n", [Data]),
            cofferd_manager:store_blob(Token, Data),
            iterate_over_writing_chunks(Req2, Token, FinalSize+size(Data));
        {done, Req2} ->
            cofferd_manager:store_blob_end(Token),
            io:format("Final size: ~p~n", [FinalSize]),
            {ok, Req2};
        {error, Reason} ->
            lager:error("An error occured during writing a blob: ~p", [Reason]),
            {error, Reason}
    end.

%%

% acc_multipart(Req) ->
%     acc_multipart(cowboy_req:multipart_data(Req), []).

% acc_multipart({headers, Headers, Req}, Acc) ->
%     lager:info("HEADERS ARE: ~p~n", [Headers]),
%     acc_multipart(cowboy_req:multipart_data(Req), [{Headers, []}|Acc]);
% acc_multipart({body, Data, Req}, [{Headers, BodyAcc}|Acc]) ->
%     acc_multipart(cowboy_req:multipart_data(Req), [{Headers, [Data|BodyAcc]}|Acc]);
% acc_multipart({end_of_part, Req}, [{Headers, BodyAcc}|Acc]) ->
%     acc_multipart(cowboy_req:multipart_data(Req),
%         [{Headers, list_to_binary(lists:reverse(BodyAcc))}|Acc]);
% acc_multipart({eof, Req}, Acc) ->
%     {lists:reverse(Acc), Req}.
