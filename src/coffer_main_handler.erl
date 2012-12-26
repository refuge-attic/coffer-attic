-module(coffer_main_handler).

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
    case coffer_manager:exists(ContentId) of
        true  ->
            cowboy_req:reply(200, [], <<"OK">>, Req);
        false ->
            cowboy_req:reply(404, [], <<"Doesn't exist">>, Req)
    end;

maybe_process_it(<<"POST">>, true, Req) ->
    {Result, Req2} = acc_multipart(Req),
    io:format("Result: ~p~n", [Result]),
    cowboy_req:reply(200, [], <<"success">>, Req2);

maybe_process_it(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"bad request, check input data">>, Req);

maybe_process_it(<<"DELETE">>, false, Req0) ->
    {[ContentId], Req} = cowboy_req:path_info(Req0),
    case coffer_manager:exists(ContentId) of
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


acc_multipart(Req) ->
    acc_multipart(cowboy_req:multipart_data(Req), []).

acc_multipart({headers, Headers, Req}, Acc) ->
    io:format("HEADERS ARE: ~p~n", [Headers]),
    acc_multipart(cowboy_req:multipart_data(Req), [{Headers, []}|Acc]);
acc_multipart({body, Data, Req}, [{Headers, BodyAcc}|Acc]) ->
    acc_multipart(cowboy_req:multipart_data(Req), [{Headers, [Data|BodyAcc]}|Acc]);
acc_multipart({end_of_part, Req}, [{Headers, BodyAcc}|Acc]) ->
    acc_multipart(cowboy_req:multipart_data(Req),
        [{Headers, list_to_binary(lists:reverse(BodyAcc))}|Acc]);
acc_multipart({eof, Req}, Acc) ->
    {lists:reverse(Acc), Req}.
