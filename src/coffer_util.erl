
-module(coffer_util).

-define(CONTENT_HASH, sha).

-export([content_hash/1, content_hash_on_stream/2]).
-export([content_directory/1, content_filename/1, content_location/1]).

content_hash(Data) ->
    <<Mac:160/integer>> = crypto:hash(?CONTENT_HASH, Data),
    lists:flatten(io_lib:format("~40.16.0b", [Mac])).

%
% Func is a
%   fun(State) -> {Data, NewState}
%              -> {Data, eof}      when it's over
content_hash_on_stream(Func, InitState) ->
    Context = crypto:hash_init(?CONTENT_HASH),
    iterate_hash_over_stream(Func, Context, InitState).

iterate_hash_over_stream(_, Context, eof) ->
    <<Mac:160/integer>> = crypto:hash_final(Context),
    lists:flatten(io_lib:format("~40.16.0b", [Mac]));
iterate_hash_over_stream(Func, Context, State) ->
    {Data, NewState} = Func(State),
    NewContext = crypto:hash_update(Context, Data),
    iterate_hash_over_stream(Func, NewContext, NewState).

content_directory(ContentHash) ->
    string:sub_string(ContentHash, 1,2).

content_filename(ContentHash) ->
    string:sub_string(ContentHash, 3).

content_location(ContentHash) ->
    content_directory(ContentHash) ++ "/" ++ content_filename(ContentHash).