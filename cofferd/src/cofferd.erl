-module(cofferd).

-export([init_storage/0, init_storage/1]).
-export([compact_storage/0]).
-export([read_blob_init/1, read_blob/1, read_blob_end/1]).
-export([write_blob_init/1, write_blob/2, write_blob_end/1]).
-export([delete_blob/1]).
-export([list_blobs/2]).
-export([is_blob/1]).

%%% Storage operations

init_storage() ->
    init_storage([]).

init_storage(Options) ->
    cofferd_manager:init_storage(Options).

% TODO to refine
compact_storage() ->
    throw({error, not_yet_designed}).

%%% CRUD blob

read_blob_init(Id) when is_binary(Id) ->
    case cofferd_manager:is_blob(Id) of
        true ->
            cofferd_manager:read_blob_init(Id);
        false ->
            {error, doesnt_exist}
    end.

read_blob(Handle) ->
    cofferd_manager:read_blob(Handle).

read_blob_end(Handle) ->
    cofferd_manager:read_blob_end(Handle).

write_blob_init(Id) when is_binary(Id) ->
    case cofferd_manager:is_blob(Id) of
        true ->
            {error, already_exists};
        false ->
            cofferd_manager:write_blob_init(Id).
    end.

write_blob(Handle, Data) ->
    cofferd_manager:write_blob(Handle, Data).

write_blob_end(Handle) ->
    cofferd_manager:write_blob_end(Handle).

delete_blob(Id) when is_binary(Id) ->
    case cofferd_manager:is_blob(Id) of
        true ->
            cofferd_manager:delete_blob(Id);
        false ->
            {error, doesnt_exist}
    end.

list_blobs(Func, InitState) ->
    cofferd_manager:list_blobs(Func, InitState).

is_blob(Id) when is_binary(Id) ->
    cofferd_manager:is_blob(Id).

