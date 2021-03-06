%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.

-module(coffer_storage_null).
-behaviour(coffer_storage).

-export([start/1, stop/1]).
-export([init_storage/1]).
-export([open/2, close/1]).
-export([put/3, get/3, delete/2, all/1, foldl/4, foreach/2]).

start(_Config) ->
    {ok, []}.

stop(_) ->
    ok.

init_storage(_) ->
    ok.

open(_InitState, _Options) ->
    {ok, []}.

close(_Ref) ->
    ok.

put(_Ref, _Id, _Chunk) ->
    {ok, []}.

get(_Ref, _Id, _Options) ->
    {ok, <<>>, []}.

delete(_Ref, _Id) ->
    {ok, []}.

all(_Ref) ->
    {ok, []}.

foldl(_Ref, _Func, _Initstate, _Options) ->
    {ok, []}.

foreach(_Ref, _Func) ->
    ok.

