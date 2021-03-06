%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.

-module(coffer_resource).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("coffer/includes/coffer.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([list/0]).
-export([add/3, remove/1]).
-export([open/2, close/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

list() ->
    gen_server:call(?MODULE, {list}).

add(ResourceName, Backend, Config) ->
    gen_server:call(?MODULE, {add, ResourceName, Backend, Config}).

remove(ResourceName) ->
    gen_server:call(?MODULE, {remove, ResourceName}).

open(ResourceName, Options) ->
    gen_server:call(?MODULE, {open, ResourceName, Options}).

close(Ref) ->
    gen_server:call(?MODULE, {close, Ref}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

% gen_server state records
-record(state, {
    resources = [],
    options = []
}).
-record(resource, {
    name,
    backend,
    config = [],
    init
}).

init([]) ->
    %
    % resources : [
    %     { ResourceName, Backend, Config } 
    % ]
    %
    ResourcesConfig = case application:get_env(coffer, resources) of
        undefined ->
            [];
        {ok, Other} ->
            Other
    end,
    FinalState = lists:foldl(
        fun({ResourceName, Backend, Config}, State) ->
            {_, NewState} = do_add_resource(ResourceName, Backend, Config, State),
            NewState
        end,
        #state{},
        ResourcesConfig
    ),
    {ok, FinalState}.

handle_call({list}, _From, #state{resources=Resources}=State) ->
    Reply = lists:foldl(
        fun({ResourceName, _Resource}, Acc) ->
            [ResourceName|Acc]
        end,
        [],
        Resources
    ),
    {reply, Reply, State};
handle_call({add, ResourceName, Backend, Config}, _From, State) ->
    {Reply, NewState} = do_add_resource(ResourceName, Backend, Config, State),
    {reply, Reply, NewState};
handle_call({remove, ResourceName}, _From, State) ->
    {Reply, NewState} = do_remove_resource(ResourceName, State),
    {reply, Reply, NewState};
handle_call({open, ResourceName, Options}, _From, #state{resources=Resources}=State) ->
    case proplists:get_value(ResourceName, Resources) of
        undefined ->
            {reply, {error, not_such_resource}, State};
        #resource{backend=Backend, init=InitState} ->
            Reply = case Backend:open(InitState, Options) of
                {ok, SRef} ->
                    {ok, #ref{backend=Backend, sref=SRef}};
                {error, Reason} ->
                    lagger:error("Error: ~p opening resource: ~p", [Reason, ResourceName]),
                    {error, Reason};
                Other ->
                    lagger:error("Unexpected error: ~p opening resource: ~p", [Other, ResourceName]),
                    {error, unexpected_error, Other}
            end,
            {reply, Reply, State}
    end;
handle_call({close, #ref{backend=Backend, sref=SRef, active=Active}=Ref}, _From, State) ->
    Reply = case Active of
        true ->
            case Backend:close(SRef) of
                ok ->
                    {ok, Ref#ref{active=false}};
                {error, Reason} ->
                    lagger:error("Error: ~p closing resource using backend: ~p", [Reason, Backend]),
                    {error, Reason};
                Other ->
                    lagger:error("Unexpected error: ~p closing resource using backend: ~p", [Other, Backend]),
                    {error, unexpected_error, Other}
            end;
        false ->
            {error, already_closed}
    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{resources=Resources}=_State) ->
    lists:foldl(
        fun(#resource{backend=Backend, init=InitState}=_Resource, _Acc) ->
            Backend:stop(InitState)
        end,
        [],
        Resources
    ),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_add_resource(ResourceName, Backend, Config, #state{resources=Resources}=State) ->
    case proplists:get_value(ResourceName, Resources, undefined) of
        undefined ->
            lager:info("Starting resource: ~p with backend: ~p", [ResourceName, Backend]),
            case Backend:start(Config) of
                {ok, InitState} ->
                    lager:info("Resource ~p successfully started!", [ResourceName]),
                    Resource = #resource{name=ResourceName, backend=Backend, config=Config, init=InitState},
                    UpdatedResources = [ {ResourceName, Resource} | Resources ],
                    NewState = State#state{resources=UpdatedResources},
                    {ok, NewState};
                ErrorAtLoad ->
                    lager:error("An error occured loading the storage ~p with error: ~p~n", [ResourceName, ErrorAtLoad]),
                    {{error, cant_start}, State}
            end;
        _AlreadyThere ->
            lager:error("Resource ~p already exists!", [ResourceName]),
            {{error, already_exists}, State}
    end.

 do_remove_resource(ResourceName, #state{resources=Resources}=State) ->
    case proplists:get_value(ResourceName, Resources, undefined) of
        undefined ->
            {{error, doesnt_exist}, State};
        #resource{backend=Backend, init=InitState}=_Resource ->
            lager:info("Stopping resource ~p", [ResourceName]),
            case Backend:stop(InitState) of
                ok ->
                    lager:info("Resource ~p is now stopped", [ResourceName]),
                    UpdatedResources = proplists:delete(ResourceName, Resources),
                    {ok, State#state{resources=UpdatedResources}};
                {error, Reason} ->
                    {{error, Reason}, State}
            end
    end.
