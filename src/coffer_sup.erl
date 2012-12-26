
-module(coffer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    BlobManager = ?CHILD(coffer_manager, worker),
    BlobDumbStorage = ?CHILD(coffer_dumb_storage, worker),

    Children = [BlobManager, BlobDumbStorage],
    RestartStrategy = {one_for_one, 1, 60},
    {ok, { RestartStrategy, Children} }.
