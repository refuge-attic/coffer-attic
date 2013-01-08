-module(cofferd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Dispatch = [
		{'_', [
			{[<<"blob">>, '...'], cofferd_main_handler, []}
		]}
	],
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{dispatch, Dispatch}
	]),

    cofferd_sup:start_link().

stop(_State) ->
    ok.
