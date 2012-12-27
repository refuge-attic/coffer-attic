-module(coffer_simple_storage_test).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_files_test_() ->
	[{"Testing with simple files first.",
	 ?setup(fun store_and_retrieve_a_file/1)}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
	{ok, Pid} = coffer_simple_storage:start_link(),
	coffer_simple_storage:init_storage(),
	Pid.

stop(_) ->
	coffer_simple_storage:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

store_and_retrieve_a_file(_) ->
	Content = <<"Hello World!">>,
	ContentHash = coffer_util:content_hash(Content),

	{ok, Ref} = coffer_simple_storage:store_blob_init(ContentHash),
	coffer_simple_storage:store_blob(Ref, Content),
	Res = coffer_simple_storage:store_blob_end(Ref),

	C2 = coffer_simple_storage:get_blob_content(ContentHash),
	
	[?_assert(ok =:= Res), ?_assert({ok, Content} =:= C2)].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% nothing here yet
