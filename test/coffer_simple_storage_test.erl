-module(coffer_simple_storage_test).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_files_test_() ->
	[{"Testing with simple files first.",
	 ?setup(fun store_and_retrieve_a_file/1)},
	 {"Testing with a big file.",
	 ?setup(fun store_and_retrieve_a_big_file/1)},
	 {"Storing a file and testing deletion.",
	 ?setup(fun store_and_delete_a_file/1)}
	].

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

	Res = coffer_simple_storage:store_blob_content(ContentHash, Content),
	C2 = coffer_simple_storage:get_blob_content(ContentHash),
	
	[?_assert(ok =:= Res), ?_assert({ok, Content} =:= C2)].

store_and_retrieve_a_big_file(_) ->
	{ok, ContentBit} = file:read_file("/etc/passwd"),
	Id = "1234567890",

	% writing many "bits"
	{ok, Ref} = coffer_simple_storage:store_blob_init(Id),
	Res = store_loop(Ref, ContentBit, 0),

	Size = size(ContentBit),
	ExpectedFinalSize = 1000 * Size,

	{ok, Ref2} = coffer_simple_storage:get_blob_init(Id),
	ActualSize = compute_size(Ref2, 0),

	[?_assert(ok =:= Res),
	 ?_assert(ExpectedFinalSize =:= ActualSize)].

store_loop(Ref, _ContentBit, 1000) ->
	coffer_simple_storage:store_blob_end(Ref);
store_loop(Ref, ContentBit, N) ->
	coffer_simple_storage:store_blob(Ref, ContentBit),
	store_loop(Ref, ContentBit, N+1).

compute_size(Ref, N) ->
	case coffer_simple_storage:get_blob(Ref) of
		eof ->
			N;
		{ok, Data} ->
			CurrentSize = size(Data),
			compute_size(Ref, N + CurrentSize);
		Other ->
			Other
	end.

store_and_delete_a_file(_) ->
	Content = <<"Hello World!">>,
	ContentHash = coffer_util:content_hash(Content),

	Res  = coffer_simple_storage:store_blob_content(ContentHash, Content),
	Res2 = coffer_simple_storage:remove_blob(ContentHash),
	Res3 = coffer_simple_storage:get_blob_init(ContentHash),

	[?_assert(ok =:= Res),
	 ?_assert(ok =:= Res2),
	 ?_assert({error, not_exist} =:= Res3)].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% nothing here yet
