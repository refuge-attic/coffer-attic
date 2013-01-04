-module(coffer_manager_test).
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
	 ?setup(fun store_and_delete_a_file/1)},
	 {"Testing the existence.",
	 ?setup(fun does_it_exist/1)}
	].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
	Options = [coffer_simple_storage, [
        {repo_home, "/tmp/coffer_test_data"},
        {chunk_size, 4096}
        ]],
	{ok, Pid} = coffer_manager:start_link(Options),
	coffer_manager:init_storage([]),
	Pid.

stop(_) ->
	coffer_manager:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

store_and_retrieve_a_file(_) ->
	Content = <<"Hello World!">>,
	ContentHash = coffer_util:content_hash(Content),

	Res = coffer_manager:store_blob_content(ContentHash, Content),
	C2 = coffer_manager:get_blob_content(ContentHash),
	
	[?_assert(ok =:= Res), ?_assert({ok, Content} =:= C2)].

store_and_retrieve_a_big_file(_) ->
	{ok, ContentBit} = file:read_file("/etc/passwd"),
	Id = <<"1234567890">>,

	% writing many "bits"
	{ok, Token} = coffer_manager:store_blob_init(Id),
	Res = store_loop(Token, ContentBit, 0),

	Size = size(ContentBit),
	ExpectedFinalSize = 1000 * Size,

	{ok, Token2} = coffer_manager:get_blob_init(Id),
	ActualSize = compute_size(Token2, 0),

	[?_assert(ok =:= Res),
	 ?_assert(ExpectedFinalSize =:= ActualSize)].

store_and_delete_a_file(_) ->
	Content = <<"Hello World!">>,
	ContentHash = coffer_util:content_hash(Content),

	Res  = coffer_manager:store_blob_content(ContentHash, Content),
	Res2 = coffer_manager:remove_blob(ContentHash),
	Res3 = coffer_manager:get_blob_init(ContentHash),

	[?_assert(ok =:= Res),
	 ?_assert(ok =:= Res2),
	 ?_assert({error, not_exist} =:= Res3)].

does_it_exist(_) ->
	Content = <<"Hello World!">>,
	ContentHash = coffer_util:content_hash(Content),

	coffer_manager:store_blob_content(ContentHash, Content),

	ShouldbeThere = coffer_manager:exists(ContentHash),
	ShouldnotbeThere = coffer_manager:exists(<<"BogusId">>),

	[?_assert(ShouldbeThere =:= true),
	 ?_assert(ShouldnotbeThere =:= false)].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

store_loop(Token, _ContentBit, 1000) ->
	coffer_manager:store_blob_end(Token);
store_loop(Token, ContentBit, N) ->
	coffer_manager:store_blob(Token, ContentBit),
	store_loop(Token, ContentBit, N+1).

compute_size(Token, N) ->
	case coffer_manager:get_blob(Token) of
		eof ->
			N;
		{ok, Data} ->
			CurrentSize = size(Data),
			compute_size(Token, N + CurrentSize);
		Other ->
			Other
	end.
