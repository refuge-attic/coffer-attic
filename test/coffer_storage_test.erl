-module(coffer_storage_test).
-include_lib("eunit/include/eunit.hrl").

-define(setup(Storage, Options, F), {setup, fun() -> start(Storage, Options) end, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

do_test_() ->
	[
	test_given_storage(coffer_simple_storage, [{repo_home, "/tmp/coffer_test_data"}, {chunk_size, 4096}]),
	test_given_storage(coffer_manager, [coffer_simple_storage, [{repo_home, "/tmp/coffer_test_data"}, {chunk_size, 4096}]])
	].

test_given_storage(Storage, Options) ->
	[{"Testing with simple files first.",
	 ?setup(Storage, Options, fun store_and_retrieve_a_file/1)},
	 {"Testing with a big file.",
	 ?setup(Storage, Options, fun store_and_retrieve_a_big_file/1)},
	 {"Storing a file and testing deletion.",
	 ?setup(Storage, Options, fun store_and_delete_a_file/1)},
	 {"Testing the existence.",
	 ?setup(Storage, Options, fun does_it_exist/1)},
	 {"List several files",
	  ?setup(Storage, Options, fun listing_files/1)}
	].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start(Storage, Options) ->
	{ok, _Pid} = Storage:start_link(Options),
	Storage:init_storage([]),
	Storage.

stop(Storage) ->
	Storage:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

store_and_retrieve_a_file(Storage) ->
	Content = <<"Hello World!">>,
	ContentHash = coffer_util:content_hash(Content),

	Res = Storage:store_blob_content(ContentHash, Content),
	C2 = Storage:get_blob_content(ContentHash),
	
	[?_assert(ok =:= Res), ?_assert({ok, Content} =:= C2)].

store_and_retrieve_a_big_file(Storage) ->
	{ok, ContentBit} = file:read_file("/etc/passwd"),
	Id = <<"1234567890">>,

	% writing many "bits"
	{ok, Token} = Storage:store_blob_init(Id),
	Res = store_loop(Storage, Token, ContentBit, 0),

	Size = size(ContentBit),
	ExpectedFinalSize = 1000 * Size,

	{ok, Token2} = Storage:get_blob_init(Id),
	ActualSize = compute_size(Storage, Token2, 0),

	[?_assert(ok =:= Res),
	 ?_assert(ExpectedFinalSize =:= ActualSize)].

store_and_delete_a_file(Storage) ->
	Content = <<"Hello World!">>,
	ContentHash = coffer_util:content_hash(Content),

	Res  = Storage:store_blob_content(ContentHash, Content),
	Res2 = Storage:remove_blob(ContentHash),
	Res3 = Storage:get_blob_init(ContentHash),

	[?_assert(ok =:= Res),
	 ?_assert(ok =:= Res2),
	 ?_assert({error, not_exist} =:= Res3)].

does_it_exist(Storage) ->
	Content = <<"Hello World!">>,
	ContentHash = coffer_util:content_hash(Content),

	Storage:store_blob_content(ContentHash, Content),

	ShouldbeThere = Storage:exists(ContentHash),
	ShouldnotbeThere = Storage:exists(<<"BogusId">>),

	[?_assert(ShouldbeThere =:= true),
	 ?_assert(ShouldnotbeThere =:= false)].

listing_files(Storage) ->
	Content1 = <<"Hello World!">>,
	ContentHash1 = <<"123">>,%coffer_util:content_hash(Content1),
	Content2 = <<"Foo bar!">>,
	ContentHash2 = <<"456">>,%coffer_util:content_hash(Content2),
	Content3 = <<"Something else">>,
	ContentHash3 = <<"789">>,%coffer_util:content_hash(Content3),

	Storage:store_blob_content(ContentHash1, Content1),
	Storage:store_blob_content(ContentHash2, Content2),
	Storage:store_blob_content(ContentHash3, Content3),

	SimpleListFunc = fun(X, Acc) ->
		[X | Acc]
	end,

	Res = Storage:fold_blobs(SimpleListFunc, []),
	Res2 = Storage:remove_blob(ContentHash2),
	Res3 = Storage:fold_blobs(SimpleListFunc, []),

	[?_assertEqual([ContentHash1, ContentHash2, ContentHash3], Res),
	 ?_assertEqual(ok, Res2),
	 ?_assertEqual([ContentHash1, ContentHash3], Res3)].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

store_loop(Storage, Token, _ContentBit, 1000) ->
	Storage:store_blob_end(Token);
store_loop(Storage, Token, ContentBit, N) ->
	Storage:store_blob(Token, ContentBit),
	store_loop(Storage, Token, ContentBit, N+1).

compute_size(Storage, Token, N) ->
	case Storage:get_blob(Token) of
		eof ->
			N;
		{ok, Data} ->
			CurrentSize = size(Data),
			compute_size(Storage, Token, N + CurrentSize);
		Other ->
			Other
	end.
