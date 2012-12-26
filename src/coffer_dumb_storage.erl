-module(coffer_dumb_storage).

-behaviour(gen_server).

-define(REPO_HOME, "/tmp/coffer_dumb_storage").
-define(CHUNK_SIZE, 1024).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, stop/0, init_storage/0, init_storage/1, get_blob/1, store_blob/2, remove_blob/1, fold_blobs/2]).

%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, {stop}).

init_storage() ->
	init_storage([]).

init_storage(Options) ->
	gen_server:call(?MODULE, {init, Options}).

get_blob(Id) ->
	gen_server:call(?MODULE, {get, Id}).

store_blob(Id, Bin) when is_binary(Bin) ->
	gen_server:call(?MODULE, {store_binary, Id, Bin});
store_blob(Id, Func) ->
	gen_server:call(?MODULE, {store_stream, Id, Func}).

remove_blob(Id) ->
	gen_server:call(?MODULE, {remove, Id}).

fold_blobs(Func, InitState) ->
	gen_server:call(?MODULE, {fold, Func, InitState}).

%%

init(Args) ->
	maybe_init_repo(),

	{ok, Args}.

handle_call({stop}, _From, State) ->
	{stop, normal, ok, State};
handle_call({init, Options}, _From, State) ->
	io:format("Initialising the dumb storage with options: ~p~n", [Options]),
	Reply = do_init_storage(Options),
	{reply, Reply, State};
handle_call({get, Id}, _From, State) ->
	io:format("getting the blob with id: ~p~n", [Id]),
	Reply = get_blob_directly(Id),
	{reply, Reply, State};
handle_call({store_binary, Id, Bin}, _From, State) ->
	io:format("Storing the blob with id: ~p and Bin: ~p~n", [Id, Bin]),
	Reply = store_binary_content(Id, Bin),
	{reply, Reply, State};
handle_call({store_stream, Id, Func}, _From, State) ->
	io:format("Storing the blob with id: ~p and func: ~p~n", [Id, Func]),
	{reply, {ok, Id}, State};
handle_call({remove, Id}, _From, State) ->
	io:format("Removing the blob with id: ~p~n", [Id]),
	{reply, ok, State};
handle_call({fold, Func, InitState}, _From, State) ->
	Reply = fold_all_blobs(Func, InitState),
	{reply, Reply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%

maybe_init_repo() ->
	case file:make_dir(?REPO_HOME) of
		ok ->
			ok;
		{error, eexist} ->
			%% TODO add here something to scan the directory
			ok;
		Error ->
			throw({error, cant_init_dumb_storage, Error})
	end.

do_init_storage(_Options) ->
	% first remove any blob
	filelib:fold_files(
		?REPO_HOME,
		".+",
		true,
		fun(Filename, _Acc) ->
			case file:delete(Filename) of
				ok -> ok;
				{error, Reason} -> io:format("OOPS file ~p: ~p~n", [Filename, Reason])
			end
		end,
		[]),
	
	% then remove their directories
	Dirs = filelib:wildcard("*", ?REPO_HOME),
	lists:foldl(
		fun(Dir, _Acc) ->
			case file:del_dir(?REPO_HOME ++ "/" ++ Dir) of
				ok -> ok;
				{error, Reason} -> io:format("OOPS dir ~p: ~p~n", [Dir, Reason])
			end
		end,
		[],
		Dirs
	),
	ok.

content_directory(Id) ->
    string:sub_string(Id, 1,2).

content_filename(Id) ->
    string:sub_string(Id, 3).

content_location(Id) ->
    content_directory(Id) ++ "/" ++ content_filename(Id).
    
content_full_location(Id) ->
	?REPO_HOME ++ "/" ++ content_location(Id).

maybe_create_directory(Path) ->
	FullPath = ?REPO_HOME ++ "/" ++ Path,
	file:make_dir(FullPath).

store_binary_content(Id, Bin) ->
	maybe_create_directory(content_directory(Id)),
	
	Filename = content_full_location(Id),
	case file:write_file(Filename, Bin) of
		ok ->
			{ok, Id};
		{error, Reason} ->
			{error, Reason}
	end.

get_blob_directly(Id) ->
	Filename = content_full_location(Id),
	{ok, Bin} = file:read_file(Filename),
	{ok, Bin}.

fold_all_blobs(Func, InitState) ->
	ProcessFilename = fun(Filename, Acc) ->
		Elements = string:tokens(Filename, "/"),
		Length = length(Elements),
		Id = lists:nth(Length - 1, Elements) ++ lists:nth(Length, Elements),
		Func(Id, Acc)
	end,

	filelib:fold_files(
		?REPO_HOME,
		".+",
		true,
		ProcessFilename,
		InitState
	).
