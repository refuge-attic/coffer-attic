-module(coffer_simple_storage).
-behaviour(coffer_storage).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(DEFAULT_REPO_HOME, "./data").
-define(DEFAULT_CHUNK_SIZE, 4096).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, stop/0]).
-export([init_storage/1]).
-export([get_blob_init/1, get_blob/1, get_blob_end/1]).
-export([store_blob_init/1, store_blob/2, store_blob_end/1]).
-export([get_blob_content/1]).
-export([store_blob_content/2]).
-export([remove_blob/1]).
-export([fold_blobs/2]).
-export([exists/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
	gen_server:call(?MODULE, {stop}).

init_storage(Options) ->
	gen_server:call(?MODULE, {init_storage, Options}).

get_blob_init(Id) ->
	gen_server:call(?MODULE, {get_blob_init, Id}).

get_blob(Ref) ->
	gen_server:call(?MODULE, {get_blob, Ref}).

get_blob_end(Ref) ->
	gen_server:call(?MODULE, {get_blob_end, Ref}).

get_blob_content(Id) ->
	gen_server:call(?MODULE, {get_blob_content, Id}).

store_blob_init(Id) ->
	gen_server:call(?MODULE, {store_blob_init, Id}).

store_blob(Ref, Data) ->
	gen_server:call(?MODULE, {store_blob, Ref, Data}).

store_blob_end(Ref) ->
	gen_server:call(?MODULE, {store_blob_end, Ref}).

store_blob_content(Id, Data) ->
	gen_server:call(?MODULE, {store_blob_content, Id, Data}).

remove_blob(Id) ->
	gen_server:call(?MODULE, {remove_blob, Id}).

fold_blobs(Func, InitState) ->
	gen_server:call(?MODULE, {fold_blobs, Func, InitState}).

exists(Id) ->
	gen_server:call(?MODULE, {exists, Id}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(access, {id, iodevice}).
-record(config, {repo_home, chunk_size}).
-record(state, {config, references}).

init(Properties) ->

	% read the configuration
	RepoHome = proplists:get_value(repo_home, Properties, ?DEFAULT_REPO_HOME),
	ChunkSize = proplists:get_value(chunk_size, Properties, ?DEFAULT_CHUNK_SIZE),
	Config = #config{repo_home=RepoHome, chunk_size=ChunkSize},

	% see if the repository needs to be created
	maybe_init_repo(RepoHome),

	% create a reference storage
	References = ets:new(simple_storage_refs, [set]),

	State = #state{config=Config, references=References},
	{ok, State}.

handle_call({stop}, _From, State) ->
	{stop, normal, ok, State};
handle_call({init_storage, Options}, _From, #state{config=Config, references=References}=State) ->
	Reply = do_init_storage(Config, References, Options),
	{reply, Reply, State};
handle_call({get_blob_init, Id}, _From, #state{config=Config, references=References}=State) ->
	Reply = do_start_get(Id, Config, References),
	{reply, Reply, State};
handle_call({get_blob, Ref}, _From, #state{config=Config, references=References}=State) ->
	Reply = do_get(Ref, Config, References),
	{reply, Reply, State};
handle_call({get_blob_end, Ref}, _From, #state{references=References}=State) ->
	Reply = do_get_end(Ref, References),
	{reply, Reply, State};
handle_call({get_blob_content, Id}, _From, #state{config=Config, references=References}=State) ->
	Reply = do_get_blob_content(Id, Config, References),
	{reply, Reply, State};
handle_call({store_blob_init, Id}, _From, #state{config=Config, references=References}=State) ->
	Reply = do_start_store(Id, Config, References),
	{reply, Reply, State};
handle_call({store_blob, Ref, Data}, _From, #state{references=References}=State) ->
	Reply = do_store(Ref, Data, References),
	{reply, Reply, State};
handle_call({store_blob_end, Ref}, _From, #state{references=References}=State) ->
	Reply = do_store_end(Ref, References),
	{reply, Reply, State};
handle_call({store_blob_content, Id, Data}, _From, #state{config=Config, references=References}=State) ->
	Reply = do_store_blob_content(Id, Config, References, Data),
	{reply, Reply, State};
handle_call({remove_blob, Id}, _From, #state{config=Config}=State) ->
	Reply = do_remove(Id, Config),
	{reply, Reply, State};
handle_call({fold_blobs, Func, InitState}, _From, #state{config=Config}=State) ->
	Reply = do_fold_blobs(Func, InitState, Config),
	{reply, Reply, State};
handle_call({exists, Id}, _From, #state{config=Config}=State) ->
	Reply = do_exists(Id, Config),
	{reply, Reply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

maybe_init_repo(RepoHome) ->
	case file:make_dir(RepoHome) of
		ok ->
			ok;
		{error, eexist} ->
			%% TODO add here something to scan the directory
			ok;
		Error ->
			throw({error, cant_init_simple_storage, Error})
	end.

do_init_storage(Config, References, _Options) ->
	RepoHome = Config#config.repo_home,

	% first remove any blob
	filelib:fold_files(
		RepoHome,
		".+",
		true,
		fun(Filename, _Acc) ->
			case file:delete(Filename) of
				ok -> ok;
				{error, Reason} -> lager:error("OOPS file ~p: ~p~n", [Filename, Reason])
			end
		end,
		[]),
	
	% then remove their directories
	Dirs = filelib:wildcard("*", RepoHome),
	lists:foldl(
		fun(Dir, _Acc) ->
			case file:del_dir(RepoHome ++ "/" ++ Dir) of
				ok -> ok;
				{error, Reason} -> lager:error("OOPS dir ~p: ~p~n", [Dir, Reason])
			end
		end,
		[],
		Dirs
	),

	% clear the refs
	ets:delete_all_objects(References),

	ok.

do_start_get(Id, Config, References) ->
	RepoHome = Config#config.repo_home,
	Filename = content_full_location(RepoHome, Id),
	case filelib:is_file(Filename) of
		false ->
			{error, not_exist};
		true  ->
			Ref = make_ref(),
			{ok, IoDevice} = file:open(Filename, [read, binary]),
			Access = #access{ id=Id, iodevice=IoDevice },
			ets:insert(References, {Ref, Access}),
			{ok, Ref}
	end.

do_get(Ref, Config, References) ->
	ChunkSize = Config#config.chunk_size,
	case ets:match(References, {Ref, '$1'}) of
		[] ->
			{error, wrong_ref};
		[[#access{iodevice=IoDevice}]] ->
			case file:read(IoDevice, ChunkSize) of
				{ok, Data} ->
					{ok, Data};
				eof ->
					eof;
				{error, Reason} ->
					{error, Reason}
			end;
		_ ->
			{error, bad_state}
	end.

do_get_end(Ref, References) ->
	case ets:match(References, {Ref, '$1'}) of
		[] ->
			{error, wrong_ref};
		[[#access{iodevice=IoDevice}]] ->
			file:close(IoDevice),
			ets:delete(References, Ref),
			ok;
		_ ->
			{error, bad_state}
	end.

%%

do_start_store(Id, Config, References) ->
	RepoHome = Config#config.repo_home,
	Filename = content_full_location(RepoHome, Id),
	maybe_create_directory(RepoHome, content_directory(Id)),
	Ref = make_ref(),
	{ok, IoDevice} = file:open(Filename, [write, binary]),
	Access = #access{id=Id, iodevice=IoDevice},
	ets:insert(References, {Ref, Access}),
	{ok, Ref}.

do_store(Ref, Data, References) ->
	case ets:match(References, {Ref, '$1'}) of
		[] ->
			{error, wrong_ref};
		[[#access{iodevice=IoDevice}]] ->
			case file:write(IoDevice, Data) of
				ok ->
					ok;
				{error, Reason} ->
					{error, Reason}
			end;
		_ ->
			{error, bad_state}
	end.

do_store_end(Ref, References) ->
	case ets:match(References, {Ref, '$1'}) of
		[] ->
			{error, wrong_ref};
		[[#access{iodevice=IoDevice}]] ->
			file:close(IoDevice),
			ets:delete(References, Ref),
			ok;
		_ ->
			{error, bad_state}
	end.

%%

do_get_blob_content(Id, Config, References) ->
	{ok, Ref} = do_start_get(Id, Config, References),
	iterate_over_data(Ref, Config, References, do_get(Ref, Config, References), []).

do_store_blob_content(Id, Config, References, Data) ->
	{ok, Ref} = do_start_store(Id, Config, References),
	do_store(Ref, Data, References),
	do_store_end(Ref, References).

%%

do_remove(Id, Config) ->
	RepoHome = Config#config.repo_home,
	Filename = content_full_location(RepoHome, Id),
	case file:delete(Filename) of
		ok ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

do_fold_blobs(Func, InitState, Config) ->
	RepoHome = Config#config.repo_home,

	ProcessFilename = fun(Filename, Acc) ->
		Elements = string:tokens(Filename, "/"),
		Length = length(Elements),
		Id = lists:nth(Length - 1, Elements) ++ lists:nth(Length, Elements),
		Func(Id, Acc)
	end,

	filelib:fold_files(
		RepoHome,
		".+",
		true,
		ProcessFilename,
		InitState
	).

do_exists(Id, Config) ->
	RepoHome = Config#config.repo_home,
	Filename = content_full_location(RepoHome, Id),
	filelib:is_file(Filename).

%%

content_directory(Id) ->
    string:sub_string(Id, 1,2).

content_filename(Id) ->
    string:sub_string(Id, 3).

content_location(Id) ->
    content_directory(Id) ++ "/" ++ content_filename(Id).
    
content_full_location(RepoHome, Id) ->
	RepoHome ++ "/" ++ content_location(Id).

maybe_create_directory(RepoHome, Path) ->
	FullPath = RepoHome ++ "/" ++ Path,
	file:make_dir(FullPath).

iterate_over_data(Ref, _Config, References, eof, Acc) ->
	do_get_end(Ref, References),
	{ok, list_to_binary(lists:reverse(Acc))};
iterate_over_data(Ref, Config, References, {ok, Data}, Acc) ->
	NewAcc = [Data | Acc],
	iterate_over_data(Ref, Config, References, do_get(Ref, Config, References), NewAcc);
iterate_over_data(Ref, _Config, References, {error, Reason}, _) ->
	do_get_end(Ref, References),
	{error, Reason}.
