-module(coffer_simple_storage).

-behaviour(gen_server).

-define(REPO_HOME, "/tmp/coffer_simple_storage").
-define(CHUNK_SIZE, 1024).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, stop/0]).
-export([init_storage/0, init_storage/1]).
-export([get_blob_init/1, get_blob/1, get_blob_end/1, get_blob_content/1]).
-export([store_blob_init/1, store_blob/2, store_blob_end/1]).
-export([remove_blob/1]).
-export([fold_blobs/2]).

%%

-record(access,
	{
	id,
	iodevice
	}
).

%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, {stop}).

%

init_storage() ->
	init_storage([]).

init_storage(Options) ->
	gen_server:call(?MODULE, {init, Options}).

%

get_blob_init(Id) ->
	gen_server:call(?MODULE, {start_get, Id}).

get_blob(Ref) ->
	gen_server:call(?MODULE, {get, Ref}).

get_blob_end(Ref) ->
	gen_server:call(?MODULE, {get_end, Ref}).

get_blob_content(Id) ->
	{ok, Ref} = get_blob_init(Id),
	iterate_over_data(Ref, get_blob(Ref), []).

iterate_over_data(Ref, eof, Acc) ->
	get_blob_end(Ref),
	{ok, list_to_binary(lists:reverse(Acc))};
iterate_over_data(Ref, {ok, Data}, Acc) ->
	NewAcc = [Data | Acc],
	iterate_over_data(Ref, get_blob(Ref), NewAcc);
iterate_over_data(Ref, {error, Reason}, _) ->
	get_blob_end(Ref),
	{error, Reason}.

%

store_blob_init(Id) ->
	gen_server:call(?MODULE, {start_store, Id}).

store_blob(Ref, Data) ->
	gen_server:call(?MODULE, {store, Ref, Data}).

store_blob_end(Ref) ->
	gen_server:call(?MODULE, {store_end, Ref}).
%

remove_blob(Id) ->
	gen_server:call(?MODULE, {remove, Id}).

%

fold_blobs(Func, InitState) ->
	gen_server:call(?MODULE, {fold, Func, InitState}).

%%

init(_Args) ->
	maybe_init_repo(),
	References = ets:new(simple_storage_refs, [set]),
	{ok, {References}}.

handle_call({stop}, _From, State) ->
	{stop, normal, ok, State};
handle_call({init, Options}, _From, {References}=State) ->
	Reply = do_init_storage(References, Options),
	{reply, Reply, State};
handle_call({start_get, Id}, _From, {References}=State) ->
	Reply = do_start_get(Id, References),
	{reply, Reply, State};
handle_call({get, Ref}, _From, {References}=State) ->
	Reply = do_get(Ref, References),
	{reply, Reply, State};
handle_call({get_end, Ref}, _From, {References}=State) ->
	Reply = do_get_end(Ref, References),
	{reply, Reply, State};
handle_call({start_store, Id}, _From, {References}=State) ->
	Reply = do_start_store(Id, References),
	{reply, Reply, State};
handle_call({store, Ref, Data}, _From, {References}=State) ->
	Reply = do_store(Ref, Data, References),
	{reply, Reply, State};
handle_call({store_end, Ref}, _From, {References}=State) ->
	Reply = do_store_end(Ref, References),
	{reply, Reply, State};
handle_call({remove, Id}, _From, State) ->
	Reply = do_remove(Id),
	{reply, Reply, State};
handle_call({fold, Func, InitState}, _From, State) ->
	Reply = do_fold_blobs(Func, InitState),
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
			throw({error, cant_init_simple_storage, Error})
	end.

do_init_storage(References, _Options) ->
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

	% clear the refs
	ets:delete_all_objects(References),

	ok.

do_start_get(Id, References) ->
	Filename = content_full_location(Id),
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

do_get(Ref, References) ->
	case ets:match(References, {Ref, '$1'}) of
		[] ->
			{error, wrong_ref};
		[[#access{iodevice=IoDevice}]] ->
			case file:read(IoDevice, ?CHUNK_SIZE) of
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

do_start_store(Id, References) ->
	Filename = content_full_location(Id),
	maybe_create_directory(content_directory(Id)),
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

do_remove(_Id) ->
	ok.

do_fold_blobs(Func, InitState) ->
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

%%

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
