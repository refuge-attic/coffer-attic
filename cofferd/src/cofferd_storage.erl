-module(cofferd_storage).

-include("includes/coffer.hrl").

%
% cofferd_storage is a backend implementation interface used by cofferd_manager.
%

-type storage_state() :: list().

-type blob_id() :: binary().
-type blob_data() :: binary().

-callback init(Args :: list()) ->
    {ok, State :: storage_state()} |
    {error, Reason :: atom()}.

-callback terminate(State :: storage_state()) ->
    ok.

-callback init_storage(State :: storage_state()) ->
    {ok, NewState :: storage_state()} |
    {error, Reason :: atom()}.

-callback get_blob_init(Id :: blob_id(), State :: storage_state()) ->
    {ok, BHandle :: pid(), NewState :: storage_state()} |
    {error, Reason :: atom()}.

-callback get_blob(BHandle :: pid(), State :: storage_state()) ->
    {ok, Data ::  data()} |
    eof |
    {error, Reason :: atom()}.

-callback get_blob_end(BHandle :: blob_handle()) ->
    ok | {error, Reason :: atom()}.

-callback store_blob_init(State :: storage_state(), Id :: blob_id()) ->
    {ok, BHandle :: blob_handle()} | {error, Reason :: atom()}.

-callback store_blob(State :: storage_state(), BHandle :: blob_handle(), Data :: data()) ->
    ok | {error, Reason :: atom()}.

-callback store_blob_end(State :: storage_state(), BHandle :: blob_handle()) ->
    ok | {error, Reason :: atom()}.

-callback remove_blob(State :: storage_state(), Id :: blob_id()) ->
    ok | {error, Reason :: atom()}.

-callback fold_blobs(State :: storage_state(), Func :: fun((Id :: blob_id(), Acc :: any()) -> any()), InitState :: any()) ->
    any().

-callback exists(State :: storage_state(), Id :: blob_id()) ->
    boolean().
