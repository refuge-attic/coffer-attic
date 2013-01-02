-module(coffer_storage).

-include("includes/coffer.hrl").

-callback start_link(Args :: list()) ->
	pid().

-callback stop() ->
	ok.

-callback init_storage(Options :: list()) ->
	ok.

-callback get_blob_init(Id :: blob_id()) ->
	{ok, reference()} | {error, Reason :: atom()}.

-callback get_blob(Ref :: reference()) ->
	{ok, Data ::  data()} | eof | {error, Reason :: atom()}.

-callback get_blob_end(Ref :: reference()) ->
	ok | {error, Reason :: atom()}.

-callback store_blob_init(Id :: blob_id()) ->
	{ok, Ref :: reference()} | {error, Reason :: atom()}.

-callback store_blob(Ref :: reference(), Data :: data()) ->
	ok | {error, Reason :: atom()}.

-callback store_blob_end(Ref :: reference()) ->
	ok | {error, Reason :: atom()}.

-callback remove_blob(Id :: blob_id()) ->
	ok | {error, Reason :: atom()}.

-callback fold_blobs(Func :: fun((Id :: blob_id(), Acc :: any()) -> any()), InitState :: any()) ->
	any().

-callback exists(Id :: blob_id()) ->
	boolean().