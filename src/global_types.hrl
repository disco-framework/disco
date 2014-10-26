%% -------------------------------------------------------------------
%% Contains global data type definitions.
%% -------------------------------------------------------------------

-type worker_id() :: atom().

-type worker_spec() :: {WorkerID     :: worker_id(),
                        IP           :: atom(),
                        RankingGroup :: string()}.

-type score_mode() :: raw | normalized | ranked.
