-include("../global_types.hrl").

-type proposition() :: {WorkerID         :: worker_id(),
                        WorkerInputLines :: [string()],
                        WorkerOutput     :: string()}.
