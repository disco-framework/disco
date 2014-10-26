%% -------------------------------------------------------------------
%% Contains data type definitions specific to the dj.
%% -------------------------------------------------------------------
-include("global_types.hrl").

-type problem() :: {Description   :: string(),
                    Specification :: string(),
                    AnswerTime    :: pos_integer(),
                    StartState    :: string()}.

%% loaded values from config
-record(config, {problems            = []  :: [problem()],
                 worker_call_timeout = 0   :: non_neg_integer(),
                 score_mode          = raw :: score_mode()}).

%% worker list item
-record(worker, {
          name                      = ""    :: string(),
          last_proposition          = none  :: string() | none,
          last_prop_score           = 0     :: integer(),
          last_prop_processed_score = 0     :: integer(),
          last_prop_caption         = ""    :: string(),
          problem_score             = 0     :: integer(),
          working                   = false :: boolean(),
          ranking_group             = ""    :: string(),
          blocked                   = no    :: no | {idx, non_neg_integer()}}).

%% state data
-record(state, {config        = #config{}  :: #config{},
                workers       = dict:new() :: dict(),
                problem_idx   = 0          :: non_neg_integer(),
                round         = 0          :: non_neg_integer(),
                problem_state = ""         :: string(),
                worker_input  = []         :: [string()]}).
