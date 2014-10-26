
-define(DEBUG_MODULE(Module), lager:trace_console([{module, Module}], debug)).

-define(DEBUG_THIS_MODULE, ?DEBUG_MODULE(?MODULE)).

-define(DEBUG_ALL, ?DEBUG_MODULE('*')).
