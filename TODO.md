# TODOs


## should be done soon

- Better error messages on framework startup problems
  - e.g. missing .beam files on client (code distribution errors)
  - consider all possible errors on framework startup
  - ssh errors


## should be done at some point

- create a convention, where calls to get_env are allowed and refactor code accordingly
- move list of problem specifications out of main config file into a file that can be (re)loaded at runtime
- simple (ping) test of communication with components on startup. answer syntactically correct? Goal: Better eedback for component developers
- allow to deactivate all testing of components on startup (via config file) to speed up system startup
- rename "barkeeper" to "input generator"


## could make sense later

- find out whether heartbeat monitoring could be useful to detect communication errors / crashed machines
- clean up JSON communication protocols
  - "worker updated" message from dj to gui
    - contains complex type for value "blocked" ("no" | {"idx": integer()}). Make this easier to parse and process
    - value "proposition" is nullable, "caption" and score are not. Either all three should be nullable or none of them.


## not sure if these make sense at all

- add "globally valid timestamp" (first define what that is) to worker propositions
  (could allow to rank on time taken if score is the same for two workers)
  - maybe just use "time since round started", do not try to use a universal clock
- count number of proposition per worker and forward to validator so the score can be calculated based on that
- use memoization so the validator does not validate the same proposition twice (from same or different worker)
  - could be done in validator_queue module
- try to get rid of the worker app by starting an "empty" erlang noe on the worker machine and then sending a function via erlang message that contains the worker wrapper logic
- support windows (using Cygwin, MinGW or similar)
  - easy_open_killer_port does not work currently because it uses "kill -9" (at least with MinGW)
  - could perhaps be realised via taskkill.exe (http://ss64.com/nt/taskkill.html). Call: "taskkill /F /T /PID {PID}"
