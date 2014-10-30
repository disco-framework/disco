# The DisCo Framework

A framework for hosting language-agnostic programming contests with distributed live shootouts (**Dis** tributed **Co** ntest shootout framework).

The framework itself is written in Erlang. To use the framework however you should not need any knowledge of the Erlang language.

## Understand

The DisCo framework is based on two architectural ideas.

1. Distribution: let all programs from participants (workers) run at the same time on separate machines to get a live shootout. The workers are wrapped by erlang programs (worker wrappers) on the client machines. There is a central machine to coordinate the contest and display the results.

2. Components: The framework is adapted to a specific contest by replacing executables (components). There are components for GUI (visualization & control) as well as worker output validation and worker input generation.

The components can be written in any language, so no Erlang knowledge is needed to use and adapt the framework.

## Use

A detailed user's guide in german is included, this is only a short overview for people not fluent in german. If anything important is missing, please just open an issue and we will add an explanation.

To use the DisCo framework for your own contest, you should provide at least your own validator component. Other components can be replaced if you want to fine tune functionality.

### Prerequisites

 - Linux
 - Erlang R16B or 17
 - `make`
 - `git` (to automatically download erlang dependencies)
 - ssh client (e.g. `openssh`)
 - `ip` (to find out local ip address on the server)
 - `rsync`
 - `python`, `pyqt4-dev-tools` (for general gui component)
 - `python3` (only for the integration tests)

### Configuration

Look at the documented sample config files for the contests 'squares' and 'countdown2'.

### Execution

Make sure that the required components and workers for the desired contest are available and executable.
In case of a sample contest, they can be build with `make all` in the corresponding directory (e.g. `make all -C priv/countdown2`).

Then simply run:

    make run

## Contribute

### General

If you want to add features, fix bugs or make other modifications please

 - include tests for your modifications
 - ensure that all tests are running (see below)
 - create a pull request

### Running Tests

All these should succeed:

- typechecks
- unit tests
- integration tests

executed via

    make dialyze
    make test
    make integration-test

### Project structure

Important folders

 - `src` - Sources of the central erlang server
 - `apps/worker/src` - Sources of the erlang client (worker wrapper)
 - `doc` - Developer documentation (server, generated by `make doc`)
 - `apps/worker/doc` - Developer documentation (client, generated by `make doc`)
 - `doc/users-guide` - User's guide (in german, generated by `make -sC doc/users-guide`)
 - `priv` - External components and participant workers
   - `general` - contest-independent default components
   - `countdown2`, `squares` - components for different example contests
   - `integration-test` - components for integration tests
 - `scripts` - miscellanous helper scripts
