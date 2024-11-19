# The `--fmt` command is currently unstable.

set unstable := true

default: run

alias b := build
alias f := format
alias fc := format-check
alias l := lint
alias t := test
alias r := run
alias rc := run-custom

# build app
build:
    cabal build --enable-tests

# run tests
test:
    cabal test

# format files
format:
    just --fmt
    fourmolu -i app src test
    cabal-fmt tick-tock-tui.cabal -i

# check formats
format-check:
    fourmolu --mode check app src test

# lint
lint:
    hlint app src test

# run app
run:
    cabal run tick-tock-tui

# run app with custom args (mempool url + seconds to refresh data)
run-custom url sec:
    cabal run tick-tock-tui -- -m {{ url }} -r {{ sec }}
