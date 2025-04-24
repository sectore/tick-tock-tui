# The `--fmt` command is currently unstable.

set unstable := true

default:
    @just --list

alias b := build

# build app
[group('build')]
build:
    cabal build --enable-tests

alias t := test

# run tests
[group('test')]
test:
    cabal test

alias f := format

# format files
[group('misc')]
format:
    just --fmt
    fourmolu -i app src test
    cabal-fmt tick-tock-tui.cabal -i

alias fc := format-check

# check formats
[group('misc')]
format-check:
    fourmolu --mode check app src test

alias l := lint

# lint
[group('misc')]
lint:
    hlint app src test

alias r := run

# run app
[group('dev')]
run:
    cabal run tick-tock-tui

# run app with custom args (mempool url + seconds to refresh data)
[group('dev')]
run-custom url sec:
    cabal run tick-tock-tui -- -m {{ url }} -r {{ sec }}

# demos

alias dd := demo-dashboard

# build demo: dashboard
[group('demo')]
demo-dashboard:
    vhs demo/dashboard.tape

alias df := demo-fees

# build demo: fees
[group('demo')]
demo-fees:
    vhs demo/fees.tape

alias db := demo-block

# build demo: block
[group('demo')]
demo-block:
    vhs demo/block.tape

alias dc := demo-converter

# build demo: converter
[group('demo')]
demo-converter:
    vhs demo/converter.tape

alias dr := demo-ratio

# build demo: ratio
[group('demo')]
demo-ratio:
    vhs demo/ratio.tape

alias dm := demo-menu

# build demo: menu
[group('demo')]
demo-menu:
    vhs demo/menu.tape
