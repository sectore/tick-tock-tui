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
alias ad := animated-demo
alias ap := animated-price

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

# animated demo
animated-demo:
    vhs demo/demo.tape
    ffmpeg -ss 2 -i demo/output.gif -vf "split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" demo/tick-tock-demo.gif -y
    rm demo/output.gif

# animated screen shot (mempool url + seconds to refresh data)
animated-price:
    vhs demo/price.tape
    ffmpeg -ss 4 -t 5 -i demo/output.gif -vf "split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" demo/tick-tock-price.gif -y
    rm demo/output.gif
