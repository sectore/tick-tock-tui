# tick-tock-tui

> TICK TOCK NEXT ₿LOCK

TUI app to handle Bitcoin data provided by [Mempool REST API](https://mempool.space/docs/api/rest) incl. blocks, fees and price converter. Connect to your own Mempool instance if you like.

## Preview

![ttt](https://github.com/user-attachments/assets/ca790c1d-a29d-4913-9e96-814801167893)


## Installation ⚙️

soon


## Customize via arguments 🔧

```sh
tick-tock-tui --help

Usage: tick-tock-tui [-m|--mempool URL] [-r|--refresh SECONDS]
                     [-s|--storage DIRECTORY]

  TUI app to handle Bitcoin data provided by Mempool: fees, blocks and price
  converter.

Available options:
  -m,--mempool URL         Mempool URL (default: "https://mempool.space")
  -r,--refresh SECONDS     Interval to auto-reload data in seconds
                           (default: 180)
  -s,--storage DIRECTORY   Folder to store application state
                           (default: "~/.local/state/tick-tock-tui")
  -h,--help                Show this help text
```

## Local development

### Requirements

#### Nix (recommended):

Install [`Nix`](https://zero-to-nix.com/start/install). Enable [`flakes`](https://zero-to-nix.com/concepts/flakes).

After that, `cd` into project directory to run `nix develop`. Check out available commands in a next chapter.

#### Others:

Following needs to be installed:

- [Haskell](https://www.haskell.org)
- [cabal](https://cabal.readthedocs.io)
- [cabal-fmt](https://github.com/phadej/cabal-fmt)
- [fourmoulu](https://github.com/fourmolu/fourmolu)
- [just](https://just.systems)

After installing everyting, check out available commands in next chapter.

#### Commands to build etc.

```sh
just --list

Available recipes:
    build              # build app
    format             # format files
    format-check       # check formats
    lint               # lint
    run                # run app
    run-custom url sec # run app with custom args (mempool url + seconds to refresh data)
    test               # run tests
```

## FAQ

#### How to connect to a custom Mempool instance?

Start the app with `-m` parameter:
```sh
cabal run tick-tock-tui -- -m {custom-mempool-url}

```

#### Any data are stored locally?

Yes. To start the app with latest settings, some data (not all) of latest application state
is stored locally. Locations to store data are following [`XdgState`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:XdgState):
- non-Windows `~/.local/state/tick-tock-tui/data{version}.json`
- Windows: `%LOCALAPPDATA%` (e.g. `C:/Users/<user>/AppData/Local/tick-tock-tui/data{version}.json`)

## License

[MIT License](./LICENSE)
