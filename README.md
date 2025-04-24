# tick-tock-tui

> TICK TOCK NEXT ₿LOCK

Get the latest [Bitcoin](https://bitcoin.org) data in your terminal: `blocks`, `fees`, `ratio` and `prices` incl. a `price` converter.

Most data is based on [Mempool REST API](https://mempool.space/docs/api/rest). To calculate the `BTC/Asset` ratio [Kraken Spot Rest API](https://docs.kraken.com/api/docs/rest-api/get-ticker-information) is used to get latest prices for other assets.

BTW: Connect `tick-tock-tui` to your own Mempool instance if you like. Check [FAQ](./#faq).

# Preview

_Side note:_ Theme colors depend on your terminal preferences.

## Fees

<a href="demo/fees.gif">
  <img alt="fees" src="demo/fees.gif" />
</a>

## Block

<a href="demo/block.gif">
  <img alt="block" src="demo/block.gif" />
</a>

## Converter

<a href="demo/converter.gif">
  <img alt="converter" src="demo/converter.gif" />
</a>

## Ratio

<a href="demo/ratio.gif">
  <img alt="ratio" src="demo/ratio.gif" />
</a>

## Dashboard

<a href="demo/dashboard.gif">
  <img alt="dashboard" src="demo/dashboard.gif" />
</a>


## Menu

<a href="demo/menu.gif">
  <img alt="menu" src="demo/menu.gif" />
</a>

# Installation

soon


# CLI

```sh
tick-tock-tui --help

Usage: tick-tock-tui [-m|--mempool URL] [-r|--refresh SECONDS]
                     [-s|--storage DIRECTORY] [-i|--ignore]

  Get the latest Bitcoin data in your terminal: fees, blocks, ratio and prices
  incl. a price converter. Most data is provided by Mempool. Connect to your own
  Mempool instance if you like. Latest prices of other assets come from Krakens
  Spot API.

Available options:
  -m,--mempool URL         Mempool URL (default: "https://mempool.space")
  -r,--refresh SECONDS     Interval to auto-reload data in seconds
                           (default: 180)
  -s,--storage DIRECTORY   Folder to store application state
                           (default: "~/.local/state/tick-tock-tui")
  -i,--ignore              Ignore previous stored application state to use
                           default data instead.
  -h,--help                Show this help text
```

# Development

## Requirements

### Nix (recommended):

Install [`Nix`](https://zero-to-nix.com/start/install). Enable [`flakes`](https://zero-to-nix.com/concepts/flakes).

`cd` into project directory to run `nix develop`. Check out available commands in a next chapter.

### Others:

Following needs to be installed:

- [Haskell](https://www.haskell.org)
- [cabal](https://cabal.readthedocs.io)
- [cabal-fmt](https://github.com/phadej/cabal-fmt)
- [fourmoulu](https://github.com/fourmolu/fourmolu)
- [just](https://just.systems)

When everything is installed, check out all the commands available below.

### Commands to build etc.

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

# FAQ

### How to connect to a custom Mempool instance?

Start the app with `-m` parameter:
```sh
cabal run tick-tock-tui -- -m {custom-mempool-url}
```

### Do I need an API key to get asset price data from Kraken's API?

Nope. Quote from [Kraken Support page](https://support.kraken.com/hc/en-us/articles/360000919966-How-to-create-an-API-key):

> "API keys are not required to call the market data (public) API endpoints, as the market data endpoints are not associated with any specific Kraken account."

### Is any data stored locally?

Yes, users settings are stored locally. That's needed to start the app with the latest user settings. Locations to persist data are defined by [`XdgState`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:XdgState):
- non-Windows `~/.local/state/tick-tock-tui/data{version}.json`
- Windows: `%LOCALAPPDATA%` (e.g. `C:/Users/<user>/AppData/Local/tick-tock-tui/data{version}.json`)

# License

[MIT License](./LICENSE)
