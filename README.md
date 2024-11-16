# tick-tock-tui

> TICK TOCK NEXT ₿LOCK

TUI app to handle Bitcoin data provided by [Mempool REST API](https://mempool.space/docs/api/rest) incl. blocks, fees and price converter. Connect to your own Mempool instance if you like.

## Preview

![ttt](https://github.com/user-attachments/assets/ca790c1d-a29d-4913-9e96-814801167893)


## ⚙️ Installation

TODO


## 🔧 Arguments

```bash
tick-tock-tui --help

Usage: tick-tock-tui [-m|--mempool URL] [-r|--refresh SECONDS]

  TUI app to handle Bitcoin data provided by Mempool: fees, blocks and price
  converter.

Available options:
  -m,--mempool URL         Mempool URL (default: "https://mempool.space")
  -r,--refresh SECONDS     Interval to auto-reload data in seconds
                           (default: 180)
  -h,--help                Show this help text

```

## Build

TODO

## FAQ

#### Are there any data stored locally, e.g. an application state?

Yes. Partial data (not all) of latest application state is stored locally at following directories
defined by [`XdgState`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:XdgState):
- non-Windows `~/.local/state/tick-tock-tui/data{version}.json`
- Windows: `%LOCALAPPDATA%` (e.g. `C:/Users/<user>/AppData/Local/tick-tock-tui/data{version}.json`)

## License

[MIT License](./LICENSE)
