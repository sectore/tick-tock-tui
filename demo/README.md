# Demo

## Clean build

To have latest build, always clean and re-build sources before creating any demo:

```sh
cabal clean
cabal build
```

## Build demos

All demos will be created as animated `gif` in folder `demo` after running following commands (from `root` folder):

### Dashboard

```sh
just demo-dashboard
```

### Fees view

```sh
just demo-fees
```

### Block view

```sh
just demo-block
```

### Converter view

```sh
just demo-converter
```

### Ratio view

```sh
just demo-ratio
```

### Menu

```sh
just demo-menu
```
