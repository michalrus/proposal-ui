# Proposal Update UI Tool

## Setup

### Lorri, Direnv and Nix Shell
* Git clone this repo and change directory into the top level repo directory.
* If [Lorri](https://github.com/target/lorri) or [Direnv](https://direnv.net/) are installed, execute `direnv allow` and allow the project to build for the first time.
* Lorri will be used preferentially over Direnv if both are installed.
* If neither Lorri or Direnv are installed, build the project by running `nix-shell`.
* Run `proposal-ui` once the project has successfully built and becomes available in the shell.
* This command is a bash wrapper to the Haskell proposal UI tool and will present interactive dialog boxes to select a credentials file for sourcing prior to launching the actual Haskell proposal UI tool.

### Nix build
* To build only the Haskell proposal UI utility, run:
```
# Build the Haskell proposal UI tool
nix-build -A proposal-ui

# By default, the output will be located at:
# result/bin/proposal-ui
```
* Keep in mind, building and running the Haskell binary only will not prompt for and source appropriate credential files for bucket pushes; this will still need to be done manually.

### Dev shell
* For a Haskell environment dev shell with `ghc` and `ghci` available, run:
```
nix-shell shell-dev.nix
```
### State
* The proposal UI wrapper with credentials file selector will automatically default to selection from a `static` directory if one exists.
