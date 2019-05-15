with import (builtins.fetchTarball https://github.com/nixos/nixpkgs/archive/34aa254f9eb.tar.gz) {};
haskellPackages.callCabal2nix "proposal-ui" ./. {}
