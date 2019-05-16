with import (builtins.fetchTarball https://github.com/nixos/nixpkgs/archive/bc94dcf5002.tar.gz) {};
haskellPackages.callCabal2nix "proposal-ui" ./. {}
