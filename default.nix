let
  pkgs = import (builtins.fetchTarball https://github.com/nixos/nixpkgs/archive/f1682a7f126d4d56dfbb96bb8c8c5582abb22828.tar.gz) { config.allowBroken = true; };
  hsOverlay = self: super: {
    prometheus = pkgs.haskell.lib.doJailbreak super.prometheus;
    amazonka-core = pkgs.haskell.lib.appendPatch super.amazonka-core ./amazonka-content-length.patch;
  };
  myHsPkgs = pkgs.haskellPackages.extend hsOverlay;
in
  myHsPkgs.callCabal2nix "proposal-ui" ./. {}
