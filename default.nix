with import <nixpkgs> {};
haskellPackages.callCabal2nix "proposal-ui" ./. {}
