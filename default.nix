let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs-proposal-ui { config.allowBroken = true; };
  hsOverlay = self: super: {
    prometheus = pkgs.haskell.lib.doJailbreak super.prometheus;
    amazonka-core = pkgs.haskell.lib.appendPatch super.amazonka-core ./patches/amazonka-content-length.patch;
  };
  myHsPkgs = pkgs.haskellPackages.extend hsOverlay;
in {
  inherit pkgs;
  proposal-ui = myHsPkgs.callCabal2nix "proposal-ui" ./src {};
}
