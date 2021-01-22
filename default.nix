let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs-proposal-ui { config.allowBroken = true; };
  hsOverlay = self: super: {
    prometheus = pkgs.haskell.lib.doJailbreak super.prometheus;
    amazonka-core = pkgs.haskell.lib.appendPatch super.amazonka-core ./patches/amazonka-content-length.patch;
  };
  myHsPkgs = pkgs.haskellPackages.extend hsOverlay;
in rec {
  inherit pkgs;
  proposal-ui = myHsPkgs.callCabal2nix "proposal-ui" ./src {};
  proposal-ui-wrapper = let
    deps = with pkgs; [ dialog proposal-ui ];
  in pkgs.runCommand "proposal-ui" {
    script = ./scripts/proposal-ui-wrapper.sh;
    nativeBuildInputs = [ pkgs.makeWrapper ];
    preferLocalBuild = true;
    allowSubstitues = false;
  } ''
    makeWrapper $script $out/bin/proposal-ui \
      --prefix PATH : ${pkgs.lib.makeBinPath deps}
  '';
}
