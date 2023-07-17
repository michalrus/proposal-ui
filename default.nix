let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs { config.allowBroken = true; };
  hsOverlay = self: super: {
    prometheus = pkgs.haskell.lib.doJailbreak super.prometheus;
    brick = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "brick" (pkgs.fetchFromGitHub {
      owner = "jtdaugherty"; repo = "brick";
      rev = "0.47.1";
      sha256 = "0lh5wnc9i1f61i77yx6pnmxbxbaclfmmysx3zj77kps9p976nydy";
    }) {});
    amazonka-core = pkgs.haskell.lib.doJailbreak (
      pkgs.haskell.lib.appendPatch (
        pkgs.haskell.lib.appendPatch super.amazonka-core ./patches/amazonka-core--new-aeson.patch
      ) ./patches/amazonka-content-length.patch
    );
    amazonka = pkgs.haskell.lib.doJailbreak (
      pkgs.haskell.lib.appendPatch super.amazonka ./patches/amazonka--new-aeson.patch
    );
  };
  myHsPkgs = pkgs.haskellPackages.extend hsOverlay;
in rec {
  inherit pkgs myHsPkgs;
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
