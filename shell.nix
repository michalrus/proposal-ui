let
  defaultNix = import ./.;
  inherit (defaultNix) pkgs proposal-ui;
in pkgs.mkShell {
  buildInputs = [
    proposal-ui
  ];
}
