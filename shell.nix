let
  defaultNix = import ./.;
  inherit (defaultNix) pkgs proposal-ui-wrapper;
in pkgs.mkShell {
  buildInputs = with pkgs; [
    proposal-ui-wrapper
  ];
}
