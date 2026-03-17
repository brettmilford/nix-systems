{ inputs }:
let
  mkOverlay =
    nixpkgsInput: packageNames: final: prev:
    let
      overlaid = import nixpkgsInput {
        system = prev.system;
        inherit (prev) config;
      };
    in
    builtins.listToAttrs (
      map (name: {
        inherit name;
        value = overlaid.${name};
      }) packageNames
    );

  overlayList = [
    (final: prev: {
      # available as pkgs.unstable....
      unstable = (prev.unstable or { }) // {
        podman = (import inputs.nixpkgs-unstable {
          system = prev.system;
          inherit (prev) config;
        }).podman;
      };
    })

    # Overlay directly
    (mkOverlay inputs.nixpkgs-24-11 [
      "unifi8"
    ])

    (mkOverlay inputs.nixpkgs-unstable [
      "claude-code"
      "emacs-macport"
    ])
  ];
in
# Combine all overlays into a single overlay function
final: prev: builtins.foldl' (acc: overlay: acc // (overlay final prev)) { } overlayList
