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
      unstable = (prev.unstable or { }) // {
        immich =
          (import inputs.nixpkgs-unstable {
            system = prev.system;
            inherit (prev) config;
          }).immich;
      };
    })

    (mkOverlay inputs.nixpkgs-24-11 [
      "unifi8"
    ])
  ];
in
# Combine all overlays into a single overlay function
final: prev: builtins.foldl' (acc: overlay: acc // (overlay final prev)) { } overlayList
