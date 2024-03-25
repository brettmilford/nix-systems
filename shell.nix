{pkgs ? import <nixpkgs> {}}:
with pkgs; let
  nixBin = writeShellScriptBin "nix" ''
    ${nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
  '';
in
  mkShell {
    buildInputs = [
      git
      nix-zsh-completions
      yubikey-manager
      age-plugin-yubikey
    ];
    shellHook = ''
      export FLAKE="$(pwd)"
      export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
    '';
  }
