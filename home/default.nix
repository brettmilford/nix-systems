{ config, lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./emacs.nix
    ./tmux.nix
  ];
}
