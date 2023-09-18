{ config, lib, pkgs, ... }:

{
  imports = [
    ./alacritty.nix
    ./emacs.nix
    ./tmux.nix
  ];
}
