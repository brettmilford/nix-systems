{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../common.nix
      ../cloud.nix
      ../desktop.nix
      ../zerotierone.nix
    ];

  networking.hostName = "orpheus";
  networking.hostId = "19a6d028";
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

}
