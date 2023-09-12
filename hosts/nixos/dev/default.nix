{ config, lib, pkgs, ... }:

{
  imports = [
    ../common.nix
    ../cloud.nix
  ];

  networking.hostName = "dev";
  networking.hostId = "25f4937c";
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
  system.stateVersion = "23.11";

}
