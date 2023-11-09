{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      (modulesPath + "/profiles/headless.nix")
      ../common.nix
      ../cloud.nix
      ./postgresql.nix
      ./nextcloud.nix
    ];

  networking.hostName = "Calliope";
  networking.hostId = "25f4937c";
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

}
