{ config, lib, pkgs, ... }:

{
  services.xserver.displayManager.lightdm.enable = true;
  services.displayManager.defaultSession = "xfce";
  services.xserver.desktopManager.xfce.enable = true;

}
