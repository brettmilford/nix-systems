{ config, lib, pkgs, ... }:

{
  time.timeZone = "Australia/Brisbane";
  i18n.defaultLocale = "en_AU.UTF-8";
  environment.systemPackages = with pkgs; [
     vim
     screen
  ];
  nix.settings.auto-optimise-store = true;
}
