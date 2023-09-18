{ config, lib, pkgs, ... }:

{
  time.timeZone = "Australia/Brisbane";
  i18n.defaultLocale = "en_AU.UTF-8";
  environment.systemPackages = with pkgs; [
     git
  ];
  nix.settings.auto-optimise-store = true;
  programs.vim = {
    enable = true;
    defaultEditor = true;
  };
}
