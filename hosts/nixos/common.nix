{ config, lib, pkgs, ... }:

{
  time.timeZone = "Australia/Brisbane";
  i18n.defaultLocale = "en_AU.UTF-8";
  environment.systemPackages = with pkgs; [
    gcc
    git
  ];
  programs.vim = {
    defaultEditor = true;
  };

  programs.git = {
    enable = true;
    config = {
      userEmail = "root@hostname.com";
      userName = "Root";
    };
  };
}
