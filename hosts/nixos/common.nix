{
  config,
  lib,
  pkgs,
  ...
}: {
  time.timeZone = "Australia/Brisbane";
  i18n.defaultLocale = "en_AU.UTF-8";

  environment.systemPackages = with pkgs; [
    gcc
    git
  ];

  environment.shellAliases = [
    nrl = "sudo nixos-rebuild switch --flake /etc/nixos";
    nup = "nix flake update /etc/nixos && nrl";
  ];

  programs.vim = {
    defaultEditor = true;
  };

  programs.git = {
    enable = true;
  };

  programs.mosh.enable = true;
}
