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

  environment.shellAliases = {
    nrs = "sudo nixos-rebuild switch --flake /etc/nixos?submodules=1";
    nup = "nix flake update /etc/nixos && nrs";
  };

  programs.vim = {
    defaultEditor = true;
  };

  programs.git = {
    enable = true;
  };

  programs.mosh.enable = true;
}
