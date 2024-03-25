{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.git = {
    enable = true;
    userEmail = "brettmilford@gmail.com";
    userName = "Brett Milford";
  };
}
