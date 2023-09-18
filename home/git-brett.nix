{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    extraConfig = {
      gitreview.username = "brett";
      gitubuntu.lpuser = "brettmilford";
      url = {
        "git+ssh://brettmilford@git.launchpad.net/" = {
          insteadOf = "lp:";
        };
      };
    };
    userEmail = "brettmilford@gmail.com";
    userName = "Brett Milford";
  };
}
