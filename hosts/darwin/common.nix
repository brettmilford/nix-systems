{
  config,
  lib,
  pkgs,
  ...
}: {

  programs.gnupg.agent = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
  };

  services.emacs = {
    enable = true;
  };

  launchd.user.agents.emacs = {
    environment = {
      COLORTERM = "truecolor";
    };
  };
}
