{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ripgrep
    fd
    (aspellWithDicts (d: [d.en]))
    terminal-notifier
  ];

  programs.emacs = {
    enable = true;
    #package = pkgs.emacsNativeComp;
  };

  home.file.doom-emacs = {
    source = ../config/doom;
    recursive = true;
    target = ".config/doom";
    onChange = ".emacs.d/bin/doom sync";
  };

  ## TODO: how to use nix-darwin's services.emacs here?
  # services.emacs = {
  #   enable = true;
  # };
  # 
  # launchd.user.agents.emacs = {
  #   environment = {
  #     COLORTERM = "truecolor";
  #   };
  # };

}
