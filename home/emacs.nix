{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    binutils
    git
    (ripgrep.override { withPCRE2 = true; })
    gnutls
    fd
    imagemagick
    pinentry_emacs
    zstd
    nixfmt
    editorconfig-core-c
    sqlite
    clang
    (aspellWithDicts (d: [d.en]))
    emacs-all-the-icons-fonts
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    terminal-notifier
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    xclip
  ];

  programs.emacs = {
    enable = true;
    package = ((pkgs.emacsPackagesFor pkgs.emacsNativeComp).emacsWithPackages
        (epkgs: [ epkgs.vterm ]));
  };

  home = {
    sessionVariables = {
      EMACSDIR = "${config.xdg.configHome}/emacs";
      DOOMDIR = "${config.xdg.configHome}/doom";
      DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
      XDG_CONFIG_HOME = "${config.xdg.configHome}";
    };
    sessionPath = [ "${config.xdg.configHome}/emacs/bin"];
  };

  xdg = {
    enable = true;
    configFile = {
      "doom" = {
        source = ../config/doom/.config/doom;
        recursive = true;
        onChange = "${pkgs.writeShellScript "doom-change" ''
          export PATH="$PATH:$HOME/.nix-profile/bin"
          export DOOMDIR="${config.home.sessionVariables.DOOMDIR}"
          export DOOMLOCALDIR="${config.home.sessionVariables.DOOMLOCALDIR}"
          export EMACSDIR="${config.home.sessionVariables.EMACSDIR}"
          export XDG_CONFIG_HOME="${config.home.sessionVariables.XDG_CONFIG_HOME}";
          if [ ! -d "EMACSDIR" ]; then
            git clone --depth 1 https://github.com/doomemacs/doomemacs.git $EMACSDIR
          fi
          if [ ! -d "$DOOMLOCALDIR" ]; then
            ${config.xdg.configHome}/emacs/bin/doom --force install
          else
            ${config.xdg.configHome}/emacs/bin/doom sync -u
          fi
        ''}";

      };
    };
  };

  ### TODO: how to use nix-darwin's services.emacs here?
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
