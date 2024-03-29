{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    emacsNativeComp
    binutils
    git
    (ripgrep.override { withPCRE2 = true; })
    gnutls
    fd
    imagemagick
    pinentry-emacs
    zstd
    nixfmt
    editorconfig-core-c
    sqlite
    (aspellWithDicts (d: [d.en]))
    emacs-all-the-icons-fonts
    nodejs
    fontconfig
    nerdfonts
# development tools
    gnumake
    shellcheck
    go
    gopls
    gotools
    go-tools
    python311
    python311Packages.python-lsp-server
    python311Packages.python-lsp-ruff
    python311Packages.python-lsp-jsonrpc
    python311Packages.python-lsp-black
    python311Packages.pyls-isort
    python311Packages.pylsp-mypy
    python311Packages.pylsp-rope
    poetry
  ] ++ lib.optionals pkgs.stdenv.isDarwin [
    terminal-notifier
    coreutils-prefixed
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    xclip
  ];

  home = {
    sessionVariables = {
      EMACSDIR = "${config.xdg.configHome}/emacs";
      DOOMDIR = "${config.xdg.configHome}/doom";
      DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
      XDG_CONFIG_HOME = "${config.xdg.configHome}";
    };
    sessionPath = ["${config.xdg.configHome}/emacs/bin"];
  };

  xdg = {
    enable = true;
    configFile = {
      "doom" = {
        source = ../config/doom;
        recursive = true;
        onChange = "${pkgs.writeShellScript "doom-change" ''
          export PATH="$PATH:$HOME/.nix-profile/bin:/etc/profiles/per-user/${config.home.username}/bin"
          export DOOMDIR="${config.home.sessionVariables.DOOMDIR}"
          export DOOMLOCALDIR="${config.home.sessionVariables.DOOMLOCALDIR}"
          export EMACSDIR="${config.home.sessionVariables.EMACSDIR}"
          export XDG_CONFIG_HOME="${config.home.sessionVariables.XDG_CONFIG_HOME}";
          if [ ! -d "$EMACSDIR" ]; then
            git clone --depth 1 https://github.com/doomemacs/doomemacs.git $EMACSDIR
          fi
          if [ ! -d "$DOOMLOCALDIR" ]; then
            ${config.xdg.configHome}/emacs/bin/doom --force install
          else
            ${config.xdg.configHome}/emacs/bin/doom sync -e
          fi
        ''}";
      };
    };
  };

  home.file.".emacs.d/early-init.el".text = ''
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "${config.home.sessionVariables.DOOMLOCALDIR}")))
    (setenv "EMACSDIR" (expand-file-name (file-name-as-directory "${config.home.sessionVariables.EMACSDIR}")))
    (load (concat (expand-file-name (file-name-as-directory "${config.home.sessionVariables.EMACSDIR}")) "early-init.el") nil 'nomessage)
  '';
}
