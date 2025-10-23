{
  self,
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages =
    with pkgs;
    [
      binutils
      cmake
      glibtool
      git
      (ripgrep.override { withPCRE2 = true; })
      gnutls
      fd
      imagemagick
      pinentry-emacs
      zstd
      nixfmt-rfc-style
      editorconfig-core-c
      sqlite
      (aspellWithDicts (d: [ d.en ]))
      emacs-all-the-icons-fonts
      nodejs
      fontconfig
      nerd-fonts.iosevka
      texlive.combined.scheme-medium
      graphviz
      # development tools
      gnumake
      shellcheck
      go
      gopls
      go-tools
      (python3.withPackages (
        ps: with ps; [
          black
          isort
          mypy
          pyls-isort
          pylsp-mypy
          pylsp-rope
          python-lsp-black
          python-lsp-jsonrpc
          python-lsp-ruff
          python-lsp-server
          numpy
          poetry-core
          matplotlib
          scipy
        ]
      ))
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [
      terminal-notifier
      coreutils-prefixed
      emacs29-macport
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      emacs
      xclip
    ];

  home = {
    sessionVariables = {
      EMACSDIR = "${config.xdg.configHome}/emacs";
      DOOMDIR = "${config.xdg.configHome}/doom";
      DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
      XDG_CONFIG_HOME = "${config.xdg.configHome}";
    };
    sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];
  };

  xdg = {
    enable = true;
    configFile = {
      "doom" = {
        source = "${self}/config/doom";
        recursive = true;
      };
    };
  };

  home.file.".emacs.d/early-init.el".text = ''
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "${config.home.sessionVariables.DOOMLOCALDIR}")))
    (setenv "EMACSDIR" (expand-file-name (file-name-as-directory "${config.home.sessionVariables.EMACSDIR}")))
    (load (concat (expand-file-name (file-name-as-directory "${config.home.sessionVariables.EMACSDIR}")) "early-init.el") nil 'nomessage)
  '';
}
