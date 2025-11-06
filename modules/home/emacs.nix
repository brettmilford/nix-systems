{
  self,
  config,
  lib,
  pkgs,
  ...
}:
let
  emacsMacportWithPackages =
    with pkgs;
    (emacsPackagesFor (emacs-macport.override { withNativeCompilation = true; })).emacsWithPackages (
      epkgs: with epkgs; [
        treesit-grammars.with-all-grammars
        vterm
      ]
    );

  emacsWithPackages =
    with pkgs;
    (emacsPackagesFor (emacs.override { withNativeCompilation = true; })).emacsWithPackages (
      epkgs: with epkgs; [
        treesit-grammars.with-all-grammars
        vterm
      ]
    );

  doomemacs = pkgs.fetchFromGitHub {
    owner = "doomemacs";
    repo = "doomemacs";
    rev = "f9664ae058d67b8d97cb8a9c40744fefc3e5479f";
    hash = "sha256-voIvrHMgs2zFNtYDxVnyBpmSCE3NFZAhhcZsUneDMLw=";
  };

  doom-cli = pkgs.writeShellScriptBin "doom" ''
    export PATH="$PATH:${
      lib.makeBinPath [
        pkgs.git
        pkgs.ripgrep
      ]
    }"
    exec ${config.xdg.configHome}/emacs/bin/doom "$@"
  '';
in
{
  home.packages =
    with pkgs;
    [
      ## Emacs dependencies
      binutils
      ## Doom dependencies
      doom-cli
      git
      (ripgrep.override { withPCRE2 = true; })
      gnutls
      ## Doom optional
      fd
      imagemagick
      pinentry-emacs
      zstd

      ## Module dependencies
      cmake
      glibtool

      # :lang nix
      nixfmt-rfc-style

      editorconfig-core-c
      sqlite
      # :checkers spell
      (aspellWithDicts (d: [ d.en ]))
      texlive.combined.scheme-medium


      fontconfig
      iosevka-bin
      (iosevka-bin.override { variant = "Aile"; })
      (iosevka-bin.override { variant = "Etoile"; })
      (iosevka-bin.override { variant = "Slab"; })
      nerd-fonts.symbols-only
      graphviz

      # development tools
      nodejs
      gnumake
      shellcheck
      # Go
      go
      gopls
      go-tools
      # Python
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
      emacsMacportWithPackages
      terminal-notifier
      coreutils-prefixed
      #emacs29-macport
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      emacsWithPackages
      xclip
    ];

  fonts.fontconfig.enable = true;

  home = {
    sessionVariables = {
      EMACSDIR = "${config.xdg.configHome}/emacs";
      DOOMDIR = "${config.xdg.configHome}/doom";
      DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
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
      "emacs".source = doomemacs;
    };
  };

  home.file.".emacs.d/early-init.el".text = ''
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "${config.home.sessionVariables.DOOMLOCALDIR}")))
    (setenv "EMACSDIR" (expand-file-name (file-name-as-directory "${config.home.sessionVariables.EMACSDIR}")))
    (load (concat (expand-file-name (file-name-as-directory "${config.home.sessionVariables.EMACSDIR}")) "early-init.el") nil 'nomessage)
  '';
}
