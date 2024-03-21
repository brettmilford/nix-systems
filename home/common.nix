{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs;
    [
      direnv
      qemu
      git-review
      tmux
      jq
      remarshal
      texlive.combined.scheme-basic
      ansible
      sshpass
      (pass.withExtensions (ext: [ext.pass-otp]))
      podman
      kubectl
      kubernetes-helm
      bitwarden-cli
      ncdu
      xz
      gh
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      nextcloud-client
    ];

  home.extraOutputsToInstall = ["doc" "info" "devdoc"];

  home.shellAliases = {
    em = "emacs -nw";
    emc = "emacsclient -tcq -a \"\"";
    t = "tmux";
    g = "git ";
    o = "openstack ";
    k = "kubectl ";
    kx = "f() { [ \"$1\" ] && kubectl config use-context $1 || kubectl config current-context ; } ; f";
    kn = "f() { [ \"$1\" ] && kubectl config set-context --current --namespace $1 || kubectl config view --minify | grep namespace | cut -d\" \" -f6 ; } ; f";
    kns = "kubectl get ns";
    ls = "ls -G ";
    http_serve = "python3 -m http.server 8080";
    mkd = "f() { [ \"$1\" ] && mkdir -p \"\${1}\"; cd $!; } ; f";
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = config.programs.zsh.enable;
    enableBashIntegration = config.programs.bash.enable;
  };

  programs.git = {
    enable = true;
    lfs.enable = true;
    aliases = {
      lol = "log --graph --decorate --pretty=oneline --abbrev-commit";
      lola = "log --graph --decorate --pretty=oneline --abbrev-commit --all";
      hist = "log --pretty=format:\"%h %ad | %s%d [%an]\" --graph --date=short";
      type = "cat-file -t";
      dump = "cat-file -p";
      s = "status";
      a = "add -p";
      co = "checkout";
      cob = "checkout -b";
      f = "fetch -p";
      c = "commit";
      caa = "commit --amend -C HEAD -a";
      p = "push";
      ba = "branch -a";
      bd = "branch -d";
      bD = "branch -D";
      d = "diff";
      dc = "diff --cached";
      ds = "diff --staged";
      r = "restore";
      rs = "restore --staged";
      st = "status -sb";
      soft = "reset --soft";
      hard = "reset --hard";
      s1ft = "soft HEAD~1";
      h1rd = "hard HEAD~1";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      plog = "log --graph --pretty='format:%C(red)%d%C(reset) %C(yellow)%h%C(reset) %ar %C(green)%aN%C(reset) %s'";
      tlog = "log --stat --since='1 Day Ago' --graph --pretty=oneline --abbrev-commit --date=relative";
      rank = "shortlog -sn --no-merges";
      bdm = "!git branch --merged | grep -v '*' | xargs -n 1 git branch -d";
    };
    attributes = ["*.pdf diff=pdf"];
    extraConfig = {
      init.defaultBranch = "devel";
      pull.rebase = false;
      credential.helper = "cache";
      core.excludesfile = "~/.gitignore_global";
      filter.lfs = {
        required = true;
        clean = "git-lfs clean -- %f";
        smudge = "git-lfs smudge -- %f";
        process = "git-lfs filter-process";
      };
      color = {
        branch = "auto";
        diff = "auto";
        interactive = "auto";
        status = "auto";
      };
    };
    ignores = [
      ".DS_Store"
      ".AppleDouble"
      ".LSOverride"
      "Icon\r\r"
      "._*"
      ".DocumentRevisions-V100"
      ".fseventsd"
      ".Spotlight-V100"
      ".TemporaryItems"
      ".Trashes"
      ".VolumeIcon.icns"
      ".com.apple.timemachine.donotpresent"
      ".AppleDB"
      ".AppleDesktop"
      "Network Trash Folder"
      "Temporary Items"
      ".apdisk"
      ".swp"
    ];
    signing = {
      signByDefault = true;
      key = null;
    };
  };

  programs.gpg = {
    enable = true;
    settings = {
      require-cross-certification = true;
      keyserver = "https://pgp.mit.edu";
      default-key = "0x4678907F";
      no-emit-version = true;
      no-comments = true;
      keyid-format = "short";
      with-fingerprint = true;
      list-options = "show-uid-validity";
      verify-options = "show-uid-validity";
      use-agent = true;
      fixed-list-mode = true;
      charset = "utf-8";
      personal-cipher-preferences = ["AES256" "AES192" "AES" "CAST5"];
      personal-digest-preferences = "SHA256";
      cert-digest-algo = "SHA256";
      default-preference-list = ["SHA512" "SHA384" "SHA256" "SHA224" "AES256" "AES192" "AES" "CAST5" "ZLIB" "BZIP2" "ZIP" "Uncompressed"];
    };
  };

  programs.home-manager.enable = true;

  programs.man = {
    enable = true;
    generateCaches = true;
  };

  programs.pandoc.enable = true;

  programs.vim = {
    enable = true;
    packageConfigurable = pkgs.vim;
    settings = {
      background = "dark";
      expandtab = true;
      ignorecase = true;
      number = false;
      shiftwidth = 2;
      smartcase = true;
      tabstop = 2;
    };
    extraConfig = ''
      "set go+=c
      "set showmatch
      set clipboard^=unnamed,unnamedplus
    '';
  };

  home.sessionVariables = {EDITOR = "vim";};

  programs.zsh.enable = pkgs.stdenv.isDarwin;

  programs.bash = {
    enable = pkgs.stdenv.isLinux;
    shellOptions = [
      "histappend"
    ];
    sessionVariables = {
      PROMPT_COMMAND = "history -a;$PROMPT_COMMAND";
    };
  };

  services.gpg-agent = {
    enable = pkgs.stdenv.isLinux;
  };
}
