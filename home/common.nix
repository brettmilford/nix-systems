{ pkgs, pkgs_x86, config, ... }:
{
  home.packages = with pkgs; [
    direnv
    qemu
    git-review
    tmux
    jq
    remarshal
    texlive.combined.scheme-basic
    ansible
    (pass.withExtensions (ext: [ext.pass-otp]))
    lftp
    podman
    kubectl
    kubernetes-helm
    bitwarden-cli
    htop
    ncdu
    xz
    gh
  ];

  home.extraOutputsToInstall = [ "doc" "info" "devdoc" ];

  home.shellAliases = {
      em = "emacsclient -tcq -a \"\"";
      t = "tmux";
      g = "git ";
      o = "openstack ";
      k = "kubectl ";
      kx = "f() { [ \"$1\" ] && kubectl config use-context $1 || kubectl config current-context ; } ; f";
      kn = "f() { [ \"$1\" ] && kubectl config set-context --current --namespace $1 || kubectl config view --minify | grep namespace | cut -d\" \" -f6 ; } ; f";
      kns = "kubectl get ns";
      ls = "ls -G ";
      http_serve = "python3 -m http.server 8080";
      j = "juju ";
      jw = "watch -tc juju status --color";
      mkd = "f() { [ \"$1\" ] && mkdir -p \"\${1}\"; cd $!; } ; f";
      segmaassh = "f() { hostname=$1; shift; ssh ubuntu@`maas segmaas machines read hostname=\${hostname} | jq -r '.[].ip_addresses | .[]'` $@ ; } ; f";
  };

  programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
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
      attributes = [ "*.pdf diff=pdf" ];
      extraConfig = {
        init.defaultBranch = "devel";
        gitubuntu.lpuser = "brettmilford";
        pull.rebase = false;
        credential.helper = "cache";
        gitreview.username = "brett";
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
        url = {
          "git+ssh://brettmilford@git.launchpad.net/" = {
            insteadOf = "lp:";
          };
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
      ];
      signing = {
        signByDefault = true;
        key = null;
      };

      userEmail = "brettmilford@gmail.com";
      userName = "Brett Milford";
  };

  programs.gpg = {
      enable = true;
      publicKeys = [
        {
          source = ../config/gpg/pubkey.txt;
          trust = "ultimate";
        }
      ];

      settings = {
          require-cross-certification = true;
          keyserver = "https://keyserver.ubuntu.com";
          default-key = "0x4678907F";
          no-emit-version = true;
          no-comments = true;
          keyid-format = "short";
          with-fingerprint = true;
          list-options = "show-uid-validity";
          verify-options = "show-uid-validity";
          use-agent = true;
          fixed-list-mode = true;
          charset  = "utf-8";
          personal-cipher-preferences = ["AES256" "AES192" "AES" "CAST5" ];
          personal-digest-preferences = "SHA256";
          cert-digest-algo = "SHA256";
          default-preference-list = ["SHA512" "SHA384" "SHA256" "SHA224" "AES256" "AES192" "AES" "CAST5" "ZLIB" "BZIP2" "ZIP" "Uncompressed" ];
      };
  };

  programs.home-manager.enable = true;

  programs.man = {
      enable = true;
      generateCaches = true;
  };

  programs.nix-index = {
      enable = true;
      enableZshIntegration = true;
  };

  programs.pandoc.enable = true;

  programs.ssh = {
      enable = true;
  };

  programs.vim = {
      enable = true;
      defaultEditor = true;
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
'';
  };

  programs.zsh.enable = true;

}
