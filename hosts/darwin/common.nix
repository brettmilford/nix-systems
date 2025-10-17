{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    vim
    git
    gcc
  ];

  environment.variables.EDITOR = "vim";

  environment.shellAliases = {
    lctlrl = "f() { [ \"$1\"] && launchctl unload $1 && launchctl load $1 ; } ; f";
    lctlrs = "f() { [ \"$1\" ] && launchctl stop $1 && launchctl start $1 ; } ; f";
    nrs = "sudo darwin-rebuild switch --flake \"$HOME/.config/nix?submodules=1\"";
    nup = "nix flake update --flake ~/.config/nix && nrs";
  };

  environment.extraInit = ''
    if defaults read -g AppleInterfaceStyle 2>/dev/null | grep -q 'Dark'; then
      export APPEARANCE=dark
    else
      export APPEARANCE=light
    fi
  '';


  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
             "claude-code"
    ];

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
