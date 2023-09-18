{ pkgs, config, ... }:
{

  environment.systemPackages = with pkgs; [
    vim
    git
  ];

  environment.variables.EDITOR = "vim";

  environment.shellAliases = {
    lctlrl = "f() { [ \"$1\" ] && launchctl unload $1 && launchctl load $1 ; } ; f";
    lctlrs = "f() { [ \"$1\" ] && launchctl stop $1 && launchctl start $1 ; } ; f";
    nrl = "darwin-rebuild switch --flake ~/nix-systems#";
    nup = "nix flake update ~/nix-systems && nrl";
  };

  nix.package = pkgs.nixFlakes;

  programs.gnupg.agent = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
  };

  security.pam.enableSudoTouchIdAuth = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  system.defaults = {
    NSGlobalDomain.AppleEnableSwipeNavigateWithScrolls = true;
    NSGlobalDomain.InitialKeyRepeat = 25;
    NSGlobalDomain.KeyRepeat = 2;
    NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
    alf.globalstate = 1;
    alf.stealthenabled = 1;
    dock.orientation = "left";
    dock.wvous-bl-corner = 14;
    dock.wvous-br-corner = 7;
    finder.FXDefaultSearchScope = "SCcf";
    finder.FXPreferredViewStyle = "clmv";
    loginwindow.GuestEnabled = false;
    magicmouse.MouseButtonMode = "TwoButton";
  };
}
