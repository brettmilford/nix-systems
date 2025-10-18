{
  config,
  lib,
  pkgs,
  ...
}:
let
  primaryUser = config.system.primaryUser;
in
{
  imports = [
    ./podman.nix
    ./libvirt.nix
  ];

  environment.systemPackages = with pkgs; [
    vim
    git
    gcc
  ];

  environment.variables.EDITOR = "vim";

  environment.shellAliases = {
    lctlrl = "f() { [ \"$1\"] && launchctl unload $1 && launchctl load $1 ; } ; f";
    lctlrs = "f() { [ \"$1\" ] && launchctl stop $1 && launchctl start $1 ; } ; f";
  };

  environment.extraInit = ''
    if defaults read -g AppleInterfaceStyle 2>/dev/null | grep -q 'Dark'; then
      export APPEARANCE=dark
    else
      export APPEARANCE=light
    fi
  '';

  system.stateVersion = 4;
  ids.gids.nixbld = 350;
  nix = {
    distributedBuilds = true;
    # TODO: factor out builders service
    extraOptions = ''
      extra-platforms = aarch64-darwin x86_64-darwin
      experimental-features = nix-command flakes
      builders = ssh://nix@eurydice /Users/${primaryUser}/.ssh/id_ed25519
    '';
    settings.trusted-users = [
      "@admin"
    ];
    optimise.automatic = true;
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
      interval = {
        Hour = 3;
        Minute = 15;
        Weekday = 6;
      };
    };
  };
}
