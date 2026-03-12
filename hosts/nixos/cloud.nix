{
  config,
  lib,
  pkgs,
  users,
  ...
}: {
  networking.useDHCP = lib.mkDefault true;

  users.groups.nix = {};

  users.users.nix = {
    isNormalUser = true;
    home = "/home/nix";
    createHome = true;
    description = "Nix User";
    group = "nix";
    extraGroups = ["wheel" "systemd-journal" "git"];
    openssh.authorizedKeys.keys = [
      users.nix.sshKey
      users.brett.sshKey
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB9iwf2c7cAHQQpfkImGNDeZnYPzGbudZcZaBWkS03mu bmj"
    ];
  };

  security.sudo.extraRules = [
    {
      users = ["nix"];
      commands = [
        {
          command = "ALL";
          options = ["NOPASSWD"];
        }
      ];
    }
  ];

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
      X11Forwarding = true;
      AllowUsers = ["nix"];
    };
  };

  environment.systemPackages = with pkgs; [
    screen
  ];
}
