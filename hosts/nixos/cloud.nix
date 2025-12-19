{
  config,
  lib,
  pkgs,
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
    extraGroups = ["wheel" "systemd-journal"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR brett@thamrys"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB9iwf2c7cAHQQpfkImGNDeZnYPzGbudZcZaBWkS03mu bmj"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBy5tD71f2uRLQvbZL0wwyZNUmximBOM19KuENx791Rl nix@polyhymnia"
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
