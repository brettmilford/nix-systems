{ config, lib, pkgs, ... }:

{
  users.users.nix = {
    isNormalUser = true;
    home = "/home/nix";
    description = "Nix User";
    group = "nix";
    extraGroups = ["wheel" "networkmaanger"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR brett@thamrys"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB9iwf2c7cAHQQpfkImGNDeZnYPzGbudZcZaBWkS03mu bmj"
    ];
  };

  users.groups.nix = {};

  security.sudo.extraRules = [{
    users = [ "nix" ];
    commands = [{
      command = "ALL";
      options = [ "NOPASSWD" ];
    }];
  }];

  networking.useDHCP = lib.mkDefault true;
  services.openssh.enable = true;

  environment.systemPackages = with pkgs; [
    screen
  ];
}
