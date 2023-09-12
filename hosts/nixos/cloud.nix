{ config, lib, pkgs, ... }:

{
  users.users.nix = {
    isNormalUser = true;
    home = "/home/nix";
    extraGroups = ["wheel" "networkmaanger"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR brett@thamrys"
    ];
  };

  security.sudo.extraRules = [{
    users = [ "nix" ];
    commands = [{
      command = "ALL";
      options = [ "NOPASSWD" ];
    }];
  }];

  networking.useDHCP = true;
  services.openssh.enable = true;
}
