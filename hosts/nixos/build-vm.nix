{ config, lib, pkgs, ... }:

{
  users.users.nix = {
    isNormalUser = true;
    home = "/home/nix";
    description = "Nix User";
    group = "nix";
    extraGroups = ["wheel" "networkmaanger"];
    initialPassword = "test";
  };

  users.groups.nix = {};

  virtualisation.vmVariant = {
    virtualisation = {
      memorySize =  2048;
      cores = 2;
    };
  };
  virtualisation.vmVariantWithBootLoader = {
    virtualisation = {
      memorySize =  2048;
      cores = 2;
    };
  };
}
