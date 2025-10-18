{
  config,
  lib,
  pkgs,
  ...
}:
let
  vmConfig = {
    users.mutableUsers = true;
    users.users = lib.mapAttrs (
      name: user:
      lib.mkIf (builtins.elem "wheel" (user.extraGroups or [ ])) {
        initialPassword = lib.mkDefault "test";
      }
    ) config.users.users;

    virtualisation = {
      memorySize = 4092;
      cores = 4;
      qemu.options = [
        "-enable-kvm"
        "-cpu host"
      ];
    };
  };

in
{
  virtualisation.vmVariant = vmConfig;
  virtualisation.vmVariantWithBootLoader = vmConfig;
}
