{
  config,
  lib,
  pkgs,
  users,
  ...
}:
with lib;
let
  mkUser = username: user: {

    users.users.${username} = {
      home = "/home/${username}";
      isNormalUser = true;
      group = username;
      description = user.name;
      extraGroups = [
        "wheel"
        "networkmanager"
        "libvirt"
        "kvm"
      ];
      openssh.authorizedKeys.keys = [ user.sshKey ];
    };
    users.groups.${username} = { };
  };
in
mkMerge (builtins.attrValues (builtins.mapAttrs mkUser users))
