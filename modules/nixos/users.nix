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
    services.openssh.settings.AllowUsers = lib.mkAfter [ username ];
  };
in
mkMerge (builtins.attrValues (builtins.mapAttrs mkUser (lib.filterAttrs (_: u: u ? name) users)))
