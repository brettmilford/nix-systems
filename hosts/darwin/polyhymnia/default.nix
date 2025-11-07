{
  config,
  lib,
  pkgs,
  hostname,
  users,
  ...
}:
{
  imports = [
    ../common.nix
    ../homebrew.nix
  ];

  system.stateVersion = 5;
  system.primaryUser = "brett";

  users.users =
    let
      primaryUser = config.system.primaryUser;
      user = users.${primaryUser};
    in
    {
      ${primaryUser} = {
        name = user.name;
        home = "/Users/${primaryUser}";
      };

      nix = {
        name = "nix";
        uid = 1001;
        gid = 80;
        home = "/Users/nix";
        createHome = true;
        shell = pkgs.bash;
        openssh.authorizedKeys.keys = [
          user.sshKey
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBy5tD71f2uRLQvbZL0wwyZNUmximBOM19KuENx791Rl nix"
        ];
      };
    };

  users.knownUsers = [ "nix" ];

  security.sudo.extraConfig = ''
    nix ALL=(ALL:ALL) NOPASSWD: ALL
  '';

  services.openssh.enable = true;

  networking = {
    hostName = hostname;
    computerName = hostname;
    localHostName = hostname;
  };

  services.podman.enable = true;
}
