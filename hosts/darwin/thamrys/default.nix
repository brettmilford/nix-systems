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

  system.stateVersion = 4;
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
