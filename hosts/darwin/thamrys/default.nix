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
        description = user.name;
        home = "/Users/${primaryUser}";
        openssh.authorizedKeys.keys = [
          user.sshKey
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJne1Tfz++nVphhTLDHnF0qza2KH6hj9ROPmYN1aYEoZ opencode@terpsichore"
        ];
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
          users.nix.sshKey
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJne1Tfz++nVphhTLDHnF0qza2KH6hj9ROPmYN1aYEoZ opencode@terpsichore"
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
