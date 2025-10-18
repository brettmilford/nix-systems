{
  config,
  lib,
  pkgs,
  users,
  ...
}:
let
  hostname = "thamrys";
  user = users.${config.system.primaryUser};
in
{
  imports = [
    ../common.nix
    ../homebrew.nix
  ];

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
    };

  networking = {
    hostName = hostname;
    computerName = hostname;
    localHostName = hostname;
  };

  services.podman.enable = true;
}
