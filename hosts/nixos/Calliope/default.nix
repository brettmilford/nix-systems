{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    (modulesPath + "/profiles/headless.nix")
    ../common.nix
    ../cloud.nix
    ../zerotierone.nix
    ./postgresql.nix
    ./nextcloud.nix
    ./bomftp.nix
  ];

  networking.hostName = "Calliope";
  networking.hostId = "25f4937c";
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
}
