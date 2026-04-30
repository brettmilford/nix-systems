{
  self,
  config,
  lib,
  pkgs,
  modulesPath,
  hostname,
  nodes,
  services,
  ...
}:
let
  thisNode = nodes.${hostname};
  isBackupTarget = services.lib.isBackupTarget hostname;
  backupTargetRepos = services.lib.getBackupTargetConfig hostname;
in
{
  imports = [
    ./hardware-configuration.nix
    (modulesPath + "/profiles/headless.nix")
    ./localBackup.nix
    ./backup.nix
    ../common.nix
    ../cloud.nix
  ];

  system.stateVersion = "25.05";

  networking.hostName = hostname;
  networking.hostId = "37393231";

  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  environment.systemPackages = with pkgs; [
    wpa_supplicant
    iw
    pciutils
    usbutils
    tpm2-tools
    sbctl
    smartmontools
    nvme-cli
  ];

  services.postgresql.package = pkgs.postgresql_16;
}
