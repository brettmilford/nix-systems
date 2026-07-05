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

  networking.nftables.enable = true;
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

  # terpsichore reads its Proton Bridge SASL password from this manually managed
  # file, bypassing the agenix `postfix-sasl-passwd` secret (which calliope still
  # uses, so it can't be removed). The bridge password comes from
  # `protonmail-bridge --cli` > info; to rotate, edit the file and
  # `systemctl reload postfix`.
  services.mail-relay.secretPaths.postfix-sasl-passwd = "/var/lib/postfix/sasl_passwd";
}
