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
    ../common.nix
    ../cloud.nix
    "${self}/modules/gateway.nix"
    "${self}/modules/monitoring"
  ];

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

  services.gateway.enable = true;
  services.monitoring = {
    enable = true;
    domain = "metrics.cirriform.au";
    enableUnpoller = true;
  };

  # Check with nix eval .#nixosConfigurations.terpsichore.config.services.borgbackup.repos --json | jq
  services.borgbackup.repos = lib.mkIf isBackupTarget (
    lib.mapAttrs' (
      repoName: repoConfig:
      lib.nameValuePair repoName {
        path = repoConfig.repoPath;
        authorizedKeys =
          let
            sourceNode = nodes.${repoConfig.source};
          in
          lib.optional (sourceNode ? backupSshKey) sourceNode .backupSshKey;
      }
    ) backupTargetRepos
  );
  services.openssh.settings.AllowUsers = lib.mkIf isBackupTarget (lib.mkAfter [ "borg" ]);

}
