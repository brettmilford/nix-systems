{
  self,
  config,
  lib,
  pkgs,
  modulesPath,
  lanzaboote,
  hosts,
  serviceMap,
  users,
  ...
}: let
  hostname = "terpsichore";
  thisHost = hosts.${hostname};
  isBackupTarget = serviceMap.lib.isBackupTarget hostname;
  backupTargetRepos = serviceMap.lib.getBackupTargetConfig hostname;
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
  networking.firewall.allowedTCPPorts = [ 80 443 ];
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

  services.borgbackup.repos = lib.mkIf isBackupTarget (
    lib.mapAttrs' (repoName: repoConfig:
          lib.nameValuePair repoName {
            path = repoConfig.repoPath;
            authorizedKeys = let
              sourceHost = hosts.${repoConfig.source};
              in
                lib.optional (sourceHost ? backupSshKey) sourceHost.backupSshKey;
          }
    ) backupTargetRepos
  );
  services.openssh.settings.AllowUsers = lib.mkIf isBackupTarget (lib.mkAfter ["borg"]);

}
