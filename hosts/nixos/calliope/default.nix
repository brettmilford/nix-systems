{
  self,
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
    ../virt.nix
    "${self}/modules/paperless.nix"
    ./postfix.nix
    "${self}/modules/auth.nix"
    "${self}/modules/mealie.nix"
    "${self}/modules/immich.nix"
    "${self}/modules/gateway.nix"
    "${self}/modules/cloud.nix"
  ];

  networking.hostName = "calliope";
  networking.hostId = "25f4937c";
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

  services.prometheus.exporters.node = {
    enable = true;
    listenAddress = "172.22.70.58";
    openFirewall = true;
    enabledCollectors = [
      "systemd"
      "filesystem"
      "meminfo"
      "loadavg"
      "stat"
      "processes"
      "interrupts"
    ];
  };

  services.rsyslogd = {
    enable = true;
    defaultConfig = ''
      # Forward all logs to remote rsyslog server on port 1514
      *.* @@192.168.1.2:1514
    '';
  };

  services.openssh = {
    settings = {
      PermitRootLogin = "prohibit-password";  # or "without-password"
      PasswordAuthentication = false;
    };
  };

  services.gateway.enable = true;
  services.auth = {
    enable = true;
    domain = "auth.cirriform.au";
  };
  services.cloud.enable = true;
  services.paperless-ngx.enable = true;
  services.mealie-oidc.enable = true;
  services.immich-oidc.enable = true;
}
