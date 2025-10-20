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
    ./backup.nix
    (modulesPath + "/profiles/headless.nix")
    ../common.nix
    ../cloud.nix
    ../zerotierone.nix
    ../virt.nix
    "${self}/modules/paperless"
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
  services.openssh.openFirewall = false;
  networking.firewall.interfaces."zth6rkq2c5".allowedTCPPorts = [ 22 80 443 ];

  services.postgresql = {
    enable = true;
    # Removing this will triger the install of a newer version of postgresql without migrating the data
    package = pkgs.postgresql_14;
  };

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
      # Rate limit syslog source
      $SystemLogRateLimitInterval 5
      $SystemLogRateLimitBurst 50000
      $SystemLogRateLimitSeverity 5

      # Has default rate limiting
      # https://www.rsyslog.com/doc/configuration/modules/imjournal.html
      $ModLoad imjournal

      $ModLoad imfile
      # Nginx access log
      $InputFileName /var/log/nginx/*access.log
      $InputFileTag nginx-access:
      $InputFileStateFile nginx-access-state
      $InputFileSeverity info
      $InputFileFacility local1
      $InputRunFileMonitor

      # Nginx error log
      $InputFileName /var/log/nginx/*error.log
      $InputFileTag nginx-error:
      $InputFileStateFile nginx-error-state
      $InputFileSeverity error
      $InputFileFacility local1
      $InputRunFileMonitor

      # Forward all logs to remote rsyslog server on port 1514
      *.* @@192.168.1.2:1514
    '';
  };

  services.gateway.enable = true;
  services.auth = {
    enable = true;
    domain = "auth.cirriform.au";
  };
  services.cloud.enable = true;
  services.cloud.enableOffice = true;
  services.paperless-ngx.enable = true;
  services.mealie-oidc.enable = false;
  services.immich-oidc.enable = true;
  services.kubo = {
    enable = false;
    autoMount = true;

    # Ensure API is accessible
    settings = {
      Addresses.API = "/ip4/127.0.0.1/tcp/5001";
      Addresses.Gateway = "/ip4/127.0.0.1/tcp/8081";
      Mounts.IPFS = "/ipfs";
      Mounts.FuseAllowOther = true;
      Addresses.Swarm = [
        "/ip4/172.22.70.58/tcp/4001"
        "/ip4/172.22.70.58/udp/4001/quic-v1"
        "/ip6/::1/tcp/4001"
        "/ip6/::1/udp/4001/quic-v1"
        "/ip4/127.0.0.1/tcp/4001"
        "/ip4/127.0.0.1/udp/4001/quic-v1"
      ];
      Peering.Peers = [{
        "ID" = "12D3KooWT1Uq4VFBL8eLXumcqoMP3oYer4oYQSPbvjTg981FtiFB";
        "Addrs" = ["/ip4/192.168.10.31/tcp/4001"];
      }];
      Addresses.NoAnnounce = [
        "/ip4/10.0.0.0/ipcidr/8"
        "/ip4/100.64.0.0/ipcidr/10"
        "/ip4/169.254.0.0/ipcidr/16"
        #"/ip4/172.16.0.0/ipcidr/12"
        "/ip4/192.0.0.0/ipcidr/24"
        "/ip4/192.0.2.0/ipcidr/24"
        #"/ip4/192.168.0.0/ipcidr/16"
        "/ip4/198.18.0.0/ipcidr/15"
        "/ip4/198.51.100.0/ipcidr/24"
        "/ip4/203.0.113.0/ipcidr/24"
        "/ip4/240.0.0.0/ipcidr/4"
        "/ip6/100::/ipcidr/64"
        "/ip6/2001:2::/ipcidr/48"
        "/ip6/2001:db8::/ipcidr/32"
        "/ip6/fc00::/ipcidr/7"
        "/ip6/fe80::/ipcidr/10"
      ];
      Swarm.AddrFilters = [
          "/ip4/10.0.0.0/ipcidr/8"
          "/ip4/100.64.0.0/ipcidr/10"
          "/ip4/169.254.0.0/ipcidr/16"
          #"/ip4/172.16.0.0/ipcidr/12"
          "/ip4/192.0.0.0/ipcidr/24"
          "/ip4/192.0.2.0/ipcidr/24"
          #"/ip4/192.168.0.0/ipcidr/16"
          "/ip4/198.18.0.0/ipcidr/15"
          "/ip4/198.51.100.0/ipcidr/24"
          "/ip4/203.0.113.0/ipcidr/24"
          "/ip4/240.0.0.0/ipcidr/4"
          "/ip6/100::/ipcidr/64"
          "/ip6/2001:2::/ipcidr/48"
          "/ip6/2001:db8::/ipcidr/32"
          "/ip6/fc00::/ipcidr/7"
          "/ip6/fe80::/ipcidr/10"
      ];
      Swarm.RelayClient.Enabled = false;
      Swarm.EnableAutoRelay = false;
    };
  };
  #systemd.services.ipfs.serviceConfig = {
  #  Environment="IPFS_FUSE_DEBUG=true";
  #};
  #networking.firewall = {
  #  allowedTCPPorts = [
  #    4001  # IPFS swarm port
  #    5001  # IPFS API port (optional, usually only for localhost)
  #    8080  # IPFS gateway port (optional)
  #  ];
  #  allowedUDPPorts = [
  #    4001  # IPFS swarm port (QUIC)
  #  ];
  #};

  age.secrets.borg-ssh-key.file = "${self}/secrets/borg-calliope-ssh-key.age";

  systemd.tmpfiles.rules = [
    "d /var/lib/postgresql/backups 0750 postgres postgres -"
  ];

  services.borgbackup.jobs.calliope = let
    backupLocation = "/var/lib/postgresql/backups/";
  in {
    paths = [
      "/var/lib"
    ];
    exclude = [
      "/var/lib/acme"
      "/var/lib/containers"
      "/var/lib/fail2ban"
      "/var/lib/ipfs"
      "/var/lib/redis-immich"
      "/var/lib/redis-nextcloud"
      "/var/lib/redis-paperless"
      "/var/lib/systemd"
    ];
    repo = "borg@192.168.1.2:.";
    doInit = true;
    environment.BORG_RSH = "ssh -i ${config.age.secrets.borg-ssh-key.path}";
    compression = "auto,lzma";
    startAt = "daily";
    encryption.mode = "none";
    readWritePaths = [ "${backupLocation}" ];
    preHook = ''
      echo "Starting PostgreSQL backup..."

      # Backup global objects (roles, tablespaces, etc.)
      echo "Backing up PostgreSQL globals..."
      ${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql}/bin/pg_dumpall --globals-only > ${backupLocation}/postgres_globals.sql
      ${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql}/bin/pg_dumpall > ${backupLocation}/postgres_all.sql

      # Get list of databases and backup each individually
      echo "Backing up individual databases..."
      for db in $(${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql}/bin/psql -t -c "select datname from pg_database where not datistemplate" | ${pkgs.gnugrep}/bin/grep '\S' | ${pkgs.gawk}/bin/awk '{$1=$1};1'); do
        echo "  Backing up database: $db"
        ${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql}/bin/pg_dump --create --format=custom "$db" > "${backupLocation}/$db.pgdump"
      done

      echo "PostgreSQL backup completed"
    '';
  };
}
