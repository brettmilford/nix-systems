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
    ./backup.nix
    ../common.nix
    ../cloud.nix
    ../virt.nix
  ];

  system.stateVersion = "23.05";

  networking.hostName = "calliope";
  networking.hostId = "25f4937c";
  networking.nameservers = [ "172.16.1.1" ];
  networking.search = [ "cirriform.au" ];
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
  services.openssh.openFirewall = false;

  services.postgresql = {
    enable = true;
    # Removing this will triger the install of a newer version of postgresql without migrating the data
    package = pkgs.postgresql_14;
    extensions = with pkgs.postgresql_14.pkgs; [
      pgvector
    ];
  };

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
}
