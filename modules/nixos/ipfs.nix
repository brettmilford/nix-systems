{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.services.ipfs;
in
{
  options.services.ipfs = {
    enable = mkEnableOption "IPFS setup";
  };

  config = mkIf cfg.enable {
    services.kubo = {
      enable = true;
      autoMount = true;

      # Ensure API is accessible
      settings = {
        Addresses.Swarm = [
          "/ip4/0.0.0.0/tcp/4001"
          "/ip6/::/tcp/4001"
          "/ip4/0.0.0.0/udp/4001/quic-v1"
          "/ip6/::/udp/4001/quic-v1"
        ];
        Addresses.NoAnnounce = [
          "/ip4/10.0.0.0/ipcidr/8"
          "/ip4/100.64.0.0/ipcidr/10"
          "/ip4/169.254.0.0/ipcidr/16"
          "/ip4/172.16.0.0/ipcidr/12"
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
      };
    };
    systemd.services.ipfs.serviceConfig = {
      Environment = "IPFS_FUSE_DEBUG=true";
    };
    networking.firewall = {
      allowedTCPPorts = [
        4001 # IPFS swarm port
        5001 # IPFS API port
        8080 # IPFS gateway port
      ];
      allowedUDPPorts = [
        4001 # IPFS swarm port (QUIC)
      ];
    };
  };
}
