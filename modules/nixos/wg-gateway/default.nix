{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.wg-gateway;
in
{
  options.services.wg-gateway = {
    enable = mkEnableOption "WireGuard Gateway with ZeroTier routing";

    secretPaths = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = "Paths to secret files";
    };

    listenPort = mkOption {
      type = types.port;
      default = 51820;
      description = "Port for WireGuard to listen on";
    };

    serverAddress = mkOption {
      type = types.str;
      default = "172.16.0.1/16";
      description = "Server IP address in WireGuard network";
    };

    externalInterface = mkOption {
      type = types.str;
      default = "eth0";
      description = "External interface for NAT";
    };

    externalIP = mkOption {
      type = types.str;
      description = "External IP address for SNAT";
    };

    peers = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            publicKey = mkOption {
              type = types.str;
              description = "Peer's public key";
            };

            allowedIPs = mkOption {
              type = types.listOf types.str;
              description = "IP addresses allowed for this peer";
              example = [ "172.16.0.2/32" ];
            };

          };
        }
      );
      default = [ ];
      description = "List of WireGuard peers";
    };
  };

  config = mkIf cfg.enable {
    # Enable IP forwarding
    environment.systemPackages = with pkgs; [ wireguard-tools ];
    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
    };

    # Enable systemd-networkd
    networking.useNetworkd = true;
    systemd.network.enable = true;

    # NAT configuration for WireGuard traffic
    networking.nat = {
      enable = true;
      externalInterface = cfg.externalInterface;
      externalIP = cfg.externalIP;
      internalIPs = [ "172.16.0.0/16" ];
    };

    # Firewall configuration
    networking.firewall = {
      enable = true;
      allowedUDPPorts = [ cfg.listenPort ];
      trustedInterfaces = [ "wg0" ];
      extraCommands = ''
        # Allow WireGuard clients to access ZeroTier network
        iptables -I FORWARD 1 -s 172.16.0.0/16 -d 172.22.0.0/16 -j ACCEPT
        iptables -I FORWARD 1 -s 172.22.0.0/16 -d 172.16.0.0/16 -j ACCEPT

        # Allow WireGuard clients to access 192.168.0.0/16 via ZeroTier
        iptables -I FORWARD 1 -s 172.16.0.0/16 -d 192.168.0.0/16 -j ACCEPT
        iptables -I FORWARD 1 -s 192.168.0.0/16 -d 172.16.0.0/16 -j ACCEPT

        # Allow DNS queries from WireGuard clients to ZeroTier DNS
        iptables -I FORWARD 1 -s 172.16.0.0/16 -d 172.22.0.1 -p udp --dport 53 -j ACCEPT
        iptables -I FORWARD 1 -s 172.16.0.0/16 -d 172.22.0.1 -p tcp --dport 53 -j ACCEPT
      '';

      extraStopCommands = ''
        # Clean up forward rules
        iptables -D FORWARD -s 172.16.0.0/16 -d 172.22.0.0/16 -j ACCEPT 2>/dev/null || true
        iptables -D FORWARD -s 172.22.0.0/16 -d 172.16.0.0/16 -j ACCEPT 2>/dev/null || true
        iptables -D FORWARD -s 172.16.0.0/16 -d 192.168.0.0/16 -j ACCEPT 2>/dev/null || true
        iptables -D FORWARD -s 192.168.0.0/16 -d 172.16.0.0/16 -j ACCEPT 2>/dev/null || true
        iptables -D FORWARD -s 172.16.0.0/16 -d 172.22.0.1 -p udp --dport 53 -j ACCEPT 2>/dev/null || true
        iptables -D FORWARD -s 172.16.0.0/16 -d 172.22.0.1 -p tcp --dport 53 -j ACCEPT 2>/dev/null || true
      '';
    };

    # WireGuard netdev configuration
    systemd.network.netdevs."50-wg0" = {
      netdevConfig = {
        Kind = "wireguard";
        Name = "wg0";
      };
      wireguardConfig = {
        PrivateKeyFile = cfg.secretPaths.wg_server_private;
        ListenPort = cfg.listenPort;
        RouteTable = "main";
        FirewallMark = 42;
      };
      wireguardPeers = map (peer: {
        PublicKey = peer.publicKey;
        AllowedIPs = peer.allowedIPs;
      }) cfg.peers;
    };

    # WireGuard network configuration
    systemd.network.networks."50-wg0" = {
      matchConfig.Name = "wg0";
      address = [ cfg.serverAddress ];
      networkConfig = {
        IPv4Forwarding = true;
      };
    };
  };
}
