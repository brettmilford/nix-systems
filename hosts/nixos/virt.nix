{ pkgs, ... }:
{
  virtualisation.containers.enable = true;
  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };

  environment.systemPackages = with pkgs; [
    podman-compose
    qemu
  ];

  virtualisation.oci-containers.backend = "podman";
  #nixos/nixpkgs#226365
  networking.firewall.interfaces."podman+".allowedUDPPorts = [ 53 ];
}
