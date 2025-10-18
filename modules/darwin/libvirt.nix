{
  config,
  lib,
  pkgs,
  pkgs-x86_64,
  ...
}:
with lib;
let
  cfg = config.services.libvirt;
in
{
  options.services.libvirt = {
    enable = mkEnableOption "Libvirt";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.qemu
      pkgs-x86_64.libvirt
    ];

    launchd.agents.libvirt = {
      command = "${pkgs-x86_64.libvirt}/bin/libvirtd -d -f /opt/homebrew/etc/libvirt/libvirtd.conf";
      serviceConfig.KeepAlive = true;
      serviceConfig.RunAtLoad = true;
    };
  };
}
