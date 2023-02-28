{ pkgs, pkgs_x86, ... }:
{
  environment.systemPackages = [ pkgs.qemu pkgs_x86.libvirt ];

  launchd.agents.libvirt = {
    command = "${pkgs_x86.libvirt}/bin/libvirtd -d -f /opt/homebrew/etc/libvirt/libvirtd.conf";
    serviceConfig.KeepAlive = true;
    serviceConfig.RunAtLoad = true;
  };
}
