{ pkgs_x86, ... }:
{
  environment.packages = [ pkgs_x86.libvirt ];

  #launchd.user.agents.libvirt = {
  #  command = "/opt/homebrew/sbin/libvirtd -d -f /opt/homebrew/etc/libvirt/libvirtd.conf";
  #  serviceConfig.KeepAlive = true;
  #  serviceConfig.RunAtLoad = true;
  #};

}
