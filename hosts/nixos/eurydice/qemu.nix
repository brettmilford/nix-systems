{ self, config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    qemu
    OVMF
    qemu-snapshot
  ];

  systemd.tmpfiles.rules = [
    "d /var/lib/qemu/images 0750 root root -"
    "d /var/lib/qemu/snapshots 0750 root root -"
  ];
}
