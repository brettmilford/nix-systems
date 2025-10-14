{ self, config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs;[
    qemu
    OVMF
    (writeShellApplication {
      name = "qemu-snapshot";
      runtimeInputs = with pkgs; [socat];
      text = builtins.readFile ./qemu-snapshot.sh;
    })
  ];

  systemd.tmpfiles.rules = [
    "d /var/lib/qemu/images 0750 root root -"
    "d /var/lib/qemu/snapshots 0750 root root -"
  ];
}
