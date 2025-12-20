{ self, config, pkgs, lib, ... }:
let
  vmName = "opnsense";
  vmMemory = 4096;
  vmCores = 4;
  vmImagePath = "/var/lib/qemu/images/${vmName}";
  tapInterface = "tap0";
in {

  imports =[
    ./qemu.nix
  ];

  networking.interfaces.eno1 = {
    useDHCP = false;
    ipv4.addresses = [
      { address = "192.168.100.100"; prefixLength = 24; }
    ];
  };

  networking.interfaces.${tapInterface} = {
    virtual = true;
    virtualType = "tap";
    ipv4.addresses =  [
      { address = "192.168.1.2"; prefixLength = 24; }
    ];
  };

  systemd.services."${tapInterface}-netdev" = {
    # Prevent rebuild from stopping/restarting
    restartIfChanged = false;
    stopIfChanged = false;
  };

  networking.defaultGateway.address = "192.168.1.1";
  networking.defaultGateway.interface = "tap0";
  networking.nameservers = [ "192.168.1.1" "1.0.0.1" "1.1.1.1" ];
  networking.search = [ "local" "internal" ];

  systemd.tmpfiles.rules = [
    "d /var/lib/qemu/images/${vmName} 0750 root root -"
    "d /var/lib/qemu/snapshots/${vmName} 0750 root root -"
  ];

  systemd.services."qemu-${vmName}" = {
    description = "${vmName} QEMU Virtual Machine";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target"];
    requires = [ "network-online.target" ];
    # Prevent rebuild from stopping/restarting
    restartIfChanged = false;
    stopIfChanged = false;

    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "5s";
      OOMScoreAdjust = -1000;
      RuntimeDirectory = "qemu/${vmName}";
      RuntimeDirectoryMode = "0750";

      ExecStart = ''
        ${pkgs.qemu}/bin/qemu-kvm \
          -name ${vmName} \
          -enable-kvm \
          -machine type=q35,accel=kvm \
          -cpu host,kvm=on,vendor=GenuineIntel,+invtsc,+topoext \
          -smp ${toString vmCores},sockets=1 \
          -m ${toString vmMemory} \
          -drive file=${vmImagePath}/disk.qcow2,format=qcow2,if=virtio,cache=none,aio=native \
          -boot menu=on,strict=on \
          -device qemu-xhci \
          -device virtio-balloon \
          -device virtio-rng-pci \
          -device virtio-serial \
          -usb \
          -vga std \
          -vnc :0 \
          -netdev tap,id=net0,ifname=${tapInterface},script=no,downscript=no \
          -device virtio-net-pci,netdev=net0 \
          -device vfio-pci,host=01:00.0,multifunction=on \
          -device vfio-pci,host=01:00.1,multifunction=on \
          -device vfio-pci,host=01:00.2,multifunction=on \
          -device vfio-pci,host=01:00.3,multifunction=on \
          -monitor unix:/run/qemu/${vmName}/monitor.sock,server,nowait \
          -qmp unix:/run/qemu/${vmName}/qmp.sock,server,nowait
      '';
    };
  };
}
