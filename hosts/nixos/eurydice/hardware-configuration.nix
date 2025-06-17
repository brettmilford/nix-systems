{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "vfio_pci" ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [ "iommu=pt" "intel_iommu=on" "vfio-pci.ids=8086:1521" ];

  fileSystems."/" =
    { device = "zpool/root";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    { device = "zpool/nix";
      fsType = "zfs";
    };

  fileSystems."/var" =
    { device = "zpool/var";
      fsType = "zfs";
    };

  fileSystems."/home" =
    { device = "zpool/home";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/428C-56BC";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };

  swapDevices = [ ];

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
