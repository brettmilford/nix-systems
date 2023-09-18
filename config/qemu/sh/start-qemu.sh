#!/bin/sh -xeu

ARCH=${ARCH:-"aarch64"}

if [ `uname` == "Darwin" ]; then
    ACCEL_ARG="-accel hvf"
else
    ACCEL_ARG="-accel kvm"
fi


_start_qemu () {
    if [ $ARCH == "aarch64" ]; then
      if [ `command -v brew 2>/dev/null` ]; then
        BIOS_ARG="-bios $(brew --prefix qemu)/share/qemu/edk2-aarch64-code.fd"
      else
        BIOS_ARG="-bios $(nix-store -q `which qemu-system-aarch64`)/share/qemu/edk2-aarch64-code.fd"
      fi

      if [ ! -z ${GRAPHICAL+x} ]; then
          GRAPHICAL_ARG="
              -device virtio-gpu-pci
              -display default,show-cursor=on
              -device qemu-xhci
              -device usb-kbd
              -device usb-tablet
              -device intel-hda
              -device hda-duplex"
      fi
    fi

    if [ ! -z ${ISO:+x} ]; then
        ISO_ARG="-cdrom $ISO"
        if [ -f ./rootfs.qcow2 ]; then
          echo "Error: ./rootfs.qcow2 already exists"
          exit 1
        fi
        qemu-img create -f qcow2 rootfs.qcow2 60G
    fi

    QEMU_ARGS="
        -monitor none
        -machine virt
        -cpu host
        -smp 4
        -m 8G
        -rtc base=utc,clock=host
        -drive file=rootfs.qcow2,if=virtio
        -device virtio-net,netdev=vmnic
        -netdev user,id=vmnic,hostfwd=tcp:127.0.0.1:2222-:22
        ${BIOS_ARG}
        ${ACCEL_ARG}
        ${GRAPHICAL_ARG}
        ${ISO_ARG}
"
    qemu-system-$ARCH $QEMU_ARGS

}

for arg in $@; do
  case "${arg:-h}" in
    -i)
      shift
      ISO="$1"
      GRAPHICAL="true";
      ;;

    -g)
      shift
      GRAPHICAL="true";
      ;;

    -a)
      shift
      ARCH="$1"
      ;;

    -h|--help|help|*)
        echo "${0} -g graphical [ -a ARCH ] [-i ISO]"
  esac
done
_start_qemu
