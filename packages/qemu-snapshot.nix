{
  writeShellApplication,
  socat,
}:

writeShellApplication {
  name = "qemu-snapshot";
  runtimeInputs = [ socat ];
  text = builtins.readFile ../hosts/nixos/eurydice/qemu-snapshot.sh;
}
