{
  pkgs ? import <nixpkgs> { },
}:

let
  lib = pkgs.lib;

  # Convert runTests results to derivations
  mkTestDrv =
    name: testResults:
    if testResults == [ ] then
      pkgs.runCommand "lib-tests-${name}" { } ''
        echo "All ${name} tests passed"
        mkdir -p $out
        echo "success" > $out/result
      ''
    else
      pkgs.runCommand "lib-tests-${name}-failure" { } ''
        echo "Tests failed for ${name}:"
        echo "${lib.concatStringsSep "\n" (map toString testResults)}"
        exit 1
      '';

  # Individual test derivations
  serviceMapTests = mkTestDrv "serviceMap" (import ./serviceMap.nix { inherit lib; });
  validateServiceTests = mkTestDrv "validateServices" (import ./validateServices.nix { inherit lib; });

in
# Aggregate all tests into a single derivation
pkgs.symlinkJoin {
  name = "lib-tests";
  paths = [
    serviceMapTests
  ];
}
