{ lib }:

let
  nodes = import ../../nodes.nix;
  services = import ../../services.nix;
  inherit
    (import ../serviceMap.nix {
      inherit lib nodes services;
    })
    validateServices
    validateBackupSets
    ;

in
lib.runTests {
  testValidateServices = {
    expr = validateServices nodes services;
    expected = true;
  };

  testValidateBackupSets = {
    expr = validateBackupSets nodes services.backup.config.repos or { };
    expected = true;
  };
}
