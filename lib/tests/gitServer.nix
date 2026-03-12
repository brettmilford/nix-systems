{ lib }:

let
  # Replicate the authorizedKeys collection logic from serviceModules.nix git-server getOptions.
  # Collects backupSshKey from all nodes and sshKey from all users.
  collectAuthorizedKeys =
    nodes: users:
    lib.mapAttrsToList (_name: node: node.backupSshKey) (
      lib.filterAttrs (_name: node: node ? backupSshKey) nodes
    )
    ++ lib.mapAttrsToList (_name: user: user.sshKey) (
      lib.filterAttrs (_name: user: user ? sshKey) users
    );

  testNodes = {
    nodeA = {
      ip = "192.168.1.1";
      system = "x86_64-linux";
      dataPath = "/srv/data";
      backupSshKey = "ssh-ed25519 AAAAC keyA";
    };
    nodeB = {
      ip = "192.168.1.2";
      system = "x86_64-linux";
      # No backupSshKey - should be excluded
    };
    nodeC = {
      ip = "192.168.1.3";
      system = "x86_64-linux";
      dataPath = "/data";
      backupSshKey = "ssh-ed25519 AAAAC keyC";
    };
  };

  testUsers = {
    alice = { sshKey = "ssh-ed25519 AAAAC keyAlice"; };
    bob = { name = "Bob"; }; # No sshKey - should be excluded
  };

in
lib.runTests {
  testCollectsNodeAndUserKeys = {
    expr = lib.length (collectAuthorizedKeys testNodes testUsers);
    expected = 3; # nodeA + nodeC + alice
  };

  testIncludesNodeKey = {
    expr = lib.elem "ssh-ed25519 AAAAC keyA" (collectAuthorizedKeys testNodes testUsers);
    expected = true;
  };

  testIncludesUserKey = {
    expr = lib.elem "ssh-ed25519 AAAAC keyAlice" (collectAuthorizedKeys testNodes testUsers);
    expected = true;
  };

  testExcludesNodeWithoutKey = {
    expr = lib.any (k: k == null) (collectAuthorizedKeys testNodes testUsers);
    expected = false;
  };

  testEmptyInputsGivesEmptyList = {
    expr = collectAuthorizedKeys { } { };
    expected = [ ];
  };
}
