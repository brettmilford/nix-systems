{ lib }:

let
  inherit
    (import ../serviceMap.nix {
      inherit lib;
      hosts = testHosts;
      services = testServices;
      domain = "test.com";
    })
    hasService
    primaryHost
    hostsFor
    getServiceDataPath
    shouldBackup
    isBackupTarget
    ;

  # Test data
  testHosts = {
    host1 = {
      hostname = "host1";
      ip = "192.168.1.10";
      dataPath = "/srv/data";
      system = "x86_64-linux";
    };
    host2 = {
      hostname = "host2";
      ip = "192.168.1.11";
      dataPath = "/var/lib";
      system = "x86_64-linux";
    };
  };

  testServices = {
    testApp = {
      hosts = [
        "host1"
        "host2"
      ];
      fqdn = "app.test.com";
      config = {
        dataPath = "custom-app";
        port = 8080;
      };
    };
    singleService = {
      hosts = [ "host1" ];
    };
    backup = {
      config = {
        repos = {
          host1 = {
            targets = [ "host2" ];
          };
        };
      };
    };
  };

in
lib.runTests {
  testHasService = {
    expr = hasService "host1" "testApp";
    expected = true;
  };

  testPrimaryHost = {
    expr = primaryHost "testApp";
    expected = "host1";
  };

  testHostsFor = {
    expr = hostsFor "testApp";
    expected = [
      "host1"
      "host2"
    ];
  };

  testGetServiceDataPath = {
    expr = getServiceDataPath "host1" "testApp";
    expected = "/srv/data/custom-app";
  };

  testShouldBackup = {
    expr = shouldBackup "host1";
    expected = true;
  };

  testIsBackupTarget = {
    expr = isBackupTarget "host2";
    expected = true;
  };
}
