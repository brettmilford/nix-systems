{
  lib,
  nodes,
  services,
  users,
}:

let
  mod = ../modules/nixos;
  sec = ../secrets;

  # Map services to their corresponding modules and options
  serviceModuleMap = {
    # Default monitoring clients (applied to all hosts)
    monitoring-rsyslog = {
      modules = [ "${mod}/monitoring/rsyslog" ];
      getOptions =
        hostname:
        let
          # Find all hosts that run the monitoring service
          monitoringHosts = lib.filter (host: services.lib.hasService host "monitoring") (
            lib.attrNames nodes
          );
        in
        {
          enable = true;
          servers = monitoringHosts;
        };
    };

    monitoring-node = {
      modules = [ "${mod}/monitoring/node-exporter" ];
      getOptions = hostname: {
        enable = true;
        port = 9100;
        # Use the node's IP if available, otherwise listen on all interfaces
        listenAddress = nodes.${hostname}.ip or "0.0.0.0";
        openFirewall = true;
      };
    };

    # Main monitoring server
    monitoring = {
      modules = [ "${mod}/monitoring" ];
      getSecrets =
        hostname:
        let
          monitoringService = services.services.monitoring or { };
        in
        {
          snmp_env = {
            file = "${sec}/snmp.env.age";
          };
          grafana_pass = {
            file = "${sec}/grafana_pass.age";
            owner = "grafana";
            group = "grafana";
          };
          grafana_key = {
            file = "${sec}/grafana_key.age";
            owner = "grafana";
            group = "grafana";
          };
          hass_prometheus_token = {
            file = "${sec}/hass_prometheus_token.age";
            owner = "prometheus";
            group = "prometheus";
          };
        };
      # TODO: inherit options like ports from services.<service>.config?
      # TODO: inherit options like ports from services.<service>.<host>.config - support overriding?
      getOptions =
        hostname:
        let
          monitoringService = services.services.monitoring or { };
          webServices = lib.filterAttrs (name: svc: svc ? fqdn) services.services;

          # Generate node-exporter targets for all hosts
          allHosts = lib.attrNames nodes;
          nodeExporterTargets = map (host: {
            hostname = host;
            ip = nodes.${host}.ip;
            port = 9100;
          }) allHosts;

          # Generate Home Assistant monitoring targets
          hassHosts = services.services.hass.hosts or [ ];
          hassTargets = map (host: {
            hostname = host;
            ip = nodes.${host}.ip;
            home_assistant_port = 8123;
            mqtt_exporter_port = 9641;
          }) hassHosts;

          # Generate UniFi monitoring targets
          unifiHosts = services.services.unifi-controller.hosts or [ ];
          unifiTargets = map (host: {
            hostname = host;
            ip = nodes.${host}.ip;
            unifi_poller_port = 9130;
          }) unifiHosts;

          # Legacy hosts list for backward compatibility
          legacyHosts = lib.unique (
            lib.flatten (lib.mapAttrsToList (name: svc: svc.hosts or [ ]) services.services)
          );
        in
        {
          enable = true;
          enableAlertManager = true;
          inherit (monitoringService) fqdn;
          targets = {
            # Node exporter targets for all hosts
            nodes = nodeExporterTargets;

            # Home Assistant monitoring targets
            hass = hassTargets;

            # UniFi monitoring targets
            unifi = unifiTargets;

            # Legacy targets for backward compatibility
            legacyNodes = map (host: {
              name = host;
              ip = nodes.${host}.ip;
            }) legacyHosts;

            web = lib.mapAttrsToList (name: svc: {
              inherit name;
              hosts = map (host: {
                name = host;
                ip = nodes.${host}.ip;
                port = (svc.config or { }).prometheusPort or 8080;
              }) (svc.hosts or [ ]);
            }) webServices;
            snmp = lib.optional (nodes ? opnsense) {
              name = "opnsense";
              ip = nodes.opnsense.ip;
            };
          };
        }
        // lib.optionalAttrs ((monitoringService.config or { }) ? ports) {
          ports = monitoringService.config.ports;
        };
    };

    cloud = {
      modules = [ "${mod}/cloud.nix" ];
      getOptions =
        hostname:
        let
          cloudService = services.services.cloud or { };
        in
        {
          enable = true;
          inherit (cloudService) fqdn;
          enableOffice = true;
          dataPath = services.lib.getServiceDataPath hostname "nextcloud";
        };
      getSecrets = hostname: {
        nextcloud-admin-passwd = {
          file = "${sec}/admin-passwd.age";
          owner = "nextcloud";
          group = "nextcloud";
        };

        "nextcloud-secrets.json" = {
          file = "${sec}/nextcloud-secrets.json.age";
          owner = "nextcloud";
          group = "nextcloud";
        };
      };
    };

    auth = {
      modules = [ "${mod}/auth.nix" ];
      getOptions =
        hostname:
        let
          authService = services.services.auth or { };
        in
        {
          enable = true;
          inherit (authService) fqdn;
        };
      getSecrets = hostname: {
        keycloak-db-passwd = {
          file = "${sec}/keycloak-db-passwd.age";
        };
      };
    };

    wg-gateway = {
      modules = [ "${mod}/wg-gateway" ];
      getOptions =
        hostname:
        let
          wgService = services.services.wg-gateway or { };
          wgConfig = wgService.config or { };
        in
        {
          enable = true;
          inherit (wgConfig) peers externalIP;
          externalInterface = wgConfig.externalInterface or "eth0";
          listenPort = wgConfig.listenPort or 51820;
          serverAddress = wgConfig.serverAddress or "172.16.0.1/16";
        };
      getSecrets = hostname: {
        wg_server_private = {
          file = "${sec}/wg_server_private.age";
          owner = "systemd-network";
          group = "systemd-network";
        };
      };
    };

    photos = {
      modules = [ "${mod}/photos.nix" ];
      getOptions =
        hostname:
        let
          immichService = services.services.photos or { };
        in
        {
          enable = true;
          inherit (immichService) fqdn;
          dataPath = services.lib.getServiceDataPath hostname "immich";
        };
      getSecrets = hostname: {
        "immich.json" = {
          file = "${sec}/immich.json.age";
          owner = "immich";
          group = "immich";
        };
      };
    };

    paperless-ngx = {
      modules = [ "${mod}/paperless" ];
      getOptions =
        hostname:
        let
          paperlessService = services.services.paperless-ngx or { };
        in
        {
          enable = true;
          inherit (paperlessService) fqdn;
          dataDir = services.lib.getServiceDataPath hostname "paperless";
        };
      getSecrets = hostname: {
        paperless-admin-passwd = {
          file = "${sec}/admin-passwd.age";
          owner = "paperless";
          group = "paperless";
        };

        "paperless.env" = {
          file = "${sec}/paperless.env.age";
          owner = "paperless";
          group = "paperless";
        };

        paperless-api-token = {
          file = "${sec}/paperless-api-token.age";
          owner = "paperless";
          group = "paperless";
        };
      };

    };

    backup = {
      modules = [ "${mod}/backup.nix" ];
      getOptions =
        hostname:
        let
          backupTargetRepos = services.lib.getBackupTargetConfig hostname;
          # Resolve node references and compute SSH keys here
          resolvedRepos = lib.mapAttrs (
            repoName: repoConfig:
            let
              sourceNode = nodes.${repoConfig.source} or { };
              authorizedKeys = lib.optional (sourceNode ? backupSshKey) sourceNode.backupSshKey;
            in
            repoConfig // { inherit authorizedKeys; }
          ) backupTargetRepos;
        in
        {
          enable = services.lib.isBackupTarget hostname;
          repos = resolvedRepos;
        };
    };

    hass = {
      modules = [ "${mod}/hass" ];
      getOptions =
        hostname:
        let
          hassService = services.services.hass or { };
          ftpHost = services.lib.primaryNode "ftp";
          ftpDataPath = services.lib.getServiceDataPath ftpHost "ftp";
          ftpIp = nodes.${ftpHost}.ip;
        in
        {
          enable = true;
          inherit (hassService) fqdn;
          nfsMounts = [
            {
              name = "reolink";
              server = ftpIp;
              remotePath = "${ftpDataPath}/reolink";
            }
          ];
        };
      getSecrets = hostname: {
        cfdCredentialsFile = {
          file = "${sec}/cfd_tunnel_config.json.age";
        };
      };
    };

    # Home Assistant monitoring (MQTT exporter + Prometheus endpoint)
    monitoring-hass = {
      modules = [ "${mod}/monitoring/mqtt-exporter" ];
      getOptions = hostname: {
        enable = services.lib.hasService hostname "hass";
      };
    };

    # UniFi monitoring (unifi-poller)
    monitoring-unifi = {
      modules = [ "${mod}/monitoring/unpoller" ];
      getOptions = hostname: {
        enable = services.lib.hasService hostname "unifi-controller";
      };
      getSecrets = hostname: {
        unifipoller_pass = {
          file = "${sec}/unifipoller_pass.age";
          owner = "unifi-poller";
        };
      };
    };

    unifi-controller = {
      modules = [ "${mod}/unifi-controller" ];
      getOptions =
        hostname:
        let
          unifiService = services.services.unifi-controller or { };
        in
        {
          enable = true;
          docker-compose = true;
          inherit (unifiService) fqdn;
        };
      getSecrets = hostname: {
        dockerEnv = {
          file = "${sec}/unifi-controller.env.age";
        };
      };
    };

    realestate-calculators = {
      modules = [ "${mod}/realestate-calculators.nix" ];
      getOptions =
        hostname:
        let
          svc = services.services.realestate-calculators or { };
        in
        {
          enable = true;
          fqdn = svc.fqdn;
          rev = svc.config.rev;
        };
    };

    garmin-collect = {
      modules = [ "${mod}/garmin-collect" ];
      getOptions =
        hostname:
        let
          svc = services.services.garmin-collect or { };
        in
        {
          enable = true;
          fqdn = svc.fqdn;
          rev = svc.config.rev;
        }
        // lib.optionalAttrs ((svc.config or { }) ? measurePort) { measurePort = svc.config.measurePort; };
      getSecrets = hostname: {
        "garmin-collect.env" = {
          file = "${sec}/garmin-collect.env.age";
          owner = "garmin";
          group = "garmin";
        };
      };
    };

    account-service = {
      modules = [ "${mod}/account-service" ];
      getOptions =
        hostname:
        let
          svc = services.services.account-service or { };
        in
        {
          enable = true;
          fqdn = svc.fqdn;
          rev = svc.config.rev;
        }
        // lib.optionalAttrs ((svc.config or { }) ? port) { port = svc.config.port; }
        // lib.optionalAttrs ((svc.config or { }) ? webhookPort) { webhookPort = svc.config.webhookPort; };
      getSecrets = hostname: {
        "account-service.env" = {
          file = "${sec}/account-service.env.age";
          owner = "accsvc";
          group = "accsvc";
        };
      };
    };

    ftp = {
      modules = [ "${mod}/ftp" ];
      getOptions =
        hostname:
        let
          svc = services.services.ftp or { };
        in
        {
          enable = true;
          fqdn = svc.fqdn;
          dataPath = services.lib.getServiceDataPath hostname "ftp";
          lanAddress = nodes.${hostname}.ip;
        };
    };

    nfs-server = {
      modules = [ "${mod}/nfs-server" ];
      getOptions =
        hostname:
        let
          ftpHost = services.lib.primaryNode "ftp";
          ftpDataPath = services.lib.getServiceDataPath ftpHost "ftp";
          hassHost = services.lib.primaryNode "hass";
          hassIp = nodes.${hassHost}.ip;
        in
        {
          enable = true;
          exports = [
            {
              path = "${ftpDataPath}/reolink";
              clients = [ hassIp ];
            }
          ];
        };
    };

    git-server = {
      modules = [ "${mod}/gitServer.nix" ];
      getOptions =
        hostname:
        let
          dataPath = services.lib.getServiceDataPath hostname "git";
          # Collect SSH public keys from all nodes and users
          authorizedKeys =
            lib.mapAttrsToList (_name: node: node.backupSshKey) (
              lib.filterAttrs (_name: node: node ? backupSshKey) nodes
            )
            ++ lib.mapAttrsToList (_name: user: user.sshKey) (
              lib.filterAttrs (_name: user: user ? sshKey) users
            )
            ++ (services.services."git-server".config.extraAuthorizedKeys or []);
        in
        {
          enable = true;
          inherit dataPath authorizedKeys;
        };
    };

    mail-relay = {
      modules = [ "${mod}/mail-relay" ];
      getSecrets =
        hostname:
        lib.optionalAttrs (hostname == "calliope") {
          postfix-sasl-passwd = {
            file = "${sec}/postfix-sasl-passwd.age";
          };
        };
      getOptions =
        hostname:
        let
          domain = services.services.domain;
        in
        if hostname == "calliope" then
          {
            enable = true;
            relayhost = "127.0.0.1:1025";
            inherit domain;
            hostname = hostname + "." + domain;
            saslAuth = {
              enable = true;
              secretPath = "/run/agenix/postfix-sasl-passwd";
            };
            listenInterfaces = [
              "localhost"
              "172.16.0.1"
            ];
            mynetworks = [
              "127.0.0.0/8"
              "172.16.0.0/16"
            ];
          }
        else
          {
            enable = true;
            relayhost = "172.16.0.1:25";
            inherit domain;
            hostname = hostname + "." + domain;
            saslAuth.enable = false;
          };
    };
  };

  # Factory function that takes hostname and returns modules + service options
  createModulesForHost =
    hostname:
    let
      # Get services based on hosts arrays in services.nix
      regularServices = lib.filter (serviceName: services.lib.hasService hostname serviceName) (
        lib.attrNames serviceModuleMap
      );

      # Default monitoring clients for all hosts
      defaultClients = [
        "monitoring-rsyslog"
        "monitoring-node"
      ];

      # Conditional clients based on services
      conditionalClients =
        lib.optionals (services.lib.hasService hostname "hass") [
          "monitoring-hass"
        ]
        ++ lib.optionals (services.lib.hasService hostname "unifi-controller") [
          "monitoring-unifi"
        ];

      # Combine regular services with default and conditional clients
      hostServices = lib.unique (regularServices ++ defaultClients ++ conditionalClients);

      # Get modules and options for each service
      serviceResults = map (
        serviceName:
        let
          serviceConfig = serviceModuleMap.${serviceName};
        in
        {
          modules = serviceConfig.modules;
          options = serviceConfig.getOptions hostname;
          secrets =
            if serviceConfig ? getSecrets then
              serviceConfig.getSecrets hostname
            else
              (serviceConfig.secrets or { });
          serviceName = serviceName;
        }
      ) hostServices;

      # Flatten modules
      allModules = lib.flatten (map (result: result.modules) serviceResults);

      # Create services.* configuration
      serviceOptions = lib.foldl' (
        acc: result: acc // { ${result.serviceName} = result.options; }
      ) { } serviceResults;

      # Collect all secrets from all services
      # nix eval --json '.#nixosConfigurations.eurydice.config.age.secrets' --apply 'secrets: builtins.mapAttrs (name: cfg: { file = cfg.file; owner = cfg.owner or "root"; }) secrets' | jq
      allSecrets = lib.foldl' (acc: result: acc // result.secrets) { } serviceResults;

    in
    {
      modules = allModules;
      inherit serviceOptions;

      # Create a module that configures services.* and age.secrets
      optionsModule =
        { config, ... }:
        {
          services = lib.mapAttrs (
            serviceName: serviceConfig:
            let
              # Get secrets for this specific service
              serviceResult = lib.findFirst (r: r.serviceName == serviceName) null serviceResults;
              serviceSecrets = if serviceResult != null then serviceResult.secrets else { };
              hasSecrets = serviceSecrets != { };

              # Generate secretPaths from the service's secrets
              secretPaths = lib.mapAttrs (
                secretName: secretConfig: config.age.secrets.${secretName}.path
              ) serviceSecrets;
            in
            if hasSecrets then serviceConfig // { inherit secretPaths; } else serviceConfig
          ) serviceOptions;
          age.secrets = allSecrets;
        };
    };

in
{
  inherit createModulesForHost serviceModuleMap;
}
