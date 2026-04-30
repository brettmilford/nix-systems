# Alerting & Mail Relay Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add email alerting for borg failures, disk space, and paperless consume errors via a new mail-relay module and configured Alertmanager.

**Architecture:** A reusable `mail-relay` NixOS module wraps postfix config. Calliope runs Protonmail Bridge + postfix (relay for itself and other hosts). Terpsichore runs postfix relaying to calliope. Alertmanager on terpsichore sends alerts via local postfix. Prometheus handles borg/disk alerts; Loki ruler handles paperless log alerts.

**Tech Stack:** NixOS modules, Nix flake, Postfix, Protonmail Bridge, Prometheus, Alertmanager, Loki

---

### Task 1: Create mail-relay module

**Files:**
- Create: `modules/nixos/mail-relay/default.nix`

- [ ] **Step 1: Create the module file**

```nix
{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.mail-relay;
in
{
  options.services.mail-relay = {
    enable = mkEnableOption "outbound mail relay";

    relayhost = mkOption {
      type = types.str;
      example = "127.0.0.1:1025";
      description = "SMTP relay destination";
    };

    hostname = mkOption {
      type = types.str;
      example = "mail.cirriform.au";
      description = "HELO/EHLO hostname for postfix";
    };

    domain = mkOption {
      type = types.str;
      example = "cirriform.au";
      description = "Mail origin domain";
    };

    saslAuth = {
      enable = mkEnableOption "SASL authentication to relay";

      secretPath = mkOption {
        type = types.str;
        default = "";
        description = "Path to file containing SASL credentials for relayhost";
      };
    };

    listenInterfaces = mkOption {
      type = types.listOf types.str;
      default = [ "localhost" ];
      description = "Interfaces postfix listens on";
    };

    mynetworks = mkOption {
      type = types.listOf types.str;
      default = [ "127.0.0.0/8" ];
      description = "Networks allowed to relay through this postfix";
    };
  };

  config = mkIf cfg.enable {
    services.postfix = {
      enable = true;
      setSendmail = true;

      settings.main.relayhost = [ cfg.relayhost ];

      config = {
        myhostname = cfg.hostname;
        mydomain = cfg.domain;
        myorigin = "$mydomain";
        inet_protocols = "all";
        inet_interfaces = concatStringsSep ", " cfg.listenInterfaces;
        mydestination = "";
        mynetworks = concatStringsSep ", " cfg.mynetworks;
        bounce_sender = "admin@${cfg.domain}";
        smtp_tls_security_level = "may";
      }
      // optionalAttrs cfg.saslAuth.enable {
        smtp_sasl_auth_enable = "yes";
        smtp_sasl_security_options = "noanonymous";
        smtp_sasl_password_maps = "texthash:${cfg.saslAuth.secretPath}";
        smtp_use_tls = "no";
        smtp_sasl_mechanism_filter = "plain,login";
      };
    };
  };
}
```

- [ ] **Step 2: Verify the module evaluates**

Run: `nix flake check 2>&1 | tail -10`

Expected: No errors related to mail-relay (module is defined but not yet used).

- [ ] **Step 3: Commit**

```bash
git add modules/nixos/mail-relay/default.nix
git commit -m "Add mail-relay module"
```

---

### Task 2: Create Protonmail Bridge host config for calliope

**Files:**
- Create: `hosts/nixos/calliope/bridge.nix`

- [ ] **Step 1: Create bridge.nix with the Protonmail Bridge config extracted from postfix.nix**

```nix
{ pkgs, ... }:

{
  services.protonmail-bridge = {
    enable = true;
    path = [ pkgs.pass ];
    logLevel = "info";
  };
}
```

- [ ] **Step 2: Commit**

```bash
git add hosts/nixos/calliope/bridge.nix
git commit -m "Extract Protonmail Bridge config to bridge.nix"
```

---

### Task 3: Migrate calliope from postfix.nix to mail-relay module

**Files:**
- Modify: `hosts/nixos/calliope/default.nix:9-17` (imports)
- Delete: `hosts/nixos/calliope/postfix.nix`

- [ ] **Step 1: Update calliope's imports — replace `./postfix.nix` with `./bridge.nix`**

In `hosts/nixos/calliope/default.nix`, change line 16 from:

```nix
    ./postfix.nix
```

to:

```nix
    ./bridge.nix
```

- [ ] **Step 2: Delete the old postfix.nix**

```bash
git rm hosts/nixos/calliope/postfix.nix
```

- [ ] **Step 3: Commit**

```bash
git add hosts/nixos/calliope/default.nix
git commit -m "Migrate calliope from postfix.nix to bridge.nix"
```

---

### Task 4: Wire mail-relay into services.nix and serviceModules.nix

**Files:**
- Modify: `services.nix` (add mail-relay service after git-server block, around line 156)
- Modify: `lib/serviceModules.nix` (add mail-relay entry in serviceModuleMap)

- [ ] **Step 1: Add mail-relay to services.nix**

Insert before the closing `}` at end of file (after the `git-server` block):

```nix
  mail-relay = {
    hosts = [
      "calliope"
      "terpsichore"
    ];
  };
```

- [ ] **Step 2: Add mail-relay mapping to serviceModules.nix**

Insert a new entry in `serviceModuleMap` (after the `git-server` entry around line 476):

```nix
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
          isCalliope = hostname == "calliope";
        in
        {
          enable = true;
          hostname = "mail.cirriform.au";
          domain = "cirriform.au";
          relayhost = if isCalliope then "127.0.0.1:1025" else "172.16.0.1:25";
          saslAuth = lib.optionalAttrs isCalliope {
            enable = true;
            secretPath = config.age.secrets.postfix-sasl-passwd.path;
          };
        }
        // lib.optionalAttrs isCalliope {
          listenInterfaces = [
            "localhost"
            "172.16.0.1"
          ];
          mynetworks = [
            "127.0.0.0/8"
            "172.16.0.0/16"
          ];
        };
    };
```

Note: The `getSecrets` function handles the agenix secret. The `getOptions` function references `config.age.secrets.postfix-sasl-passwd.path` — check that the serviceModules framework passes `config` in scope. If not, the secret path can be hardcoded as a string pattern or the module can reference the secret path directly. Verify during implementation by checking how other modules with secrets (e.g., `backup`, `monitoring`) reference their secret paths.

- [ ] **Step 3: Verify the flake evaluates**

Run: `nix flake check 2>&1 | tail -10`

Expected: Pass with no new errors. The mail-relay module should now be applied to calliope and terpsichore.

- [ ] **Step 4: Commit**

```bash
git add services.nix lib/serviceModules.nix
git commit -m "Wire mail-relay into service catalog"
```

---

### Task 5: Enable Alertmanager and configure email

**Files:**
- Modify: `lib/serviceModules.nix:108-109` (monitoring getOptions)
- Modify: `modules/nixos/monitoring/alertmanager/default.nix`

- [ ] **Step 1: Enable alertmanager in monitoring getOptions**

In `lib/serviceModules.nix`, in the monitoring `getOptions` return value (around line 108-109), add `enableAlertManager`:

```nix
        {
          enable = true;
          enableAlertManager = true;
          inherit (monitoringService) fqdn;
```

- [ ] **Step 2: Configure alertmanager email receiver**

Replace the entire contents of `modules/nixos/monitoring/alertmanager/default.nix`:

```nix
{ config, lib, pkgs, ... }:
let
  cfg = config.services.monitoring;
in
{
  services.prometheus.alertmanager = {
    port = cfg.ports.alertmanager;
    listenAddress = "localhost";
    webExternalUrl = "http://${cfg.fqdn}/alertmanager/";
    extraFlags = [ "--web.route-prefix=/" ];

    configuration = {
      global = {
        smtp_smarthost = "localhost:25";
        smtp_from = "admin@cirriform.au";
        smtp_require_tls = false;
      };

      route = {
        group_by = [ "alertname" ];
        group_wait = "30s";
        group_interval = "5m";
        repeat_interval = "4h";
        receiver = "email";
      };

      receivers = [
        {
          name = "email";
          email_configs = [
            {
              to = "brettmilford@gmail.com";
              send_resolved = true;
            }
          ];
        }
      ];
    };
  };

  services.nginx.virtualHosts."${cfg.fqdn}".locations."/alertmanager/" = lib.mkIf cfg.enableAlertManager {
    proxyPass = "http://localhost:${toString cfg.ports.alertmanager}/";
  };
}
```

- [ ] **Step 3: Verify the flake evaluates**

Run: `nix flake check 2>&1 | tail -10`

Expected: Pass.

- [ ] **Step 4: Commit**

```bash
git add lib/serviceModules.nix modules/nixos/monitoring/alertmanager/default.nix
git commit -m "Enable alertmanager with email receiver"
```

---

### Task 6: Add Prometheus alert rules and alertmanager endpoint

**Files:**
- Modify: `modules/nixos/monitoring/prometheus/default.nix:7-61` (rules and alertmanagers)

- [ ] **Step 1: Add alertmanagers config and new alert rules**

In `modules/nixos/monitoring/prometheus/default.nix`, add `alertmanagers` inside `services.prometheus` (after `checkConfig` around line 13), and append new rule groups to the existing `rules` list.

Add after line 13 (`checkConfig = "syntax-only";`):

```nix
      alertmanagers = lib.optionals config.services.prometheus.alertmanager.enable [
        {
          static_configs = [
            {
              targets = [ "localhost:${toString cfg.ports.alertmanager}" ];
            }
          ];
        }
      ];
```

Add the following alert groups to the `rules` list (inside the `groups` list, after the `HighMemoryUsage` rule closing brace on line 56):

```nix
                {
                  alert = "BorgBackupFailed";
                  expr = ''node_systemd_unit_state{name=~"borgbackup-job-.*", state="failed"} == 1'';
                  for = "5m";
                  labels.severity = "critical";
                  annotations = {
                    summary = "Borg backup failed on {{ $labels.instance }}";
                    description = "{{ $labels.name }} has been in failed state for more than 5 minutes.";
                  };
                }
                {
                  alert = "DiskSpaceWarning";
                  expr = ''(node_filesystem_avail_bytes{fstype!~"tmpfs|overlay|squashfs|iso9660|efivarfs|nsfs"} / node_filesystem_size_bytes) < 0.1'';
                  for = "10m";
                  labels.severity = "warning";
                  annotations = {
                    summary = "Disk space low on {{ $labels.instance }}";
                    description = "{{ $labels.mountpoint }} has less than 10% free space.";
                  };
                }
                {
                  alert = "DiskSpaceCritical";
                  expr = ''(node_filesystem_avail_bytes{fstype!~"tmpfs|overlay|squashfs|iso9660|efivarfs|nsfs"} / node_filesystem_size_bytes) < 0.05'';
                  for = "10m";
                  labels.severity = "critical";
                  annotations = {
                    summary = "Disk space critically low on {{ $labels.instance }}";
                    description = "{{ $labels.mountpoint }} has less than 5% free space.";
                  };
                }
```

- [ ] **Step 2: Verify the flake evaluates**

Run: `nix flake check 2>&1 | tail -10`

Expected: Pass.

- [ ] **Step 3: Commit**

```bash
git add modules/nixos/monitoring/prometheus/default.nix
git commit -m "Add borg and disk space alert rules, connect alertmanager"
```

---

### Task 7: Add Loki ruler for paperless alerts

**Files:**
- Modify: `modules/nixos/monitoring/loki/default.nix`

- [ ] **Step 1: Create the Loki alert rule file**

In `modules/nixos/monitoring/loki/default.nix`, add a `let` binding for the rule file and the ruler configuration. Replace the file contents with:

```nix
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.monitoring;

  lokiRulesDir = pkgs.runCommand "loki-rules" { } ''
    mkdir -p $out/fake
    cat > $out/fake/paperless.yaml << 'EOF'
    groups:
      - name: paperless
        rules:
          - alert: PaperlessConsumeError
            expr: 'count_over_time({app=~"paperless.*", host="calliope"} |~ "ERROR" [15m]) > 0'
            for: 0s
            labels:
              severity: warning
            annotations:
              summary: "Paperless consume error on calliope"
              description: "Paperless consumer logged errors in the last 15 minutes. Check paperless-consumer logs."
    EOF
  '';
in
{
  services.loki = {

    configuration = {
      auth_enabled = false;

      analytics = {
        reporting_enabled = false;
      };

      server = {
        http_listen_port = cfg.ports.loki;
        http_listen_address = "localhost";
        grpc_server_max_recv_msg_size = 52428800;
        grpc_server_max_send_msg_size = 52428800;
      };

      query_scheduler = {
        grpc_client_config = {
          max_recv_msg_size = 52428800;
          max_send_msg_size = 52428800;
        };
      };

      frontend_worker = {
        grpc_client_config = {
          max_recv_msg_size = 52428800;
          max_send_msg_size = 52428800;
        };
      };

      ingester_client = {
        grpc_client_config = {
          max_recv_msg_size = 52428800;
          max_send_msg_size = 52428800;
        };
      };

      limits_config = {
        max_entries_limit_per_query = 1000000;
        max_query_length = "12000h";
        reject_old_samples = true;
        reject_old_samples_max_age = "168h";
        allow_structured_metadata = true;
      };

      common = {
        ring = {
          instance_addr = "127.0.0.1";
          kvstore = {
            store = "inmemory";
          };
        };
        replication_factor = 1;
        path_prefix = "/var/lib/loki";
      };

      schema_config = {
        configs = [
          {
            from = "2024-01-01";
            store = "tsdb";
            object_store = "filesystem";
            schema = "v13";
            index = {
              prefix = "index_";
              period = "24h";
            };
          }
        ];
      };

      storage_config = {
        tsdb_shipper = {
          active_index_directory = "/var/lib/loki/tsdb-index";
          cache_location = "/var/lib/loki/tsdb-cache";
        };
        filesystem = {
          directory = "/var/lib/loki/chunks";
        };
      };

      compactor = {
        working_directory = "/var/lib/loki/compactor";
        compaction_interval = "5m";
      };

      ruler = {
        storage = {
          type = "local";
          local = {
            directory = "${lokiRulesDir}";
          };
        };
        rule_path = "/var/lib/loki/rules-temp";
        alertmanager_url = "http://localhost:${toString cfg.ports.alertmanager}";
        ring = {
          kvstore = {
            store = "inmemory";
          };
        };
        enable_api = true;
      };
    };
  };
}
```

Note: The `limits_config` had a duplicate key in the original file (defined twice). This version merges both into a single `limits_config` block.

- [ ] **Step 2: Verify the flake evaluates**

Run: `nix flake check 2>&1 | tail -10`

Expected: Pass.

- [ ] **Step 3: Commit**

```bash
git add modules/nixos/monitoring/loki/default.nix
git commit -m "Add Loki ruler with paperless consume alert"
```

---

### Task 8: Final verification

**Files:** None (verification only)

- [ ] **Step 1: Run full flake check**

Run: `nix flake check 2>&1 | tail -15`

Expected: All checks pass.

- [ ] **Step 2: Build calliope config to verify no evaluation errors**

Run: `nix build '.#nixosConfigurations.calliope.config.system.build.toplevel' --dry-run 2>&1 | tail -10`

Expected: No errors.

- [ ] **Step 3: Build terpsichore config to verify no evaluation errors**

Run: `nix build '.#nixosConfigurations.terpsichore.config.system.build.toplevel' --dry-run 2>&1 | tail -10`

Expected: No errors.

- [ ] **Step 4: Verify postfix is no longer imported directly on calliope**

Run: `grep -r "postfix.nix" hosts/nixos/calliope/`

Expected: No results.

- [ ] **Step 5: Commit and push**

```bash
git push origin devel
```

---

## Post-deployment verification

After deploying calliope and terpsichore:

1. **Test mail relay on calliope:** `echo "test" | mail -s "calliope test" brettmilford@gmail.com`
2. **Test mail relay on terpsichore:** `echo "test" | mail -s "terpsichore test" brettmilford@gmail.com`
3. **Verify alertmanager is running:** `curl -s http://localhost:9093/api/v2/status | jq .`
4. **Verify prometheus alert rules:** `curl -s http://localhost:9090/api/v1/rules | jq '.data.groups[].rules[].name'`
5. **Verify Loki ruler:** `curl -s http://localhost:3100/loki/api/v1/rules | jq .`
6. **Check paperless log labels in Grafana Explore** — query `{host="calliope"} |~ "paperless"` and check the `app` label value. Update the Loki rule's `app` matcher if it differs from `paperless.*`.
