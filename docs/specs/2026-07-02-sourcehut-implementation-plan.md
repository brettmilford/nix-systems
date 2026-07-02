# SourceHut Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deploy SourceHut services on terpsichore via Incus Alpine containers with host-native PostgreSQL, Redis, and nginx.

**Architecture:** A reusable `incus.nix` module provides container infrastructure. The `sourcehut/` module imports it and adds PostgreSQL, Redis, and a container factory. Each SourceHut service (meta, git, todo, hub, man, pages, builds) gets an Alpine container with static IP, provisioned declaratively via systemd activation scripts. nginx on the host provides TLS termination and reverse proxy.

**Tech Stack:** NixOS modules, Incus system containers, Alpine Linux, PostgreSQL, Redis, nginx (gateway.nix), agenix secrets

---

## File Structure

| Action | File | Responsibility |
|--------|------|----------------|
| Create | `modules/nixos/incus.nix` | Reusable Incus infrastructure (DONE) |
| Create | `modules/nixos/sourcehut/default.nix` | SourceHut options, imports, shared infra (PostgreSQL, Redis, Incus) |
| Create | `modules/nixos/sourcehut/containers.nix` | Container factory function + all 7 service container definitions |
| Modify | `services.nix` | Add `sourcehut` service entry |
| Modify | `lib/serviceModules.nix` | Add `sourcehut` to `serviceModuleMap` |
| Modify | `secrets/secrets.nix` | Add SourceHut secret public key declarations |

---

### Task 1: Validate incus.nix module

**Files:**
- Already created: `modules/nixos/incus.nix`

- [ ] **Step 1: Run nix flake check**

Run: `nix flake check`
Expected: PASS (incus.nix is not yet imported by any host, so it won't cause errors)

- [ ] **Step 2: Format with nixfmt-rfc-style**

Run: `nixfmt-rfc-style modules/nixos/incus.nix`
Expected: No errors

- [ ] **Step 3: Commit**

```bash
git add modules/nixos/incus.nix
git commit -m "feat: add reusable incus.nix infrastructure module"
```

---

### Task 2: Create sourcehut/default.nix — shared infrastructure

**Files:**
- Create: `modules/nixos/sourcehut/default.nix`

- [ ] **Step 1: Create the sourcehut directory**

```bash
mkdir -p modules/nixos/sourcehut
```

- [ ] **Step 2: Write default.nix**

Create `modules/nixos/sourcehut/default.nix`:

```nix
{ self, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.sourcehut;

  serviceDefs = {
    meta   = { ip = "10.0.100.10"; webPort = 5000; apiPort = 5100; };
    git    = { ip = "10.0.100.11"; webPort = 5001; apiPort = 5101; };
    todo   = { ip = "10.0.100.12"; webPort = 5002; apiPort = 5102; };
    hub    = { ip = "10.0.100.13"; webPort = 5003; apiPort = 5103; };
    man    = { ip = "10.0.100.14"; webPort = 5004; apiPort = 5104; };
    pages  = { ip = "10.0.100.15"; webPort = 5005; apiPort = 5105; };
    builds = { ip = "10.0.100.16"; webPort = 5006; apiPort = 5106; };
  };

  enabledServices = lib.filterAttrs (name: _: builtins.elem name cfg.services) serviceDefs;
in
{
  imports = [
    ../incus.nix
    ../gateway.nix
    ./containers.nix
  ];

  options.services.sourcehut = {
    enable = mkEnableOption "SourceHut";

    domain = mkOption {
      type = types.str;
      default = "cirriform.au";
      description = "Base domain for SourceHut services";
    };

    services = mkOption {
      type = types.listOf (types.enum [
        "meta" "git" "todo" "hub" "man" "pages" "builds"
      ]);
      default = [ "meta" "git" "todo" "hub" "man" "pages" "builds" ];
      description = "Which SourceHut services to enable";
    };

    dataPath = mkOption {
      type = types.str;
      default = "/srv/data/sourcehut";
      description = "Base data directory for SourceHut";
    };

    secretPaths = mkOption {
      type = types.attrs;
      description = "Agenix secret file paths (auto-populated by framework)";
    };

    serviceDefs = mkOption {
      type = types.attrs;
      internal = true;
      default = enabledServices;
      description = "Resolved service definitions for enabled services";
    };
  };

  config = mkIf cfg.enable {

    services.incus = {
      enable = true;
      dataPath = "${cfg.dataPath}/containers";
    };

    services.gateway.enable = true;

    services.postgresql = {
      enable = true;
      enableTCPIP = true;
      authentication = ''
        host all all 10.0.100.0/24 scram-sha-256
      '';
      ensureDatabases = map (name: "${name}_srht") cfg.services;
      ensureUsers = map (name: {
        name = "${name}_srht";
        ensureDBOwnership = true;
      }) cfg.services;
    };

    services.redis.servers.sourcehut = {
      enable = true;
      bind = "10.0.100.1";
      port = 6379;
    };

    services.nginx.virtualHosts = lib.mapAttrs' (name: svc:
      lib.nameValuePair "${name}.${cfg.domain}" {
        locations."/" = {
          proxyPass = "http://${svc.ip}:${toString svc.webPort}";
        };
        locations."/query" = {
          proxyPass = "http://${svc.ip}:${toString svc.apiPort}";
        };
      }
    ) enabledServices;

    systemd.tmpfiles.rules = [
      "d ${cfg.dataPath} 0755 root root -"
      "d ${cfg.dataPath}/containers 0755 root root -"
    ];
  };
}
```

- [ ] **Step 3: Format**

Run: `nixfmt-rfc-style modules/nixos/sourcehut/default.nix`

- [ ] **Step 4: Commit**

```bash
git add modules/nixos/sourcehut/default.nix
git commit -m "feat: add sourcehut module with shared infrastructure"
```

---

### Task 3: Create sourcehut/containers.nix — container factory

**Files:**
- Create: `modules/nixos/sourcehut/containers.nix`

- [ ] **Step 1: Write containers.nix**

Create `modules/nixos/sourcehut/containers.nix`:

```nix
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.sourcehut;

  mkContainerService = name: svc:
    let
      containerName = "srht-${name}";
      secretName = "srht-${name}-config";
      extraConfig =
        if name == "builds" then ''
          incus config set ${containerName} security.nesting=true
          incus config device add ${containerName} kvm unix-char path=/dev/kvm 2>/dev/null || true
        '' else "";
    in
    lib.nameValuePair "srht-container-${name}" {
      description = "SourceHut ${name}.sr.ht container";
      wantedBy = [ "multi-user.target" ];
      after = [ "incus.service" "incus-preseed.service" "postgresql.service" "redis-sourcehut.service" ];
      requires = [ "incus.service" ];
      path = [ config.virtualisation.incus.package pkgs.bash ];

      script = ''
        if ! incus info ${containerName} &>/dev/null; then
          incus launch images:alpine/edge ${containerName}
          incus config set ${containerName} boot.autostart true
          incus config device set ${containerName} eth0 ipv4.address=${svc.ip}
          ${extraConfig}

          incus exec ${containerName} -- sh -c '
            setup-apkrepos -c -e
            wget -qO /etc/apk/keys/alpine@sr.ht.rsa.pub \
              https://mirror.sr.ht/alpine/sr.ht.rsa.pub
            echo "https://mirror.sr.ht/alpine/edge/sr.ht" \
              >> /etc/apk/repositories
            apk update
            apk add ${name}.sr.ht
            rc-update add ${name}.sr.ht-api default
            rc-update add ${name}.sr.ht default
          '

          incus exec ${containerName} -- sourcehut-migrate ${name}.sr.ht init
        fi

        incus file push \
          ${cfg.secretPaths.${secretName}} \
          ${containerName}/etc/sr.ht/config.ini

        incus start ${containerName} || true
        incus exec ${containerName} -- rc-service ${name}.sr.ht-api restart
        incus exec ${containerName} -- rc-service ${name}.sr.ht restart
      '';

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
    };

  containerServices = lib.mapAttrs' mkContainerService cfg.serviceDefs;
in
{
  config = mkIf cfg.enable {
    systemd.services = containerServices;
  };
}
```

- [ ] **Step 2: Format**

Run: `nixfmt-rfc-style modules/nixos/sourcehut/containers.nix`

- [ ] **Step 3: Commit**

```bash
git add modules/nixos/sourcehut/containers.nix
git commit -m "feat: add sourcehut container factory for Alpine Incus containers"
```

---

### Task 4: Add sourcehut to services.nix

**Files:**
- Modify: `services.nix`

- [ ] **Step 1: Add sourcehut entry to services.nix**

Add the following entry to `services.nix` (after the `opencode-web` entry):

```nix
  sourcehut = {
    hosts = [ "terpsichore" ];
    config = {
      dataPath = "sourcehut";
    };
  };
```

- [ ] **Step 2: Commit**

```bash
git add services.nix
git commit -m "feat: add sourcehut service to catalog"
```

---

### Task 5: Add sourcehut to serviceModules.nix

**Files:**
- Modify: `lib/serviceModules.nix`

- [ ] **Step 1: Add sourcehut entry to serviceModuleMap**

Add the following entry to `serviceModuleMap` in `lib/serviceModules.nix` (after the `opencode-web` entry):

```nix
    sourcehut = {
      modules = [ "${mod}/sourcehut" ];
      getOptions =
        hostname:
        let
          svc = services.services.sourcehut or { };
        in
        {
          enable = true;
          dataPath = services.lib.getServiceDataPath hostname "sourcehut";
        };
      getSecrets = hostname: {
        "srht-meta-config" = {
          file = "${sec}/srht-meta-config.age";
          owner = "root";
          group = "root";
        };
        "srht-git-config" = {
          file = "${sec}/srht-git-config.age";
          owner = "root";
          group = "root";
        };
        "srht-todo-config" = {
          file = "${sec}/srht-todo-config.age";
          owner = "root";
          group = "root";
        };
        "srht-hub-config" = {
          file = "${sec}/srht-hub-config.age";
          owner = "root";
          group = "root";
        };
        "srht-man-config" = {
          file = "${sec}/srht-man-config.age";
          owner = "root";
          group = "root";
        };
        "srht-pages-config" = {
          file = "${sec}/srht-pages-config.age";
          owner = "root";
          group = "root";
        };
        "srht-builds-config" = {
          file = "${sec}/srht-builds-config.age";
          owner = "root";
          group = "root";
        };
        "srht-shared-secret" = {
          file = "${sec}/srht-shared-secret.age";
          owner = "root";
          group = "root";
        };
      };
    };
```

- [ ] **Step 2: Commit**

```bash
git add lib/serviceModules.nix
git commit -m "feat: add sourcehut to service module map with secrets"
```

---

### Task 6: Add SourceHut secrets to secrets.nix

**Files:**
- Modify: `secrets/secrets.nix`

- [ ] **Step 1: Add secret entries to secrets.nix**

Add the following lines to `secrets/secrets.nix` (before the closing `}`):

```nix
  "srht-meta-config.age".publicKeys = [nix terpsichore brett];
  "srht-git-config.age".publicKeys = [nix terpsichore brett];
  "srht-todo-config.age".publicKeys = [nix terpsichore brett];
  "srht-hub-config.age".publicKeys = [nix terpsichore brett];
  "srht-man-config.age".publicKeys = [nix terpsichore brett];
  "srht-pages-config.age".publicKeys = [nix terpsichore brett];
  "srht-builds-config.age".publicKeys = [nix terpsichore brett];
  "srht-shared-secret.age".publicKeys = [nix terpsichore brett];
```

- [ ] **Step 2: Commit**

```bash
git add secrets/secrets.nix
git commit -m "feat: add sourcehut agenix secret declarations"
```

---

### Task 7: Validate with nix flake check

**Files:**
- All previously created/modified files

- [ ] **Step 1: Format all new and modified files**

```bash
nixfmt-rfc-style modules/nixos/incus.nix modules/nixos/sourcehut/default.nix modules/nixos/sourcehut/containers.nix services.nix lib/serviceModules.nix secrets/secrets.nix
```

- [ ] **Step 2: Run nix flake check**

Run: `nix flake check`
Expected: PASS (no evaluation errors). Note: the `.age` secret files don't exist yet, but `nix flake check` only evaluates Nix expressions — it doesn't check that agenix secret files exist on disk.

- [ ] **Step 3: Build terpsichore system closure**

Run: `nix build '.#nixosConfigurations.terpsichore.config.system.build.toplevel'`
Expected: Builds successfully. This validates the full module integration including option types, service dependencies, and systemd unit generation.

- [ ] **Step 4: Fix any errors**

If `nix flake check` or `nix build` fails, fix the reported errors and re-run.

- [ ] **Step 5: Final commit**

```bash
git add -A
git commit -m "feat: sourcehut module — complete with validation"
```

---

## Post-Implementation: Manual Steps (not automated)

After the NixOS modules are deployed to terpsichore, these manual steps are required:

1. **Create agenix secret files** — encrypt actual config.ini content for each service:
   ```bash
   agenix -e secrets/srht-meta-config.age
   # Paste the config.ini content for meta.sr.ht, save
   # Repeat for each service
   ```

2. **Generate SourceHut private key** — shared across all services:
   ```bash
   ssh-keygen -t rsa -b 4096 -f srht-private-key -N ""
   # Encrypt as srht-shared-secret.age
   ```

3. **Deploy and verify** — follow the Verification section in the spec
