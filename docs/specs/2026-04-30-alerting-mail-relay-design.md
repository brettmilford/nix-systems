# Alerting & Mail Relay Design

## Overview

Add email alerting for borg backup failures, disk space, and paperless consume
errors. Includes a new reusable mail-relay module and Alertmanager email
configuration.

## 1. Mail Relay Module

**Path:** `modules/nixos/mail-relay/default.nix`

A parameterised postfix module for outbound email relay.

### Options

| Option | Type | Default | Description |
|---|---|---|---|
| `enable` | bool | false | Enable the mail relay |
| `relayhost` | string | required | SMTP relay destination (e.g. `"127.0.0.1:1025"`) |
| `hostname` | string | required | HELO/EHLO hostname |
| `domain` | string | required | Mail origin domain |
| `saslAuth.enable` | bool | false | Enable SASL authentication to relay |
| `saslAuth.secretPath` | string | - | Path to agenix secret with SASL credentials |
| `listenInterfaces` | list of str | `["localhost"]` | Interfaces postfix listens on |
| `mynetworks` | list of str | `["127.0.0.0/8"]` | Networks allowed to relay |

### Per-Host Configuration

**Calliope:**
- `relayhost = "127.0.0.1:1025"` (local Protonmail Bridge)
- `saslAuth.enable = true`, secret from agenix
- `listenInterfaces = ["localhost" "172.16.0.1"]` (exposed on WG for other hosts)
- `mynetworks = ["127.0.0.0/8" "172.16.0.0/16"]`

**Terpsichore:**
- `relayhost = "172.16.0.1:25"` (calliope's postfix via WG)
- No SASL (trusted WG network)
- `listenInterfaces = ["localhost"]`
- Default mynetworks

## 2. Service Catalog Integration

### services.nix

```nix
mail-relay = {
  hosts = [ "calliope" "terpsichore" ];
};
```

### lib/serviceModules.nix

New `mail-relay` entry in `serviceModuleMap`:
- Maps to `modules/nixos/mail-relay`
- `getOptions` provides per-host config (calliope vs terpsichore as above)
- `getSecrets` provides SASL password secret for calliope only

### Migration

- Remove `./postfix.nix` import from `hosts/nixos/calliope/default.nix`
- Move Protonmail Bridge config to `hosts/nixos/calliope/bridge.nix`
- Delete `hosts/nixos/calliope/postfix.nix`

## 3. Alertmanager Email Configuration

### Enable Alertmanager

Set `enableAlertManager = true` in the monitoring `getOptions` in
`serviceModules.nix`.

### Alertmanager Config

In `modules/nixos/monitoring/alertmanager/default.nix`:

- `smtp_smarthost`: `"localhost:25"` (terpsichore's local postfix)
- `smtp_from`: `"admin@cirriform.au"`
- `smtp_require_tls`: false (local relay)
- Receiver: email to `brettmilford@gmail.com`

### Prometheus to Alertmanager

Add `alertmanagers` config in `modules/nixos/monitoring/prometheus/default.nix`
pointing to `localhost:9093`.

## 4. Alert Rules

### Prometheus Rules

Added to existing `rules` in `prometheus/default.nix`:

**Borg backup failures:**
```promql
node_systemd_unit_state{name=~"borgbackup-job-.*", state="failed"} == 1
```
- Severity: critical
- For: 5m

**Disk space warning (<10% free):**
```promql
(node_filesystem_avail_bytes{fstype!~"tmpfs|overlay|squashfs|iso9660|efivarfs|nsfs"}
 / node_filesystem_size_bytes) < 0.1
```
- Severity: warning
- For: 10m

**Disk space critical (<5% free):**
```promql
(node_filesystem_avail_bytes{fstype!~"tmpfs|overlay|squashfs|iso9660|efivarfs|nsfs"}
 / node_filesystem_size_bytes) < 0.05
```
- Severity: critical
- For: 10m

### Loki Ruler (Paperless)

Add `ruler` section to `modules/nixos/monitoring/loki/default.nix`:
- `alertmanager_url`: `http://localhost:9093`
- Rule storage: local filesystem
- Rule file created via `pkgs.writeText`

**Paperless consume errors:**
```logql
count_over_time({app=~"paperless.*", host="calliope"} |~ "ERROR" [15m]) > 0
```
- Severity: warning
- For: 0s (any error in 15m window fires)

**Caveat:** The exact `app` label value for paperless-consumer needs
verification in Grafana Explore after deployment. Adjust the LogQL query
if the label doesn't match.

## Files Changed

| File | Action |
|---|---|
| `modules/nixos/mail-relay/default.nix` | Create — new mail relay module |
| `services.nix` | Edit — add `mail-relay` service |
| `lib/serviceModules.nix` | Edit — add `mail-relay` mapping, enable alertmanager |
| `hosts/nixos/calliope/postfix.nix` | Delete — replaced by module |
| `hosts/nixos/calliope/bridge.nix` | Create — Protonmail Bridge config |
| `hosts/nixos/calliope/default.nix` | Edit — swap postfix.nix for bridge.nix |
| `modules/nixos/monitoring/alertmanager/default.nix` | Edit — email receiver |
| `modules/nixos/monitoring/prometheus/default.nix` | Edit — alert rules, alertmanager endpoint |
| `modules/nixos/monitoring/loki/default.nix` | Edit — ruler config with paperless rule |
