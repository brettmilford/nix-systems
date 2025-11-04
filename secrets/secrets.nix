let
  nix = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBy5tD71f2uRLQvbZL0wwyZNUmximBOM19KuENx791Rl";
  brett = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR";
  thamrys = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPNT9L1KhAcQAxYOpXBy61dRr00sWVty4b/d/ZjXhGAg";
  calliope = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICYnaqrEbdb9apQNMccZgfM0YRX6VTi1JPS+BmVd8Vb0";
  eurydice = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJfkZRQxQLgNZmGzSu9+XG3v3QoWhumJKzphxduT+5wI";
  terpsichore = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIShGK1lWBkHiW7bLh9gbEVfxNLEgylyHO5956+ehhYZ";
  orpheus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJD9JScMbvyixRtjd4zwrUmZNUndlMdbtkkvtPnFZ3Md";
in {
  "admin-passwd.age".publicKeys = [nix thamrys calliope brett];
  "cf_origin_cert.pem.age".publicKeys = [nix eurydice terpsichore calliope brett];
  "cf_cert_2025.pem.age".publicKeys = [nix eurydice calliope brett];
  "cf_origin_key.pem.age".publicKeys = [nix eurydice terpsichore calliope brett];
  "cf_key_2025.pem.age".publicKeys = [nix eurydice calliope brett];
  "cf-api-key.age".publicKeys = [nix eurydice calliope brett];
  "cfd_tunnel_config.json.age".publicKeys = [nix eurydice brett];
  "grafana_pass.age".publicKeys = [nix eurydice terpsichore brett];
  "grafana_key.age".publicKeys = [nix eurydice terpsichore brett];
  "unifipoller_pass.age".publicKeys = [nix eurydice terpsichore brett];
  "snmp.env.age".publicKeys = [nix eurydice terpsichore brett];
  "paperless.env.age".publicKeys = [nix calliope brett];
  "postfix-sasl-passwd.age".publicKeys = [nix calliope brett];
  "keycloak-db-passwd.age".publicKeys = [nix calliope brett];
  "nextcloud-secrets.json.age".publicKeys = [nix calliope brett];
  "mealie.env.age".publicKeys = [nix calliope brett];
  "immich.json.age".publicKeys = [nix calliope brett];
  "acme-cf.env.age".publicKeys = [nix calliope terpsichore eurydice brett];
  "fail2ban-cf.conf.age".publicKeys = [nix calliope terpsichore eurydice brett];
  "paperless-api-token.age".publicKeys = [nix calliope brett];
  "borg-calliope-ssh-key.age".publicKeys = [nix calliope brett];
  "borg-orpheus-ssh-key.age".publicKeys = [nix orpheus brett];
  "borg-thamrys-ssh-key.age".publicKeys = [nix thamrys brett];
  "borg-eurydice-ssh-key.age".publicKeys = [nix eurydice brett];
  "borg-terpsichore-ssh-key.age".publicKeys = [nix terpsichore brett];
  "hass_prometheus_token.age".publicKeys = [nix terpsichore brett];
}
