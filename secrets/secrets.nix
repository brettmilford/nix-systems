let
  brett = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR";
  thamrys = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPNT9L1KhAcQAxYOpXBy61dRr00sWVty4b/d/ZjXhGAg";
  calliope = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICYnaqrEbdb9apQNMccZgfM0YRX6VTi1JPS+BmVd8Vb0";
  eurydice = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJfkZRQxQLgNZmGzSu9+XG3v3QoWhumJKzphxduT+5wI";
  terpsichore = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIShGK1lWBkHiW7bLh9gbEVfxNLEgylyHO5956+ehhYZ";
  orpheus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJD9JScMbvyixRtjd4zwrUmZNUndlMdbtkkvtPnFZ3Md";
in {
  "admin-passwd.age".publicKeys = [thamrys calliope brett];
  "cf_origin_cert.pem.age".publicKeys = [eurydice terpsichore calliope brett];
  "cf_cert_2025.pem.age".publicKeys = [eurydice calliope brett];
  "cf_origin_key.pem.age".publicKeys = [eurydice terpsichore calliope brett];
  "cf_key_2025.pem.age".publicKeys = [eurydice calliope brett];
  "cf-api-key.age".publicKeys = [eurydice calliope brett];
  "cfd_tunnel_config.json.age".publicKeys = [eurydice brett];
  "grafana_pass.age".publicKeys = [eurydice terpsichore brett];
  "grafana_key.age".publicKeys = [eurydice terpsichore brett];
  "unifipoller_pass.age".publicKeys = [eurydice terpsichore brett];
  "snmp.env.age".publicKeys = [eurydice terpsichore brett];
  "paperless.env.age".publicKeys = [calliope brett];
  "postfix-sasl-passwd.age".publicKeys = [calliope brett];
  "keycloak-db-passwd.age".publicKeys = [calliope brett];
  "nextcloud-secrets.json.age".publicKeys = [calliope brett];
  "mealie.env.age".publicKeys = [calliope brett];
  "immich.json.age".publicKeys = [calliope brett];
  "acme-cf.env.age".publicKeys = [calliope terpsichore brett];
  "fail2ban-cf.conf.age".publicKeys = [calliope terpsichore brett];
  "borg-calliope-ssh-key.age".publicKeys = [calliope brett];
  "paperless-api-token.age".publicKeys = [calliope brett];
  "borg-orpheus-ssh-key.age".publicKeys = [orpheus brett];
}
