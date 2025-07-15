let
  brett = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR";
  thamrys = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPNT9L1KhAcQAxYOpXBy61dRr00sWVty4b/d/ZjXhGAg";
  calliope = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICYnaqrEbdb9apQNMccZgfM0YRX6VTi1JPS+BmVd8Vb0";
  eurydice = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJfkZRQxQLgNZmGzSu9+XG3v3QoWhumJKzphxduT+5wI";
in {
  "admin-passwd.age".publicKeys = [thamrys calliope brett];
  "cf_origin_cert.pem.age".publicKeys = [eurydice calliope brett];
  "cf_cert_2025.pem.age".publicKeys = [eurydice calliope brett];
  "cf_origin_key.pem.age".publicKeys = [eurydice calliope brett];
  "cf_key_2025.pem.age".publicKeys = [eurydice calliope brett];
  "cfApiKey.age".publicKeys = [eurydice calliope brett];
  "cfd_tunnel_config.json.age".publicKeys = [eurydice brett];
  "grafana_pass.age".publicKeys = [eurydice brett];
  "grafana_key.age".publicKeys = [eurydice brett];
  "unifipoller_pass.age".publicKeys = [eurydice brett];
  "snmp.env.age".publicKeys = [eurydice brett];
  "paperless.env.age".publicKeys = [calliope brett];
  "postfix-sasl-passwd.age".publicKeys = [calliope brett];
  "keycloak-db-passwd.age".publicKeys = [calliope brett];
  "nextcloud-secrets.json.age".publicKeys = [calliope brett];
  "mealie.env.age".publicKeys = [calliope brett];
}
