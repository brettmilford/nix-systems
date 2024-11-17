let
  brett = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR";
  thamrys = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPNT9L1KhAcQAxYOpXBy61dRr00sWVty4b/d/ZjXhGAg";
  calliope = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICYnaqrEbdb9apQNMccZgfM0YRX6VTi1JPS+BmVd8Vb0";
in {
  "node_exporter.crt.age".publicKeys = [thamrys brett];
  "node_exporter.key.age".publicKeys = [thamrys brett];
  "node_exporter_config.age".publicKeys = [thamrys brett];
  "nextcloud.age".publicKeys = [thamrys calliope brett];
  "cf_origin_cert.pem.age".publicKeys = [calliope brett];
  "cf_origin_key.pem.age".publicKeys = [calliope brett];
  "cfApiKey.age".publicKeys = [calliope brett];
}
