let
  brett = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR";
  thamrys = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPNT9L1KhAcQAxYOpXBy61dRr00sWVty4b/d/ZjXhGAg";
  calliope = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICYnaqrEbdb9apQNMccZgfM0YRX6VTi1JPS+BmVd8Vb0";
  eurydice = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJfkZRQxQLgNZmGzSu9+XG3v3QoWhumJKzphxduT+5wI";
in {
  "nextcloud.age".publicKeys = [thamrys calliope brett];
  "cf_origin_cert.pem.age".publicKeys = [eurydice calliope brett];
  "cf_origin_key.pem.age".publicKeys = [eurydice calliope brett];
  "cfApiKey.age".publicKeys = [eurydice calliope brett];
  "cfd_tunnel_config.json.age".publicKeys = [eurydice brett];
}
