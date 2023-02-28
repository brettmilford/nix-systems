let
  brett = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR";
  thamrys = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPNT9L1KhAcQAxYOpXBy61dRr00sWVty4b/d/ZjXhGAg";
in
{
  "privkey.age".publicKeys = [ thamrys brett ];
  "node_exporter.crt.age".publicKeys = [ thamrys brett ];
  "node_exporter.key.age".publicKeys = [ thamrys brett ];
  "node_exporter_config.age".publicKeys = [thamrys brett ];
}
