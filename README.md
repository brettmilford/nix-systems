1) Install nix

```
curl -L https://nixos.org/nix/install | sh -s -- --daemon
```

2) Build Flake

```
nix build "git+https://github.com/brettmilford/nix-systems.git?ref=devel#darwinConfigurations.thamrys.system" --extra-experimental-features "nix-command flakes"
```

3) Switch

```
./result/sw/bin/darwin-rebuild switch --flake "git+https://github.com/brettmilford/nix-systems.git?ref=devel#thamrys"
```
