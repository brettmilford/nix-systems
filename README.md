## Nix-darwin

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


## Nixos

Checking for differences between configurations.

``` sh
nix build '.#nixosConfigurations.orpheus.config.system.build.toplevel'
# or
sudo nixos-rebuild build --flake /etc/nixos?submodules=1
# then
nix store diff-closures /run/current-system ./result
```

Testing configurations in a VM

``` sh
nix run '.#nixosConfigurations.orpheus.config.system.build.vm'
```
