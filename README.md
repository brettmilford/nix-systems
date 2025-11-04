## Nix-darwin

1) Install nix & homebrew

```
xcode-select --install
curl -L https://nixos.org/nix/install | sh -s -- --daemon
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

2) Build Flake

```
nix build "git+https://github.com/brettmilford/nix-systems.git?ref=devel#darwinConfigurations.thamrys.system" --extra-experimental-features "nix-command flakes"
# OR
git clone --recursive https://github.com/brettmilford/nix-systems.git ~/.config/nix
cd ~/.config/nix
nix develop --extra-experimental-features "nix-command flakes"
nix build ".#darwinConfigurations.${HOSTNAME}.system"
```

3) Switch

```
sudo ./result/sw/bin/darwin-rebuild switch --flake "git+https://github.com/brettmilford/nix-systems.git?ref=devel#thamrys"
# OR
sudo ./result/sw/bin/darwin-rebuild switch --flake ".#${HOSTNAME}"
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
