# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains Nix configurations for managing multiple systems using a flake-based approach:

- **hosts/darwin**: macOS configuration for system `thamrys`
- **hosts/nixos**: NixOS configurations for systems `orpheus`, `eurydice`, and `calliope`
- **home**: Home-manager configurations for user environments
- **deployments**: Container-based service deployments
- **config**: Application-specific configurations

## System Architecture

### Darwin (macOS)
- System: `thamrys` (aarch64)
- Uses nix-darwin and home-manager
- Configured with yabai window manager, podman for containers
- Shell environment with zsh, tmux, and Doom Emacs

### NixOS Hosts
- **calliope**: Server for Nextcloud, PostgreSQL, and containerized services
- **eurydice**: Desktop system with smart home and network management
- **orpheus**: Desktop system with backup capabilities
- All systems use ZeroTier for networking and shared home-manager configs

### Home Manager
- Common shell environment with git, vim, tmux, and command aliases
- Doom Emacs with custom modules for knowledge management and AI assistance
- Tmux configuration with vim integration and session persistence

### Deployments
- Container-based services using Podman/Docker Compose
- NixOS modules with systemd services to manage deployments
- Services include: Elasticsearch, Home Assistant, LLama.cpp, OctoPrint, UniFi

## Common Commands

### Rebuild Commands

For macOS (Darwin):
```bash
# Rebuild and switch the current system
nrs  # Alias for: sudo darwin-rebuild switch --flake "$HOME/.config/nix?submodules=1"

# Update flake and rebuild
nup  # Alias for: nix flake update --flake ~/.config/nix && nrs
```

For NixOS:
```bash
# Build and test configuration
nix build '.#nixosConfigurations.<hostname>.config.system.build.toplevel'

# Build and test VM
nix run '.#nixosConfigurations.<hostname>.config.system.build.vm'

# Switch to configuration
sudo nixos-rebuild switch --flake '.#<hostname>'
```

### Container Management

For deployments:
```bash
# View running containers
podman ps

# Restart a specific service
sudo systemctl restart <service-name>

# View container logs
sudo journalctl -u <service-name>
```

### Development

```bash
# Enter development shell
nix develop

# Lint Nix files
alejandra .

# Test flake
nix flake check
```

## Working with this Repository

- Use the flake.nix file to understand system definitions and entry points
- Respect the separation of system configuration (hosts/) and user configuration (home/)
- Look for specific host configurations when making system-specific changes
- Service deployments are defined in the deployments/ directory
- ZeroTier is used for networking between hosts
- Secrets are managed with agenix (see secrets/ directory)