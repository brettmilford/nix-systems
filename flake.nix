{
  description = "Nix systems config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-darwin.url = "github:lnl7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";
    lanzaboote.url = "github:nix-community/lanzaboote/v0.4.2";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-unstable,
      nix-darwin,
      home-manager,
      agenix,
      nixos-generators,
      lanzaboote,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      top@{
        config,
        withSystem,
        moduleWithSystem,
        ...
      }:
      let
        users = import ./users.nix;
      in
      {
        imports = [ inputs.home-manager.flakeModules.home-manager ];

        systems = [
          "x86_64-linux"
          "aarch64-linux"
          "aarch64-darwin"
        ];

        perSystem =
          {
            pkgs,
            system,
            inputs',
            ...
          }:
          let
            mkHome =
              username: user:
              let
                homeDirectory = if pkgs.stdenv.isDarwin then "/Users/${username}" else "/home/${username}";
              in
              home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                extraSpecialArgs = {
                  inherit self;
                  inputs = inputs;
                };
                modules = [
                  {
                    home.username = username;
                    home.homeDirectory = homeDirectory;
                    home.stateVersion = "25.05";
                    programs.home-manager.enable = true;
                    _module.args.userConfig = user // {
                      username = username;
                    };
                  }
                  self.homeModules.default
                ];
              };
          in
          {
            legacyPackages.homeConfigurations = builtins.mapAttrs (username: user: mkHome username user) users;

            devShells.default =
              let
                nixBin = pkgs.writeShellScriptBin "nix" ''
                  ${pkgs.nixVersions.stable}/bin/nix --option experimental-features "nix-command flakes" "$@"
                '';
                nrs = pkgs.writeShellScriptBin "nrs" (
                  if pkgs.stdenv.isDarwin then
                    ''
                      sudo darwin-rebuild switch --flake "''${FLAKE}" "$@"
                    ''
                  else
                    ''
                      sudo nixos-rebuild switch --flake "''${FLAKE}" "$@"
                    ''
                );
                nrsr = pkgs.writeShellScriptBin "nrsr" ''
                  ssh $1 "sudo nixos-rebuild switch --flake 'github:brettmilford/nix-systems/devel'"
                '';
              in
              pkgs.mkShell {
                packages = with pkgs; [
                  nixBin
                  nrs
                  nrsr
                  inputs'.agenix.packages.default
                  inputs'.home-manager.packages.default
                  nixfmt-tree
                  jq
                ];
                shellHook = ''
                  export PS1='\[\033[1;32m\](nix-systems)[\u@\h:\w]\$\[\033[0m\] '
                  export FLAKE="$(pwd)"
                  alias hms='home-manager switch --flake "''${FLAKE}?submodules=1#''${USER}"'
                  alias nup='nix flake update --flake "''${FLAKE}" && nrs'
                  alias nvm='nix run ".#nixosConfigurations.$(hostname -s).config.system.build.vmWithBootLoader"'
                '';
              };
            formatter = pkgs.nixfmt-rfc-style;
            checks = {
              libTests = import ./lib/tests/release.nix { inherit pkgs; };
            };
          };

        flake =
          let
            hosts = import ./hosts;
            catalog = import ./services.nix;

            # Create our extended lib
            ourLib = import ./lib { lib = nixpkgs.lib; };
            extendedLib = nixpkgs.lib // ourLib;

            # Validate at import time
            _ = extendedLib.validateServices hosts catalog.services;
            __ = extendedLib.validateBackupSets hosts (catalog.services.backup.config.repos or { });

            # Create serviceMap with hosts and lib
            serviceMap = {
              # Expose raw data at top level
              inherit (catalog) services domain;
              inherit hosts;

              # Wrap functions under lib
              lib = import ./lib/serviceMap.nix {
                lib = extendedLib;
                inherit hosts;
                inherit (catalog) services domain;
              };
            };

            # Standard arguments passed to all configurations
            commonSpecialArgs = {
              inherit
                self
                users
                hosts
                serviceMap
                ;
              inputs = inputs;
            };

            # Standard modules for all configurations
            commonModuleArgs = {
              _module.args = commonSpecialArgs;
            };

            # Filter hosts by system type
            nixosHosts = nixpkgs.lib.filterAttrs (name: host: nixpkgs.lib.hasInfix "linux" host.system) hosts;

            darwinHosts = nixpkgs.lib.filterAttrs (name: host: nixpkgs.lib.hasInfix "darwin" host.system) hosts;

            # Generate NixOS configuration
            mkNixosConfiguration =
              hostname: host:
              nixpkgs.lib.nixosSystem {
                system = host.system;
                specialArgs = commonSpecialArgs;
                modules = [
                  commonModuleArgs
                  self.nixosModules.default
                  ./hosts/nixos/${hostname}
                ]
                ++ nixpkgs.lib.optional (serviceMap.lib.hasService hostname "secure-boot") lanzaboote.nixosModules.lanzaboote
                ++ nixpkgs.lib.optional (serviceMap.lib.hasService hostname "desktop") self.nixosModules.users;
              };
            # Generate Darwin configuration
            mkDarwinConfiguration =
              hostname: host:
              nix-darwin.lib.darwinSystem {
                system = host.system;
                specialArgs = commonSpecialArgs // {
                  pkgs-x86_64 = import nixpkgs { system = "x86_64-darwin"; };
                };
                modules = [
                  commonModuleArgs
                  self.darwinModules.default
                  ./hosts/darwin/${hostname}
                ];
              };
          in
          {
            lib = {
              inherit users hosts serviceMap;
            };

            homeModules.default = {
              imports = [
                ./modules/home
              ];
            };

            darwinModules.default = {
              imports = [
                agenix.darwinModules.default
                ./modules/darwin
              ];
            };

            nixosModules = {
              default = {
                imports = [
                  agenix.nixosModules.default
                  ./modules/nixos
                ];
              };

              users = {
                imports = [
                  ./modules/nixos/users.nix
                ];
              };
            };

            nixosConfigurations = builtins.mapAttrs mkNixosConfiguration nixosHosts;

            darwinConfigurations = builtins.mapAttrs mkDarwinConfiguration darwinHosts;
          };
      }
    );
}
