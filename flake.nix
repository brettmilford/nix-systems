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
      {
        systems = [
          "x86_64-linux"
          "aarch64-linux"
          "aarch64-darwin"
        ];

        perSystem =
          { pkgs, ... }:
          {
            devShells.default =
              let
                nixBin = pkgs.writeShellScriptBin "nix" ''
                  ${pkgs.nixVersions.stable}/bin/nix --option experimental-features "nix-command flakes" "$@"
                '';
              in
              pkgs.mkShell {
                packages = with pkgs; [
                  inputs.agenix.packages.${system}.default
                ];
                shellHook = ''
                  export FLAKE="$(pwd)"
                  export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
                  export PS1='\[\033[1;32m\][nix-systems:\w]\$\[\033[0m\] '
                '';
              };
            formatter = pkgs.nixfmt-rfc-style;
          };

        flake =
          let
            homeManagerCommonConfig = with self.homeManagerModules; {
              home.stateVersion = "25.05";
              imports = [
                ./home
              ];
            };
            nixDarwinCommonModules =
              { user }:
              [
                {
                  system.stateVersion = 4;
                  system.primaryUser = user;
                  ids.gids.nixbld = 350;
                  nix = {
                    distributedBuilds = true;
                    extraOptions = ''
                      extra-platforms = aarch64-darwin x86_64-darwin
                      experimental-features = nix-command flakes
                      builders = ssh://nix@eurydice /Users/brett/.ssh/id_ed25519
                    '';
                    optimise.automatic = true;
                    settings.trusted-users = [
                      "${user}"
                    ];
                  };
                }
                home-manager.darwinModules.home-manager
                {
                  users.users.${user}.home = "/Users/${user}";
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.sharedModules = [
                    ./home/git-${user}.nix
                  ];
                  home-manager.users.${user} = homeManagerCommonConfig;
                }
                agenix.darwinModules.default
              ];
            nixosCommonModules = [
              home-manager.nixosModules.home-manager
              {
                system.stateVersion = "25.05";
                nix = {
                  extraOptions = ''
                    extra-platforms = aarch64-linux x86_64-linux
                    experimental-features = nix-command flakes
                  '';
                  settings.auto-optimise-store = true;
                  gc = {
                    automatic = true;
                    dates = "weekly";
                    options = "--delete-older-than 30d";
                  };
                };
              }
              agenix.nixosModules.default
              lanzaboote.nixosModules.lanzaboote
            ];
            nixosUserModules =
              {
                user,
                desc,
              }:
              [
                home-manager.nixosModules.home-manager
                {
                  users.users.${user} = {
                    home = "/home/${user}";
                    isNormalUser = true;
                    group = "${user}";
                    description = "${desc}";
                    extraGroups = [
                      "wheel"
                      "networkmanager"
                    ];
                  };
                  users.groups.${user} = { };
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.sharedModules = [
                    ./home/git-${user}.nix
                  ];
                  home-manager.users.${user} = homeManagerCommonConfig;
                  systemd.services."home-manager-${user}".serviceConfig.TimeoutSec = 900;
                }
              ];
          in
          {
            darwinConfigurations."thamrys" = nix-darwin.lib.darwinSystem {
              system = "aarch64-darwin";
              specialArgs = {
                pkgs_x86 = import nixpkgs { system = "x86_64-darwin"; };
              };
              modules = nixDarwinCommonModules { user = "brett"; } ++ [
                ./hosts/darwin/thamrys
              ];
            };

            nixosConfigurations."orpheus" = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules =
                nixosCommonModules
                ++ nixosUserModules {
                  user = "brett";
                  desc = "Brett";
                }
                ++ [
                  ./hosts/nixos/orpheus
                ];
            };

            nixosConfigurations."orpheus-vm" = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules =
                nixosCommonModules {
                  user = "brett";
                  desc = "Brett";
                }
                ++ [
                  ./hosts/nixos/orpheus
                  ./hosts/nixos/build-vm.nix
                ];
            };

            nixosConfigurations."eurydice" = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = nixosCommonModules ++ [
                ./hosts/nixos/eurydice
              ];
              specialArgs = {
                inherit self;
                inputs = inputs;
              };
            };

            nixosConfigurations."eurydice-vm" = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = nixosCommonModules ++ [
                ./hosts/nixos/eurydice
                ./hosts/nixos/build-vm.nix
              ];
            };

            nixosConfigurations."calliope" = nixpkgs.lib.nixosSystem {
              system = "aarch64-linux";
              modules = nixosCommonModules ++ [
                ./hosts/nixos/calliope
              ];
              specialArgs = {
                inherit self;
                inputs = inputs;
              };
            };

            nixosConfigurations."terpsichore" = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = nixosCommonModules ++ [
                ./hosts/nixos/terpsichore
              ];
              specialArgs = {
                inherit self;
                inputs = inputs;
              };
            };

            nixosConfigurations."dev" = nixpkgs.lib.nixosSystem {
              system = "aarch64-linux";
              modules =
                nixosCommonModules {
                  user = "brett";
                  desc = "Brett";
                }
                ++ [
                  {
                    imports = [ nixos-generators.nixosModules.all-formats ];
                    nixpkgs.hostPlatform = "aarch64-linux";
                  }
                  ./hosts/nixos/dev
                ];
            };
          };
      }
    );
}
