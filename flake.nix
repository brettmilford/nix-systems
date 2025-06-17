{
  description = "Nix systems config";
  nixConfig.bash-prompt = "\[nix-develop\]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nix-darwin.url = "github:lnl7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    nix-darwin,
    home-manager,
    flake-utils,
    agenix,
    nixos-generators,
  } @ inputs: let
    system = flake-utils.lib.system;
    homeManagerCommonConfig = with self.homeManagerModules; {
      home.stateVersion = "25.05";
      imports = [
        ./home
      ];
    };
    nixDarwinCommonModules = {user}: [
      {
        system.stateVersion = 4;
        system.primaryUser = user;
        ids.gids.nixbld = 350;
        nix = {
          extraOptions = ''
            extra-platforms = aarch64-darwin x86_64-darwin
            experimental-features = nix-command flakes
          '';
          optimise.automatic = true;
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
        };
      }
      agenix.nixosModules.default
    ];
    nixosUserModules = {
      user,
      desc,
    }: [
      home-manager.nixosModules.home-manager
      {
        users.users.${user} = {
          home = "/home/${user}";
          isNormalUser = true;
          group = "${user}";
          description = "${desc}";
          extraGroups = ["wheel" "networkmanager"];
        };
        users.groups.${user} = {};
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
        system = system.aarch64-darwin;
        specialArgs = {
          pkgs_x86 = import nixpkgs {system = system.x86_64-darwin;};
        };
        modules =
          nixDarwinCommonModules {user = "brett";}
          ++ [
            ./hosts/darwin/thamrys
          ];
      };

      nixosConfigurations."orpheus" = nixpkgs.lib.nixosSystem {
        system = system.x86_64-linux;
        modules =
          nixosCommonModules {
            user = "brett";
            desc = "Brett";
          }
          ++ [
            ./hosts/nixos/orpheus
          ];
      };

      nixosConfigurations."orpheus-vm" = nixpkgs.lib.nixosSystem {
        system = system.x86_64-linux;
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
        system = system.x86_64-linux;
        modules =
          nixosCommonModules ++ [
            ./hosts/nixos/eurydice
          ];
      };

      nixosConfigurations."eurydice-vm" = nixpkgs.lib.nixosSystem {
        system = system.x86_64-linux;
        modules =
          nixosCommonModules ++ [
            ./hosts/nixos/eurydice
            ./hosts/nixos/build-vm.nix
          ];
      };


      nixosConfigurations."calliope" = nixpkgs.lib.nixosSystem {
        system = system.aarch64-linux;
        modules =
          nixosCommonModules ++
          nixosUserModules{
            user = "brett";
            desc = "Brett";
          }
          ++ [
            ./hosts/nixos/calliope
          ];
      };

      nixosConfigurations."dev" = nixpkgs.lib.nixosSystem {
        system = system.aarch64-linux;
        modules =
          nixosCommonModules {
            user = "brett";
            desc = "Brett";
          }
          ++ [
            {
              imports = [nixos-generators.nixosModules.all-formats];
              nixpkgs.hostPlatform = system.aarch64-linux;
            }
            ./hosts/nixos/dev
          ];
      };
    }
    // flake-utils.lib.eachDefaultSystem
    (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        buildInputs = [
          agenix.packages.${system}.default
        ];
        devShell = import ./shell.nix {inherit pkgs;};
        formatter = nixpkgs.legacyPackages.${system}.alejandra;
      }
    );
}
