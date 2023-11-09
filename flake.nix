{
  description = "Nix systems config";
  nixConfig.bash-prompt = "\[nix-develop\]$ ";

  inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
      nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-23.05-darwin";
      darwin.url = "github:lnl7/nix-darwin";
      darwin.inputs.nixpkgs.follows = "nixpkgs-darwin";
      home-manager.url = "github:nix-community/home-manager/release-23.05";
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
    nixpkgs-darwin,
    darwin,
    home-manager,
    flake-utils,
    agenix,
    nixos-generators,
  }@inputs:
  let
    system = flake-utils.lib.system;
    homeManagerCommonConfig = with self.homeManagerModules; {
      home.stateVersion = "23.05";
      imports = [
        ./home
      ];
    };
    nixDarwinCommonModules = { user }: [
      {
        system.stateVersion = 4;
        nix = {
          extraOptions = ''
            extra-platforms = aarch64-darwin x86_64-darwin
            experimental-features = nix-command flakes
          '';
          settings.auto-optimise-store = true;
        };
      }
      home-manager.darwinModules.home-manager
      {
        users.users.${user}.home = "/Users/${user}";
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.sharedModules = [
          ./home/common.nix
          ./home/git-${user}.nix
        ];
        home-manager.users.${user} = homeManagerCommonConfig;
      }
      agenix.darwinModules.default
    ];
    nixosCommonModules = { user }: [
      home-manager.nixosModules.home-manager
      {
        system.stateVersion = "23.05";
        nix = {
          extraOptions = ''
            extra-platforms = aarch64-linux x86_64-linux
            experimental-features = nix-command flakes
          '';
          settings.auto-optimise-store = true;
        };
        users.users.${user} = {
          home = "/home/${user}";
          isNormalUser = true;
          extraGroups = ["wheel" "networkmanager"];
        };
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.sharedModules = [
          ./home/common.nix
          ./home/git-${user}.nix
        ];
        home-manager.users.${user} = homeManagerCommonConfig;
      }
      agenix.nixosModules.default
    ];
  in {

    darwinConfigurations."thamrys" = darwin.lib.darwinSystem {
      system = system.aarch64-darwin;
      specialArgs = {
        pkgs_x86 = import nixpkgs { system = system.x86_64-darwin; };
      };
      modules = nixDarwinCommonModules { user = "brett"; } ++ [
        ./hosts/darwin/thamrys
      ];
    };

    nixosConfigurations."Calliope" = nixpkgs.lib.nixosSystem {
      system = system.aarch64-linux;
      modules = nixosCommonModules { user = "brett"; } ++ [
        ./hosts/nixos/Calliope
      ];
    };

    nixosConfigurations."dev" = nixpkgs.lib.nixosSystem {
      system = system.aarch64-linux;
      modules = nixosCommonModules { user = "brett"; } ++ [
        {
          imports = [nixos-generators.nixosModules.all-formats];
          nixpkgs.hostPlatform = system.aarch64-linux;
        }
        ./hosts/nixos/dev
      ];
    };

  } //
  flake-utils.lib.eachDefaultSystem
    (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        buildInputs = [
          agenix.packages.${system}.default
        ];
        devShell = import ./shell.nix { inherit pkgs; };
      }
    );
}
