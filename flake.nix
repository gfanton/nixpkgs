{
  inputs = {
    # Package sets
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Environment/system management
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # flake utils
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    flake-utils.url = "github:numtide/flake-utils";

    # overlay
    home-manager.url = "github:nix-community/home-manager/master";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # Other sources

    # emacs
    spacemacs.url = "github:syl20bnr/spacemacs/develop";
    spacemacs.flake = false;

    doomemacs.url = "github:doomemacs/doomemacs/master";
    doomemacs.flake = false;

    chemacs2.url = "github:plexus/chemacs2/main";
    chemacs2.flake = false;

    # yabai
    # yabai.url = "github:koekeishiya/yabai";
    # yabai.flake = false;

    # zsh plugins
    fast-syntax-highlighting.url = "github:zdharma-continuum/fast-syntax-highlighting";
    fast-syntax-highlighting.flake = false;

    powerlevel10k.url = "github:romkatv/powerlevel10k";
    powerlevel10k.flake = false;

    # My project
    project.url = "github:gfanton/project/v0.16.6";
    project.inputs.nixpkgs.follows = "nixpkgs-unstable";
  };

  outputs =
    {
      self,
      darwin,
      home-manager,
      flake-utils,
      ...
    }@inputs:
    let
      inherit (self.lib)
        attrValues
        makeOverridable
        mkForce
        optionalAttrs
        singleton
        ;

      homeStateVersion = "25.11";

      # Configuration for `nixpkgs`
      nixpkgsDefaults = {
        config = {
          allowUnfree = true;
        };
        overlays = attrValues self.overlays ++ [
          # Emacs overlay for latest packages and optimizations
          inputs.emacs-overlay.overlays.default
        ];
      };

      primaryUserInfo = {
        username = "gfanton";
        fullName = "";
        email = "8671905+gfanton@users.noreply.github.com";
        nixConfigDirectory = "/Users/gfanton/nixpkgs";
      };

      ciUserInfo = {
        username = "runner";
        fullName = "";
        email = "github-actions@github.com";
        nixConfigDirectory = "/Users/runner/work/nixpkgs/nixpkgs";
      };
    in
    {

      # Add some additional functions to `lib`.
      lib = inputs.nixpkgs-unstable.lib.extend (
        _: _: {
          mkDarwinSystem = import ./lib/mkDarwinSystem.nix inputs;
          lsnix = import ./lib/lsnix.nix;
        }
      );

      overlays = {
        # Overlays to add different versions `nixpkgs` into package set
        pkgs-master = _: prev: {
          pkgs-master = import inputs.nixpkgs-master {
            inherit (prev.stdenv) system;
            inherit (nixpkgsDefaults) config;
          };
        };
        pkgs-stable = _: prev: {
          pkgs-stable = import inputs.nixpkgs-stable {
            inherit (prev.stdenv) system;
            inherit (nixpkgsDefaults) config;
          };
        };
        pkgs-unstable = _: prev: {
          pkgs-unstable = import inputs.nixpkgs-unstable {
            inherit (prev.stdenv) system;
            inherit (nixpkgsDefaults) config;
          };
        };

        # Overlay useful on Macs with Apple Silicon
        pkgs-silicon =
          _: prev:
          optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
            # Add access to x86 packages system is running Apple Silicon
            pkgs-x86 = import inputs.nixpkgs-unstable {
              system = "x86_64-darwin";
              inherit (nixpkgsDefaults) config;
            };
          };

        # non flake inputs
        my-inputs = final: prev: {
          spacemacs = inputs.spacemacs;
          doomemacs = inputs.doomemacs;
          chemacs2 = inputs.chemacs2;
          zsh-plugins.fast-syntax-highlighting = inputs.fast-syntax-highlighting;
          zsh-plugins.powerlevel10k = inputs.powerlevel10k;
          # yabai = inputs.yabai;
          # Use project from flake input with fixed vendorHash
          project = inputs.project.packages.${final.system}.default.overrideAttrs (oldAttrs: {
            vendorHash = "sha256-B375AvklOVKxpIR60CatnmRgOFpqhlKyKF32isB+ncI=";
          });
        };

        # My overlays
        my-loon = import ./overlays/loon.nix;
        my-libvterm = import ./overlays/libvterm.nix;
        my-tmux = import ./overlays/tmux.nix;
        my-emacs = import ./overlays/emacs.nix;
      };

      # Non-system outputs --------------------------------------------------------------------- {{{

      commonModules = {
        colors = import ./modules/home/colors;
        my-colors = import ./home/colors.nix;
      };

      darwinModules = {
        # My configurations
        my-bootstrap = import ./darwin/bootstrap.nix;
        my-defaults = import ./darwin/defaults.nix;
        my-env = import ./darwin/env.nix;
        my-homebrew = import ./darwin/homebrew.nix;
        my-yabai = import ./darwin/yabai.nix;
        my-jankyborders = import ./darwin/jankyborders.nix;
        my-skhd = import ./darwin/skhd.nix;
        my-colima = import ./darwin/colima.nix;

        # local modules
        services-my-emacs = import ./modules/darwin/services/my-emacs.nix;
        services-colima = import ./modules/darwin/services/colima.nix;
        users-primaryUser = import ./modules/darwin/users.nix;
        programs-nix-index = import ./modules/darwin/programs/nix-index.nix;
      };

      nixosModules = {
        emacs = import ./modules/nixos/emacs.nix;
      };

      homeManagerModules = {
        # My configurations
        my-shells = import ./home/shells.nix;
        my-git = import ./home/git.nix;
        my-kitty = import ./home/kitty.nix;
        my-alacritty = import ./home/alacritty.nix;
        my-packages = import ./home/packages.nix;
        my-asdf = import ./home/asdf.nix;
        my-emacs = import ./home/emacs.nix;
        my-config = import ./home/config.nix;
        my-colima = import ./home/colima.nix;

        # local modules
        programs-truecolor = import ./modules/home/programs/truecolor;
        programs-kitty-extras = import ./modules/home/programs/kitty/extras.nix;
        programs-zsh-oh-my-zsh-extra = import ./modules/home/programs/zsh/oh-my-zsh/extras.nix;

        home-user-info =
          { lib, ... }:
          {
            options.home.user-info =
              (self.darwinModules.users-primaryUser { inherit lib; }).options.users.primaryUser;
          };
      };
      # }}}

      # System outputs ------------------------------------------------------------------------- {{{

      # My `nix-darwin` configs
      darwinConfigurations = rec {
        # Mininal configurations to bootstrap systems
        bootstrap-x86 = makeOverridable darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          modules = [
            ./darwin/bootstrap.nix
            { nixpkgs = nixpkgsDefaults; }
          ];
        };
        bootstrap-arm = bootstrap-x86.override { system = "aarch64-darwin"; };

        # My Apple Silicon macOS laptop config
        macbook = makeOverridable self.lib.mkDarwinSystem (
          primaryUserInfo
          // {
            system = "aarch64-darwin";
            modules =
              (attrValues self.darwinModules)
              ++ (attrValues self.commonModules)
              ++ singleton {
                nixpkgs = nixpkgsDefaults;
                networking.computerName = "guicp";
                networking.hostName = "ghost";
                networking.knownNetworkServices = [
                  "Wi-Fi"
                  "USB 10/100/1000 LAN"
                ];
                nix.registry.my.flake = inputs.self;
              };

            inherit homeStateVersion;
            homeModules =
              (attrValues self.homeManagerModules)
              ++ (attrValues self.commonModules)
              ++ [

              ];
          }
        );

        # Config with small modifications needed/desired for CI with GitHub workflow
        githubCI = self.darwinConfigurations.macbook.override {
          system = "aarch64-darwin";
          username = "runner";
          nixConfigDirectory = "/Users/runner/work/nixpkgs/nixpkgs";
          extraModules = singleton {
            environment.etc.shells.enable = mkForce false;
            environment.etc."nix/nix.conf".enable = mkForce false;
            homebrew.enable = mkForce false;
            services.yabai.enable = mkForce false;
            services.skhd.enable = mkForce false;
            ids.gids.nixbld = 350; # [hack]
          };
        };
      };

      # NixOS configurations for cloud VMs
      # Use with: nixos-rebuild switch --flake ~/nixpkgs#cloud-vm
      nixosConfigurations = {
        cloud-vm = inputs.nixpkgs-unstable.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            # Base system configuration
            ./nixos/cloud-vm/configuration.nix

            # Apply overlays and allow unfree
            {
              nixpkgs.overlays = attrValues self.overlays ++ [ inputs.emacs-overlay.overlays.default ];
              nixpkgs.config.allowUnfree = true;
            }

            # Emacs daemon (uses pkgs.myEmacs from overlay)
            self.nixosModules.emacs

            # Home-manager as NixOS module
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs; };
                users.gfanton = {
                  imports =
                    attrValues self.homeManagerModules
                    ++ attrValues self.commonModules;

                  home.user-info = primaryUserInfo // {
                    nixConfigDirectory = "/home/gfanton/nixpkgs";
                  };
                  home.stateVersion = homeStateVersion;
                };
              };
            }
          ];
        };

        cloud-vm-arm = inputs.nixpkgs-unstable.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            # Base system configuration
            ./nixos/cloud-vm/configuration.nix

            # Apply overlays, allow unfree, override platform
            {
              nixpkgs.overlays = attrValues self.overlays ++ [ inputs.emacs-overlay.overlays.default ];
              nixpkgs.config.allowUnfree = true;
              nixpkgs.hostPlatform = "aarch64-linux";
            }

            # Emacs daemon
            self.nixosModules.emacs

            # Home-manager as NixOS module
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs; };
                users.gfanton = {
                  imports =
                    attrValues self.homeManagerModules
                    ++ attrValues self.commonModules;

                  home.user-info = primaryUserInfo // {
                    nixConfigDirectory = "/home/gfanton/nixpkgs";
                  };
                  home.stateVersion = homeStateVersion;
                };
              };
            }
          ];
        };
      };

      # Config I use with non-NixOS Linux systems (e.g., cloud VMs etc.)
      # Build and activate on new system with:
      # `nix build .#homeConfigurations.cloud.activationPackage && ./result/activate`
      homeConfigurations = {
        cloud-x86 = home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs-unstable (nixpkgsDefaults // { system = "x86_64-linux"; });
          modules =
            attrValues self.homeManagerModules
            ++ (attrValues self.commonModules)
            ++ singleton (
              { config, lib, ... }:
              {
                home.user-info = primaryUserInfo // {
                  nixConfigDirectory = "${config.home.homeDirectory}/nixpkgs";
                };
                home.username = config.home.user-info.username;
                home.homeDirectory = "/home/${config.home.username}";
                home.stateVersion = homeStateVersion;

              }
            );
        };

        cloud-arm = home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs-unstable (nixpkgsDefaults // { system = "aarch64-linux"; });
          modules =
            attrValues self.homeManagerModules
            ++ (attrValues self.commonModules)
            ++ singleton (
              { config, lib, ... }:
              {
                home.user-info = primaryUserInfo // {
                  nixConfigDirectory = "${config.home.homeDirectory}/nixpkgs";
                };
                home.username = config.home.user-info.username;
                home.homeDirectory = "/home/${config.home.username}";
                home.stateVersion = homeStateVersion;

              }
            );
        };

        # Alias for backward compatibility
        cloud = self.homeConfigurations.cloud-x86;

        # specific config for github ci
        githubCI = home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs-unstable (nixpkgsDefaults // { system = "x86_64-linux"; });
          modules =
            attrValues self.homeManagerModules
            ++ (attrValues self.commonModules)
            ++ singleton (
              { config, ... }:
              {
                home.user-info = ciUserInfo // {
                  nixConfigDirectory = "${config.home.homeDirectory}/nixpkgs";
                };
                home.username = config.home.user-info.username;
                home.homeDirectory = "/home/${config.home.username}";
                home.stateVersion = homeStateVersion;
              }
            );
        };
      };
      # }}}

      # Add re-export `nixpkgs` packages with overlays.
      # This is handy in combination with `nix registry add my /Users/gfanton/nixpkgs`
    }
    // flake-utils.lib.eachDefaultSystem (system: {
      # Re-export `nixpkgs-unstable` with overlays.
      # This is handy in combination with setting `nix.registry.my.flake = inputs.self`.
      # Allows doing things like `nix run my#prefmanager -- watch --all`
      legacyPackages = import inputs.nixpkgs-unstable (nixpkgsDefaults // { inherit system; });

      # Development shells ----------------------------------------------------------------------{{{
      # Shell environments for development
      # With `nix.registry.my.flake = inputs.self`, development shells can be created by running,
      # e.g., `nix develop my#python`.
      devShells =
        let
          pkgs = self.legacyPackages.${system};
        in
        {
          asdf = pkgs.mkShell {
            name = "asdf";
            inputsFrom = attrValues { inherit (pkgs) asdf-vm; };
            shellHook = ''
              if [ -f "${pkgs.asdf-vm}/share/asdf-vm/asdf.sh" ]; then
                . "${pkgs.asdf-vm}/share/asdf-vm/asdf.sh"
              fi

              fpath=(${pkgs.asdf-vm}/share/asdf-vm/completions $fpath)

              if [ -f "''${ASDF_DATA_DIR}/.asdf/plugins/java/set-java-home.zsh" ]; then
                 . "''${ASDF_DATA_DIR}/.asdf/plugins/java/set-java-home.zsh"
              fi
            '';
          };
        };
      # }}}
    });
}
