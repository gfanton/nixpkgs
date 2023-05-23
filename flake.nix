{
  inputs = {
    # Package sets
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-22.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Environment/system management
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager/master";

    # flake utils
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-utils.url = "github:numtide/flake-utils";

    # Other sources

    # emacs
    spacemacs = {
      url = "github:syl20bnr/spacemacs/develop";
      flake = false;
    };
    doomemacs = {
      url = "github:doomemacs/doomemacs/master";
      flake = false;
    };
    chemacs2 = {
      url = "github:plexus/chemacs2/main";
      flake = false;
    };
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };

    # asdf
    asdf-plugins = {
      url = "github:asdf-vm/asdf-plugins";
      flake = false;
    };

    # zsh_plugins
    zi = {
      url = "github:z-shell/zi";
      flake = false;
    };
    fast-syntax-highlighting = {
      url = "github:zdharma-continuum/fast-syntax-highlighting";
      flake = false;
    };
    fzf-tab = {
      url = "github:Aloxaf/fzf-tab";
      flake = false;
    };
    zsh-abbrev-alias = {
      url = "github:momo-lab/zsh-abbrev-alias";
      flake = false;
    };
    zsh-colored-man-pages = {
      url = "github:ael-code/zsh-colored-man-pages";
      flake = false;
    };
    powerlevel10k = {
      url = "github:romkatv/powerlevel10k";
      flake = false;
    };

    # yabai = {
    #   url = "github.com:koekeishiya/yabai";
    #   flake = false;
    # }

    forgit.url = "github:wfxr/forgit";
    forgit.flake = false;
  };

  outputs = { self, darwin, home-manager, flake-utils, ... }@inputs:
    let
      inherit (self.lib) attrValues makeOverridable optionalAttrs singleton;

      homeStateVersion = "23.05";

      # Configuration for `nixpkgs`
      nixpkgsDefaults = {
        config = { allowUnfree = true; };
        overlays = attrValues self.overlays ++ [
          # put stuff here
        ];
      };

      primaryUserInfo = {
        username = "gfanton";
        fullName = "";
        email = "8671905+gfanton@users.noreply.github.com";
        nixConfigDirectory = "/Users/gfanton/nixpkgs";
      };
    in {

      # Add some additional functions to `lib`.
      lib = inputs.nixpkgs-unstable.lib.extend (_: _: {
        mkDarwinSystem = import ./lib/mkDarwinSystem.nix inputs;
        lsnix = import ./lib/lsnix.nix;
      });

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
        pkgs-silicon = _: prev:
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
          asdf-plugins = inputs.asdf-plugins;
          zsh-plugins.fast-syntax-highlighting =
            inputs.fast-syntax-highlighting;
          zsh-plugins.fzf-tab = inputs.fzf-tab;
          zsh-plugins.zsh-abbrev-alias = inputs.fzf-tab;
          zsh-plugins.zsh-colored-man-pages = inputs.zsh-colored-man-pages;
          zsh-plugins.powerlevel10k = inputs.powerlevel10k;
          zsh-plugins.zi = inputs.zi;
        };

        my-loon = import ./overlays/loon.nix;
        my-libvterm = import ./overlays/libvterm.nix;
      };

      # System outputs ------------------------------------------------------------------------- {{{

      # My `nix-darwin` configs
      darwinConfigurations = rec {
        # Mininal configurations to bootstrap systems
        bootstrap-x86 = makeOverridable darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          modules = [ ./darwin/bootstrap.nix { nixpkgs = nixpkgsDefaults; } ];
        };
        bootstrap-arm = bootstrap-x86.override { system = "aarch64-darwin"; };

        # My Apple Silicon macOS laptop config
        macbook = makeOverridable self.lib.mkDarwinSystem (primaryUserInfo // {
          system = "aarch64-darwin";
          modules = attrValues self.darwinModules ++ singleton {
            nixpkgs = nixpkgsDefaults;
            users.primaryUser = primaryUserInfo;
            networking.computerName = "guicp";
            networking.hostName = "ghost";
            networking.knownNetworkServices = [ "Wi-Fi" "USB 10/100/1000 LAN" ];
            nix.registry.my.flake = inputs.self;
          };

          inherit homeStateVersion;
          homeModules = attrValues self.homeManagerModules;
        });
      };

      # Config I use with Linux cloud VMs
      # Build and activate on new system with:
      # `nix build .#homeConfigurations.cloud.activationPackage; ./result/activate`
      homeConfigurations.cloud = home-manager.lib.homeManagerConfiguration {
        pkgs = import inputs.nixpkgs-unstable {
          system = "x86_64-linux";
          inherit (nixpkgsDefaults) config overlays;
        };
        modules = attrValues self.homeManagerModules ++ singleton
          ({ config, ... }: {
            home.username = config.home.user-info.username;
            home.homeDirectory = "/home/${config.home.username}";
            home.stateVersion = homeStateVersion;
            home.user-info = primaryUserInfo // {
              nixConfigDirectory = "${config.home.homeDirectory}/nixpkgs";
            };
          });
      };
      # }}}

      # Non-system outputs --------------------------------------------------------------------- {{{

      darwinModules = {
        # My configurations
        my-bootstrap = import ./darwin/bootstrap.nix;
        my-defaults = import ./darwin/defaults.nix;
        my-env = import ./darwin/env.nix;
        my-homebrew = import ./darwin/homebrew.nix;
        my-config = import ./darwin/config.nix;

        # local modules
        # security-pam = import ./modules/darwin/security/pam.nix;
        services-emacsd = import ./modules/darwin/services/emacsd.nix;
        users-primaryUser = import ./modules/darwin/users.nix;
        programs-nix-index = import ./modules/darwin/programs/nix-index.nix;
      };

      homeManagerModules = {
        # My configurations
        my-shells = import ./home/shells.nix;
        my-colors = import ./home/colors.nix;
        my-git = import ./home/git.nix;
        my-kitty = import ./home/kitty.nix;
        my-packages = import ./home/packages.nix;
        my-asdf = import ./home/asdf.nix;
        my-emacs = import ./home/emacs.nix;
        my-config = import ./home/config.nix;

        # local modules
        colors = import ./modules/home/colors;
        programs-truecolor = import ./modules/home/programs/truecolor;
        programs-asdf = import ./modules/home/programs/asdf;
        programs-zi = import ./modules/home/programs/zi;
        programs-kitty-extras = import ./modules/home/programs/kitty/extras.nix;

        # programs-neovim-extras =
        #   import ./modules/home/programs/neovim/extras.nix;
        home-user-info = { lib, ... }: {
          options.home.user-info = (self.darwinModules.users-primaryUser {
            inherit lib;
          }).options.users.primaryUser;
        };
      };
      # }}}

      # Add re-export `nixpkgs` packages with overlays.
      # This is handy in combination with `nix registry add my /Users/gfanton/nixpkgs`
    } // flake-utils.lib.eachDefaultSystem (system: {
      # Re-export `nixpkgs-unstable` with overlays.
      # This is handy in combination with setting `nix.registry.my.flake = inputs.self`.
      # Allows doing things like `nix run my#prefmanager -- watch --all`
      legacyPackages =
        import inputs.nixpkgs-unstable (nixpkgsDefaults // { inherit system; });

      # Development shells ----------------------------------------------------------------------{{{
      # Shell environments for development
      # With `nix.registry.my.flake = inputs.self`, development shells can be created by running,
      # e.g., `nix develop my#python`.
      devShells = let pkgs = self.legacyPackages.${system};
      in {
        python = pkgs.mkShell {
          name = "python310";
          inputsFrom = attrValues {
            inherit (pkgs.pkgs-master.python310Packages) black isort;
            inherit (pkgs) poetry python310 pyright;
          };
        };
      };
      # }}}
    });
}
