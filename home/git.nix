{ pkgs, lib, ... }:

{
  # Git
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.git.enable
  # Aliases config imported in flake.
  programs.git ={
    enable = true;

    # `$GITHUB_TOKEN` which `gh` uses for authentication is set in `./private.nix`. `gh auth` can't
    # be used since it tries to write to the config, which is in the store.
    # imports = lib.filter lib.pathExists [ ./private.nix ];
    userEmail = "8671905+gfanton@users.noreply.github.com";
    userName = "gfanton";

    aliases = {
      co = "checkout";
    };
    package = pkgs.buildEnv {
      name = "myGitEnv";
      paths = with pkgs.silicon.gitAndTools; [git gh tig];
    };
    delta = {
      enable = true;
    };
    lfs = {
      enable = true;
    };
    ignores = [
      "*~"
      "*.swp"
      "*#"
      ".#*"
      ".DS_Store"
    ];
    extraConfig = {
      core = {
        whitespace = "trailing-space,space-before-tab";
        editor = "em";
      };
      pull = {
        rebase = true;
      };
      url."git@github.com:".insteadOf = "https://github.com/";
    };
  };
}
