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
      # Basic commands
      d = "diff";
      # Other commands
      lg = "log --graph --abbrev-commit --decorate --format=format:'%C(blue)%h%C(reset) - %C(green)(%ar)%C(reset) %s %C(italic)- %an%C(reset)%C(magenta bold)%d%C(reset)' --all";
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
