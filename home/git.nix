{ config, pkgs, ... }:

{
  # Git
  programs.git = {
    # https://rycee.gitlab.io/home-manager/options.html#opt-enable
    # Aliases config in ./configs/git-aliases.nix
    enable = true;

    # XXX: master need to be used to avoid sigkill on git
    package = pkgs.pkgs-master.git;

    signing = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAkCFGpoXMDC2mrBKPB7kx+DL49LeCbhWulOsbUnp7a6";
      signByDefault = false;
    };

    settings = {
      user.email = config.home.user-info.email;
      user.name = config.home.user-info.username;
      gpg.format = "ssh";
      gpg.ssh.program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
      core.editor = "em";
      core.whitespace = "trailing-space,space-before-tab";
      diff.colorMoved = "default";
      pull.rebase = true;
      alias.lg = "log --graph --abbrev-commit --decorate --format=format:'%C(blue)%h%C(reset) - %C(green)(%ar)%C(reset) %s %C(italic)- %an%C(reset)%C(magenta bold)%d%C(reset)' --all";
    };

    ignores = [
      "*~"
      "*.swp"
      "*#"
      ".#*"
      ".DS_Store"
      ".mynote"
      ".claude"
      "CLAUDE.md"
    ];

    # large file
    lfs.enable = true;
  };

  # Enhanced diffs
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
  };

  # GitHub CLI
  # https://rycee.gitlab.io/home-manager/options.html#opt-programs.gh.enable
  programs.gh = {
    enable = true;
    settings.git_protocol = "ssh";
  };
}
