{ ... }:

{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      add_newline = true;
      command_timeout = 2000;
      scan_timeout = 2;

      format = builtins.concatStringsSep "" [
        "$username"
        "$hostname"
        "$directory"
        "$git_branch"
        "$git_status"
        "$nix_shell"
        "$env_var"
        "$cmd_duration"
        "$custom"
        "\n"
        "$character"
      ];

      character = {
        success_symbol = "[❯](bold green)";
        error_symbol = "[❯](bold red)";
        vimcmd_symbol = "[❮](bold yellow)";
      };

      directory = {
        truncation_length = 3;
        style = "bold blue";
      };

      git_branch.style = "bold purple";

      git_status.style = "white";

      nix_shell = {
        heuristic = true;
        symbol = "❄️ ";
        format = "[$symbol$state( \\($name\\))]($style) ";
        style = "bold blue";
        unknown_msg = "nix";
      };

      env_var.DEVENV_ROOT = {
        format = "[devenv]($style) ";
        style = "bold cyan";
      };

      username = {
        show_always = false;
        format = "[$user]($style)@";
        style_user = "bold green";
      };

      hostname = {
        ssh_only = true;
        format = "[$hostname]($style) ";
        style = "bold red";
      };

      cmd_duration = {
        min_time = 2000;
        format = "[$duration]($style) ";
        style = "bold yellow";
      };

      custom.arch = {
        command = "arch";
        when = "true";
        format = "[$output]($style) ";
        style = "bold yellow";
      };
    };
  };
}
