claude-config:

{
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs) stdenv;

  hasConfig = builtins.pathExists "${claude-config}/CLAUDE.md";

  notifyHook =
    if stdenv.isDarwin then "~/.claude/hooks/notify.sh" else "~/.claude/hooks/notify-linux.sh";
in
lib.mkIf hasConfig {
  programs.claude-code = {
    enable = true;

    # Global instructions
    memory.source = "${claude-config}/CLAUDE.md";

    # Agent definitions
    agentsDir = "${claude-config}/agents";

    # Settings (declarative, read-only nix store symlink)
    settings = {
      env.CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
      permissions = {
        allow = [
          "Bash(go mod init:*)"
          "Bash(go:*)"
          "Bash(mkdir:*)"
          "WebFetch(domain:docs.anthropic.com)"
          "Bash(./test-claude)"
          "Bash(./simple-test)"
          "Bash(make:*)"
          "Bash(ls:*)"
        ];
        deny = [ ];
        defaultMode = "dontAsk";
      };
      enabledPlugins = {
        "frontend-design@claude-code-plugins" = true;
        "frontend-design@claude-plugins-official" = true;
      };
      alwaysThinkingEnabled = true;
      skipDangerousModePermissionPrompt = true;
      effortLevel = "high";
      includeCoAuthoredBy = false;
      hooks = {
        Stop = [
          {
            matcher = "";
            hooks = [
              {
                type = "command";
                command = notifyHook;
                timeout = 10;
              }
            ];
          }
        ];
        Notification = [
          {
            matcher = "permission_prompt";
            hooks = [
              {
                type = "command";
                command = notifyHook;
                timeout = 10;
              }
            ];
          }
        ];
        PreToolUse = [
          {
            matcher = "AskUserQuestion";
            hooks = [
              {
                type = "command";
                command = notifyHook;
                timeout = 10;
              }
            ];
          }
        ];
      };
    }
    // lib.optionalAttrs stdenv.isDarwin {
      statusLine = {
        type = "command";
        command = "~/.claude/statusline.sh";
      };
    };
  };

  # ---- Hook and Statusline Scripts
  # Managed via home.file for executable bit (upstream hooksDir doesn't set it)

  home.file.".claude/hooks/notify.sh" = lib.mkIf stdenv.isDarwin {
    source = "${claude-config}/hooks/notify.sh";
    executable = true;
  };

  home.file.".claude/hooks/notify-linux.sh" = lib.mkIf stdenv.isLinux {
    source = "${claude-config}/hooks/notify-linux.sh";
    executable = true;
  };

  home.file.".claude/statusline.sh" = lib.mkIf stdenv.isDarwin {
    source = "${claude-config}/statusline.sh";
    executable = true;
  };

  # Linux: ensure libnotify is available for notify-send
  home.packages = lib.optionals stdenv.isLinux [ pkgs.libnotify ];
}
