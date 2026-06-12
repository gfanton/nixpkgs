claude-config:

{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs) stdenv;
  inherit (config.home) homeDirectory;

  hasConfig = builtins.pathExists "${claude-config}/CLAUDE.md";

  # Shim that exec's claude with --plugin-dir flags. Prefers the upstream
  # auto-updating binary at ~/.local/bin/claude when present, otherwise
  # falls back to the nix-packaged claude-code from
  # programs.claude-code.package. Lives at ~/.local/share/claude-shim/
  # and is prepended to PATH so it wins over both
  # /etc/profiles/per-user/.../bin/claude AND ~/.local/bin/claude. The
  # upstream install path is hardcoded (no env var to relocate it), so
  # PATH-precedence is the only way to inject plugin flags into a binary
  # that auto-updates itself.
  claudeShimDir = "${homeDirectory}/.local/share/claude-shim";
  claudePluginDirs = with pkgs.claude-plugins; [
    superpowers
    frontend-design
    skill-creator
    pocock-skills
  ];

  jq = "${pkgs.jq}/bin/jq";

  # Shared notification payload parsing (jq with full Nix store paths)
  notifyPreamble = ''
    set -euo pipefail

    payload=$(cat)

    event=$(echo "$payload" | ${jq} -r '.hook_event_name // ""')
    cwd=$(echo "$payload" | ${jq} -r '.cwd // ""')
    project=$(basename "$cwd")
    title="Claude Code''${project:+ — $project}"
    body=""

    case "$event" in
      Stop)
        stop_active=$(echo "$payload" | ${jq} -r '.stop_hook_active // false')
        [ "$stop_active" = "true" ] && exit 0
        body="Task complete" ;;
      Notification)
        body=$(echo "$payload" | ${jq} -r '.message // "Notification"') ;;
      PreToolUse)
        tool=$(echo "$payload" | ${jq} -r '.tool_name // ""')
        [ "$tool" != "AskUserQuestion" ] && exit 0
        body=$(echo "$payload" | ${jq} -r '.tool_input.questions[0].question // "Has a question"' | head -c 100) ;;
      *) exit 0 ;;
    esac

    [ -z "$body" ] && exit 0
  '';

  notifyHook =
    if stdenv.isDarwin then "~/.claude/hooks/notify.sh" else "~/.claude/hooks/notify-linux.sh";

  # Declarative settings.json content. Rendered to a real file (not a store
  # symlink) via the activation script below so Claude Code's interactive
  # commands (/effort, /config, theme, ...) can write back to it at runtime.
  # nix remains source of truth: a rebuild overwrites interactive changes.
  claudeSettings = {
    "$schema" = "https://json.schemastore.org/claude-code-settings.json";
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
      defaultMode = "bypassPermissions";
    };
    alwaysThinkingEnabled = true;
    skipDangerousModePermissionPrompt = true;
    effortLevel = "high";
    includeCoAuthoredBy = false;
    hooks = {
      # Stop hook is flaky from settings.json (anthropics/claude-code#26770).
      # Notification hook is the reliable path for alerts.
      Stop = [
        {
          matcher = "";
          hooks = [
            {
              type = "command";
              command = "bash ${notifyHook}";
              timeout = 10;
            }
          ];
        }
      ];
      Notification = [
        {
          hooks = [
            {
              type = "command";
              command = "bash ${notifyHook}";
              timeout = 10;
            }
          ];
        }
      ];
      PreToolUse = [
        {
          matcher = "Bash";
          hooks = [
            {
              type = "command";
              command = "bash ~/.claude/hooks/rtk-rewrite.sh";
            }
          ];
        }
        {
          matcher = "AskUserQuestion";
          hooks = [
            {
              type = "command";
              command = "bash ${notifyHook}";
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
      # Re-run every 30s so rate-limit countdowns tick while the session idles.
      refreshInterval = 30;
    };
  };

  settingsFile = (pkgs.formats.json { }).generate "claude-code-settings.json" claudeSettings;
in
lib.mkIf hasConfig {
  programs.claude-code = {
    enable = true;

    # Agent definitions
    agentsDir = "${claude-config}/agents";

    # Skill definitions
    skills = "${claude-config}/skills";

    # LSP servers
    lspServers.go = {
      command = "gopls";
      args = [ "serve" ];
      extensionToLanguage = {
        ".go" = "go";
      };
    };

    # settings.json is rendered as a real, writable file via the activation
    # script below (see claudeSettings) rather than through this option, which
    # would emit a read-only /nix/store symlink that interactive commands
    # cannot write to.
  };

  # ---- Native binary shim (PATH-prepended so it wins over ~/.local/bin/claude)
  home.sessionPath = lib.mkBefore [ claudeShimDir ];

  home.file.".local/share/claude-shim/claude" = {
    executable = true;
    text = ''
      #!${pkgs.bash}/bin/bash
      if [ -x "$HOME/.local/bin/claude" ]; then
        bin="$HOME/.local/bin/claude"
      else
        bin="${config.programs.claude-code.package}/bin/claude"
      fi
      exec "$bin" \
        ${lib.concatMapStringsSep " \\\n        " (p: ''--plugin-dir ${p}'') claudePluginDirs} \
        "$@"
    '';
  };

  # Second account: same shim (plugins, upstream-binary fallback), isolated
  # config dir. On macOS the OAuth Keychain entry is namespaced per config
  # dir, so this is a fully separate login from ~/.claude.
  home.file.".local/share/claude-shim/claude-work" = {
    executable = true;
    text = ''
      #!${pkgs.bash}/bin/bash
      export CLAUDE_CONFIG_DIR="$HOME/.claude-work"
      exec "${claudeShimDir}/claude" "$@"
    '';
  };

  # ---- Global Memory (CLAUDE.md)
  # Copied as a real file instead of a store symlink so relative `@imports`
  # inside CLAUDE.md resolve to paths under $HOME. Claude Code treats paths
  # outside $HOME as external includes and silently drops them without an
  # approval prompt when the CLAUDE.md itself is a symlink into /nix/store.
  home.activation.claudeCodeMemory = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run rm -f $HOME/.claude/CLAUDE.md
    run install -Dm644 ${claude-config}/CLAUDE.md $HOME/.claude/CLAUDE.md
  '';

  # ---- Settings (settings.json)
  # Copied as a real writable file instead of a store symlink so Claude Code's
  # interactive commands (/effort, /config, theme) can persist changes. A
  # rebuild re-applies the declarative content, so nix stays source of truth.
  # The work profile (~/.claude-work) gets the same baseline; work-specific
  # content stays machine-local.
  home.activation.claudeCodeSettings = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run rm -f $HOME/.claude/settings.json $HOME/.claude-work/settings.json
    run install -Dm644 ${settingsFile} $HOME/.claude/settings.json
    run install -Dm644 ${settingsFile} $HOME/.claude-work/settings.json
  '';

  # ---- Hook and Statusline Scripts
  # Managed via home.file for executable bit (upstream hooksDir doesn't set it)

  home.file.".claude/hooks/rtk-rewrite.sh" = {
    source = "${pkgs.my-rtk}/libexec/rtk/hooks/rtk-rewrite.sh";
    executable = true;
  };

  home.file.".claude/RTK.md".source = "${pkgs.my-rtk}/share/rtk/RTK.md";

  home.file.".claude/hooks/notify.sh" = lib.mkIf stdenv.isDarwin {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ${notifyPreamble}

      # Detect terminal bundle ID for click-to-activate
      terminal_bid=""
      for bid in com.mitchellh.ghostty com.googlecode.iterm2 com.apple.Terminal dev.warp.Warp-Stable net.kovidgoyal.kitty org.alacritty com.github.wez.wezterm; do
        if [ -n "$(lsappinfo find bundleid="$bid" 2>/dev/null)" ]; then
          terminal_bid="$bid"
          break
        fi
      done

      args=(-title "$title" -message "$body" -sound Glass)
      if [ -n "$terminal_bid" ]; then
        args+=(-activate "$terminal_bid" -sender "$terminal_bid")
      fi

      ${pkgs.terminal-notifier}/bin/terminal-notifier "''${args[@]}" 2>/dev/null || true
    '';
  };

  home.file.".claude/hooks/notify-linux.sh" = lib.mkIf stdenv.isLinux {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      ${notifyPreamble}
      ${pkgs.libnotify}/bin/notify-send "$title" "$body" --icon=dialog-information 2>/dev/null || true
    '';
  };

  home.file.".claude/statusline.sh" = lib.mkIf stdenv.isDarwin {
    source = "${claude-config}/statusline.sh";
    executable = true;
  };
}
