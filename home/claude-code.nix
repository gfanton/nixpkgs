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

  # Notification click-to-jump and the agent-session integration both shell out
  # to herdr. Taken from the package rather than PATH because the click handler
  # runs under gnotify, which does not inherit the login shell's PATH.
  herdr = pkgs.pkgs-herdr.herdr;

  # Skill-library convention hooks. Both the hook scripts and the references they
  # read live in the claude-config tree and run straight from the read-only store.
  # SKILL_LIB_REFS points the hooks at the references; the prepended PATH supplies
  # jq/git/awk/sed/coreutils because Claude Code invokes hooks with a thin PATH
  # that may not include the nix profile.
  #
  # Subagent delivery: the UserPromptSubmit/SessionStart injections reach the main
  # loop only — subagents get no hook context and no $CLAUDE_PLUGIN_ROOT (the
  # library is not a plugin here). They resolve the references ambiently instead:
  # settings env exports SKILL_LIB_REFS session-wide (inherited by every Bash call,
  # subagents included), and a stable symlink at <config-dir>/skill-library/
  # references backs the literal path documented in SKILL-LIBRARY.md.
  conventionRefs = "${claude-config}/skill-library/references";
  conventionHooks = "${claude-config}/playground/hooks";
  conventionHookEnv =
    "PATH=${lib.makeBinPath [ pkgs.jq pkgs.git pkgs.gawk pkgs.gnused pkgs.coreutils ]}:$PATH "
    + "SKILL_LIB_REFS=${conventionRefs}";

  # The repo-root skills and the skill-library's own skills, merged into one tree
  # so every subdir lands in ~/.claude/skills.
  claudeSkills = pkgs.symlinkJoin {
    name = "claude-skills";
    paths = [
      "${claude-config}/skills"
      "${claude-config}/skill-library/skills"
    ];
  };

  # Same merge for agents. skill-library/agents holds the generic code-reviewer,
  # which the library's architecture treats as the review path for every language;
  # without this it ships in the repo and reaches no session.
  claudeAgents = pkgs.symlinkJoin {
    name = "claude-agents";
    paths = [
      "${claude-config}/agents"
      "${claude-config}/skill-library/agents"
    ];
  };

  # Both Claude config profiles: the default account and the isolated work
  # account (its shim sets CLAUDE_CONFIG_DIR=~/.claude-work). Shared config —
  # CLAUDE.md, settings, skills, agents — deploys identically to each.
  configDirs = [ ".claude" ".claude-work" ];

  # MCP servers: definitions and merge logic live in claude-config
  # (mcp/default.nix); only the machine-specific wrapper directory is
  # injected from here.
  claudeMcp = import "${claude-config}/mcp" { inherit pkgs claudeShimDir; };

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
    env = {
      # CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS stays unset: it makes the Agent
      # tool's `name` route spawns onto the teammate mailbox, where the agent's
      # final report is discarded (anthropics/claude-code#71723).
      SKILL_LIB_REFS = conventionRefs;
    };
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
        # NOTE: the Edit|Write convention gate (gate-conventions.sh) is intentionally
        # un-wired — we're testing the lighter "preferences framing + FULL/CORE load
        # classes" approach first. The script stays in the tree as the escalation
        # fallback if the experiment shows framing alone isn't enough.
      ];
      # Force-load house code conventions: per-prompt push to the mandatory
      # core (remind-conventions.sh) + session-start awareness map (session-start.sh).
      UserPromptSubmit = [
        {
          matcher = "";
          hooks = [
            {
              type = "command";
              command = "${conventionHookEnv} bash ${conventionHooks}/remind-conventions.sh";
              timeout = 20;
            }
          ];
        }
      ];
      SessionStart = [
        {
          matcher = "";
          hooks = [
            {
              type = "command";
              command = "${conventionHookEnv} bash ${conventionHooks}/session-start.sh";
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
lib.mkIf hasConfig (lib.mkMerge [
  {
  programs.claude-code = {
    enable = true;

    # Skills and agents deploy to both profiles via home.file below, not through
    # the module (which targets ~/.claude only).

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

  # GitHub MCP launcher: resolves the PAT from a local secret file and passes
  # it to the containerized github-mcp-server as a per-process env var.
  # Referenced by mcpServers.github (see claudeCodeMcp activation below).
  home.file.".local/share/claude-shim/github-mcp" = lib.mkIf stdenv.isDarwin {
    executable = true;
    text = ''
      #!${pkgs.bash}/bin/bash
      set -euo pipefail
      pat_file="$HOME/.config/claude/github-pat"
      if [ ! -r "$pat_file" ]; then
        echo "github-mcp: $pat_file is missing or unreadable." >&2
        echo "Seed it: install -m600 /dev/null \"$pat_file\" && printf %s '<token>' > \"$pat_file\"" >&2
        exit 1
      fi
      # $(<...) strips any trailing newline so the token is passed verbatim.
      token="$(cat "$pat_file")"
      exec docker run -i --rm \
        -e GITHUB_PERSONAL_ACCESS_TOKEN="$token" \
        ghcr.io/github/github-mcp-server
    '';
  };

  # ---- Global Memory (CLAUDE.md)
  # Copied as a real file instead of a store symlink so relative `@imports`
  # inside CLAUDE.md resolve to paths under $HOME. Claude Code treats paths
  # outside $HOME as external includes and silently drops them without an
  # approval prompt when the CLAUDE.md itself is a symlink into /nix/store.
  home.activation.claudeCodeMemory = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    for dir in ${lib.concatMapStringsSep " " (d: "\"$HOME/${d}\"") configDirs}; do
      run rm -f "$dir/CLAUDE.md"
      run install -Dm644 ${claude-config}/CLAUDE.md "$dir/CLAUDE.md"
    done
  '';

  # ---- Settings (settings.json)
  # Copied as a real writable file instead of a store symlink so Claude Code's
  # interactive commands (/effort, /config, theme) can persist changes. A
  # rebuild re-applies the declarative content, so nix stays source of truth.
  # The work profile (~/.claude-work) gets the same baseline; work-specific
  # content stays machine-local.
  home.activation.claudeCodeSettings = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    for dir in ${lib.concatMapStringsSep " " (d: "\"$HOME/${d}\"") configDirs}; do
      run rm -f "$dir/settings.json"
      run install -Dm644 ${settingsFile} "$dir/settings.json"
    done
  '';

  # ---- herdr agent integration
  # A SessionStart hook reporting Claude's session id and transcript path to the
  # enclosing herdr pane, which is what lets herdr resume an agent into its own
  # conversation after a server restart. herdr versions both the hook script and
  # its settings.json entry, so this runs herdr's installer instead of vendoring
  # a copy that would go stale on upgrade. It must follow claudeCodeSettings,
  # which rewrites settings.json wholesale and would otherwise drop the entry on
  # every rebuild.
  home.activation.herdrClaudeIntegration = lib.hm.dag.entryAfter [ "claudeCodeSettings" ] ''
    for dir in ${lib.concatMapStringsSep " " (d: "\"$HOME/${d}\"") configDirs}; do
      run env CLAUDE_CONFIG_DIR="$dir" ${lib.getExe herdr} integration install claude
    done
  '';

  # ---- MCP servers (mcpServers in .claude.json)
  # All MCP logic lives in claude-config (mcp/default.nix); this only
  # schedules its activation snippet. Darwin-only because the github wrapper
  # runs the containerized server via docker.
  home.activation.claudeCodeMcp = lib.mkIf stdenv.isDarwin (
    lib.hm.dag.entryAfter [ "writeBoundary" ] claudeMcp.activationScript
  );

  # ---- Hook and Statusline Scripts
  # Managed via home.file for executable bit (upstream hooksDir doesn't set it)

  home.file.".claude/hooks/rtk-rewrite.sh" = {
    source = "${pkgs.my-rtk}/libexec/rtk/hooks/rtk-rewrite.sh";
    executable = true;
  };

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

      args=(--title "$title" --message "$body" --sound default)
      [ -n "$terminal_bid" ] && args+=(--activate "$terminal_bid")

      # Inside herdr: clicking the notification jumps to the originating
      # workspace and tab. herdr exports these into every pane the way tmux
      # exports $TMUX_PANE, so the ids need no lookup. Pane-level focus is
      # socket-API only — the CLI exposes just directional moves — so a split
      # tab lands on the tab rather than on Claude's own pane.
      if [ "''${HERDR_ENV:-}" = "1" ] && [ -n "''${HERDR_WORKSPACE_ID:-}" ]; then
        herdr_bin="${lib.getExe herdr}"
        jump="\"$herdr_bin\" workspace focus \"$HERDR_WORKSPACE_ID\" >/dev/null 2>&1"
        [ -n "''${HERDR_TAB_ID:-}" ] && jump="$jump; \"$herdr_bin\" tab focus \"$HERDR_TAB_ID\" >/dev/null 2>&1"
        args+=(--exec "$jump")

      # Inside tmux: clicking the notification jumps to the originating pane.
      # The hook inherits $TMUX/$TMUX_PANE from Claude's pane. Resolve the target
      # and the client viewing it now, then switch to it on click via --exec.
      elif [ -n "''${TMUX:-}" ] && [ -n "''${TMUX_PANE:-}" ]; then
        tmux_bin=$(command -v tmux || true)
        if [ -n "$tmux_bin" ]; then
          socket="''${TMUX%%,*}"
          target=$("$tmux_bin" display-message -p -t "$TMUX_PANE" '#S:#I.#P' 2>/dev/null || true)
          client_tty=$("$tmux_bin" -S "$socket" list-clients -F '#{pane_id} #{client_tty}' 2>/dev/null \
            | awk -v p="$TMUX_PANE" '$1==p{print $2; exit}')
          if [ -n "$target" ]; then
            jump="\"$tmux_bin\" -S \"$socket\" switch-client"
            [ -n "$client_tty" ] && jump="$jump -c \"$client_tty\""
            jump="$jump -t \"$target\" 2>/dev/null"
            jump="$jump; \"$tmux_bin\" -S \"$socket\" select-window -t \"$target\" 2>/dev/null"
            jump="$jump; \"$tmux_bin\" -S \"$socket\" select-pane -t \"$target\" 2>/dev/null"
            args+=(--exec "$jump")
          fi
        fi
      fi

      ${pkgs.gnotify}/bin/gnotify "''${args[@]}" 2>/dev/null || true
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

  # Config shared identically across both profiles. RTK.md + SKILL-LIBRARY.md are
  # @imported by CLAUDE.md and resolve beside it; skills/agents are read from
  # CLAUDE_CONFIG_DIR. recursive lets externally-managed entries (e.g. the gno
  # skill) coexist in the same directory.
  {
    home.file = lib.mkMerge (
      map (dir: {
        "${dir}/RTK.md".source = "${pkgs.my-rtk}/share/rtk/RTK.md";
        "${dir}/SKILL-LIBRARY.md".source = "${claude-config}/SKILL-LIBRARY.md";
        "${dir}/skill-library/references".source = conventionRefs;
        "${dir}/skills" = {
          source = claudeSkills;
          recursive = true;
        };
        "${dir}/agents" = {
          source = claudeAgents;
          recursive = true;
        };
      }) configDirs
    );
  }
])
