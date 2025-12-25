# Ultimate Clipboard Solution for Kitty + tmux + Emacs (macOS & Linux)

## Overview
This solution uses OSC 52 escape sequences to provide seamless clipboard integration across:
- Local macOS with Kitty + tmux + Emacs  
- Remote Linux SSH sessions with the same nixpkgs config
- Nested tmux sessions
- Works without X11 forwarding

## The Solution: Clipetty + OSC 52

### 1. Install Clipetty Package

Add to your Nix Emacs packages:
```nix
# home/emacs.nix
myEmacsPackages = epkgs: with epkgs; [
  # ... other packages
  clipetty  # OSC 52 clipboard support
  # ... other packages
];
```

### 2. Emacs Configuration

Add this to your `config.org`:

```elisp
* Clipboard Integration (Cross-platform)

#+begin_src emacs-lisp
  ;; Universal clipboard solution using OSC 52
  ;; Works on macOS, Linux, over SSH, in tmux, etc.
  (use-package clipetty
    :ensure t
    :config
    ;; Clipetty automatically detects if we're in SSH/tmux/screen
    ;; and handles the escape sequences appropriately
    
    ;; For tmux users: Tell clipetty to assume nested mux when over SSH
    (setq clipetty-assume-nested-mux t)
    
    ;; Optional: Only enable in terminal mode
    (if (not (display-graphic-p))
        (global-clipetty-mode 1))
    
    (emacs-nix-log "Clipetty OSC 52 clipboard integration enabled"))
    
  ;; Alternative: Use clipetty on-demand with a keybinding
  ;; This avoids overwriting clipboard on every kill
  (global-set-key (kbd "H-c") 'clipetty-kill-ring-save)  ; Cmd+C on macOS
#+end_src
```

### 3. tmux Configuration

Add to your tmux config (`~/.tmux.conf` or in your nixpkgs):

```bash
# Enable OSC 52 clipboard support
set -g set-clipboard on

# Allow OSC 52 even for applications that don't think the terminal supports it
set -ag terminal-overrides ",xterm-256color:Ms=\\E]52;%p1%s;%p2%s\\007"

# For tmux 3.3a and later, allow passthrough of escape sequences
# This is crucial for nested tmux sessions
set -g allow-passthrough on

# Update SSH_TTY environment variable on reattach
# This fixes clipboard after detaching and reattaching tmux sessions
set -ag update-environment "SSH_TTY"

# If you're using tmux 3.2 or earlier, use this instead:
# set -g allow-rename on
```

### 4. Kitty Terminal Configuration

Add to your `kitty.conf`:

```conf
# Enable reading and writing to clipboard via OSC 52
clipboard_control write-clipboard write-primary read-clipboard read-primary

# For older Kitty versions, you might need:
# clipboard_control yes
```

### 5. Complete Integrated Solution

Here's a comprehensive Emacs configuration that handles all scenarios:

```elisp
* Universal Clipboard Integration

#+begin_src emacs-lisp
  ;; Detect environment
  (defun my/clipboard-environment ()
    "Detect clipboard environment."
    (cond
     ;; GUI mode - use built-in clipboard
     ((display-graphic-p) 'gui)
     ;; SSH session - use OSC 52
     ((getenv "SSH_TTY") 'ssh)
     ;; tmux session - use OSC 52
     ((getenv "TMUX") 'tmux)
     ;; Local terminal on macOS - could use pbcopy or OSC 52
     ((eq system-type 'darwin) 'macos-terminal)
     ;; Local terminal on Linux
     ((eq system-type 'gnu/linux) 'linux-terminal)
     (t 'unknown)))
  
  ;; Configure clipboard based on environment
  (let ((env (my/clipboard-environment)))
    (cond
     ;; GUI mode - nothing to do, works out of the box
     ((eq env 'gui)
      (emacs-nix-log "GUI mode detected - using native clipboard"))
     
     ;; All terminal environments - use clipetty
     ((memq env '(ssh tmux macos-terminal linux-terminal))
      (use-package clipetty
        :ensure t
        :config
        ;; Assume nested mux for SSH sessions
        (when (getenv "SSH_TTY")
          (setq clipetty-assume-nested-mux t))
        
        ;; Enable globally for terminal mode
        (global-clipetty-mode 1)
        (emacs-nix-log (format "Clipetty enabled for %s environment" env))))
     
     ;; Unknown environment - try clipetty anyway
     (t
      (use-package clipetty
        :ensure t
        :config
        (global-clipetty-mode 1)
        (emacs-nix-log "Unknown environment - clipetty enabled as fallback")))))
  
  ;; Optional: Fallback for macOS when OSC 52 isn't available
  (when (and (eq system-type 'darwin)
             (not (display-graphic-p))
             (not (featurep 'clipetty)))
    (defun my/pbcopy-fallback (text &optional _)
      "Fallback to pbcopy on macOS."
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" nil "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))
    
    (defun my/pbpaste-fallback ()
      "Fallback to pbpaste on macOS."
      (shell-command-to-string "pbpaste"))
    
    (setq interprogram-cut-function 'my/pbcopy-fallback)
    (setq interprogram-paste-function 'my/pbpaste-fallback)
    (emacs-nix-log "Using pbcopy/pbpaste fallback"))
#+end_src
```

## Testing Your Setup

### Test OSC 52 Support
```bash
# Test if your terminal supports OSC 52
printf "\033]52;c;$(printf "Hello, World" | base64)\a"
# Then try pasting - you should get "Hello, World"
```

### Test in Emacs
```elisp
;; Kill some text and check system clipboard
(kill-new "Test from Emacs")
;; Try pasting outside Emacs

;; Check clipetty status
(if (bound-and-true-p clipetty-mode)
    "Clipetty is active"
  "Clipetty is not active")
```

### Debug Issues
```elisp
;; Check what clipetty detects
(message "Multiplexer: %s" (clipetty--get-tmux-ssh-tty))
(message "SSH TTY: %s" (getenv "SSH_TTY"))
(message "TMUX: %s" (getenv "TMUX"))
```

## Advantages of This Solution

1. **Universal**: Works on macOS, Linux, locally, and over SSH
2. **No X11 Required**: Works purely through terminal escape sequences
3. **tmux Compatible**: Handles nested tmux sessions correctly
4. **Kitty Optimized**: Takes advantage of Kitty's extended OSC 52 support
5. **Automatic**: Seamlessly integrates with Emacs kill-ring
6. **Detach/Reattach Safe**: Handles tmux session detach/reattach properly

## Troubleshooting

### Clipboard Not Working Over SSH
- Ensure your SSH client terminal (Kitty) supports OSC 52
- Check tmux has `allow-passthrough on` (tmux 3.3a+)
- Verify `SSH_TTY` environment variable is set

### Large Copies Fail
- Some terminals have OSC 52 size limits
- Kitty has extended support for larger clipboards
- Consider using `clipetty-max-bytes` to limit size

### tmux Not Passing Through
```bash
# Check tmux version
tmux -V

# For tmux < 3.3a, you need different config
set -g set-clipboard on
# Instead of allow-passthrough, use:
set -g terminal-overrides 'xterm*:Ms=\\E]52;%p1%s;%p2%s\\007'
```

### Testing Without Installing
```elisp
;; Test OSC 52 manually in Emacs
(defun test-osc52 (text)
  (send-string-to-terminal
   (concat "\e]52;c;"
           (base64-encode-string text t)
           "\a")))

(test-osc52 "Test clipboard")
```

## Summary

This solution provides a robust, cross-platform clipboard integration that:
- Works identically on macOS and Linux
- Functions over SSH without X11 forwarding  
- Handles tmux (including nested sessions)
- Integrates seamlessly with Emacs kill-ring
- Requires minimal configuration

The key is using OSC 52 escape sequences via Clipetty, which are universally supported by modern terminals and work across all your environments.