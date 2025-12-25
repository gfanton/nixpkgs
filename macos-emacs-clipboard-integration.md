# macOS Emacs Clipboard Integration Guide

## Overview
When running Emacs in terminal mode on macOS, the default clipboard integration doesn't work properly. This guide provides modern solutions for seamless copy/paste between Emacs and macOS system clipboard.

## The Problem
- Terminal Emacs cannot directly access the macOS clipboard
- Standard Emacs kill/yank operations don't sync with system clipboard
- Copy/paste between Emacs and other applications doesn't work out of the box

## Solutions

### 1. Basic pbcopy/pbpaste Integration (Simple)

For manual copy/paste operations with dedicated keybindings:

```elisp
(defun osx/pbcopy ()
  "Copy region to macOS clipboard."
  (interactive)
  (if (use-region-p)
      (progn
        (call-process-region (region-beginning) (region-end) "pbcopy")
        (deactivate-mark))
    (message "No region selected")))

(defun osx/pbpaste ()
  "Paste from macOS clipboard."
  (interactive)
  (call-process "pbpaste" nil t))

(defun osx/pbcut ()
  "Cut region to macOS clipboard."
  (interactive)
  (when (use-region-p)
    (osx/pbcopy)
    (delete-region (region-beginning) (region-end))))

;; Keybindings
(global-set-key (kbd "H-c") 'osx/pbcopy)   ; Cmd+C
(global-set-key (kbd "H-v") 'osx/pbpaste)  ; Cmd+V  
(global-set-key (kbd "H-x") 'osx/pbcut)    ; Cmd+X
```

### 2. Automatic Kill-Ring Integration (Recommended)

This approach automatically syncs Emacs kill-ring with macOS clipboard:

```elisp
;; macOS clipboard integration for terminal Emacs
(when (and (eq system-type 'darwin) (not (display-graphic-p)))
  
  (defun osx-clipboard-cut-function (text &optional _)
    "Add TEXT to macOS clipboard."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  
  (defun osx-clipboard-paste-function ()
    "Paste from macOS clipboard."
    (shell-command-to-string "pbpaste"))
  
  ;; Override Emacs' default clipboard functions
  (setq interprogram-cut-function 'osx-clipboard-cut-function)
  (setq interprogram-paste-function 'osx-clipboard-paste-function))
```

### 3. tmux Integration

If running Emacs inside tmux, you need `reattach-to-user-namespace`:

```bash
# Install via Homebrew
brew install reattach-to-user-namespace
```

Then update your Emacs config:

```elisp
(when (and (eq system-type 'darwin) 
           (not (display-graphic-p))
           (getenv "TMUX"))
  
  (defun tmux-osx-cut-function (text &optional _)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" 
                                "reattach-to-user-namespace" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  
  (defun tmux-osx-paste-function ()
    (shell-command-to-string "reattach-to-user-namespace pbpaste"))
  
  (setq interprogram-cut-function 'tmux-osx-cut-function)
  (setq interprogram-paste-function 'tmux-osx-paste-function))
```

### 4. Modern Terminal Alternative (OSC 52)

For modern terminals supporting OSC 52 escape sequences (Kitty, iTerm2, Alacritty with config):

```elisp
;; Enable OSC 52 clipboard support
(setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))

;; For remote SSH sessions
(setq select-enable-clipboard t)
(setq select-enable-primary t)
```

### 5. Enhanced Implementation with Error Handling

A more robust implementation with proper error handling:

```elisp
(defun osx-clipboard-available-p ()
  "Check if pbcopy/pbpaste are available."
  (and (executable-find "pbcopy")
       (executable-find "pbpaste")))

(when (and (eq system-type 'darwin)
           (not (display-graphic-p))
           (osx-clipboard-available-p))
  
  (defun osx-clipboard-cut (text &optional _)
    "Copy TEXT to macOS clipboard with error handling."
    (condition-case err
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" nil "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc)
            (message "Copied %d characters to clipboard" (length text))))
      (error (message "Failed to copy to clipboard: %s" 
                     (error-message-string err)))))
  
  (defun osx-clipboard-paste ()
    "Paste from macOS clipboard with error handling."
    (condition-case err
        (let ((clipboard-text (shell-command-to-string "pbpaste")))
          (if (string-empty-p clipboard-text)
              nil
            clipboard-text))
      (error 
       (message "Failed to paste from clipboard: %s" 
               (error-message-string err))
       nil)))
  
  (setq interprogram-cut-function 'osx-clipboard-cut)
  (setq interprogram-paste-function 'osx-clipboard-paste)
  
  ;; Optional: Also sync with kill-ring
  (setq save-interprogram-paste-before-kill t))
```

## Comparison with Popular Configurations

### Doom Emacs
- Uses native NS clipboard when available
- Falls back to system trash integration
- Focuses on GUI Emacs compatibility

### Spacemacs OSX Layer
- Provides comprehensive macOS integration
- Includes dictionary lookup, text scaling
- Uses similar pbcopy/pbpaste approach for terminal

### Modern Community Configs (2024)
- Trend toward minimal, robust implementations
- Focus on error handling and tmux compatibility
- Support for OSC 52 in modern terminals

## Testing Your Configuration

Test clipboard integration with these commands:

```elisp
;; Test copy
(kill-new "Test clipboard text")

;; Test paste  
(insert (current-kill 0))

;; Check if functions are set
(message "Cut function: %s" interprogram-cut-function)
(message "Paste function: %s" interprogram-paste-function)
```

## Troubleshooting

1. **Nothing happens when copying**: Check if pbcopy/pbpaste are in PATH
2. **tmux issues**: Ensure reattach-to-user-namespace is installed
3. **SSH sessions**: Consider using OSC 52 or clipboard forwarding
4. **Performance**: Large clipboard operations may be slow; consider limiting size

## Recommended Configuration

For most users, the automatic kill-ring integration (#2) provides the best experience. Add error handling (#5) for production use.

For tmux users, combine solutions #2 and #3.

For modern terminal users (iTerm2, Kitty), consider OSC 52 (#4) as a cleaner alternative.