;; I want this really early on so you don't see the startup message
;; flash on the screen before this suppresses it
(setq inhibit-startup-message t)

;; Set up logging for debugging
(defvar emacs-nix-log-file "~/.local/log/emacs-nix.log"
  "Log file for emacs-nix debugging.")

(defun emacs-nix-log (message)
  "Log MESSAGE to the emacs-nix log file with timestamp."
  (with-temp-buffer
    (insert (format "[%s] %s\n"
                    (format-time-string "%Y-%m-%d %H:%M:%S")
                    message))
    (append-to-file (point-min) (point-max) emacs-nix-log-file)))

(emacs-nix-log "=== Starting emacs-nix initialization ===")
(emacs-nix-log (format "user-emacs-directory: %s" user-emacs-directory))
(emacs-nix-log (format "load-path length: %d" (length load-path)))

(require 'org)
(emacs-nix-log "org-mode required successfully")

;; when using home-manager, config.org will always have mtime of the
;; unix epoch, and org-babel-load-file won't rebuild config.el from
;; config.org if config.el is newer (which it always will be).  So we
;; delete config.el here.
(let ((config-el (concat user-emacs-directory "config.el")))
  (when (file-exists-p config-el)
    (delete-file config-el)
    (emacs-nix-log "Deleted existing config.el")))

;; load the main config file
(emacs-nix-log "About to load config.org")
(condition-case err
    (progn
      (org-babel-load-file (concat user-emacs-directory "config.org"))
      (emacs-nix-log "config.org loaded successfully"))
  (error
   (emacs-nix-log (format "Error loading config.org: %s" (error-message-string err)))
   (signal (car err) (cdr err))))
