;;; magit-live-diff.el --- Live diff reload for Magit -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 1.1.0
;; Package-Requires: ((emacs "27.1") (magit "3.0"))
;; Keywords: git, tools

;;; Commentary:

;; Live diff reload for Magit - auto-refresh on file changes.
;; M-x magit-live-diff-mode to toggle

;;; Code:

(require 'filenotify)
(require 'subr-x)  ; Provides when-let*, if-let*, etc.

;; ---- Customization

(defgroup magit-live-diff nil
  "Live diff reload for Magit."
  :group 'magit
  :prefix "magit-live-diff-")

(defcustom magit-live-diff-debounce-delay 0.5
  "Delay in seconds before refreshing Magit buffers after a file change."
  :type 'number
  :group 'magit-live-diff)

(defcustom magit-live-diff-use-after-save-hook t
  "Whether to use `after-save-hook' for internal change detection."
  :type 'boolean
  :group 'magit-live-diff)

(defcustom magit-live-diff-debug nil
  "If non-nil, print debug messages to *Messages*."
  :type 'boolean
  :group 'magit-live-diff)

;; ---- Internal Variables

(defvar magit-live-diff--watches nil
  "Alist of (directory . watch-descriptor) for active file watchers.")

(defvar magit-live-diff--timer nil
  "Debounce timer for refreshing Magit buffers.")

(defvar magit-live-diff--pending-repos nil
  "List of repository directories with pending refreshes.")

(defvar magit-live-diff--refreshing nil
  "Non-nil when we are currently refreshing (to ignore self-triggered events).")

(defvar-local magit-live-diff--buffer-repo nil
  "Cached repo root for this buffer.")

;; ---- Helper Functions

(defun magit-live-diff--log (format-string &rest args)
  "Log a message if debug enabled."
  (when magit-live-diff-debug
    (apply #'message (concat "[magit-live-diff] " format-string) args)))

(defun magit-live-diff--normalize-path (path)
  "Normalize PATH by expanding and removing trailing slashes."
  (when path
    (directory-file-name (expand-file-name path))))

;; ---- Core Functions

(defun magit-live-diff--get-repo-root-uncached ()
  "Get repo root without caching (calls git)."
  (when (and (featurep 'magit) (fboundp 'magit-toplevel))
    (magit-live-diff--normalize-path (magit-toplevel))))

(defun magit-live-diff--get-repo-root ()
  "Get the cached repo root for current buffer."
  (or magit-live-diff--buffer-repo
      (setq magit-live-diff--buffer-repo
            (magit-live-diff--get-repo-root-uncached))))

(defun magit-live-diff--find-status-buffer (repo)
  "Find the magit-status buffer for REPO (fast, no git calls)."
  (let ((normalized-repo (magit-live-diff--normalize-path repo))
        (result nil))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf) (not result))
        (with-current-buffer buf
          (when (and (eq major-mode 'magit-status-mode)
                     magit-live-diff--buffer-repo
                     (string= magit-live-diff--buffer-repo normalized-repo))
            (setq result buf)))))
    ;; Fallback: search by buffer name pattern
    (unless result
      (let ((name (format "magit: %s" (file-name-nondirectory normalized-repo))))
        (setq result (get-buffer name))))
    result))

(defun magit-live-diff--refresh ()
  "Refresh Magit status buffer for pending repos."
  (setq magit-live-diff--timer nil)
  (let ((repos magit-live-diff--pending-repos))
    (setq magit-live-diff--pending-repos nil)
    (when repos
      (magit-live-diff--log "Refreshing %d repo(s)" (length repos))
      ;; Set flag to ignore file-notify events during refresh
      (setq magit-live-diff--refreshing t)
      (unwind-protect
          (dolist (repo repos)
            (let ((buf (magit-live-diff--find-status-buffer repo)))
              (if (not buf)
                  (magit-live-diff--log "  No status buffer for %s" repo)
                (magit-live-diff--log "  Refreshing %s" (buffer-name buf))
                (with-current-buffer buf
                  (when (fboundp 'magit-refresh)
                    ;; Use run-with-idle-timer to not block
                    (run-with-idle-timer
                     0.01 nil
                     (lambda (b)
                       (when (buffer-live-p b)
                         (with-current-buffer b
                           (magit-refresh))))
                     buf))))))
        ;; Clear flag after a short delay (let git operations finish)
        (run-with-timer 1.0 nil (lambda () (setq magit-live-diff--refreshing nil)))))))

(defun magit-live-diff--schedule-refresh (repo)
  "Schedule a debounced refresh for REPO."
  (let ((normalized-repo (magit-live-diff--normalize-path repo)))
    (unless (member normalized-repo magit-live-diff--pending-repos)
      (push normalized-repo magit-live-diff--pending-repos))
    (when magit-live-diff--timer
      (cancel-timer magit-live-diff--timer))
    (setq magit-live-diff--timer
          (run-with-timer magit-live-diff-debounce-delay nil
                          #'magit-live-diff--refresh))
    (magit-live-diff--log "Scheduled refresh for %s" normalized-repo)))

(defun magit-live-diff--handler (event)
  "Handle file change EVENT from file-notify."
  ;; Ignore events during our own refresh
  (unless magit-live-diff--refreshing
    (let ((action (nth 1 event))
          (file (nth 2 event)))
      (unless (or (eq action 'stopped)
                  (and file (string-match-p "\\(?:#\\|~\\|\\.swp\\)$" file))
                  (and file (string-match-p "index\\.lock$" file)))
        (magit-live-diff--log "File changed: %s" file)
        (when file
          (let ((default-directory (file-name-directory file)))
            (when-let ((repo (magit-live-diff--get-repo-root-uncached)))
              (magit-live-diff--schedule-refresh repo))))))))

(defun magit-live-diff--start-watching (repo)
  "Set up file watcher for REPO's .git directory only."
  (when repo
    (let* ((normalized-repo (magit-live-diff--normalize-path repo))
           (git-dir (expand-file-name ".git" normalized-repo)))
      (when (and (file-directory-p git-dir)
                 (not (assoc git-dir magit-live-diff--watches)))
        (condition-case err
            (let ((watch (file-notify-add-watch
                          git-dir
                          '(change)
                          #'magit-live-diff--handler)))
              (push (cons git-dir watch) magit-live-diff--watches)
              (magit-live-diff--log "Watching: %s" git-dir))
          (error
           (magit-live-diff--log "Failed to watch %s: %s" git-dir err)))))))

(defun magit-live-diff--stop-watching ()
  "Stop all file watchers."
  (dolist (entry magit-live-diff--watches)
    (ignore-errors (file-notify-rm-watch (cdr entry))))
  (setq magit-live-diff--watches nil)
  (magit-live-diff--log "Stopped all watchers"))

(defun magit-live-diff--after-save-handler ()
  "Handler for `after-save-hook'."
  (when (and magit-live-diff-mode
             (not magit-live-diff--refreshing)
             (featurep 'magit))
    (when-let ((repo (magit-live-diff--get-repo-root)))
      (magit-live-diff--log "After-save: %s" (buffer-file-name))
      (magit-live-diff--schedule-refresh repo))))

(defun magit-live-diff--magit-status-hook ()
  "Hook for magit-status-mode - cache repo and start watching."
  (setq magit-live-diff--buffer-repo (magit-live-diff--get-repo-root-uncached))
  (when (and magit-live-diff-mode magit-live-diff--buffer-repo)
    (magit-live-diff--start-watching magit-live-diff--buffer-repo)))

;; ---- Minor Mode

(defun magit-live-diff--enable ()
  "Enable magit-live-diff-mode."
  (require 'magit nil t)
  ;; Setup watchers for existing magit-status buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'magit-status-mode)
        (setq magit-live-diff--buffer-repo (magit-live-diff--get-repo-root-uncached))
        (when magit-live-diff--buffer-repo
          (magit-live-diff--start-watching magit-live-diff--buffer-repo)))))
  ;; Add hooks
  (add-hook 'magit-status-mode-hook #'magit-live-diff--magit-status-hook)
  (when magit-live-diff-use-after-save-hook
    (add-hook 'after-save-hook #'magit-live-diff--after-save-handler))
  (message "Magit live diff enabled (watching %d repos)" (length magit-live-diff--watches)))

(defun magit-live-diff--disable ()
  "Disable magit-live-diff-mode."
  (magit-live-diff--stop-watching)
  (when magit-live-diff--timer
    (cancel-timer magit-live-diff--timer)
    (setq magit-live-diff--timer nil))
  (setq magit-live-diff--pending-repos nil)
  (setq magit-live-diff--refreshing nil)
  (remove-hook 'magit-status-mode-hook #'magit-live-diff--magit-status-hook)
  (remove-hook 'after-save-hook #'magit-live-diff--after-save-handler)
  (message "Magit live diff disabled"))

;;;###autoload
(define-minor-mode magit-live-diff-mode
  "Toggle live diff reload for Magit."
  :global t
  :lighter " MLive"
  :group 'magit-live-diff
  (if magit-live-diff-mode
      (magit-live-diff--enable)
    (magit-live-diff--disable)))

(provide 'magit-live-diff)
;;; magit-live-diff.el ends here
