;;; magit-live-diff.el --- Live diff reload for Magit -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (magit "3.0"))
;; Keywords: git, tools
;; URL: https://github.com/gfanton/nixpkgs

;;; Commentary:

;; Live diff reload for Magit - auto-refresh on file changes.
;; Watches directories containing git-tracked files for external changes.
;; Ported from magit-filenotify (https://github.com/ruediger/magit-filenotify).
;;
;; M-x magit-live-diff-mode to toggle

;;; Code:

(require 'filenotify)
(require 'subr-x)
(require 'cl-lib)
(require 'seq)

;; ---- Magit function declarations

(declare-function magit-toplevel "magit-git" ())
(declare-function magit-refresh "magit-mode" ())
(declare-function magit-git-lines "magit-git" (&rest args))
(declare-function magit-decode-git-path "magit-git" (path))

;; Silence byte-compiler for external variable
(defvar magit-pre-refresh-hook)

;; ---- Customization

(defgroup magit-live-diff nil
  "Live diff reload for Magit."
  :group 'magit
  :prefix "magit-live-diff-")

(defcustom magit-live-diff-idle-delay 1.57
  "Seconds of idle time before refreshing during burst events.
When receiving many file change events in quick succession (e.g., during
compilation), wait until Emacs has been idle for this many seconds before
refreshing."
  :type 'number
  :group 'magit-live-diff)

(defcustom magit-live-diff-instant-refresh-time 1.73
  "Threshold for instant refresh vs batched refresh.
If the last file change event was longer ago than this many seconds,
refresh immediately. Otherwise, batch the refresh using `magit-live-diff-idle-delay'."
  :type 'number
  :group 'magit-live-diff)

(defcustom magit-live-diff-ignored '("\\`\\.#" "\\`flycheck_")
  "List of regexps for filenames to ignore in file-notify callbacks.
Default ignores Emacs lock files and flycheck temp files."
  :type '(repeat regexp)
  :group 'magit-live-diff)

(defcustom magit-live-diff-use-after-save-hook t
  "Whether to use `after-save-hook' for internal change detection.
This provides faster refresh for files saved within Emacs."
  :type 'boolean
  :group 'magit-live-diff)

(defcustom magit-live-diff-debug nil
  "If non-nil, print debug messages to *Messages*."
  :type 'boolean
  :group 'magit-live-diff)

;; ---- Internal Variables

(defvar magit-live-diff--watches (make-hash-table :test #'equal)
  "Hash table mapping watch-descriptors to (directory . buffer).
Uses #'equal test because inotify descriptors can be lists like (17).")

(defvar magit-live-diff--last-event-times (make-hash-table :test #'eq)
  "Hash table mapping buffers to their last event time.")

(defvar magit-live-diff--idle-timer nil
  "Idle timer for batched refresh during burst events.")

(defvar magit-live-diff--refresh-timer nil
  "Timer to clear the refreshing flag after refresh.")

(defvar magit-live-diff--pending-buffers nil
  "List of buffers pending refresh.")

(defvar magit-live-diff--refreshing nil
  "Non-nil when currently refreshing (to ignore self-triggered events).")

(defvar-local magit-live-diff--buffer-repo nil
  "Cached repo root for this buffer.")

;; ---- Helper Functions

(defun magit-live-diff--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS if debug enabled."
  (when magit-live-diff-debug
    (apply #'message (concat "[magit-live-diff] " format-string) args)))

(defun magit-live-diff--normalize-path (path)
  "Normalize PATH by expanding and removing trailing slashes."
  (when path
    (directory-file-name (expand-file-name path))))

(defun magit-live-diff--ignored-p (file)
  "Return non-nil if FILE should be ignored based on `magit-live-diff-ignored'."
  (when file
    (let ((name (file-name-nondirectory file)))
      (cl-some (lambda (re) (string-match-p re name))
               magit-live-diff-ignored))))

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

(defun magit-live-diff--tracked-directories (repo)
  "List all directories containing git-tracked files in REPO.
Returns a list including REPO itself plus all subdirectories with tracked files."
  (let ((default-directory repo))
    (cons repo
          (seq-uniq
           (cl-loop for file in (magit-git-lines "ls-files")
                    for decoded = (magit-decode-git-path file)
                    for dir = (file-name-directory decoded)
                    when dir
                    collect (magit-live-diff--normalize-path
                             (expand-file-name dir repo)))
           #'string=))))

(defun magit-live-diff--directory-watched-p (dir)
  "Return non-nil if DIR is already being watched."
  (catch 'found
    (maphash (lambda (_wd data)
               (when (string= (car data) dir)
                 (throw 'found t)))
             magit-live-diff--watches)
    nil))

(defun magit-live-diff--find-status-buffer (repo)
  "Find the magit-status buffer for REPO."
  (let ((normalized-repo (magit-live-diff--normalize-path repo)))
    (cl-loop for buf in (buffer-list)
             when (and (buffer-live-p buf)
                       (with-current-buffer buf
                         (and (eq major-mode 'magit-status-mode)
                              magit-live-diff--buffer-repo
                              (string= magit-live-diff--buffer-repo normalized-repo))))
             return buf)))

;; ---- Refresh Functions

(defun magit-live-diff--refresh-buffer (buffer)
  "Refresh the magit status BUFFER."
  (when (buffer-live-p buffer)
    (magit-live-diff--log "Refreshing buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      ;; Disable magit-pre-refresh-hook to avoid prompting user to save buffers
      ;; (workaround for Emacs bug#21311 / Magit issue#2198)
      (let ((magit-pre-refresh-hook nil))
        (magit-refresh)))))

(defun magit-live-diff--clear-refreshing-flag ()
  "Clear the refreshing flag and timer."
  (setq magit-live-diff--refreshing nil
        magit-live-diff--refresh-timer nil))

(defun magit-live-diff--schedule-clear-refreshing ()
  "Schedule clearing the refreshing flag after a delay."
  (when magit-live-diff--refresh-timer
    (cancel-timer magit-live-diff--refresh-timer))
  (setq magit-live-diff--refresh-timer
        (run-with-timer 1.0 nil #'magit-live-diff--clear-refreshing-flag)))

(defun magit-live-diff--refresh-pending ()
  "Refresh all pending buffers."
  (setq magit-live-diff--idle-timer nil)
  (let ((buffers magit-live-diff--pending-buffers))
    (setq magit-live-diff--pending-buffers nil)
    (when buffers
      (magit-live-diff--log "Refreshing %d pending buffer(s)" (length buffers))
      (setq magit-live-diff--refreshing t)
      (unwind-protect
          (dolist (buf buffers)
            (magit-live-diff--refresh-buffer buf))
        (magit-live-diff--schedule-clear-refreshing)))))

(defun magit-live-diff--register-pending (buffer)
  "Register BUFFER as pending refresh and reset idle timer."
  (cl-pushnew buffer magit-live-diff--pending-buffers)
  (when magit-live-diff--idle-timer
    (cancel-timer magit-live-diff--idle-timer))
  (setq magit-live-diff--idle-timer
        (run-with-idle-timer magit-live-diff-idle-delay nil
                             #'magit-live-diff--refresh-pending)))

(defun magit-live-diff--schedule-refresh (buffer)
  "Schedule refresh for BUFFER using dual-path debouncing.
Refresh immediately if last event was long ago, otherwise batch."
  (let* ((now (current-time))
         (last-time (gethash buffer magit-live-diff--last-event-times))
         (elapsed (when last-time
                    (time-to-seconds (time-subtract now last-time)))))
    (puthash buffer now magit-live-diff--last-event-times)
    (if (or (null elapsed)
            (> elapsed magit-live-diff-instant-refresh-time))
        ;; Fast path: refresh immediately
        (progn
          (magit-live-diff--log "Instant refresh (elapsed: %s)" elapsed)
          (setq magit-live-diff--refreshing t)
          (unwind-protect
              (magit-live-diff--refresh-buffer buffer)
            (magit-live-diff--schedule-clear-refreshing)))
      ;; Slow path: batch refresh
      (magit-live-diff--log "Batching refresh (elapsed: %s)" elapsed)
      (magit-live-diff--register-pending buffer))))

;; ---- File Watcher Functions

(defun magit-live-diff--handler (event)
  "Handle file change EVENT from file-notify."
  (unless magit-live-diff--refreshing
    (let ((action (nth 1 event))
          (file (nth 2 event))
          (wd (car event)))
      (unless (or (eq action 'stopped)
                  (magit-live-diff--ignored-p file)
                  (and file (string-match-p "index\\.lock$" file)))
        (magit-live-diff--log "File changed: %s (action: %s)" file action)
        (when-let* ((watch-data (gethash wd magit-live-diff--watches))
                    (buffer (cdr watch-data)))
          (when (buffer-live-p buffer)
            (magit-live-diff--schedule-refresh buffer)))))))

(defun magit-live-diff--start-watching (buffer)
  "Set up file watchers for the repo associated with BUFFER.
Watches all directories containing git-tracked files."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((repo magit-live-diff--buffer-repo))
        (let ((dirs (magit-live-diff--tracked-directories repo))
              (watch-count 0))
          ;; Also watch .git directory for index changes
          (let ((git-dir (expand-file-name ".git" repo)))
            (when (file-directory-p git-dir)
              (push git-dir dirs)))
          (dolist (dir dirs)
            (when (and (file-directory-p dir)
                       (not (magit-live-diff--directory-watched-p dir)))
              (condition-case-unless-debug err
                  (let ((watch (file-notify-add-watch
                                dir
                                '(change attribute-change)
                                #'magit-live-diff--handler)))
                    (puthash watch (cons dir buffer) magit-live-diff--watches)
                    (cl-incf watch-count))
                (error
                 (magit-live-diff--log "Failed to watch %s: %s" dir err)))))
          (magit-live-diff--log "Watching %d directories for %s"
                                watch-count (buffer-name buffer)))))))

(defun magit-live-diff--stop-watching-buffer (buffer)
  "Stop all file watchers associated with BUFFER."
  (let ((to-remove nil))
    (maphash (lambda (wd data)
               (when (eq (cdr data) buffer)
                 (push wd to-remove)))
             magit-live-diff--watches)
    (dolist (wd to-remove)
      (ignore-errors (file-notify-rm-watch wd))
      (remhash wd magit-live-diff--watches))
    (when to-remove
      (magit-live-diff--log "Stopped %d watchers for %s"
                            (length to-remove) (buffer-name buffer)))))

(defun magit-live-diff--stop-all-watchers ()
  "Stop all file watchers."
  (maphash (lambda (wd _data)
             (ignore-errors (file-notify-rm-watch wd)))
           magit-live-diff--watches)
  (clrhash magit-live-diff--watches)
  (magit-live-diff--log "Stopped all watchers"))

;; ---- Hooks

(defun magit-live-diff--after-save-handler ()
  "Handler for `after-save-hook'."
  (when (and magit-live-diff-mode
             (not magit-live-diff--refreshing)
             (featurep 'magit))
    (when-let* ((repo (magit-live-diff--get-repo-root))
                (buffer (magit-live-diff--find-status-buffer repo)))
      (magit-live-diff--log "After-save: %s" (buffer-file-name))
      (magit-live-diff--schedule-refresh buffer))))

(defun magit-live-diff--magit-status-hook ()
  "Hook for magit-status-mode - cache repo and start watching."
  (setq magit-live-diff--buffer-repo (magit-live-diff--get-repo-root-uncached))
  (when (and magit-live-diff-mode magit-live-diff--buffer-repo)
    (magit-live-diff--start-watching (current-buffer)))
  ;; Clean up watchers when buffer is killed
  (add-hook 'kill-buffer-hook #'magit-live-diff--kill-buffer-hook nil t))

(defun magit-live-diff--kill-buffer-hook ()
  "Hook for kill-buffer - stop watchers for this buffer."
  (magit-live-diff--stop-watching-buffer (current-buffer)))

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
          (magit-live-diff--start-watching buf)
          (add-hook 'kill-buffer-hook #'magit-live-diff--kill-buffer-hook nil t)))))
  ;; Add hooks
  (add-hook 'magit-status-mode-hook #'magit-live-diff--magit-status-hook)
  (when magit-live-diff-use-after-save-hook
    (add-hook 'after-save-hook #'magit-live-diff--after-save-handler))
  (message "Magit live diff enabled (watching %d directories)"
           (hash-table-count magit-live-diff--watches)))

(defun magit-live-diff--disable ()
  "Disable magit-live-diff-mode."
  (magit-live-diff--stop-all-watchers)
  ;; Cancel timers
  (when magit-live-diff--idle-timer
    (cancel-timer magit-live-diff--idle-timer)
    (setq magit-live-diff--idle-timer nil))
  (when magit-live-diff--refresh-timer
    (cancel-timer magit-live-diff--refresh-timer)
    (setq magit-live-diff--refresh-timer nil))
  ;; Clear state
  (setq magit-live-diff--pending-buffers nil)
  (setq magit-live-diff--refreshing nil)
  (clrhash magit-live-diff--last-event-times)
  ;; Remove hooks
  (remove-hook 'magit-status-mode-hook #'magit-live-diff--magit-status-hook)
  (remove-hook 'after-save-hook #'magit-live-diff--after-save-handler)
  (message "Magit live diff disabled"))

;;;###autoload
(define-minor-mode magit-live-diff-mode
  "Toggle live diff reload for Magit.
When enabled, automatically refreshes magit-status buffers when
files in the repository change, including external changes."
  :global t
  :lighter " MLive"
  :group 'magit-live-diff
  (if magit-live-diff-mode
      (magit-live-diff--enable)
    (magit-live-diff--disable)))

(provide 'magit-live-diff)
;;; magit-live-diff.el ends here
