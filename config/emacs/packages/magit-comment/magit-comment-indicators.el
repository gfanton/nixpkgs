;;; magit-comment-indicators.el --- EOL indicators for comments -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit "3.0"))
;; Keywords: git, tools, review
;; URL: https://github.com/gfanton/nixpkgs

;;; Commentary:

;; Visual end-of-line indicators showing where comments exist in file buffers.
;; Provides:
;; - EOL markers (◆) that don't conflict with git-gutter/diff-hl
;; - Live preview popup when cursor is on a commented line
;; - Automatic updates when comments change
;;
;; Enable with `magit-comment-indicators-mode' in any file buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit-comment-core)

;; Optional popon for terminal popups (soft dependency)
(require 'popon nil t)
(declare-function popon-create "popon")
(declare-function popon-kill "popon")
(declare-function popon-x-y-at-pos "popon")
(declare-function popon-live-p "popon")

;; Silence byte-compiler
(declare-function magit-comment-db-load "magit-comment-db" ())
(declare-function magit-comment-staged-db-load-valid "magit-comment-db" ())
(declare-function magit-git-string "magit-git" (&rest args))
(declare-function pos-tip-show "pos-tip" (string &optional tip-color pos window timeout dx dy))

;; ---- Customization

(defgroup magit-comment-indicators nil
  "EOL indicators for magit-comment."
  :group 'magit-comment
  :prefix "magit-comment-indicators-")

(defcustom magit-comment-indicators-preview-delay 0.3
  "Delay in seconds before showing comment preview popup."
  :type 'number
  :group 'magit-comment-indicators)

(defcustom magit-comment-indicators-marker "◆"
  "Character to display at end of line for commented lines.
Common Unicode choices: ◆ ● ◉ ▶ ■ ✦ ★
For terminals with limited Unicode: * > #"
  :type 'string
  :group 'magit-comment-indicators)

(defcustom magit-comment-indicators-overlay-priority 100
  "Priority for indicator overlays.
Higher values display on top of lower priority overlays."
  :type 'integer
  :group 'magit-comment-indicators)

(defcustom magit-comment-indicators-use-popon t
  "Use popon for terminal popups when available.
When nil or popon unavailable, falls back to minibuffer message."
  :type 'boolean
  :group 'magit-comment-indicators)

;; ---- Faces

(defface magit-comment-indicator
  '((((class color) (background light))
     :foreground "#D75F00")
    (((class color) (background dark))
     :foreground "#FF8700"))
  "Face for commit comment indicators (EOL marker)."
  :group 'magit-comment-indicators)

(defface magit-comment-indicator-staged
  '((((class color) (background light))
     :foreground "#AF5F00")
    (((class color) (background dark))
     :foreground "#FFAF00"))
  "Face for staged comment indicators (EOL marker)."
  :group 'magit-comment-indicators)

(defface magit-comment-preview
  '((t :inherit tooltip))
  "Face for comment preview popup."
  :group 'magit-comment-indicators)

(defface magit-comment-preview-header
  '((t :inherit (bold tooltip)))
  "Face for comment preview header (author/timestamp)."
  :group 'magit-comment-indicators)

;; ---- Buffer-local Variables

(defvar-local magit-comment-indicators--overlays nil
  "List of overlay objects for comment indicators.")

(defvar-local magit-comment-indicators--preview-overlay nil
  "Overlay for the currently displayed preview.")

(defvar-local magit-comment-indicators--comments-cache nil
  "Alist of ((LINE . LINE-END) . (TYPE . COMMENT-ENTRY)) for current buffer.
LINE-END is nil for single-line comments.  TYPE is `commit' or `staged'.")

(defvar-local magit-comment-indicators--preview-timer nil
  "Timer for debounced preview display.")

(defvar-local magit-comment-indicators--popon nil
  "Current popon object for comment preview.")

(defvar-local magit-comment-indicators--file nil
  "Relative file path for current buffer (cached).")

(defvar-local magit-comment-indicators--repo-root nil
  "Repository root for current buffer (cached).")

;; Forward declaration for mode variable (defined later)
(defvar magit-comment-indicators-mode)

;; ---- Blame Cache (for mapping commit lines to current lines)

(cl-defstruct (magit-comment-indicators--blame-cache
               (:constructor magit-comment-indicators--blame-cache-create)
               (:copier nil))
  "Cache entry for blame-to-current-line mapping."
  file        ; relative file path
  commit      ; commit SHA
  mtime       ; file modification time when cache was built
  mapping)    ; hash-table: original-line -> current-line

(defvar magit-comment-indicators--blame-caches nil
  "List of blame cache entries.")

(defconst magit-comment-indicators--blame-cache-max-size 10
  "Maximum number of blame cache entries to keep.")

;; ---- Blame Mapping

(defun magit-comment-indicators--get-file-mtime ()
  "Get modification time of current buffer's file."
  (when buffer-file-name
    (file-attribute-modification-time
     (file-attributes buffer-file-name))))

(defun magit-comment-indicators--find-blame-cache (file commit)
  "Find cached blame mapping for FILE and COMMIT."
  (let ((mtime (magit-comment-indicators--get-file-mtime)))
    (seq-find
     (lambda (cache)
       (and (string= (magit-comment-indicators--blame-cache-file cache) file)
            (string= (magit-comment-indicators--blame-cache-commit cache) commit)
            (equal (magit-comment-indicators--blame-cache-mtime cache) mtime)))
     magit-comment-indicators--blame-caches)))

(defun magit-comment-indicators--add-blame-cache (cache)
  "Add CACHE to blame caches, evicting old entries if needed."
  (push cache magit-comment-indicators--blame-caches)
  ;; Evict old entries
  (when (> (length magit-comment-indicators--blame-caches)
           magit-comment-indicators--blame-cache-max-size)
    (setq magit-comment-indicators--blame-caches
          (seq-take magit-comment-indicators--blame-caches
                    magit-comment-indicators--blame-cache-max-size))))

(defun magit-comment-indicators--build-blame-mapping (file commit repo-root)
  "Build a set of current lines that came from COMMIT in FILE.
Returns a hash-table where keys are current line numbers from COMMIT.
REPO-ROOT is the repository root directory."
  (let ((mapping (make-hash-table :test 'eq))
        (default-directory repo-root))
    ;; Use git blame to find which current lines came from commit
    ;; Format: SHA orig-line current-line ...
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil
                                 "blame" "-l" "-p" "--" file))
        (goto-char (point-min))
        ;; Parse porcelain output
        ;; Each line block starts with: SHA orig-line final-line [count]
        (while (re-search-forward
                (concat "^\\([0-9a-f]+\\) "
                        "\\([0-9]+\\) "   ; original line
                        "\\([0-9]+\\)")   ; final/current line
                nil t)
          (let ((sha (match-string 1))
                (current-line (string-to-number (match-string 3))))
            ;; Check if this line comes from our target commit
            ;; Store current-line as key (comment stores current line at creation)
            (when (string-prefix-p commit sha)
              (puthash current-line t mapping))))))
    mapping))

(defun magit-comment-indicators--map-commit-line (commit line file repo-root)
  "Check if LINE in FILE still comes from COMMIT.
Returns LINE if the line still comes from that commit, nil otherwise.
REPO-ROOT is the repository root."
  ;; Check cache first
  (if-let* ((cache (magit-comment-indicators--find-blame-cache file commit)))
      (when (gethash line (magit-comment-indicators--blame-cache-mapping cache))
        line)
    ;; Build and cache mapping
    (let* ((mapping (magit-comment-indicators--build-blame-mapping
                     file commit repo-root))
           (mtime (magit-comment-indicators--get-file-mtime))
           (cache (magit-comment-indicators--blame-cache-create
                   :file file
                   :commit commit
                   :mtime mtime
                   :mapping mapping)))
      (magit-comment-indicators--add-blame-cache cache)
      (when (gethash line mapping)
        line))))

;; ---- Comment Collection

(defun magit-comment-indicators--collect-comments ()
  "Collect all comments applicable to current buffer.
Returns an alist of ((LINE . LINE-END) . (TYPE . COMMENT)) where TYPE is
`commit' or `staged'.  LINE-END is nil for single-line comments."
  (let ((file magit-comment-indicators--file)
        (repo-root magit-comment-indicators--repo-root))
    (when (and file repo-root)
      (require 'magit-comment-db)
      (let ((result nil))
        ;; Collect staged comments (direct line mapping)
        (dolist (comment (magit-comment-staged-db-load-valid))
          (when (string= (magit-comment-staged-entry-file comment) file)
            (let ((line (magit-comment-staged-entry-line comment))
                  (line-end (magit-comment-staged-entry-line-end comment)))
              (push (cons (cons line line-end) (cons 'staged comment)) result))))
        ;; Collect commit comments (need blame mapping)
        (dolist (comment (magit-comment-db-load))
          (when (string= (magit-comment-entry-file comment) file)
            (let* ((commit (magit-comment-entry-commit comment))
                   (orig-line (magit-comment-entry-line comment))
                   (orig-line-end (magit-comment-entry-line-end comment))
                   (current-line (magit-comment-indicators--map-commit-line
                                  commit orig-line file repo-root))
                   (current-line-end (when orig-line-end
                                       (magit-comment-indicators--map-commit-line
                                        commit orig-line-end file repo-root))))
              (when current-line
                (push (cons (cons current-line current-line-end)
                            (cons 'commit comment))
                      result)))))
        ;; Sort by line number
        (sort result (lambda (a b) (< (caar a) (caar b))))))))

;; ---- Overlay Management

(defun magit-comment-indicators--remove-overlays ()
  "Remove all indicator overlays from current buffer."
  (mapc #'delete-overlay magit-comment-indicators--overlays)
  (setq magit-comment-indicators--overlays nil))

(defun magit-comment-indicators--add-overlay (line _line-end type)
  "Add an EOL indicator overlay at LINE for comment TYPE.
TYPE should be `commit' or `staged'.  _LINE-END is accepted but
the marker is always placed on LINE (first line of the range)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let* ((face (if (eq type 'staged)
                     'magit-comment-indicator-staged
                   'magit-comment-indicator))
           (marker (propertize (concat " " magit-comment-indicators-marker)
                               'face face))
           (eol (line-end-position))
           (ov (make-overlay eol eol)))
      (overlay-put ov 'magit-comment-indicator t)
      (overlay-put ov 'priority magit-comment-indicators-overlay-priority)
      (overlay-put ov 'before-string " ")
      (overlay-put ov 'after-string marker)
      (push ov magit-comment-indicators--overlays))))

(defun magit-comment-indicators--update ()
  "Update all indicator overlays for current buffer."
  (when (and buffer-file-name magit-comment-indicators-mode)
    ;; Initialize buffer-local cache if needed
    (unless magit-comment-indicators--repo-root
      (setq magit-comment-indicators--repo-root (magit-comment--repo-root)))
    (unless magit-comment-indicators--file
      (when magit-comment-indicators--repo-root
        (setq magit-comment-indicators--file
              (file-relative-name buffer-file-name
                                  magit-comment-indicators--repo-root))))
    ;; Only proceed if we're in a git repo
    (when (and magit-comment-indicators--repo-root
               magit-comment-indicators--file)
      ;; Remove old overlays
      (magit-comment-indicators--remove-overlays)
      ;; Collect and cache comments
      (setq magit-comment-indicators--comments-cache
            (magit-comment-indicators--collect-comments))
      ;; Add overlays for each comment
      (dolist (entry magit-comment-indicators--comments-cache)
        (let ((line (caar entry))
              (line-end (cdar entry))
              (type (cadr entry)))
          (magit-comment-indicators--add-overlay line line-end type))))))

;; ---- Preview Popup

(defun magit-comment-indicators--comment-at-line (line)
  "Return (TYPE . COMMENT) for comment covering LINE, or nil if none."
  (cl-find-if (lambda (entry)
                (let ((start (caar entry))
                      (end (or (cdar entry) (caar entry))))
                  (and (>= line start) (<= line end))))
              magit-comment-indicators--comments-cache))

(defun magit-comment-indicators--format-preview (comment type)
  "Format COMMENT for preview display.
TYPE is `commit' or `staged'."
  (let* ((author (if (eq type 'staged)
                     (magit-comment-staged-entry-author comment)
                   (magit-comment-entry-author comment)))
         (created (if (eq type 'staged)
                      (magit-comment-staged-entry-created-at comment)
                    (magit-comment-entry-created-at comment)))
         (body (if (eq type 'staged)
                   (magit-comment-staged-entry-body comment)
                 (magit-comment-entry-body comment)))
         (type-str (if (eq type 'staged) "[staged] " ""))
         (header (format "%s%s - %s" type-str author
                         (if (and created (> (length created) 10))
                             (substring created 0 10)
                           (or created "")))))
    (concat (propertize header 'face 'magit-comment-preview-header)
            "\n"
            (propertize body 'face 'magit-comment-preview))))

(defun magit-comment-indicators--hide-popon ()
  "Hide the popon if visible."
  (when (and (featurep 'popon)
             magit-comment-indicators--popon
             (popon-live-p magit-comment-indicators--popon))
    (popon-kill magit-comment-indicators--popon)
    (setq magit-comment-indicators--popon nil)))

(defun magit-comment-indicators--show-popon (text)
  "Show TEXT in a popon popup near point.
Creates a rectangular box with consistent width and background face."
  (when-let* ((_ (featurep 'popon))
              (pos (popon-x-y-at-pos (point))))
    (let* ((raw-lines (split-string text "\n"))
           (margin 1)  ; 1 space padding on each side
           (content-width (if raw-lines
                              (max 1 (apply #'max (mapcar #'string-width raw-lines)))
                            1))
           (box-width (+ content-width (* 2 margin)))
           ;; Build each line with padding and uniform width
           (formatted-lines
            (mapcar
             (lambda (line)
               (let* ((padding-right (- content-width (string-width line)))
                      (str (concat (make-string margin ?\s)
                                   line
                                   (make-string (+ padding-right margin) ?\s))))
                 (add-face-text-property 0 (length str)
                                         'magit-comment-preview t str)
                 str))
             raw-lines))
           (box-text (string-join formatted-lines "\n"))
           ;; Position calculation
           (max-x (max 0 (- (window-max-chars-per-line) box-width)))
           (x (max 0 (min (car pos) max-x)))
           (y (1+ (cdr pos)))
           (win-height (window-body-height)))
      ;; If not enough space below, try above
      (when (> (+ y (length raw-lines)) win-height)
        (setq y (max 0 (- (cdr pos) (length raw-lines)))))
      ;; If still too tall, clamp to top
      (when (> (+ y (length raw-lines)) win-height)
        (setq y 0))
      (setq magit-comment-indicators--popon
            (popon-create (cons box-text box-width) (cons x y)
                          (selected-window) (current-buffer))))))

(defun magit-comment-indicators--hide-preview ()
  "Hide the preview if visible."
  ;; Hide popon (terminal)
  (magit-comment-indicators--hide-popon)
  ;; Hide overlay (graphical)
  (when magit-comment-indicators--preview-overlay
    (delete-overlay magit-comment-indicators--preview-overlay)
    (setq magit-comment-indicators--preview-overlay nil)))

(defun magit-comment-indicators--show-preview (comment type)
  "Show preview popup for COMMENT of TYPE."
  (magit-comment-indicators--hide-preview)
  (let ((text (magit-comment-indicators--format-preview comment type)))
    (cond
     ;; GUI: pos-tip or tooltip
     ((and (display-graphic-p) (fboundp 'pos-tip-show))
      (pos-tip-show text))
     ((display-graphic-p)
      (tooltip-show text))
     ;; Terminal with popon available and enabled
     ((and magit-comment-indicators-use-popon
           (featurep 'popon))
      (magit-comment-indicators--show-popon text))
     ;; Fallback: minibuffer
     (t
      (message "%s" (replace-regexp-in-string "\n" " | " text))))))

(defun magit-comment-indicators--cancel-preview-timer ()
  "Cancel any pending preview timer."
  (when magit-comment-indicators--preview-timer
    (cancel-timer magit-comment-indicators--preview-timer)
    (setq magit-comment-indicators--preview-timer nil)))

(defun magit-comment-indicators--show-preview-safe (buffer line)
  "Show preview for comment at LINE if BUFFER is still current."
  (when (and (buffer-live-p buffer)
             (eq buffer (current-buffer))
             (= line (line-number-at-pos)))
    (when-let* ((entry (magit-comment-indicators--comment-at-line line)))
      (let ((type (cadr entry))
            (comment (cddr entry)))
        (magit-comment-indicators--show-preview comment type)))))

(defun magit-comment-indicators--maybe-show-preview ()
  "Schedule preview if cursor is on a commented line."
  (magit-comment-indicators--cancel-preview-timer)
  (magit-comment-indicators--hide-preview)
  (let ((line (line-number-at-pos)))
    (when (magit-comment-indicators--comment-at-line line)
      (setq magit-comment-indicators--preview-timer
            (run-with-idle-timer
             magit-comment-indicators-preview-delay nil
             #'magit-comment-indicators--show-preview-safe
             (current-buffer)
             line)))))

;; ---- Cleanup

(defun magit-comment-indicators--cleanup ()
  "Clean up all indicator resources."
  (magit-comment-indicators--cancel-preview-timer)
  (magit-comment-indicators--remove-overlays)
  (magit-comment-indicators--hide-popon)
  (magit-comment-indicators--hide-preview)
  (remove-hook 'post-command-hook #'magit-comment-indicators--maybe-show-preview t)
  (remove-hook 'kill-buffer-hook #'magit-comment-indicators--cleanup t)
  (remove-hook 'after-save-hook #'magit-comment-indicators--update t)
  (setq magit-comment-indicators--comments-cache nil))

;; ---- Minor Mode

;; Byte-compile declaration for file buffer command
(declare-function magit-comment-file-add "magit-comment-file" ())

(defvar magit-comment-indicators-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-;") #'magit-comment-indicators-add)
    map)
  "Keymap for `magit-comment-indicators-mode'.")

(defun magit-comment-indicators-add ()
  "Add a comment at current line.
Wrapper around `magit-comment-file-add'."
  (interactive)
  (require 'magit-comment-file)
  (call-interactively #'magit-comment-file-add))

;;;###autoload
(define-minor-mode magit-comment-indicators-mode
  "Show comment indicators at end of lines.
When enabled, displays visual markers (◆) at the end of lines that
have associated comments. Hovering on a commented line shows a
preview popup.

\\{magit-comment-indicators-mode-map}"
  :lighter " MCind"
  :keymap magit-comment-indicators-mode-map
  :group 'magit-comment-indicators
  (if magit-comment-indicators-mode
      (progn
        (magit-comment-indicators--update)
        (add-hook 'post-command-hook
                  #'magit-comment-indicators--maybe-show-preview nil t)
        (add-hook 'kill-buffer-hook
                  #'magit-comment-indicators--cleanup nil t)
        (add-hook 'after-save-hook
                  #'magit-comment-indicators--update nil t))
    (magit-comment-indicators--cleanup)))

;; ---- Global Mode

(defun magit-comment-indicators--turn-on ()
  "Turn on `magit-comment-indicators-mode' if appropriate.
Only enables in file buffers within git repositories."
  (when (and buffer-file-name
             (not (minibufferp))
             (not (derived-mode-p 'special-mode))
             (magit-comment--repo-root))
    (magit-comment-indicators-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-magit-comment-indicators-mode
  magit-comment-indicators-mode
  magit-comment-indicators--turn-on
  :group 'magit-comment-indicators)

;; ---- Global Refresh

(defun magit-comment-indicators--refresh-buffer (buffer)
  "Refresh indicators in BUFFER if mode is enabled."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when magit-comment-indicators-mode
        (magit-comment-indicators--update)))))

(defun magit-comment-indicators--refresh-all-buffers ()
  "Refresh indicators in all buffers with mode enabled."
  (dolist (buf (buffer-list))
    (magit-comment-indicators--refresh-buffer buf)))

;; ---- Hook for DB Changes

;; Hook is defined in magit-comment-db.el
(defvar magit-comment-db-after-save-hook)

;; Register to refresh on db changes when this module loads
(with-eval-after-load 'magit-comment-db
  (add-hook 'magit-comment-db-after-save-hook
            #'magit-comment-indicators--refresh-all-buffers))

(provide 'magit-comment-indicators)
;;; magit-comment-indicators.el ends here
