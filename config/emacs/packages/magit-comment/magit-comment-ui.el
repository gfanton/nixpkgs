;;; magit-comment-ui.el --- Magit UI integration for comments -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit "3.0"))
;; Keywords: git, tools, review
;; URL: https://github.com/gfanton/nixpkgs

;;; Commentary:

;; Magit section integration for displaying comments.
;;
;; This module provides:
;; - Section rendering for comments in magit-status and magit-revision buffers
;; - Keymaps for interacting with comment sections
;; - Faces for comment display
;; - Integration with magit-dispatch transient

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'magit-section)
(require 'magit-comment-core)

;; Autoload db functions
(autoload 'magit-comment-db-get-unresolved "magit-comment-db")
(autoload 'magit-comment-db-get-for-commit "magit-comment-db")
(autoload 'magit-comment-staged-db-load-valid "magit-comment-db")

;; Autoload commands from magit-comment
(autoload 'magit-comment-add "magit-comment" nil t)
(autoload 'magit-comment-edit "magit-comment" nil t)
(autoload 'magit-comment-delete "magit-comment" nil t)
(autoload 'magit-comment-toggle-resolved "magit-comment" nil t)
(autoload 'magit-comment-export-staged "magit-comment" nil t)
(autoload 'magit-comment-export-all "magit-comment" nil t)
(autoload 'magit-comment-staged-add "magit-comment" nil t)
(autoload 'magit-comment-staged-edit "magit-comment" nil t)
(autoload 'magit-comment-staged-delete "magit-comment" nil t)
(autoload 'magit-comment-staged-clear "magit-comment" nil t)
(autoload 'magit-comment-file-add "magit-comment-file" nil t)

;; Variable declaration for magit-comment-mode (defined in magit-comment.el)
(defvar magit-comment-mode)

;; Silence native-comp warnings for magit functions
(declare-function magit-add-section-hook "magit-section" (hook function &optional at append local))
(declare-function magit-comment-transient "magit-comment-transient" ())

;; ---- Faces

(defface magit-comment-heading
  '((t :inherit magit-section-heading))
  "Face for comment section headings."
  :group 'magit-comment)

(defface magit-comment-body
  '((t :inherit default))
  "Face for comment body text."
  :group 'magit-comment)

(defface magit-comment-metadata
  '((t :inherit magit-dimmed))
  "Face for comment metadata."
  :group 'magit-comment)

(defface magit-comment-resolved
  '((t :inherit magit-dimmed :strike-through t))
  "Face for resolved comments."
  :group 'magit-comment)

(defface magit-comment-count
  '((t :inherit warning :weight bold))
  "Face for comment count indicators."
  :group 'magit-comment)

;; ---- Keymaps

(defvar magit-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'magit-comment-edit)
    (define-key map (kbd "k") #'magit-comment-delete)
    (define-key map (kbd "r") #'magit-comment-toggle-resolved)
    map)
  "Keymap for individual comment sections.")

(defvar magit-comment-comments-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'magit-comment-add)
    (define-key map (kbd "x") #'magit-comment-export-all)
    map)
  "Keymap for the comments list section.")

(defvar magit-comment-staged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'magit-comment-staged-edit)
    (define-key map (kbd "k") #'magit-comment-staged-delete)
    map)
  "Keymap for individual staged comment sections.")

(defvar magit-comment-staged-comments-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'magit-comment-staged-add)
    (define-key map (kbd "x") #'magit-comment-export-staged)
    (define-key map (kbd "S") #'magit-comment-staged-clear)
    map)
  "Keymap for the staged comments list section.")

;; ---- Section Insertion Functions

(defun magit-comment-ui--format-location (comment)
  "Format the location string for COMMENT."
  (let ((file (magit-comment-entry-file comment))
        (line (magit-comment-entry-line comment))
        (line-end (magit-comment-entry-line-end comment)))
    (cond
     ((and file line line-end) (format "%s:%d-%d" file line line-end))
     ((and file line) (format "%s:%d" file line))
     (file file)
     (t "(commit-level)"))))

(defun magit-comment-ui--insert-comment (comment)
  "Insert a section for COMMENT."
  (let ((resolved (magit-comment-entry-resolved comment)))
    (magit-insert-section (magit-comment comment)
      (magit-insert-heading
        (propertize
         (format "%s %s"
                 (if resolved "[x]" "[ ]")
                 (magit-comment-ui--format-location comment))
         'font-lock-face (if resolved
                             'magit-comment-resolved
                           'magit-comment-heading)))
      (magit-insert-section-body
        (insert (propertize
                 (format "  %s\n" (magit-comment-entry-body comment))
                 'font-lock-face 'magit-comment-body))
        (insert (propertize
                 (format "  -- %s, %s\n"
                         (magit-comment-entry-author comment)
                         (magit-comment-entry-created-at comment))
                 'font-lock-face 'magit-comment-metadata))))))

(defun magit-comment-ui--insert-comments-section ()
  "Insert the comments section in magit-status."
  (when magit-comment-mode
    (when-let* ((comments (magit-comment-db-get-unresolved)))
      (magit-insert-section (magit-comment-comments)
        (magit-insert-heading
          (format "%s (%d unresolved)"
                  (propertize "Comments" 'font-lock-face 'magit-section-heading)
                  (length comments)))
        (magit-insert-section-body
          (dolist (comment comments)
            (magit-comment-ui--insert-comment comment))
          (insert "\n"))))))

(defun magit-comment-ui--insert-commit-comments ()
  "Insert comments section in commit/revision buffer."
  (when magit-comment-mode
    ;; Get commit from revision buffer or section at point
    (when-let* ((commit (or (and (boundp 'magit-buffer-revision)
                                 magit-buffer-revision)
                            (magit-commit-at-point)))
                (comments (magit-comment-db-get-for-commit commit)))
      (magit-insert-section (magit-comment-comments)
        (magit-insert-heading
          (format "%s (%d)"
                  (propertize "Comments" 'font-lock-face 'magit-section-heading)
                  (length comments)))
        (magit-insert-section-body
          (dolist (comment comments)
            (magit-comment-ui--insert-comment comment))
          (insert "\n"))))))

;; ---- Staged Comments Section

(defun magit-comment-ui--format-staged-location (comment)
  "Format the location string for staged COMMENT."
  (let ((file (magit-comment-staged-entry-file comment))
        (line (magit-comment-staged-entry-line comment))
        (line-end (magit-comment-staged-entry-line-end comment)))
    (cond
     ((and file line line-end) (format "%s:%d-%d" file line line-end))
     ((and file line) (format "%s:%d" file line))
     (file file)
     (t "(unknown)"))))

(defun magit-comment-ui--insert-staged-comment (comment)
  "Insert a section for staged COMMENT."
  (magit-insert-section (magit-comment-staged comment)
    (magit-insert-heading
      (propertize
       (format "[ ] %s" (magit-comment-ui--format-staged-location comment))
       'font-lock-face 'magit-comment-heading))
    (magit-insert-section-body
      (insert (propertize
               (format "  %s\n" (magit-comment-staged-entry-body comment))
               'font-lock-face 'magit-comment-body))
      (insert (propertize
               (format "  -- %s, %s\n"
                       (magit-comment-staged-entry-author comment)
                       (magit-comment-staged-entry-created-at comment))
               'font-lock-face 'magit-comment-metadata)))))

(defun magit-comment-ui--insert-staged-comments-section ()
  "Insert staged comments section after staged changes."
  (when magit-comment-mode
    (when-let* ((comments (magit-comment-staged-db-load-valid)))
      (magit-insert-section (magit-comment-staged-comments)
        (magit-insert-heading
          (format "%s (%d)"
                  (propertize "Staged Comments" 'font-lock-face 'magit-section-heading)
                  (length comments)))
        (magit-insert-section-body
          (dolist (comment comments)
            (magit-comment-ui--insert-staged-comment comment))
          (insert "\n"))))))

;; ---- Setup/Teardown

(defun magit-comment-ui-setup ()
  "Set up Magit UI integration for comments."
  ;; Add comments section to status buffer (after recent commits, position 90)
  (add-hook 'magit-status-sections-hook
            #'magit-comment-ui--insert-comments-section
            90)
  ;; Add to revision sections
  (add-hook 'magit-revision-sections-hook
            #'magit-comment-ui--insert-commit-comments
            90)
  ;; Add staged comments section AFTER staged changes using magit-add-section-hook
  ;; The t at the end means append (insert after the reference section)
  (with-eval-after-load 'magit
    (magit-add-section-hook 'magit-status-sections-hook
                            #'magit-comment-ui--insert-staged-comments-section
                            'magit-insert-staged-changes
                            t))
  ;; Add transient suffix to magit-dispatch
  ;; Using "'" since "#" conflicts with evil-search-word-backward
  (with-eval-after-load 'magit
    (require 'magit-comment-transient)
    ;; Check if suffix already exists before adding
    (unless (condition-case nil
                (transient-get-suffix 'magit-dispatch "#")
              (error nil))
      (transient-append-suffix 'magit-dispatch "!"
        '("#" "Comments" magit-comment-transient)))
    ;; Bind # to open transient in magit buffers
    (define-key magit-status-mode-map (kbd "#") #'magit-comment-transient)
    (define-key magit-revision-mode-map (kbd "#") #'magit-comment-transient))
  (magit-comment--log "UI hooks installed"))

(defun magit-comment-ui-teardown ()
  "Remove Magit UI integration for comments."
  (remove-hook 'magit-status-sections-hook
               #'magit-comment-ui--insert-comments-section)
  (remove-hook 'magit-status-sections-hook
               #'magit-comment-ui--insert-staged-comments-section)
  (remove-hook 'magit-revision-sections-hook
               #'magit-comment-ui--insert-commit-comments)
  ;; Remove transient suffix from magit-dispatch
  (condition-case nil
      (transient-remove-suffix 'magit-dispatch "#")
    (error nil))
  ;; Remove direct keybindings (only if magit was loaded)
  (when (featurep 'magit)
    (define-key magit-status-mode-map (kbd "#") nil)
    (define-key magit-revision-mode-map (kbd "#") nil))
  (magit-comment--log "UI hooks removed"))

(provide 'magit-comment-ui)
;;; magit-comment-ui.el ends here
