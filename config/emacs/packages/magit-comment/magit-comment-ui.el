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

;; DWIM commands
(autoload 'magit-comment-add-dwim "magit-comment" nil t)
(autoload 'magit-comment-edit-dwim "magit-comment" nil t)
(autoload 'magit-comment-delete-dwim "magit-comment" nil t)
(autoload 'magit-comment-toggle-resolved-dwim "magit-comment" nil t)

;; Forward declaration for byte-compiler (defined in magit-comment.el)
(defvar magit-comment-mode)

;; Storage for original keybindings to restore on teardown
(defvar magit-comment-ui--saved-bindings nil
  "Alist of saved keybindings for restoration on teardown.")

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

(defface magit-comment-staged-heading
  '((t :inherit (magit-section-heading warning)))
  "Face for staged comment headings.
Inherits from `warning' face for theme-compatible orange/yellow color."
  :group 'magit-comment)

;; ---- Keymaps

(defvar magit-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'magit-comment-edit-dwim)
    (define-key map (kbd "k") #'magit-comment-delete-dwim)
    (define-key map (kbd "r") #'magit-comment-toggle-resolved-dwim)
    map)
  "Keymap for individual comment sections (both commit and staged).
DWIM commands automatically detect the comment type at point.")

;; Alias for magit section naming convention (staged sections use same keymap)
(defvaralias 'magit-comment-staged-section-map 'magit-comment-section-map
  "Alias for `magit-comment-section-map'.
Staged sections use the same keymap since DWIM commands handle both types.")

(defvar magit-comment-comments-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'magit-comment-add-dwim)
    (define-key map (kbd "x") #'magit-comment-export-all)
    map)
  "Keymap for the unified comments section.")

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

(defun magit-comment-ui--insert-comment (comment &optional show-sha)
  "Insert a section for COMMENT.
When SHOW-SHA is non-nil, include short commit SHA prefix in heading."
  (let ((resolved (magit-comment-entry-resolved comment))
        (sha-prefix (when show-sha
                      (substring (magit-comment-entry-commit comment) 0 8))))
    (magit-insert-section (magit-comment comment)
      (magit-insert-heading
        (propertize
         (format "%s %s%s"
                 (if resolved "[x]" "[ ]")
                 (if sha-prefix (concat sha-prefix " ") "")
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

(defun magit-comment-ui--format-counts (staged commits)
  "Format count string for section heading.
STAGED is the list of staged comments, COMMITS is the list of commit comments."
  (let ((parts nil))
    (when staged
      (push (format "%d staged" (length staged)) parts))
    (when commits
      (push (format "%d unresolved" (length commits)) parts))
    (string-join (nreverse parts) ", ")))

(defun magit-comment-ui--insert-unified-comments-section ()
  "Insert unified comments section with staged at top."
  (when magit-comment-mode
    (let ((staged (magit-comment-staged-db-load-valid))
          (commits (magit-comment-db-get-unresolved)))
      (when (or staged commits)
        (magit-insert-section (magit-comment-comments)
          (magit-insert-heading
            (format "%s (%s)"
                    (propertize "Comments" 'font-lock-face 'magit-section-heading)
                    (magit-comment-ui--format-counts staged commits)))
          (magit-insert-section-body
            ;; Staged comments first (orange/yellow face)
            (dolist (comment staged)
              (magit-comment-ui--insert-staged-comment comment))
            ;; Then commit comments (with SHA prefix)
            (dolist (comment commits)
              (magit-comment-ui--insert-comment comment t))
            (insert "\n")))))))

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
  "Insert a section for staged COMMENT.
Uses `magit-comment-staged-heading' face for visual differentiation."
  (magit-insert-section (magit-comment-staged comment)
    (magit-insert-heading
      (propertize
       (format "[ ] %s" (magit-comment-ui--format-staged-location comment))
       'font-lock-face 'magit-comment-staged-heading))
    (magit-insert-section-body
      (insert (propertize
               (format "  %s\n" (magit-comment-staged-entry-body comment))
               'font-lock-face 'magit-comment-body))
      (insert (propertize
               (format "  -- %s, %s\n"
                       (magit-comment-staged-entry-author comment)
                       (magit-comment-staged-entry-created-at comment))
               'font-lock-face 'magit-comment-metadata)))))


;; ---- Setup/Teardown

(defun magit-comment-ui-setup ()
  "Set up Magit UI integration for comments."
  ;; Add unified comments section AFTER staged changes using magit-add-section-hook
  ;; The t at the end means append (insert after the reference section)
  (with-eval-after-load 'magit
    (magit-add-section-hook 'magit-status-sections-hook
                            #'magit-comment-ui--insert-unified-comments-section
                            'magit-insert-staged-changes
                            t))
  ;; Add to revision sections
  (add-hook 'magit-revision-sections-hook
            #'magit-comment-ui--insert-commit-comments
            90)
  ;; Add transient suffix to magit-dispatch
  ;; Using "#" for the keybinding
  (with-eval-after-load 'magit
    (require 'magit-comment-transient)
    ;; Check if suffix already exists before adding
    (unless (condition-case nil
                (transient-get-suffix 'magit-dispatch "#")
              (error nil))
      (transient-append-suffix 'magit-dispatch "!"
        '("#" "Comments" magit-comment-transient)))
    ;; Save original bindings before overwriting
    (setq magit-comment-ui--saved-bindings
          (list (cons 'status-hash (lookup-key magit-status-mode-map (kbd "#")))
                (cons 'revision-hash (lookup-key magit-revision-mode-map (kbd "#")))
                (cons 'status-cca (lookup-key magit-status-mode-map (kbd "C-c C-a")))
                (cons 'revision-cca (lookup-key magit-revision-mode-map (kbd "C-c C-a")))))
    ;; Bind # to open transient in magit buffers
    (define-key magit-status-mode-map (kbd "#") #'magit-comment-transient)
    (define-key magit-revision-mode-map (kbd "#") #'magit-comment-transient)
    ;; Bind C-c C-a to add comment (context-aware) in magit buffers
    (define-key magit-status-mode-map (kbd "C-c C-a") #'magit-comment-add-dwim)
    (define-key magit-revision-mode-map (kbd "C-c C-a") #'magit-comment-add-dwim))
  (magit-comment--log "UI hooks installed"))

(defun magit-comment-ui-teardown ()
  "Remove Magit UI integration for comments."
  ;; Remove unified section hook
  (remove-hook 'magit-status-sections-hook
               #'magit-comment-ui--insert-unified-comments-section)
  ;; Remove legacy hooks (for backward compatibility during transition)
  (remove-hook 'magit-status-sections-hook
               #'magit-comment-ui--insert-comments-section)
  (remove-hook 'magit-status-sections-hook
               #'magit-comment-ui--insert-staged-comments-section)
  ;; Remove revision hook
  (remove-hook 'magit-revision-sections-hook
               #'magit-comment-ui--insert-commit-comments)
  ;; Remove transient suffix from magit-dispatch
  (condition-case nil
      (transient-remove-suffix 'magit-dispatch "#")
    (error nil))
  ;; Restore original keybindings (only if magit was loaded)
  (when (featurep 'magit)
    (define-key magit-status-mode-map (kbd "#")
                (alist-get 'status-hash magit-comment-ui--saved-bindings))
    (define-key magit-revision-mode-map (kbd "#")
                (alist-get 'revision-hash magit-comment-ui--saved-bindings))
    (define-key magit-status-mode-map (kbd "C-c C-a")
                (alist-get 'status-cca magit-comment-ui--saved-bindings))
    (define-key magit-revision-mode-map (kbd "C-c C-a")
                (alist-get 'revision-cca magit-comment-ui--saved-bindings)))
  (setq magit-comment-ui--saved-bindings nil)
  (magit-comment--log "UI hooks removed"))

(provide 'magit-comment-ui)
;;; magit-comment-ui.el ends here
