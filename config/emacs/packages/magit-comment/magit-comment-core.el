;;; magit-comment-core.el --- Core data structures for magit-comment -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, tools, review
;; URL: https://github.com/gfanton/nixpkgs

;;; Commentary:

;; Core data structures and helper functions shared across magit-comment modules.
;; This module has no dependencies on other magit-comment modules to avoid
;; circular dependencies.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Require magit-git for magit-toplevel, magit-get
;; declare-function for byte-compile arg checking
(require 'magit-git)
(declare-function magit-toplevel "magit-git" ())
(declare-function magit-get "magit-git" (&rest args))

;; ---- Customization

(defgroup magit-comment nil
  "Local commit commenting for Magit."
  :group 'magit
  :prefix "magit-comment-")

(defcustom magit-comment-storage-dir "magit-comment"
  "Directory name within .git for storing comments."
  :type 'string
  :group 'magit-comment)

(defcustom magit-comment-author nil
  "Author name for comments.
If nil, uses git config user.name."
  :type '(choice (const nil) string)
  :group 'magit-comment)

(defcustom magit-comment-export-include-diff t
  "Whether to include diff context in exports."
  :type 'boolean
  :group 'magit-comment)

(defcustom magit-comment-export-context-lines 3
  "Number of context lines around commented code in exports."
  :type 'integer
  :group 'magit-comment)

(defcustom magit-comment-debug nil
  "If non-nil, print debug messages to *Messages*."
  :type 'boolean
  :group 'magit-comment)

;; ---- Data Structures

(cl-defstruct (magit-comment-entry
               (:constructor magit-comment-entry-create)
               (:copier nil))
  "A comment attached to a commit and optionally a file/line."
  id           ; SHA1 hash (unique identifier)
  commit       ; Full commit SHA
  file         ; Relative file path (nil for commit-level)
  line         ; Line number (nil for file-level)
  line-end     ; End line for ranges (nil for single line)
  author       ; Author name
  created-at   ; ISO 8601 timestamp
  updated-at   ; ISO 8601 timestamp
  body         ; Comment text (markdown)
  context      ; Selected diff/code context (optional)
  tags         ; List of tags
  resolved)    ; Boolean - has this been addressed?

(cl-defstruct (magit-comment-staged-entry
               (:constructor magit-comment-staged-entry-create)
               (:copier nil))
  "A comment on staged changes (ephemeral)."
  id           ; Unique ID
  file         ; Relative file path
  line         ; Start line in staged diff
  line-end     ; End line (nil for single line)
  hunk-hash    ; SHA1 hash of the file's staged diff (for invalidation)
  author       ; Author name
  created-at   ; Timestamp
  updated-at   ; Timestamp (for edit tracking)
  body         ; Comment text
  context)     ; Selected region text (optional)

;; ---- Helper Functions

(defun magit-comment--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS when debug mode is enabled."
  (when magit-comment-debug
    (apply #'message (concat "[magit-comment] " format-string) args)))

(defun magit-comment--warn (format-string &rest args)
  "Display warning with FORMAT-STRING and ARGS."
  (apply #'message (concat "[magit-comment] Warning: " format-string) args))

(defun magit-comment--ensure-repo ()
  "Signal error if not in a Git repository."
  (unless (magit-toplevel)
    (user-error "Not in a Git repository")))

(defun magit-comment--get-author ()
  "Get the author name for comments."
  (or magit-comment-author
      (magit-get "user.name")
      "Anonymous"))

(defun magit-comment--repo-root ()
  "Get the repository root directory."
  (magit-toplevel))

(defun magit-comment--storage-path ()
  "Get the full path to the storage directory."
  (when-let* ((root (magit-comment--repo-root)))
    (expand-file-name (concat ".git/" magit-comment-storage-dir) root)))

(defun magit-comment--generate-id ()
  "Generate a unique ID for a comment."
  (secure-hash 'sha1 (format "%s-%s-%s"
                             (float-time)
                             (random)
                             (emacs-pid))))

(defun magit-comment--timestamp ()
  "Get current ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

(provide 'magit-comment-core)
;;; magit-comment-core.el ends here
