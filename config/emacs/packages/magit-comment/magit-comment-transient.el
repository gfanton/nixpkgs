;;; magit-comment-transient.el --- Transient menus for magit-comment -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit "3.0") (transient "0.4"))
;; Keywords: git, tools, review
;; URL: https://github.com/gfanton/nixpkgs

;;; Commentary:

;; Transient menu definitions for magit-comment commands.

;;; Code:

(require 'cl-lib)
(require 'transient)

;; Autoload commands from magit-comment
(autoload 'magit-comment-add "magit-comment" nil t)
(autoload 'magit-comment-edit "magit-comment" nil t)
(autoload 'magit-comment-delete "magit-comment" nil t)
(autoload 'magit-comment-toggle-resolved "magit-comment" nil t)
(autoload 'magit-comment-export-staged "magit-comment" nil t)
(autoload 'magit-comment-export-all "magit-comment" nil t)
(autoload 'magit-comment-export-commit-at-point "magit-comment" nil t)
(autoload 'magit-comment-list "magit-comment" nil t)
(autoload 'magit-comment-mode "magit-comment" nil t)
(autoload 'magit-comment-staged-add "magit-comment" nil t)
(autoload 'magit-comment-staged-edit "magit-comment" nil t)
(autoload 'magit-comment-staged-delete "magit-comment" nil t)
(autoload 'magit-comment-staged-clear "magit-comment" nil t)
(autoload 'magit-comment-clear "magit-comment" nil t)

;; File buffer command
(autoload 'magit-comment-file-add "magit-comment-file" nil t)

;; ---- Transient Menu

;;;###autoload (autoload 'magit-comment-transient "magit-comment-transient" nil t)
(transient-define-prefix magit-comment-transient ()
  "Commands for managing commit comments."
  ["Commit Comments"
   ("a" "Add comment" magit-comment-add)
   ("e" "Edit comment" magit-comment-edit)
   ("k" "Delete comment" magit-comment-delete)
   ("r" "Toggle resolved" magit-comment-toggle-resolved)
   ("C" "Clear all commit comments" magit-comment-clear)]
  ["Staged Comments"
   ("A" "Add staged comment" magit-comment-staged-add)
   ("E" "Edit staged comment" magit-comment-staged-edit)
   ("K" "Delete staged comment" magit-comment-staged-delete)
   ("S" "Clear all staged" magit-comment-staged-clear)]
  ["Export"
   ("x" "Export staged (primary)" magit-comment-export-staged)
   ("X" "Export all (staged + commits)" magit-comment-export-all)
   ("c" "Export commit at point" magit-comment-export-commit-at-point)]
  ["File Buffer"
   ("f" "Add comment at line" magit-comment-file-add)]
  ["View"
   ("l" "List all comments" magit-comment-list)]
  ["Mode"
   ("m" "Toggle magit-comment-mode" magit-comment-mode)])

(provide 'magit-comment-transient)
;;; magit-comment-transient.el ends here
