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
(autoload 'magit-comment-list "magit-comment" nil t)
(autoload 'magit-comment-mode "magit-comment" nil t)
(autoload 'magit-comment-clear-all "magit-comment" nil t)

;; DWIM commands
(autoload 'magit-comment-add-dwim "magit-comment" nil t)
(autoload 'magit-comment-edit-dwim "magit-comment" nil t)
(autoload 'magit-comment-delete-dwim "magit-comment" nil t)
(autoload 'magit-comment-toggle-resolved-dwim "magit-comment" nil t)

;; File buffer command
(autoload 'magit-comment-file-add "magit-comment-file" nil t)

;; Export commands
(autoload 'magit-comment-export-quick "magit-comment-export" nil t)
(autoload 'magit-comment-export-transient "magit-comment-export" nil t)
(autoload 'magit-comment-exports-list "magit-comment-export" nil t)
(autoload 'magit-comment-exports-transient "magit-comment-export" nil t)

;; ---- Transient Menu

;;;###autoload (autoload 'magit-comment-transient "magit-comment-transient" nil t)
(transient-define-prefix magit-comment-transient ()
  "Commands for managing comments."
  ["Comments"
   ("a" "Add comment" magit-comment-add-dwim)
   ("A" "Add commit comment" magit-comment-add)
   ("e" "Edit comment" magit-comment-edit-dwim)
   ("k" "Delete comment" magit-comment-delete-dwim)
   ("r" "Toggle resolved" magit-comment-toggle-resolved-dwim)]
  ["Export"
   ("x" "Quick export (all, copy, flush)..." magit-comment-export-quick)
   ("X" "Export with options..." magit-comment-export-transient)]
  ["File Buffer"
   ("f" "Add at line" magit-comment-file-add)]
  ["Manage"
   ("l" "List comments" magit-comment-list)
   ("L" "List exports" magit-comment-exports-list)
   ("D" "Clear ALL comments" magit-comment-clear-all)]
  ["Mode"
   ("m" "Toggle mode" magit-comment-mode)])

(provide 'magit-comment-transient)
;;; magit-comment-transient.el ends here
