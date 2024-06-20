;;; templ-mode.el --- A polymode for Templs -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; URL: https://github.com/gfanton/gno-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (polymode "0.2.2") (lsp-mode "7.0.1"))
;; Keywords: languages, polymode, templs

;;; Commentary:

;; This package provides a polymode for Templ. Templ files with
;; extension .templ will have sections highlighted in go-mode, html-mode,
;; css-mode, and text-mode based on headers. Supports LSP mode 
;; with the Templ LSP server and formats files on save with templ fmt.

;;; Code:

(require 'polymode)
(require 'lsp-mode)

;;;###autoload
(define-derived-mode templ-host-mode go-mode "Templ"
  "Major mode for GNO files, an alias for go-mode."
  (setq-local tab-width gno-tab-width) ;; Use the custom gno-tab-width variable
  ;; FIXME: disable lsp for now
  (when (fboundp 'lsp-disconnect) ;; Check if the lsp-disconnect function is available
    (lsp-disconnect)) ;; lsp doesn't work with gno yet
  )

;; Define host and inner modes
(define-hostmode templ-go-hostmode
  :mode 'templ-host-mode)

(define-innermode templ-html-innermode
  :mode 'html-mode
  :head-matcher (rx line-start "templ" (1+ space) (0+ not-newline) (0+ space) "{")
  :tail-matcher (rx line-start "}"))

(define-innermode templ-css-innermode
  :mode 'css-mode
  :head-matcher (rx line-start "css" (1+ space) (0+ not-newline) (0+ space) "{")
  :tail-matcher (rx line-start "}"))

;; Define the polymode
(define-polymode templ-mode-polymode
  :hostmode 'templ-go-hostmode
  :innermodes '(templ-html-innermode templ-css-innermode))

;;;###autoload
(defun templ-mode ()
  "Activate the Templ polymode."
  (interactive)
  ;; FIXME: disable lsp for now
  (templ-mode-polymode))


;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("templ" "lsp"))
;;                   :activation-fn (lambda (&rest _) 
;;                                    (and (eq major-mode 'go-mode)
;;                                         (string-equal "templ" (file-name-extension (buffer-file-name)))))
;;                   :server-id 'templ-lsp
;;                   :priority 1)) ;; set it to a priority higher than gopls 

;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection "gopls")
;;                   :major-modes '(go-mode)
;;                   :server-id 'gopls
;;                   :priority 0))

(defun templ-format-buffer ()
  "Format the current buffer using templ fmt."
  (when (and (eq major-mode 'templ-host-mode)
             (string-equal "templ" (file-name-extension (buffer-file-name))))
    (shell-command-to-string (concat "templ fmt " buffer-file-name))
    (revert-buffer nil t t)))

(add-hook 'after-save-hook #'templ-format-buffer)
 

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.templ\\'" . templ-mode))

(provide 'templ-mode)
;;; templ-mode.el ends here
