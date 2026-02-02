;;; magit-comment-edit-buffer.el --- Multi-line comment editing -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, tools, review
;; URL: https://github.com/gfanton/nixpkgs

;;; Commentary:

;; Provides a dedicated buffer for editing multi-line comments.
;; Similar to git commit message editing - opens a small window,
;; use C-c C-c to save, C-c C-k to cancel.
;;
;; Only one edit buffer can exist at a time (uses fixed buffer name).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; ---- Variables

(defvar-local magit-comment-edit--callback nil
  "Callback function to call with the comment text when done.")

(defvar-local magit-comment-edit--context nil
  "Context string shown in the header.")

(defvar-local magit-comment-edit--window-config nil
  "Window configuration before opening edit buffer.")

;; ---- Minor Mode

(defvar magit-comment-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'magit-comment-edit-done)
    (define-key map (kbd "C-c C-k") #'magit-comment-edit-cancel)
    map)
  "Keymap for `magit-comment-edit-mode'.")

(define-minor-mode magit-comment-edit-mode
  "Minor mode for editing magit comments.
\\<magit-comment-edit-mode-map>
\\[magit-comment-edit-done] to save the comment.
\\[magit-comment-edit-cancel] to cancel."
  :lighter " MagitComment"
  :keymap magit-comment-edit-mode-map)

;; ---- Functions

(defun magit-comment-edit-done ()
  "Finish editing and save the comment."
  (interactive)
  (let ((content (string-trim (buffer-substring-no-properties
                               (point-min) (point-max))))
        (callback magit-comment-edit--callback)
        (win-config magit-comment-edit--window-config))
    (if (string-empty-p content)
        (message "Comment cannot be empty")
      ;; Kill buffer first
      (kill-buffer)
      ;; Restore window config, then call callback (callback may error)
      (unwind-protect
          (when callback
            (funcall callback content))
        ;; Always restore window config
        (when win-config
          (set-window-configuration win-config))))))

(defun magit-comment-edit-cancel ()
  "Cancel editing without saving."
  (interactive)
  (let ((win-config magit-comment-edit--window-config))
    (kill-buffer)
    (when win-config
      (set-window-configuration win-config))
    (message "Comment cancelled")))

(defun magit-comment-edit--setup-buffer (context callback &optional initial-content)
  "Set up the comment edit buffer.
CONTEXT is a string describing what is being commented.
CALLBACK is called with the comment text when done.
INITIAL-CONTENT is optional initial text for editing."
  ;; Capture window configuration BEFORE creating buffer
  (let ((win-config (current-window-configuration))
        (buf (get-buffer-create "*magit-comment*")))
    (with-current-buffer buf
      (erase-buffer)
      ;; Insert initial content or placeholder
      (when initial-content
        (insert initial-content))
      ;; Setup mode
      (text-mode)
      (magit-comment-edit-mode 1)
      ;; Store callback, context, and window config (all buffer-local)
      (setq-local magit-comment-edit--callback callback)
      (setq-local magit-comment-edit--context context)
      (setq-local magit-comment-edit--window-config win-config)
      (setq-local header-line-format
                  (format " %s  |  C-c C-c: save, C-c C-k: cancel"
                          (propertize context 'face 'font-lock-keyword-face)))
      ;; Position cursor
      (goto-char (point-min)))
    ;; Display in a small window at bottom (1/3 of frame)
    (let ((window (display-buffer-in-side-window
                   buf
                   '((side . bottom)
                     (slot . 0)
                     (window-height . 0.33)
                     (preserve-size . (nil . t))))))
      (select-window window)
      (set-window-dedicated-p window t))
    buf))

;;;###autoload
(defun magit-comment-edit-read (context callback &optional initial-content)
  "Read a multi-line comment in a dedicated buffer.
CONTEXT is a string describing what is being commented (shown in header).
CALLBACK is called with the comment text when the user finishes (C-c C-c).
INITIAL-CONTENT is optional initial text for editing existing comments."
  (magit-comment-edit--setup-buffer context callback initial-content))

(provide 'magit-comment-edit-buffer)
;;; magit-comment-edit-buffer.el ends here
