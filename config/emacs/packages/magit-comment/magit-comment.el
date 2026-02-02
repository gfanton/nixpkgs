;;; magit-comment.el --- Local commit commenting with Claude Code export -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit "3.0") (transient "0.4"))
;; Keywords: git, tools, review
;; URL: https://github.com/gfanton/nixpkgs

;;; Commentary:

;; Local commit commenting for Magit with export to markdown for Claude Code.
;; M-x magit-comment-mode to toggle.
;;
;; Comments are stored in .git/magit-comment/comments.json within each repository.
;; Use `magit-comment-export' to generate a markdown file for Claude Code.

;;; Code:

(require 'magit-comment-core)

;; Silence native-comp warnings for magit functions
(declare-function magit-refresh "magit-mode" ())
(declare-function magit-rev-parse "magit-git" (rev &rest args))
(declare-function magit-commit-at-point "magit-diff" ())
(declare-function magit-file-at-point "magit-diff" ())
(declare-function magit-section-value-if "magit-section" (type &optional section))
(declare-function magit-current-section "magit-section" ())

;; ---- Staged Section Detection

(defun magit-comment--in-staged-section-p ()
  "Return non-nil if point is in staged changes section or its descendants."
  (when-let* ((section (magit-current-section)))
    (catch 'found
      (while section
        (when (eq (oref section type) 'staged)
          (throw 'found t))
        (setq section (oref section parent)))
      nil)))

;; ---- Autoloads from submodules

(autoload 'magit-comment-db-load "magit-comment-db")
(autoload 'magit-comment-db-save "magit-comment-db")
(autoload 'magit-comment-db-add "magit-comment-db")
(autoload 'magit-comment-db-delete "magit-comment-db")
(autoload 'magit-comment-db-update "magit-comment-db")
(autoload 'magit-comment-db-get-for-commit "magit-comment-db")
(autoload 'magit-comment-db-get-unresolved "magit-comment-db")
(autoload 'magit-comment-db-invalidate-cache "magit-comment-db")

;; Staged comments db functions
(autoload 'magit-comment-staged--file-hash "magit-comment-db")
(autoload 'magit-comment-staged-db-load "magit-comment-db")
(autoload 'magit-comment-staged-db-save "magit-comment-db")
(autoload 'magit-comment-staged-db-add "magit-comment-db")
(autoload 'magit-comment-staged-db-delete "magit-comment-db")
(autoload 'magit-comment-staged-db-update "magit-comment-db")
(autoload 'magit-comment-staged-db-load-valid "magit-comment-db")

(autoload 'magit-comment-ui-setup "magit-comment-ui")
(autoload 'magit-comment-ui-teardown "magit-comment-ui")

(autoload 'magit-comment-export-to-markdown "magit-comment-export")
(autoload 'magit-comment-export-staged-to-markdown "magit-comment-export")
(autoload 'magit-comment-export-commit "magit-comment-export")

(autoload 'magit-comment-transient "magit-comment-transient" nil t)

;; ---- Interactive Commands

;;;###autoload
(defun magit-comment-add ()
  "Add a comment to the current commit/file/line or selected region."
  (interactive)
  (magit-comment--ensure-repo)
  (let* ((has-region (use-region-p))
         (region-text (when has-region
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (commit (or (magit-commit-at-point)
                     (and (boundp 'magit-buffer-revision) magit-buffer-revision)
                     (read-string "Commit SHA: ")))
         (file (magit-file-at-point))
         (line (when file (line-number-at-pos (if has-region (region-beginning) (point)))))
         (line-end (when (and file has-region)
                     (save-excursion
                       (goto-char (region-end))
                       ;; If at beginning of line, we selected up to (not including) this line
                       (when (and (bolp) (> (point) (region-beginning)))
                         (forward-line -1))
                       (line-number-at-pos))))
         (body (read-string "Comment: ")))
    ;; Deactivate region after capturing
    (when has-region (deactivate-mark))
    (when (string-empty-p commit)
      (user-error "No commit specified"))
    (when (string-empty-p body)
      (user-error "Comment cannot be empty"))
    (let ((full-sha (magit-rev-parse commit)))
      (unless full-sha
        (user-error "Invalid commit: %s" commit))
      (require 'magit-comment-db)
      (magit-comment-db-add
       (magit-comment-entry-create
        :id (magit-comment--generate-id)
        :commit full-sha
        :file file
        :line line
        :line-end (when (and line-end (not (= line line-end))) line-end)
        :author (magit-comment--get-author)
        :created-at (magit-comment--timestamp)
        :updated-at (magit-comment--timestamp)
        :body body
        :context region-text
        :tags nil
        :resolved nil))
      (magit-refresh)
      (message "Comment added to %s%s"
               (substring full-sha 0 8)
               (cond
                ((and file line-end) (format " (%s:%s-%s)" file line line-end))
                (file (format " (%s:%s)" file line))
                (t ""))))))

;;;###autoload
(defun magit-comment-edit ()
  "Edit the comment at point."
  (interactive)
  (magit-comment--ensure-repo)
  (if-let* ((comment (magit-comment-at-point)))
      (let ((new-body (read-string "Comment: " (magit-comment-entry-body comment))))
        (when (string-empty-p new-body)
          (user-error "Comment cannot be empty"))
        (setf (magit-comment-entry-body comment) new-body)
        (setf (magit-comment-entry-updated-at comment) (magit-comment--timestamp))
        (require 'magit-comment-db)
        (magit-comment-db-update comment)
        (magit-refresh)
        (message "Comment updated"))
    (user-error "No comment at point")))

;;;###autoload
(defun magit-comment-delete ()
  "Delete the comment at point."
  (interactive)
  (magit-comment--ensure-repo)
  (if-let* ((comment (magit-comment-at-point)))
      (when (yes-or-no-p "Delete this comment? ")
        (require 'magit-comment-db)
        (magit-comment-db-delete comment)
        (magit-refresh)
        (message "Comment deleted"))
    (user-error "No comment at point")))

;;;###autoload
(defun magit-comment-toggle-resolved ()
  "Toggle the resolved status of comment at point."
  (interactive)
  (magit-comment--ensure-repo)
  (let ((comment (or (magit-comment-at-point)
                     (user-error "No comment at point"))))
    (setf (magit-comment-entry-resolved comment)
          (not (magit-comment-entry-resolved comment)))
    (setf (magit-comment-entry-updated-at comment) (magit-comment--timestamp))
    (require 'magit-comment-db)
    (magit-comment-db-update comment)
    (magit-refresh)
    (message "Comment %s"
             (if (magit-comment-entry-resolved comment) "resolved" "unresolved"))))

;;;###autoload
(defun magit-comment-export-staged ()
  "Export staged comments to markdown for Claude Code.
This is the primary export command for the typical workflow."
  (interactive)
  (magit-comment--ensure-repo)
  (require 'magit-comment-export)
  (let ((output-file (magit-comment-export-staged-to-markdown)))
    (message "Exported to %s" output-file)
    (when (yes-or-no-p "Open export file? ")
      (find-file output-file))))

;;;###autoload
(defun magit-comment-export-all ()
  "Export all comments (staged + commit) to markdown."
  (interactive)
  (magit-comment--ensure-repo)
  (require 'magit-comment-export)
  (let ((output-file (magit-comment-export-to-markdown)))
    (message "Exported to %s" output-file)
    (when (yes-or-no-p "Open export file? ")
      (find-file output-file))))

;; Keep old name as alias for backward compatibility
(defalias 'magit-comment-export 'magit-comment-export-all)

;;;###autoload
(defun magit-comment-export-commit-at-point ()
  "Export comments for commit at point."
  (interactive)
  (magit-comment--ensure-repo)
  (let ((commit (or (magit-commit-at-point)
                    (user-error "No commit at point"))))
    (require 'magit-comment-export)
    (let ((output-file (magit-comment-export-commit commit)))
      (message "Exported to %s" output-file)
      (when (yes-or-no-p "Open export file? ")
        (find-file output-file)))))

;;;###autoload
(defun magit-comment-list ()
  "List all comments in the repository."
  (interactive)
  (magit-comment--ensure-repo)
  (require 'magit-comment-db)
  (let ((comments (magit-comment-db-load)))
    (if (not comments)
        (message "No comments in this repository")
      (let ((buf (get-buffer-create "*magit-comment-list*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Comments in %s\n" (magit-comment--repo-root)))
            (insert (make-string 60 ?-) "\n\n")
            (dolist (comment comments)
              (let ((file (magit-comment-entry-file comment))
                    (line (magit-comment-entry-line comment))
                    (line-end (magit-comment-entry-line-end comment))
                    (has-context (magit-comment-entry-context comment)))
                (insert (format "[%s] %s%s%s\n"
                                (if (magit-comment-entry-resolved comment) "x" " ")
                                (substring (magit-comment-entry-commit comment) 0 8)
                                (cond
                                 ((and file line line-end) (format " %s:%s-%s" file line line-end))
                                 ((and file line) (format " %s:%s" file line))
                                 (file (format " %s" file))
                                 (t ""))
                                (if has-context " [region]" "")))
                (insert (format "    %s\n" (magit-comment-entry-body comment)))
                (insert (format "    -- %s, %s\n\n"
                                (magit-comment-entry-author comment)
                                (magit-comment-entry-created-at comment)))))
            (goto-char (point-min))
            (special-mode)))
        (pop-to-buffer buf)))))

;; ---- Staged Comment Commands

;;;###autoload
(defun magit-comment-staged-add ()
  "Add a comment to staged changes at point."
  (interactive)
  (magit-comment--ensure-repo)
  (unless (magit-comment--in-staged-section-p)
    (user-error "Point is not in staged changes section"))
  (let* ((has-region (use-region-p))
         (region-text (when has-region
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (file (or (magit-file-at-point)
                   (user-error "No file at point")))
         (line (line-number-at-pos (if has-region (region-beginning) (point))))
         (line-end (when has-region
                     (save-excursion
                       (goto-char (region-end))
                       (when (and (bolp) (> (point) (region-beginning)))
                         (forward-line -1))
                       (line-number-at-pos))))
         (body (read-string "Comment: ")))
    (when has-region (deactivate-mark))
    (require 'magit-comment-db)
    (let ((file-hash (magit-comment-staged--file-hash file)))
      (unless file-hash
        (user-error "File %s has no staged changes" file))
      (when (string-empty-p body)
        (user-error "Comment cannot be empty"))
      (magit-comment-staged-db-add
       (magit-comment-staged-entry-create
        :id (magit-comment--generate-id)
        :file file
        :line line
        :line-end (when (and line-end (not (= line line-end))) line-end)
        :hunk-hash file-hash
        :author (magit-comment--get-author)
        :created-at (magit-comment--timestamp)
        :updated-at (magit-comment--timestamp)
        :body body
        :context region-text))
      (magit-refresh)
      (message "Staged comment added to %s:%s" file line))))

;;;###autoload
(defun magit-comment-staged-edit ()
  "Edit the staged comment at point."
  (interactive)
  (magit-comment--ensure-repo)
  (if-let* ((comment (magit-comment-staged-at-point)))
      (let ((new-body (read-string "Comment: " (magit-comment-staged-entry-body comment))))
        (when (string-empty-p new-body)
          (user-error "Comment cannot be empty"))
        (setf (magit-comment-staged-entry-body comment) new-body)
        (setf (magit-comment-staged-entry-updated-at comment) (magit-comment--timestamp))
        (require 'magit-comment-db)
        (magit-comment-staged-db-update comment)
        (magit-refresh)
        (message "Staged comment updated"))
    (user-error "No staged comment at point")))

;;;###autoload
(defun magit-comment-staged-delete ()
  "Delete the staged comment at point."
  (interactive)
  (magit-comment--ensure-repo)
  (if-let* ((comment (magit-comment-staged-at-point)))
      (when (yes-or-no-p "Delete this staged comment? ")
        (require 'magit-comment-db)
        (magit-comment-staged-db-delete comment)
        (magit-refresh)
        (message "Staged comment deleted"))
    (user-error "No staged comment at point")))

;;;###autoload
(defun magit-comment-staged-clear ()
  "Clear all staged comments."
  (interactive)
  (magit-comment--ensure-repo)
  (require 'magit-comment-db)
  (let ((comments (magit-comment-staged-db-load-valid)))
    (if (not comments)
        (message "No staged comments to clear")
      (when (yes-or-no-p (format "Clear all %d staged comments? " (length comments)))
        (magit-comment-staged-db-save nil)
        (magit-refresh)
        (message "Staged comments cleared")))))

;;;###autoload
(defun magit-comment-clear ()
  "Clear all commit comments in this repository."
  (interactive)
  (magit-comment--ensure-repo)
  (require 'magit-comment-db)
  (let ((comments (magit-comment-db-load)))
    (if (not comments)
        (message "No commit comments to clear")
      (when (yes-or-no-p (format "Clear all %d commit comments? " (length comments)))
        (magit-comment-db-save nil)
        (magit-comment-db-invalidate-cache)
        (magit-refresh)
        (message "Commit comments cleared")))))

;; ---- Section Support

(defun magit-comment-at-point ()
  "Return the comment at point, if any."
  (magit-section-value-if 'magit-comment))

(defun magit-comment-staged-at-point ()
  "Return the staged comment at point, if any."
  (magit-section-value-if 'magit-comment-staged))

;; ---- Minor Mode

(defun magit-comment--enable ()
  "Enable magit-comment-mode."
  (require 'magit nil t)
  (require 'magit-comment-ui)
  (magit-comment-ui-setup)
  ;; Refresh existing buffers
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'magit-status-mode 'magit-revision-mode)
          (magit-refresh)))))
  (message "Magit comment mode enabled"))

(defun magit-comment--disable ()
  "Disable magit-comment-mode."
  (require 'magit-comment-ui)
  (magit-comment-ui-teardown)
  ;; Invalidate cache on disable
  (require 'magit-comment-db)
  (magit-comment-db-invalidate-cache)
  (message "Magit comment mode disabled"))

;;;###autoload
(define-minor-mode magit-comment-mode
  "Toggle local commit commenting for Magit."
  :global t
  :lighter " MComment"
  :group 'magit-comment
  (if magit-comment-mode
      (magit-comment--enable)
    (magit-comment--disable)))

(provide 'magit-comment)
;;; magit-comment.el ends here
