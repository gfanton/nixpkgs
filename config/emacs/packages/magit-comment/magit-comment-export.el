;;; magit-comment-export.el --- Export comments to markdown -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit "3.0") (transient "0.4"))
;; Keywords: git, tools, review
;; URL: https://github.com/gfanton/nixpkgs

;;; Commentary:

;; Export comments to markdown format optimized for Claude Code.
;; Provides quick export, advanced export menu, and export management.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'transient)
(require 'tabulated-list)
(require 'magit-comment-core)

;; Autoload db functions
(autoload 'magit-comment-db-load "magit-comment-db")
(autoload 'magit-comment-db-save "magit-comment-db")
(autoload 'magit-comment-db-invalidate-cache "magit-comment-db")
(autoload 'magit-comment-staged-db-load-valid "magit-comment-db")
(autoload 'magit-comment-staged-db-save "magit-comment-db")

;; Silence native-comp warnings
(declare-function magit-rev-format "magit-git" (format &optional rev args))
(declare-function magit-git-string "magit-git" (&rest args))
(declare-function magit-git-insert "magit-git" (&rest args))
(declare-function magit-rev-parse "magit-git" (rev &rest args))

;; ---- Internal Functions

(defun magit-comment-export--get-file-context (commit file line)
  "Get context lines around LINE in FILE at COMMIT."
  (when (and magit-comment-export-include-diff file line)
    (let ((context magit-comment-export-context-lines))
      (condition-case nil
          (with-temp-buffer
            (magit-git-insert "show" (format "%s:%s" commit file))
            (let* ((lines (split-string (buffer-string) "\n"))
                   (start (max 0 (- line context 1)))
                   (end (min (length lines) (+ line context))))
              (when (<= start end)
                (list :start (1+ start)
                      :end end
                      :lines (seq-subseq lines start end)
                      :comment-line (- line start)))))
        (error nil)))))

(defun magit-comment-export--format-context (context)
  "Format CONTEXT as code block with comment line highlighted."
  (when context
    (let ((lines (plist-get context :lines))
          (comment-offset (plist-get context :comment-line)))
      (with-temp-buffer
        (insert "```\n")
        (cl-loop for l in lines
                 for i from 0
                 do (insert (format "%s %s\n"
                                    (if (= i comment-offset) ">" " ")
                                    l)))
        (insert "```\n")
        (buffer-string)))))

(defun magit-comment-export--format-comment (comment)
  "Format COMMENT as markdown."
  (let* ((file (magit-comment-entry-file comment))
         (line (magit-comment-entry-line comment))
         (line-end (magit-comment-entry-line-end comment))
         (commit (magit-comment-entry-commit comment))
         (stored-context (magit-comment-entry-context comment))
         (location (cond
                    ((and file line line-end) (format "%s:%d-%d" file line line-end))
                    ((and file line) (format "%s:%d" file line))
                    (file file)
                    (t "Commit-level")))
         ;; Use stored context if available, otherwise fetch from commit
         (context (or stored-context
                      (when (and file line (not line-end))
                        (let ((ctx (magit-comment-export--get-file-context commit file line)))
                          (when ctx
                            (magit-comment-export--format-context ctx)))))))
    (with-temp-buffer
      (insert (format "### Comment (%s)\n\n" location))
      (insert (format "> %s\n\n"
                      (replace-regexp-in-string
                       "\n" "\n> "
                       (magit-comment-entry-body comment))))
      (when context
        (insert "**Selected Context:**\n")
        (if stored-context
            ;; Stored context is raw text, wrap it in code fences
            (progn
              (insert "```\n")
              (insert stored-context)
              (unless (string-suffix-p "\n" stored-context)
                (insert "\n"))
              (insert "```\n\n"))
          ;; Dynamic context is already formatted with code fences
          (insert context)
          (insert "\n")))
      (insert (format "**Tags**: %s\n"
                      (if-let* ((tags (magit-comment-entry-tags comment)))
                          (mapconcat #'identity tags ", ")
                        "_none_")))
      (insert (format "**Status**: %s\n"
                      (if (magit-comment-entry-resolved comment)
                          "Resolved"
                        "Unresolved")))
      (insert (format "**Author**: %s @ %s\n\n"
                      (magit-comment-entry-author comment)
                      (magit-comment-entry-created-at comment)))
      (insert "---\n\n")
      (buffer-string))))

(defun magit-comment-export--format-commit-group (commit comments)
  "Format a group of COMMENTS for COMMIT as markdown."
  (let ((short-sha (substring commit 0 (min 8 (length commit))))
        (subject (or (magit-rev-format "%s" commit) ""))
        (author (or (magit-rev-format "%an" commit) ""))
        (date (or (magit-rev-format "%ci" commit) "")))
    (with-temp-buffer
      (insert (format "## Commit: %s - %s\n\n" short-sha subject))
      (insert (format "**Author**: %s | **Date**: %s\n\n" author date))
      (dolist (comment comments)
        (insert (magit-comment-export--format-comment comment)))
      (buffer-string))))

(defun magit-comment-export--group-by-commit (comments)
  "Group COMMENTS by commit SHA.
Return alist of (commit . comments) using hash-table for O(1) lookups."
  (let ((table (make-hash-table :test 'equal)))
    ;; Group using hash-table with push (O(1) per insert)
    (dolist (comment comments)
      (let ((commit (magit-comment-entry-commit comment)))
        (puthash commit
                 (cons comment (gethash commit table))
                 table)))
    ;; Convert to alist, reversing each group to maintain order
    (let ((result nil))
      (maphash (lambda (k v)
                 (push (cons k (nreverse v)) result))
               table)
      (nreverse result))))

(defun magit-comment-export--group-staged-by-file (comments)
  "Group staged COMMENTS by file.
Return alist of (file . comments) using hash-table for O(1) lookups."
  (let ((table (make-hash-table :test 'equal)))
    ;; Group using hash-table with push (O(1) per insert)
    (dolist (comment comments)
      (let ((file (magit-comment-staged-entry-file comment)))
        (puthash file
                 (cons comment (gethash file table))
                 table)))
    ;; Convert to alist, reversing each group to maintain order
    (let ((result nil))
      (maphash (lambda (k v)
                 (push (cons k (nreverse v)) result))
               table)
      (nreverse result))))

(defun magit-comment-export--format-staged-comment (comment)
  "Format staged COMMENT as markdown."
  (let* ((file (magit-comment-staged-entry-file comment))
         (line (magit-comment-staged-entry-line comment))
         (line-end (magit-comment-staged-entry-line-end comment))
         (stored-context (magit-comment-staged-entry-context comment))
         (location (cond
                    ((and file line line-end) (format "%s:%d-%d" file line line-end))
                    ((and file line) (format "%s:%d" file line))
                    (file file)
                    (t "Unknown"))))
    (with-temp-buffer
      (insert (format "### Comment (%s)\n\n" location))
      (insert (format "> %s\n\n"
                      (replace-regexp-in-string
                       "\n" "\n> "
                       (magit-comment-staged-entry-body comment))))
      (when stored-context
        (insert "**Selected Context:**\n")
        (insert "```diff\n")
        (insert stored-context)
        (unless (string-suffix-p "\n" stored-context)
          (insert "\n"))
        (insert "```\n\n"))
      (insert (format "**Author**: %s @ %s\n\n"
                      (magit-comment-staged-entry-author comment)
                      (magit-comment-staged-entry-created-at comment)))
      (insert "---\n\n")
      (buffer-string))))

(defun magit-comment-export--format-staged-file-group (file comments)
  "Format a group of staged COMMENTS for FILE as markdown."
  (with-temp-buffer
    (insert (format "### File: %s\n\n" file))
    (dolist (comment comments)
      (insert (magit-comment-export--format-staged-comment comment)))
    (buffer-string)))

;; ---- Directory Helpers

(defun magit-comment-export--ensure-export-dir ()
  "Ensure the exports directory exists and return its path."
  (let ((dir (expand-file-name "exports" (magit-comment--storage-path))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;; ---- Content Generation Helpers

(defun magit-comment-export--generate-content (staged commits)
  "Generate markdown content for export.
STAGED: include staged comments.  COMMITS: include commit comments."
  (let* ((commit-comments (when commits (magit-comment-db-load)))
         (staged-comments (when staged (magit-comment-staged-db-load-valid)))
         (grouped (when commit-comments
                    (magit-comment-export--group-by-commit commit-comments)))
         (staged-grouped (when staged-comments
                           (magit-comment-export--group-staged-by-file staged-comments)))
         (repo-root (magit-comment--repo-root))
         (unresolved-count (length (seq-filter
                                    (lambda (c) (not (magit-comment-entry-resolved c)))
                                    (or commit-comments '()))))
         (total-count (+ (length (or commit-comments '()))
                         (length (or staged-comments '())))))
    (with-temp-buffer
      (insert "# Code Review Comments\n\n")
      (insert (format "**Repository**: %s\n" repo-root))
      (insert (format "**Exported**: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M UTC" nil t)))
      (insert (format "**Total Comments**: %d\n" total-count))
      (when commits
        (insert (format "**Unresolved**: %d\n" unresolved-count)))
      (when staged
        (insert (format "**Staged Comments**: %d\n" (length (or staged-comments '())))))
      (insert "\n---\n\n")
      ;; Staged comments section (if any)
      (when staged-grouped
        (insert "## Staged Changes (not yet committed)\n\n")
        (dolist (group staged-grouped)
          (insert (magit-comment-export--format-staged-file-group
                   (car group) (cdr group))))
        (insert "\n"))
      ;; Commit comments section
      (when grouped
        (insert "## Commit Comments\n\n")
        (dolist (group grouped)
          (insert (magit-comment-export--format-commit-group
                   (car group) (cdr group)))))
      ;; Handle empty case
      (when (and (not staged-grouped) (not grouped))
        (insert "_No comments to export._\n"))
      (buffer-string))))

(defun magit-comment-export--write-to-file (content type)
  "Write CONTENT to an export file with TYPE prefix.
TYPE should be \"review\", \"staged\", or \"commits\".
Return the file path."
  (let* ((export-dir (magit-comment-export--ensure-export-dir))
         (filename (format "claude-%s-%s.md"
                           type
                           (format-time-string "%Y%m%d-%H%M%S")))
         (output-file (expand-file-name filename export-dir)))
    (with-temp-file output-file
      (insert content))
    (magit-comment--log "Exported to %s" output-file)
    output-file))

;; ---- Export Execution

(cl-defun magit-comment-export--execute (&key staged commits args)
  "Execute export with given source and ARGS flags.
STAGED: include staged comments.  COMMITS: include commit comments.
ARGS: transient flags (--file, --clipboard, --flush)."
  (require 'magit-comment-db)
  (magit-comment--ensure-repo)
  (let* ((file-p (transient-arg-value "--file" args))
         (clipboard-p (transient-arg-value "--clipboard" args))
         (flush-p (transient-arg-value "--flush" args))
         (content (magit-comment-export--generate-content staged commits))
         output-file)
    ;; Check for empty content
    (when (string-match-p "_No comments to export._" content)
      (user-error "No comments to export"))
    ;; Write file if --file
    (when file-p
      (setq output-file (magit-comment-export--write-to-file
                         content
                         (cond ((and staged commits) "review")
                               (staged "staged")
                               (t "commits")))))
    ;; Copy to clipboard if --clipboard
    (when clipboard-p
      (kill-new content))
    ;; Flush if --flush
    (when flush-p
      (when staged (magit-comment-staged-db-save nil))
      (when commits
        (magit-comment-db-save nil)
        (magit-comment-db-invalidate-cache))
      (when (fboundp 'magit-refresh) (magit-refresh)))
    ;; Message
    (message "%s"
             (string-join
              (delq nil
                    (list (when output-file (format "Exported to %s" output-file))
                          (when clipboard-p "copied to clipboard")
                          (when flush-p "comments cleared")))
              ", "))))

;; ---- Public API

(defun magit-comment-export-staged-to-markdown ()
  "Export only staged comments to a markdown file.
Return the path to the exported file."
  (let* ((staged-comments (magit-comment-staged-db-load-valid))
         (staged-grouped (magit-comment-export--group-staged-by-file staged-comments))
         (repo-root (magit-comment--repo-root))
         (export-dir (magit-comment-export--ensure-export-dir))
         (filename (format "claude-staged-%s.md"
                           (format-time-string "%Y%m%d-%H%M%S")))
         (output-file (expand-file-name filename export-dir)))
    (with-temp-file output-file
      (insert "# Staged Changes Review\n\n")
      (insert (format "**Repository**: %s\n" repo-root))
      (insert (format "**Exported**: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M UTC" nil t)))
      (insert (format "**Comments**: %d\n\n" (length staged-comments)))
      (insert "---\n\n")
      (if (not staged-grouped)
          (insert "_No staged comments to export._\n")
        (dolist (group staged-grouped)
          (insert (magit-comment-export--format-staged-file-group
                   (car group) (cdr group))))))
    (magit-comment--log "Exported %d staged comments to %s"
                        (length staged-comments) output-file)
    output-file))

(defun magit-comment-export-to-markdown ()
  "Export all comments to a markdown file.
Return the path to the exported file."
  (let* ((comments (magit-comment-db-load))
         (staged-comments (magit-comment-staged-db-load-valid))
         (grouped (magit-comment-export--group-by-commit comments))
         (staged-grouped (magit-comment-export--group-staged-by-file staged-comments))
         (repo-root (magit-comment--repo-root))
         (export-dir (magit-comment-export--ensure-export-dir))
         (filename (format "claude-review-%s.md"
                           (format-time-string "%Y%m%d-%H%M%S")))
         (output-file (expand-file-name filename export-dir))
         (unresolved-count (length (seq-filter
                                    (lambda (c) (not (magit-comment-entry-resolved c)))
                                    comments)))
         (total-count (+ (length comments) (length staged-comments))))
    (with-temp-file output-file
      (insert "# Code Review Comments\n\n")
      (insert (format "**Repository**: %s\n" repo-root))
      (insert (format "**Exported**: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M UTC" nil t)))
      (insert (format "**Total Comments**: %d\n" total-count))
      (insert (format "**Unresolved**: %d\n" unresolved-count))
      (insert (format "**Staged Comments**: %d\n\n" (length staged-comments)))
      (insert "---\n\n")
      ;; Staged comments section (if any)
      (when staged-grouped
        (insert "## Staged Changes (not yet committed)\n\n")
        (dolist (group staged-grouped)
          (insert (magit-comment-export--format-staged-file-group
                   (car group) (cdr group))))
        (insert "\n"))
      ;; Commit comments section
      (if (not grouped)
          (unless staged-grouped
            (insert "_No comments to export._\n"))
        (insert "## Commit Comments\n\n")
        (dolist (group grouped)
          (insert (magit-comment-export--format-commit-group
                   (car group) (cdr group))))))
    (magit-comment--log "Exported %d comments to %s" total-count output-file)
    output-file))

(defun magit-comment-export-commit (commit)
  "Export comments for a single COMMIT to markdown.
Return the path to the exported file."
  (let* ((full-sha (magit-rev-parse commit))
         (comments (seq-filter
                    (lambda (c) (string= (magit-comment-entry-commit c) full-sha))
                    (magit-comment-db-load)))
         (export-dir (magit-comment-export--ensure-export-dir))
         (filename (format "claude-commit-%s-%s.md"
                           (substring full-sha 0 8)
                           (format-time-string "%Y%m%d-%H%M%S")))
         (output-file (expand-file-name filename export-dir)))
    (with-temp-file output-file
      (insert "# Code Review Comments - Single Commit\n\n")
      (insert (format "**Commit**: %s\n" full-sha))
      (insert (format "**Exported**: %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M UTC" nil t)))
      (insert "---\n\n")
      (if (not comments)
          (insert "_No comments for this commit._\n")
        (insert (magit-comment-export--format-commit-group full-sha comments))))
    (magit-comment--log "Exported %d comments for %s to %s"
                        (length comments) (substring full-sha 0 8) output-file)
    output-file))

;; ---- Quick Export

;;;###autoload
(defun magit-comment-export-quick ()
  "Export all comments, copy to clipboard, and flush.
Prompts for confirmation before clearing comments."
  (interactive)
  (magit-comment--ensure-repo)
  (require 'magit-comment-db)
  (let* ((commit-count (length (magit-comment-db-load)))
         (staged-count (length (magit-comment-staged-db-load-valid)))
         (total (+ commit-count staged-count)))
    (when (zerop total)
      (user-error "No comments to export"))
    (when (yes-or-no-p (format "Export and clear %d comments? " total))
      (let* ((content (magit-comment-export--generate-content t t))
             (output-file (magit-comment-export--write-to-file content "review")))
        (kill-new content)
        (magit-comment-db-save nil)
        (magit-comment-db-invalidate-cache)
        (magit-comment-staged-db-save nil)
        (when (fboundp 'magit-refresh) (magit-refresh))
        (message "Exported to %s, copied, comments cleared" output-file)))))

;; ---- Transient Suffixes

(defun magit-comment-export-all-with-options (&optional args)
  "Export all comments (staged + commits) with transient options.
ARGS are the transient flags."
  (interactive (list (transient-args 'magit-comment-export-transient)))
  (magit-comment-export--execute :staged t :commits t :args args))

(defun magit-comment-export-staged-with-options (&optional args)
  "Export staged comments only with transient options.
ARGS are the transient flags."
  (interactive (list (transient-args 'magit-comment-export-transient)))
  (magit-comment-export--execute :staged t :commits nil :args args))

(defun magit-comment-export-commits-with-options (&optional args)
  "Export commit comments only with transient options.
ARGS are the transient flags."
  (interactive (list (transient-args 'magit-comment-export-transient)))
  (magit-comment-export--execute :staged nil :commits t :args args))

;; ---- Export Transient

;;;###autoload (autoload 'magit-comment-export-transient "magit-comment-export" nil t)
(transient-define-prefix magit-comment-export-transient ()
  "Export comments with options."
  :value '("--file")
  ["Options"
   ("f" "Export to file"      "--file")
   ("c" "Copy to clipboard"   "--clipboard")
   ("F" "Flush after export"  "--flush")]
  ["Export"
   ("a" "All (staged + commits)" magit-comment-export-all-with-options)
   ("s" "Staged only"            magit-comment-export-staged-with-options)
   ("C" "Commits only"           magit-comment-export-commits-with-options)])

;; ---- Exports List Mode

(defvar magit-comment-exports-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'magit-comment-exports-open-at-point)
    (define-key map (kbd "d") #'magit-comment-exports-delete-at-point)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `magit-comment-exports-mode'.")

(define-derived-mode magit-comment-exports-mode tabulated-list-mode
  "Exports"
  "Major mode for listing magit-comment exports."
  :group 'magit-comment
  (setq tabulated-list-format [("File" 40 t)
                               ("Date" 20 t)
                               ("Type" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun magit-comment-exports--scan-directory ()
  "Return list of export files in the exports directory."
  (when-let* ((storage (magit-comment--storage-path))
              (dir (expand-file-name "exports" storage)))
    (when (file-directory-p dir)
      (directory-files dir t "\\.md\\'" t))))

(defun magit-comment-exports--file-to-entry (file)
  "Convert FILE path to tabulated-list entry."
  (let* ((name (file-name-nondirectory file))
         (attrs (file-attributes file))
         (mtime (format-time-string "%Y-%m-%d %H:%M"
                  (file-attribute-modification-time attrs)))
         (type (cond
                ((string-match-p "staged" name) "staged")
                ((string-match-p "commit" name) "commit")
                ((string-match-p "review" name) "all")
                (t "unknown"))))
    (list file (vector name mtime type))))

;;;###autoload
(defun magit-comment-exports-list ()
  "Display list of export files."
  (interactive)
  (magit-comment--ensure-repo)
  (let ((buf (get-buffer-create "*magit-comment-exports*"))
        (exports (magit-comment-exports--scan-directory)))
    (with-current-buffer buf
      (magit-comment-exports-mode)
      (setq tabulated-list-entries
            (mapcar #'magit-comment-exports--file-to-entry exports))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(defun magit-comment-exports-open-at-point ()
  "Open the export file at point."
  (interactive)
  (if-let* ((entry (tabulated-list-get-id)))
      (find-file entry)
    (user-error "No export at point")))

(defun magit-comment-exports-delete-at-point ()
  "Delete the export file at point."
  (interactive)
  (if-let* ((entry (tabulated-list-get-id)))
      (when (yes-or-no-p (format "Delete %s? " (file-name-nondirectory entry)))
        (delete-file entry)
        (tabulated-list-delete-entry)
        (message "Deleted %s" (file-name-nondirectory entry)))
    (user-error "No export at point")))

;;;###autoload
(defun magit-comment-exports-delete ()
  "Delete an export file by selection."
  (interactive)
  (magit-comment--ensure-repo)
  (let* ((exports (magit-comment-exports--scan-directory))
         (names (mapcar #'file-name-nondirectory exports))
         (selection (completing-read "Delete export: " names nil t)))
    (when selection
      (let ((file (seq-find (lambda (f) (string= (file-name-nondirectory f) selection))
                            exports)))
        (when (and file (yes-or-no-p (format "Delete %s? " selection)))
          (delete-file file)
          (message "Deleted %s" selection))))))

;;;###autoload
(defun magit-comment-exports-delete-all ()
  "Delete all export files."
  (interactive)
  (magit-comment--ensure-repo)
  (let ((exports (magit-comment-exports--scan-directory)))
    (if (not exports)
        (user-error "No exports to delete")
      (when (yes-or-no-p (format "Delete ALL %d export files? " (length exports)))
        (dolist (file exports)
          (delete-file file))
        (message "Deleted %d exports" (length exports))))))

;;;###autoload
(defun magit-comment-exports-open ()
  "Open an export file by selection."
  (interactive)
  (magit-comment--ensure-repo)
  (let* ((exports (magit-comment-exports--scan-directory))
         (names (mapcar #'file-name-nondirectory exports))
         (selection (completing-read "Open export: " names nil t)))
    (when selection
      (let ((file (seq-find (lambda (f) (string= (file-name-nondirectory f) selection))
                            exports)))
        (when file
          (find-file file))))))

;; ---- Exports Management Transient

;;;###autoload (autoload 'magit-comment-exports-transient "magit-comment-export" nil t)
(transient-define-prefix magit-comment-exports-transient ()
  "Manage exported comment files."
  ["Actions"
   ("l" "List exports"    magit-comment-exports-list)
   ("d" "Delete export"   magit-comment-exports-delete)
   ("D" "Delete all"      magit-comment-exports-delete-all)
   ("o" "Open export"     magit-comment-exports-open)])

(provide 'magit-comment-export)
;;; magit-comment-export.el ends here
