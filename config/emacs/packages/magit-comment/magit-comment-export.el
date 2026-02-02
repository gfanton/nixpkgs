;;; magit-comment-export.el --- Export comments to markdown -*- lexical-binding: t; -*-

;; Author: gfanton
;; Package-Requires: ((emacs "27.1") (magit "3.0"))

;;; Commentary:

;; Export comments to markdown format optimized for Claude Code.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'magit)
(require 'magit-comment-core)

;; Autoload db functions
(autoload 'magit-comment-db-load "magit-comment-db")
(autoload 'magit-comment-staged-db-load-valid "magit-comment-db")

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

;; ---- Public API

(defun magit-comment-export-staged-to-markdown ()
  "Export only staged comments to a markdown file.
Return the path to the exported file."
  (let* ((staged-comments (magit-comment-staged-db-load-valid))
         (staged-grouped (magit-comment-export--group-staged-by-file staged-comments))
         (repo-root (magit-comment--repo-root))
         (storage (magit-comment--storage-path))
         (export-dir (expand-file-name "exports" storage))
         (filename (format "claude-staged-%s.md"
                           (format-time-string "%Y%m%d-%H%M%S")))
         (output-file (expand-file-name filename export-dir)))
    (unless (file-directory-p export-dir)
      (make-directory export-dir t))
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
         (storage (magit-comment--storage-path))
         (export-dir (expand-file-name "exports" storage))
         (filename (format "claude-review-%s.md"
                           (format-time-string "%Y%m%d-%H%M%S")))
         (output-file (expand-file-name filename export-dir))
         (unresolved-count (length (seq-filter
                                    (lambda (c) (not (magit-comment-entry-resolved c)))
                                    comments)))
         (total-count (+ (length comments) (length staged-comments))))
    (unless (file-directory-p export-dir)
      (make-directory export-dir t))
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
         (storage (magit-comment--storage-path))
         (export-dir (expand-file-name "exports" storage))
         (filename (format "claude-commit-%s-%s.md"
                           (substring full-sha 0 8)
                           (format-time-string "%Y%m%d-%H%M%S")))
         (output-file (expand-file-name filename export-dir)))
    (unless (file-directory-p export-dir)
      (make-directory export-dir t))
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

(provide 'magit-comment-export)
;;; magit-comment-export.el ends here
