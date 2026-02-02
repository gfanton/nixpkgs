;;; magit-comment-file.el --- File buffer commenting -*- lexical-binding: t; -*-

;; Author: gfanton
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit "3.0"))
;; Keywords: git, tools, review
;; URL: https://github.com/gfanton/nixpkgs

;;; Commentary:

;; Integration with regular file buffers for adding comments to lines.
;; Uses git blame to attribute comments to the commit that last modified
;; each line.
;;
;; For clean (committed) lines: creates commit comments
;; For staged lines: creates ephemeral staged comments
;; For unstaged changes: errors and prompts user to stage or stash

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'pcase)
(require 'magit-git)
(require 'magit-comment-core)

;; Byte-compile declarations
(declare-function magit-git-string "magit-git" (&rest args))
(declare-function magit-git-insert "magit-git" (&rest args))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-comment-staged--file-hash "magit-comment-db" (file))
(declare-function magit-comment-staged-db-add "magit-comment-db" (comment))
(declare-function magit-comment-db-add "magit-comment-db" (comment))
(declare-function magit-comment-edit-read "magit-comment-edit-buffer" (context callback &optional initial))

;; ---- Git Blame Integration

(defun magit-comment-file--blame-line (file line)
  "Get commit SHA for LINE in FILE using git blame.
Return nil if line has no commit (new file not yet committed)."
  (when-let* ((output (magit-git-string
                       "blame" "-L" (format "%d,%d" line line)
                       "-l" "--" file))
              ((not (string-empty-p output)))
              (sha (car (split-string output)))
              ((not (string-empty-p sha)))
              ;; Filter out uncommitted lines (^0000000...)
              ((not (string-prefix-p "^0000" sha)))
              ;; Also filter lines from the initial commit marker
              ((not (string-prefix-p "0000000" sha))))
    ;; Remove leading ^ if present (for boundary commits)
    (if (string-prefix-p "^" sha)
        (substring sha 1)
      sha)))

;; ---- Modified Line Detection

(defun magit-comment-file--parse-diff-hunks (diff-string)
  "Parse DIFF-STRING and return list of (start . end) line ranges.
Only includes ranges where lines exist in the new file (count > 0)."
  (let ((ranges nil))
    (with-temp-buffer
      (insert diff-string)
      (goto-char (point-min))
      (while (re-search-forward
              "^@@ -[0-9,]+ +\\([0-9]+\\),?\\([0-9]*\\)" nil t)
        (let* ((start (string-to-number (match-string 1)))
               (count (if (string-empty-p (match-string 2))
                          1
                        (string-to-number (match-string 2)))))
          ;; Only include if lines exist in new file (count > 0)
          (when (> count 0)
            (push (cons start (+ start count -1)) ranges)))))
    (nreverse ranges)))

(defun magit-comment-file--line-in-hunks-p (hunks line)
  "Return non-nil if LINE falls within any of HUNKS ranges."
  (and hunks
       (seq-some (lambda (range)
                   (<= (car range) line (cdr range)))
                 hunks)))

(defun magit-comment-file--file-tracked-p (file)
  "Return non-nil if FILE is tracked by git."
  (magit-git-string "ls-files" "--" file))

(defun magit-comment-file--get-hunks (file &optional cached)
  "Get diff hunk ranges for FILE.
If CACHED is non-nil, check staged changes; otherwise unstaged."
  (let ((diff-output (with-temp-buffer
                       (if cached
                           (magit-git-insert "diff" "--cached" "-U0" "--" file)
                         (magit-git-insert "diff" "-U0" "--" file))
                       (buffer-string))))
    (unless (string-empty-p diff-output)
      (magit-comment-file--parse-diff-hunks diff-output))))

(defun magit-comment-file--line-status (file line)
  "Return status of LINE in FILE: `staged', `unstaged', or `clean'.
Single function to avoid multiple git subprocess calls."
  (let ((unstaged-hunks (magit-comment-file--get-hunks file nil))
        (staged-hunks (magit-comment-file--get-hunks file t)))
    (cond
     ((magit-comment-file--line-in-hunks-p unstaged-hunks line) 'unstaged)
     ((magit-comment-file--line-in-hunks-p staged-hunks line) 'staged)
     (t 'clean))))

;; ---- File Buffer Command

;;;###autoload
(defun magit-comment-file-add ()
  "Add a comment at current line in file buffer.
For clean lines: attaches to the commit that last modified the line.
For staged lines: creates an ephemeral staged comment.
Errors on unstaged changes or untracked files.

Opens a multi-line editing buffer. Use C-c C-c to save, C-c C-k to cancel."
  (interactive)
  (magit-comment--ensure-repo)
  (require 'magit-comment-db)
  (require 'magit-comment-edit-buffer)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  ;; Check for unsaved changes
  (when (buffer-modified-p)
    (if (yes-or-no-p "Buffer has unsaved changes. Save first? ")
        (save-buffer)
      (user-error "Cannot comment on unsaved buffer - git sees different content")))
  (let* ((repo-root (magit-toplevel))
         (file (file-relative-name buffer-file-name repo-root))
         (line (line-number-at-pos))
         (has-region (use-region-p))
         (region-text (when has-region
                        (buffer-substring-no-properties
                         (region-beginning) (region-end))))
         (line-end (when has-region
                     (save-excursion
                       (goto-char (region-end))
                       (when (and (bolp) (> (point) (region-beginning)))
                         (forward-line -1))
                       (line-number-at-pos)))))
    ;; Deactivate region early
    (when has-region (deactivate-mark))
    ;; Check if file is tracked
    (unless (magit-comment-file--file-tracked-p file)
      (user-error "File is not tracked by git"))
    ;; Get line status (single combined check for efficiency)
    (pcase (magit-comment-file--line-status file line)
      (`unstaged
       (user-error "Line has unstaged changes - stage or stash first"))
      (`staged
       ;; Compute hash BEFORE opening edit buffer
       (let ((file-hash (magit-comment-staged--file-hash file)))
         (unless file-hash
           (user-error "File %s has no staged changes" file))
         ;; Open multi-line edit buffer
         (magit-comment-edit-read
          (format "Staged comment: %s:%d" file line)
          (lambda (body)
            (when (string-empty-p body)
              (user-error "Comment cannot be empty"))
            ;; Re-check hash is still valid
            (let ((current-hash (magit-comment-staged--file-hash file)))
              (unless (and current-hash (string= current-hash file-hash))
                (user-error "File %s staged content changed, please try again" file)))
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
            (message "Staged comment added to %s:%d" file line)))))
      (`clean
       ;; Clean line - get commit via blame, create regular comment
       (let ((commit (magit-comment-file--blame-line file line)))
         (unless commit
           (user-error "Line %d has no commit history" line))
         ;; Open multi-line edit buffer
         (magit-comment-edit-read
          (format "Comment: %s:%d (commit %s)" file line (substring commit 0 8))
          (lambda (body)
            (when (string-empty-p body)
              (user-error "Comment cannot be empty"))
            (magit-comment-db-add
             (magit-comment-entry-create
              :id (magit-comment--generate-id)
              :commit commit
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
            (message "Comment added to %s:%d (commit %s)" file line
                     (substring commit 0 8)))))))))

(provide 'magit-comment-file)
;;; magit-comment-file.el ends here
