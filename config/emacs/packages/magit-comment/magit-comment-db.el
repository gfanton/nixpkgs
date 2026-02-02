;;; magit-comment-db.el --- JSON storage for magit-comment -*- lexical-binding: t; -*-

;; Author: gfanton
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; JSON-based storage for commit comments.
;; Provides atomic writes, error recovery, and caching.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'seq)
(require 'magit-comment-core)

;; Silence native-comp warnings for magit functions
(declare-function magit-rev-parse "magit-git" (rev &rest args))
(declare-function magit-git-insert "magit-git" (&rest args))

;; ---- Constants

(defconst magit-comment-db--version 1
  "Current database schema version.")

(defconst magit-comment-db--filename "comments.json"
  "Filename for the comments database.")

(defconst magit-comment-staged-db--filename "staged-comments.json"
  "Filename for the staged comments database.")

;; ---- Cache

(defvar magit-comment-db--cache (make-hash-table :test 'equal)
  "Cache for loaded comments, keyed by repo path.")

(defvar magit-comment-db--cache-mtime (make-hash-table :test 'equal)
  "Cache for file modification times, keyed by repo path.")

(defun magit-comment-db--cache-valid-p ()
  "Return non-nil if cache is valid for current repo."
  (when-let* ((repo (magit-comment--repo-root))
              (file (magit-comment-db--file-path))
              (cached-mtime (gethash repo magit-comment-db--cache-mtime)))
    (and (file-exists-p file)
         (equal cached-mtime (file-attribute-modification-time
                              (file-attributes file))))))

(defun magit-comment-db--cache-get ()
  "Get cached comments for current repo, or nil if cache invalid."
  (when (magit-comment-db--cache-valid-p)
    (gethash (magit-comment--repo-root) magit-comment-db--cache)))

(defun magit-comment-db--cache-set (comments)
  "Set cache for current repo to COMMENTS."
  (when-let* ((repo (magit-comment--repo-root))
              (file (magit-comment-db--file-path)))
    (puthash repo comments magit-comment-db--cache)
    (when (file-exists-p file)
      (puthash repo (file-attribute-modification-time
                     (file-attributes file))
               magit-comment-db--cache-mtime))))

(defun magit-comment-db-invalidate-cache ()
  "Invalidate cache for current repo."
  (when-let* ((repo (magit-comment--repo-root)))
    (remhash repo magit-comment-db--cache)
    (remhash repo magit-comment-db--cache-mtime)))

;; ---- Internal Functions

(defun magit-comment-db--file-path ()
  "Get the full path to the comments database file."
  (when-let* ((storage (magit-comment--storage-path)))
    (expand-file-name magit-comment-db--filename storage)))

(defun magit-comment-db--ensure-directory ()
  "Ensure the storage directory exists."
  (when-let* ((storage (magit-comment--storage-path)))
    (unless (file-directory-p storage)
      (make-directory storage t))))

(defun magit-comment-db--create-empty ()
  "Create an empty database structure."
  `((version . ,magit-comment-db--version)
    (comments . [])))

(defun magit-comment-db--comment-to-alist (comment)
  "Convert COMMENT struct to alist for JSON serialization."
  `((id . ,(magit-comment-entry-id comment))
    (commit . ,(magit-comment-entry-commit comment))
    (file . ,(magit-comment-entry-file comment))
    (line . ,(magit-comment-entry-line comment))
    (line_end . ,(magit-comment-entry-line-end comment))
    (author . ,(magit-comment-entry-author comment))
    (created_at . ,(magit-comment-entry-created-at comment))
    (updated_at . ,(magit-comment-entry-updated-at comment))
    (body . ,(magit-comment-entry-body comment))
    (context . ,(magit-comment-entry-context comment))
    (tags . ,(vconcat (magit-comment-entry-tags comment)))
    (resolved . ,(if (magit-comment-entry-resolved comment) t :json-false))))

(defun magit-comment-db--alist-to-comment (alist)
  "Convert ALIST from JSON to a `magit-comment-entry' struct."
  (magit-comment-entry-create
   :id (alist-get 'id alist)
   :commit (alist-get 'commit alist)
   :file (alist-get 'file alist)
   :line (alist-get 'line alist)
   :line-end (alist-get 'line_end alist)
   :author (alist-get 'author alist)
   :created-at (alist-get 'created_at alist)
   :updated-at (alist-get 'updated_at alist)
   :body (alist-get 'body alist)
   :context (alist-get 'context alist)
   :tags (append (alist-get 'tags alist) nil)
   :resolved (eq (alist-get 'resolved alist) t)))

(defun magit-comment-db--comments-p (x)
  "Return non-nil if X is a valid comments container."
  (or (listp x) (vectorp x)))

(defun magit-comment-db--validate (data)
  "Validate DATA has required structure.
Return DATA if valid, nil otherwise."
  (pcase data
    ((and (pred listp)
          (app (alist-get 'version) (pred integerp))
          (app (alist-get 'comments) (pred magit-comment-db--comments-p)))
     data)))

(defun magit-comment-db--load-from-disk ()
  "Load comments from disk, bypassing cache.
Return nil if file doesn't exist or is corrupt."
  (let ((file (magit-comment-db--file-path)))
    (if (not (and file (file-exists-p file)))
        nil
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (json-false :json-false)
                   (data (json-read)))
              (if-let* ((validated (magit-comment-db--validate data)))
                  (mapcar #'magit-comment-db--alist-to-comment
                          (alist-get 'comments validated))
                (magit-comment--warn "Invalid database format")
                nil)))
        (json-readtable-error
         (magit-comment--warn "Corrupt database, backing up: %s"
                              (error-message-string err))
         (rename-file file (concat file ".corrupt."
                                   (format-time-string "%Y%m%d%H%M%S")))
         nil)
        (file-error
         (magit-comment--warn "Cannot read database: %s" (error-message-string err))
         nil)))))

;; ---- Public API

(defun magit-comment-db-load ()
  "Load all comments from the database.
Uses cache when valid, otherwise loads from disk."
  (or (magit-comment-db--cache-get)
      (let ((comments (magit-comment-db--load-from-disk)))
        (magit-comment-db--cache-set comments)
        comments)))

(defun magit-comment-db-save (comments)
  "Save COMMENTS list to the database atomically."
  (magit-comment-db--ensure-directory)
  (let* ((file (magit-comment-db--file-path))
         (temp-file (make-temp-file "magit-comment-" nil ".json"))
         (data `((version . ,magit-comment-db--version)
                 (comments . ,(vconcat
                               (mapcar #'magit-comment-db--comment-to-alist
                                       comments))))))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (let ((json-encoding-pretty-print t)
                  (json-encoding-default-indentation "  "))
              (insert (json-encode data))
              (insert "\n")))
          ;; Atomic rename
          (rename-file temp-file file t)
          ;; Update cache after successful save
          (magit-comment-db--cache-set comments)
          (magit-comment--log "Saved %d comments" (length comments)))
      ;; Cleanup temp file if rename failed
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun magit-comment-db--same-id-p (c1 c2)
  "Return non-nil if C1 and C2 have the same ID."
  (string= (magit-comment-entry-id c1)
           (magit-comment-entry-id c2)))

(defun magit-comment-db-add (comment)
  "Add COMMENT to the database."
  (let ((comments (magit-comment-db-load)))
    (push comment comments)
    (magit-comment-db-save comments)))

(defun magit-comment-db-delete (comment)
  "Delete COMMENT from the database."
  (magit-comment-db-save
   (seq-remove (lambda (c) (magit-comment-db--same-id-p c comment))
               (magit-comment-db-load))))

(defun magit-comment-db-update (comment)
  "Update COMMENT in the database."
  (magit-comment-db-save
   (mapcar (lambda (c)
             (if (magit-comment-db--same-id-p c comment) comment c))
           (magit-comment-db-load))))

(defun magit-comment-db-get-for-commit (commit)
  "Get all comments for COMMIT."
  (let ((full-sha (magit-rev-parse commit)))
    (seq-filter
     (lambda (c) (string= (magit-comment-entry-commit c) full-sha))
     (magit-comment-db-load))))

(defun magit-comment-db-get-unresolved ()
  "Get all unresolved comments."
  (seq-filter (lambda (c) (not (magit-comment-entry-resolved c)))
              (magit-comment-db-load)))

;; ---- Staged Comments Database

(defun magit-comment-staged-db--file-path ()
  "Get the full path to the staged comments database file."
  (when-let* ((storage (magit-comment--storage-path)))
    (expand-file-name magit-comment-staged-db--filename storage)))

(defun magit-comment-staged--file-hash (file)
  "Get hash of staged diff for FILE.
Return nil if FILE has no staged changes."
  (let ((diff-output (with-temp-buffer
                       (magit-git-insert "diff" "--cached" "--" file)
                       (buffer-string))))
    (unless (string-empty-p diff-output)
      (secure-hash 'sha1 diff-output))))

(defun magit-comment-staged-db--comment-to-alist (comment)
  "Convert staged COMMENT struct to alist for JSON serialization."
  `((id . ,(magit-comment-staged-entry-id comment))
    (file . ,(magit-comment-staged-entry-file comment))
    (line . ,(magit-comment-staged-entry-line comment))
    (line_end . ,(magit-comment-staged-entry-line-end comment))
    (hunk_hash . ,(magit-comment-staged-entry-hunk-hash comment))
    (author . ,(magit-comment-staged-entry-author comment))
    (created_at . ,(magit-comment-staged-entry-created-at comment))
    (updated_at . ,(magit-comment-staged-entry-updated-at comment))
    (body . ,(magit-comment-staged-entry-body comment))
    (context . ,(magit-comment-staged-entry-context comment))))

(defun magit-comment-staged-db--alist-to-comment (alist)
  "Convert ALIST from JSON to a `magit-comment-staged-entry' struct."
  (magit-comment-staged-entry-create
   :id (alist-get 'id alist)
   :file (alist-get 'file alist)
   :line (alist-get 'line alist)
   :line-end (alist-get 'line_end alist)
   :hunk-hash (alist-get 'hunk_hash alist)
   :author (alist-get 'author alist)
   :created-at (alist-get 'created_at alist)
   :updated-at (alist-get 'updated_at alist)
   :body (alist-get 'body alist)
   :context (alist-get 'context alist)))

(defun magit-comment-staged-db-load ()
  "Load all staged comments from the database.
Return nil if file doesn't exist or is corrupt."
  (let ((file (magit-comment-staged-db--file-path)))
    (if (not (and file (file-exists-p file)))
        nil
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (json-false :json-false)
                   (data (json-read)))
              (if-let* ((validated (magit-comment-db--validate data)))
                  (mapcar #'magit-comment-staged-db--alist-to-comment
                          (alist-get 'comments validated))
                (magit-comment--warn "Invalid staged database format")
                nil)))
        (json-readtable-error
         (magit-comment--warn "Corrupt staged database, backing up: %s"
                              (error-message-string err))
         (rename-file file (concat file ".corrupt."
                                   (format-time-string "%Y%m%d%H%M%S")))
         nil)
        (file-error
         (magit-comment--warn "Cannot read staged database: %s" (error-message-string err))
         nil)))))

(defun magit-comment-staged-db-save (comments)
  "Save staged COMMENTS list to the database atomically."
  (magit-comment-db--ensure-directory)
  (let* ((file (magit-comment-staged-db--file-path))
         (temp-file (make-temp-file "magit-comment-staged-" nil ".json"))
         (data `((version . ,magit-comment-db--version)
                 (comments . ,(vconcat
                               (mapcar #'magit-comment-staged-db--comment-to-alist
                                       comments))))))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (let ((json-encoding-pretty-print t)
                  (json-encoding-default-indentation "  "))
              (insert (json-encode data))
              (insert "\n")))
          ;; Atomic rename
          (rename-file temp-file file t)
          (magit-comment--log "Saved %d staged comments" (length comments)))
      ;; Cleanup temp file if rename failed
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun magit-comment-staged-db--same-id-p (c1 c2)
  "Return non-nil if staged comments C1 and C2 have the same ID."
  (string= (magit-comment-staged-entry-id c1)
           (magit-comment-staged-entry-id c2)))

(defun magit-comment-staged-db-add (comment)
  "Add staged COMMENT to the database."
  (let ((comments (magit-comment-staged-db-load)))
    (push comment comments)
    (magit-comment-staged-db-save comments)))

(defun magit-comment-staged-db-delete (comment)
  "Delete staged COMMENT from the database."
  (magit-comment-staged-db-save
   (seq-remove (lambda (c) (magit-comment-staged-db--same-id-p c comment))
               (magit-comment-staged-db-load))))

(defun magit-comment-staged-db-update (comment)
  "Update staged COMMENT in the database."
  (magit-comment-staged-db-save
   (mapcar (lambda (c)
             (if (magit-comment-staged-db--same-id-p c comment) comment c))
           (magit-comment-staged-db-load))))

(defun magit-comment-staged-db-load-valid ()
  "Load staged comments, filtering out invalid ones.
Also updates storage to remove invalid comments."
  (let* ((comments (magit-comment-staged-db-load))
         (hash-cache (make-hash-table :test 'equal))
         (valid nil)
         (invalid-count 0))
    (dolist (c comments)
      (let* ((file (magit-comment-staged-entry-file c))
             (stored-hash (magit-comment-staged-entry-hunk-hash c))
             (current-hash (or (gethash file hash-cache)
                               (puthash file
                                        (magit-comment-staged--file-hash file)
                                        hash-cache))))
        (if (and current-hash stored-hash (string= stored-hash current-hash))
            (push c valid)
          (cl-incf invalid-count))))
    (setq valid (nreverse valid))
    (when (> invalid-count 0)
      (magit-comment--log "Pruned %d invalid staged comments" invalid-count)
      (magit-comment-staged-db-save valid))
    valid))

(provide 'magit-comment-db)
;;; magit-comment-db.el ends here
