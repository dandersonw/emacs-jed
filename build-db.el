(require 'json)
(require 'emacsql-sqlite)
(require 'dash-functional)

(defun execute-sql-commands-on-new-db (db-path command-path)
  (let ((db (emacsql-sqlite db-path))
        (more-lines t))
    (find-file command-path)
    (while more-lines
      (let ((this-line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
        (emacsql db (json-read-from-string this-line)))
      (setq moreLines (= 0 (forward-line 1))))
    (kill-buffer (current-buffer))
    (emacsql-close db)))

(defun build-searchable-table (db-path table-name column-names)
  (let* ((db (emacsql-sqlite db-path))
         (name-str (format "%sSearchable" (symbol-name table-name)))
         (column-str (format "ID UNINDEXED, %s" (string-join (-map 'symbol-name column-names) ",")))
         (insert-column-str (format "%s (%s)"
                                    name-str
                                    (string-join (-map 'symbol-name (cons 'ID column-names)) ","))))
    (emacsql db (format "CREATE VIRTUAL TABLE %s USING fts5(%s);"
                        name-str
                        column-str))
    (-each (emacsql db
                    [:select $i1 :from $i2]
                    (vconcat (cons 'ID column-names))
                    table-name)
      (lambda (r)
        (let ((escaped (-map (lambda (lit)
                               (format "'%s'" (replace-regexp-in-string "%" "%%"
                                               (replace-regexp-in-string "'" "''" lit))))
                             (cdr r))))
          (emacsql db (format "INSERT INTO %s VALUES (%s)"
                              insert-column-str
                              (format "%d, %s" (car r) (string-join escaped ",")))))))
    (emacsql-close db)))
