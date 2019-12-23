(defun rac-connect-db ()
  (interactive)
  (let ((server (completing-read "Server:" (mapcar (function first) sql-connection-alist))))
    (sql-connect server)))

(defun rac-open-localhost ()
  (interactive)
  (let ((port (read-string "Port:" )))
    (browse-url  (concat "http://localhost:" port))))
