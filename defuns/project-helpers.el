(defun rac-connect-db ()
  (interactive)
  (let ((server (completing-read "Server:" (mapcar (function first) sql-connection-alist))))
    (sql-connect server)))

(defun rac-open-localhost ()
  (interactive)
  (let ((port (completing-read "Port:" '("8904" "9500" "8080"))))
    (browse-url  (concat "http://localhost:" port))))

(defun rac-add-project ()
  (interactive)
  (let ((project (read-string "Project: ")))
    (apply #'magit-clone-internal
           (list (concat "https://github.com/rollacaster/" project ".git")
                 (concat "/Users/thomas/projects/" project)
                 nil))))

(defun rac-store-mail (&optional dir)
  "Copy message at point to somewhere else as <date>_<subject>.eml."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (target (format "%s_%s.eml"
                         (format-time-string "%F" (mu4e-message-field msg :date))
                         (or (mu4e-message-field msg :subject) "No subject"))))
    (copy-file
     (mu4e-message-field msg :path)
     (concat
      "~/Downloads/email-"
      (format-time-string "%Y-%m-%dT%T")
      ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
       (format-time-string "%z"))))))





