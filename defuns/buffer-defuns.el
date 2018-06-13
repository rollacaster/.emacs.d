(defun copy-file-path ()
  "Copy buffer's full path to kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(defun buffer-mode (buffer-or-string)
  "Return the major mode associated with a BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
     major-mode))

(defun rac-switch-git ()
  "Autocomplete for open magit status buffers."
  (interactive)
  (switch-to-buffer
   (completing-read "Project: " (mapcar (function buffer-name)
                                        (-filter (lambda (buffer) (string= (buffer-mode buffer) "magit-status-mode"))
                                                 (buffer-list))))))
