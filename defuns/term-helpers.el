(defun rac-kill-term-buffers ()
     (interactive)
     (mapc (lambda (buffer)
             (when (and (not (eq buffer (current-buffer)))
                        (or
                         (eq 'term-mode (buffer-local-value 'major-mode buffer))
                         (eq 'eshell-mode (buffer-local-value 'major-mode buffer))))
             (kill-buffer buffer)))
         (buffer-list)))
