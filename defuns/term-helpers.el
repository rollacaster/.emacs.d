(defun rac-kill-term-buffers ()
     (interactive)
     (mapc (lambda (buffer)
             (when (and (not (eq buffer (current-buffer)))
                        (or
                         (eq 'term-mode (buffer-local-value 'major-mode buffer))
                         (eq 'eshell-mode (buffer-local-value 'major-mode buffer))))
             (kill-buffer buffer)))
         (buffer-list)))

(defun rac-kill-on-port ()
  (interactive)
  (save-window-excursion
    (let ((port (read-number "Port:")))
      (shell-command (concat "lsof -i:" (number-to-string port)))
      (switch-to-buffer "*Shell Command Output*")
      (re-search-forward "\\([0-9]*\\)\sthomas")
      (shell-command (concat "kill " (match-string 1))))))

