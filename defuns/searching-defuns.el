(defun rac-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Ripgrep %ssearch for" (if current-prefix-arg "regexp " "")))
         current-prefix-arg))
  (let ((counsel-ag-base-command
         (concat counsel-rg-base-command (counsel--rg-targets)))
        (counsel--grep-tool-look-around
         (let ((rg (car (split-string counsel-rg-base-command)))
               (switch "--pcre2"))
           (and (eq 0 (call-process rg nil nil nil switch "--version"))
                switch))))
    (counsel-ag initial-input initial-directory extra-rg-args rg-prompt
                :caller 'counsel-rg)))
