(use-package org
  :init
  ;; Org capture templates to add todos or learn actions
  (setq org-capture-templates '(("i" "Inbox" entry (file "~/Dropbox/org/Inbox.org")
                                 "* %?  %i\n %a")
                                ("t" "Todo" entry (file+headline "~/Dropbox/org/Todo.org" "TODOs")
                                 "* TODO %? %^g")
                                ("m" "Maybe" entry (file "~/Dropbox/org/Maybe.org")
                                 "* %?\n")
                                ("r" "Read" entry (file "~/Dropbox/org/Inbox.org")
                                 "* %? %^L" :prepend t)
                                ("l" "Links" entry (file "~/Dropbox/org/Links.org")
                                 "* %? %^L")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))

  (setq org-tag-alist '(("Work" . ?w) ("Online" . ?o) ("Home" . ?h) ("Phone" . ?p) ("Train" . ?t) ("Mamming" . ?m)))

  (setq org-refile-targets '(("Links.org" :level . 1) ("Todo.org" :level . 1) ("Maybe.org" :level . 1)))

  ;; Use "⤵" instead of "..." for indicating sub-items
  (setq org-ellipsis "⤵")
  ;; Use utf8 org bullets
  (setq org-bullets-bullet-list '("🔴" "🔵" "🔘" "⚪️" "🔹" "🔻"))

  ;; Add all files in the org-directory to the agenda
  (setq org-folder "~/Dropbox/org")
  (setq org-agenda-files (nthcdr 2 (directory-files org-folder t)))

  ;; Ask if work time should be substracted after 15 minutes
  (setq org-clock-idle-time 15)

  ;; Parent TODOs cannot be resolved when a child has a TODO state
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  ;; Ignore schedule & deadline items in TODO agenda
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-tags-todo-honor-ignore-options t)

  (setq org-agenda-custom-commands
        '(("h" "Agenda"
           ((agenda "")
            (tags-todo "*")))))

  ;; Capture everywhere with emacsclient -ne "(make-capture-frame)"
  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-frame)))

  (defadvice org-capture-destroy
      (after delete-capture-frame activate)
    "Advise capture-destroy to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-frame)))

  (defun make-capture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "capture")))
    (select-frame-by-name "capture")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (org-capture)))
  :config
  (use-package org-pdfview)
  (use-package org-bullets)
  ;; Mode Hooks
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-capture 'auto-fill-mode)
  (add-hook 'org-capture 'flyspell-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'org-bullets-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (defun rac-completion-hook ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'rac-completion-hook))



(provide 'setup-org)
