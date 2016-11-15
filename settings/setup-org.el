
(use-package org
  :init
  ;; Org capture templates to add todos or learn actions
  (setq org-capture-templates '(("i" "Inbox" entry (file "~/Dropbox/org/Inbox.org")
                                 "* %?  %i\n %a\n %u")
                                ("t" "Todo" entry (file "~/Dropbox/org/Todo.org")
                                 "* TODO %? %^g\nAdded: %U")
                                ("m" "Maybe" entry (file "~/Dropbox/org/Maybe.org")
                                 "* %?\nAdded: %U")
                                ("l" "List" entry (file "~/Dropbox/org/List.org")
                                 "* %?\nAdded: %U")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))

  (setq org-tag-alist '(("Work" . ?w) ("Online" . ?o) ("Home" . ?h) ("Phone" . ?p) ("Anywhere" . ?a) ("Mamming" . ?m)))

  (setq org-refile-targets '(("Lists.org" :level . 1)))

  ;; Use "⤵" instead of "..." for indicating sub-items
  (setq org-ellipsis "⤵")
  ;; Use utf8 org bullets
  ;; (setq org-bullets-bullet-list '("🔴" "⚫" "⚪" "🔘" "🔹" "🔻"))

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

  ;; Mode Hooks
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-capture 'auto-fill-mode)
  (add-hook 'org-capture 'flyspell-mode)
  (add-hook 'org-mode 'flyspell-mode)
  (add-hook 'org-mode 'auto-fill-mode)
  (add-hook 'org-mode 'org-bullets-mode)

  (setq org-agenda-custom-commands
        '(("w" "Work agenda"
           ((agenda "")
            (tags-todo "+Work")))
          ("h" "Home agenda"
           ((agenda "")
            (tags-todo "-Work")))))

  )

(use-package org-pdfview)
(use-package org-bullets)

(provide 'setup-org)
