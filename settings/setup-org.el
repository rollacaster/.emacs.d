(use-package org
  :init
  ;; Org capture templates to add todos or learn actions
  (setq org-capture-templates '(("t" "TODO" entry (file "~/Dropbox/org/Todo.org")
                                 "* TODO %?\n  %i\n")
                                ("l" "Learn" entry (file "~/Dropbox/org/Learn.org")
                                 "* %?\n  %i\n")
                                ("c" "comSysto" entry (file "~/Dropbox/org/comSysto.org")
                                 "* TODO %?\n  %i\n")))
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
  )

(use-package org-pdfview)
(use-package org-bullets)

(provide 'setup-org)
