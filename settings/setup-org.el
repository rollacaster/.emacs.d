;; Org capture templates to add todos or learn actions
(setq org-capture-templates '(("t" "Todo" entry (file "~/Dropbox/org/Todo.org")
                               "* TODO %?\n  %i\n")
                              ("l" "Learn" entry (file "~/Dropbox/org/Learn.org")
                               "* %?\n  %i\n")
                              ("c" "comSysto" entry (file "~/Dropbox/org/comSysto.org")
                               "* TODO %?\n  %i\n")))

;; Use "⤵" instead of "..." for indicating sub-items
(setq org-ellipsis "⤵")

;; Use utf8 org bullets
(setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))

;; Use utf8 todo states
(setq org-todo-keywords '((sequence "☛ TODO(t)" "✔ DONE(d)")))

;; Add all files in the org-directory to the agenda
(setq org-folder "~/Dropbox/org")
(setq org-agenda-files (nthcdr 2 (directory-files org-folder t)))
(provide 'setup-org)
