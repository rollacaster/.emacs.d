(setq org-capture-templates '(("t" "Todo" entry (file "~/Dropbox/org/Todo.org")
                               "* TODO %?\n  %i\n")
                              ("l" "Learn" entry (file "~/Dropbox/org/Learn.org")
                               "* %?\n  %i\n")))

(provide 'setup-org)
