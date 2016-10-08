(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(json-reformat:indent-width 2)
 '(magit-log-arguments (quote ("--graph" "--decorate" "-n256")))
 '(org-agenda-files
   (quote
    ("/Users/thomas/Dropbox/org/Archive.org" "/Users/thomas/Dropbox/org/Inbox.org" "/Users/thomas/Dropbox/org/Lists.org" "/Users/thomas/Dropbox/org/Maybe.org" "/Users/thomas/Dropbox/org/Projects.org" "/Users/thomas/Dropbox/org/Projects.org_archive" "/Users/thomas/Dropbox/org/Reference.org" "/Users/thomas/Dropbox/org/Todo.org" "/Users/thomas/Dropbox/org/Todo.org_archive")))
 '(package-selected-packages
   (quote
    (move-text org-gcal s tide notmuch aggressive-indent auc-tex rainbow-delimiters jade paredit org-mode use-package org-pomodoro helm-ag ag ggtags harvest jdee emojify git-timemachine yaml-mode web-mode undo-tree swiper solarized-theme smartparens slack sass-mode restclient rainbow-mode powerline pandoc-mode palette ox-gfm org-pdfview org-bullets org-alert neotree multi-term mocha markdown-preview-mode magit loc-changes load-relative list-utils kurecolor json-mode js2-refactor js-doc helm-spotify helm-projectile guide-key gh-md framemove flycheck expand-region exec-path-from-shell elm-yasnippets elm-mode edit-server dired-details diminish csv-nav csv-mode crux company-tern company-emoji babel avy auctex)))
 '(safe-local-variable-values
   (quote
    ((js2-basic-offset . 4)
     (setq js2-basic-offset 4)
     (mocha-options . "--reporter dot --r src/test/setup.js --compilers js:babel-core/register")
     (mocha-options . "--reporter dot --r babel-core/register --r src/test/setup.js")
     (mocha-options . "--R dot --r babel-core/register --r src/test/setup.js")
     (mocha-options . "src/test/setup.js")
     (mocha-options . src/test/setup\.js)
     (setq js2-basic-offset 2)
     (flycheck-checker . javascript-standand)
     (mocha-options . "--compilers js:./test/compiler;")
     (mocha-project-test-directory . "src/**/*.spec.js?(x)")
     (mocha-project-test-directory . "**/*.spec.js")
     (mocha-options . "--compilers --require babel-core/register")
     (mocha-project-test-directory . "'src/**/*.spec.js?(x)'")
     (mocha-options . "--require babel-core/register")
     (mocha-environment-variables . "NODE_ENV=test")
     (mocha-which-node . "/Users/thomas/.nvm/versions/node/v4.2.4/bin/node")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
