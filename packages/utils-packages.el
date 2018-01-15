(use-package s)
(use-package dash)

;; Setup env variables on mac
(when (equal system-type 'darwin)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(use-package framemove
  :config
  (setq framemove-hook-into-windmove t))

(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package multi-term
  :bind
  (( "C-x t" . multi-term)
   ( "C-c t" . multi-term)))

(use-package beginend
  :diminish beginend-global-mode
  :diminish beginend-prog-mode
  :diminish beginend-magit-status-mode
  :bind (("M-<" . beginend-prog-mode-goto-beginning)
         ("M->" . beginend-prog-mode-goto-end))
  :config
  (beginend-global-mode))

(use-package vlf   :config
  (require 'vlf-setup))

(use-package crux
  :bind ( ("C-c r" . crux-rename-buffer-and-file)
          ("C-c TAB" . crux-cleanup-buffer-or-region)
          ("C-x C-e" . crux-eval-and-replace)
          ("C-c C-o" . crux-open-with)
          ("C-c C-e" . eval-last-sexp)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package smex)

(use-package swiper
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("C-x m" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-M-i" . complete-symbol)
         ("C-." . counsel-imenu)
         ("C-c 8" . counsel-unicode-char)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ( "C-h f" . counsel-describe-function)
         ( "C-h v" . counsel-describe-variable)
         ( "C-h l" . counsel-find-library)
         ( "C-h u" . counsel-unicode-char)
         ( "C-c f" . counsel-rg)
         ( "C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist (append '((counsel-M-x . "")) ivy-initial-inputs-alist))
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  (setq ivy-count-format "%d/%d ")
  (setq counsel-yank-pop-separator "\n------------\n")

  (use-package counsel-osx-app
    :bind (("C-c x" . counsel-osx-app))))

(use-package counsel-projectile)

(use-package pomidor
 :bind (("<f10>" . pomidor)))

(use-package neotree
  :bind (([f8] . neotree-toggle)))

(use-package projectile
  :config
  (projectile-global-mode)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "elm-stuff")
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name)))))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind (( "C-x g" . magit-status)
         ( "C-x M-g" . magit-dispatch-popup)))

(use-package git-timemachine)

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c s" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c o" . org-iswitchb))
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
  (use-package org-download)
  (use-package ox-jira)
  (use-package ox-gfm)
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

;; Make dired less verbose
(use-package dired-details
 :config
  (setq-default dired-details-hidden-string " ") 
  (dired-details-install))

(use-package noflet)

(use-package try)

(use-package docker)

(use-package pcre2el
  :ensure t
  :config
  (pcre-mode))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package uuidgen)

(use-package f)

(provide 'utils-packages)
