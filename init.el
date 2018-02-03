(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

;; Refresh packages if archives do not exist yet
(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(setq use-package-always-ensure t)

(package-initialize)

;; load paths
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "defuns" user-emacs-directory))

(add-to-list 'load-path "~/Projects/elcontext")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
;; add external projects to load path
(dolist (project (directory-files (expand-file-name "site-lisp" user-emacs-directory) t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'appearance)
(require 'sane-defaults)
(require 'key-bindings)

(require 'elcontext)
(elcontext-global-mode)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Refresh packages if archives do not exist yet
(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(setq use-package-always-ensure t)

(package-initialize)

(use-package s)

(use-package dash)

(use-package org-beautify-theme)
(use-package solarized-theme)
(load-theme 'org-beautify)
(load-theme 'solarized-light)

(use-package powerline
  :config
  (powerline-default-theme))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package hideshow
  :bind (("C-\\" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((java-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil)))))

(use-package expand-region
  :bind (("M-2" . er/expand-region)
         ("M-1" . er/contract-region)))

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package emojify
  :bind (("C-c e" . emojify-insert-emoji)))

(use-package hydra
  :config
  (use-package kurecolor
    :config
    (eval-after-load 'css-mode '(defhydra hydra-color (css-mode-map "C-c C-c")
                                  "color"
                                  ("u" kurecolor-increase-hue-by-step "increase hue")
                                  ("j" kurecolor-decrease-hue-by-step "decreasee hue")
                                  ("i" kurecolor-increase-saturation-by-step "increase saturation")
                                  ("k" kurecolor-decrease-saturation-by-step "decrease saturation")
                                  ("o" kurecolor-increase-brightness-by-step "increase brightness")
                                  ("l" kurecolor-decrease-brightness-by-step "decrease brightness")))))

(use-package smartparens
  :diminish smartparens-mode
  :init (smartparens-global-mode 1)
  :bind (:map smartparens-mode-map
              ("C-)" . sp-forward-slurp-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-{" . sp-backward-barf-sexp)))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (eval-after-load 'grep
    '(define-key grep-mode-map
       (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

  (eval-after-load 'wgrep
    '(define-key grep-mode-map
       (kbd "C-c C-c") 'wgrep-finish-edit)))

(use-package avy
  :bind (( "C-c g" . avy-goto-line)
         ( "C-'" . avy-goto-char)
         ( "C-c ," . avy-copy-line)
         ( "C-c m" . avy-move-line)
         ( "C-c ." . avy-copy-region)))

(use-package auctex
  :mode "\\.tex\\'"
  :defer t
  :config
  ;; Make AUCTex aware of style files and multi-files
  (setq TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(use-package paredit
  :diminish paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package company
  :diminish company-mode
  :bind (("C-;" . company-complete))
  :init (global-company-mode)
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-web)
(use-package company-emoji
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  :config
  ;; Show list of flycheck errors on the bottom in a small window
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2)))

  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  ;; Activate yasnippet
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  ;; Manually define indents
  (setq yas-indent-line 'fixed)

  ;; Load snippets
  (yas-reload-all))

(use-package restclient
  :bind (("C-c TAB" . json-pretty-print))
  :mode ("\\.api\\'" . restclient-mode))

(use-package multiple-cursors
  :bind (( "C-c RET" . mc/mark-all-like-this-dwim)
         ( "M-3" . mc/mark-next-like-this)
         ( "M-4" . mc/mark-previous-like-this)
         ( "M-#" . mc/unmark-next-like-this)
         ( "M-$" . mc/unmark-previous-like-this)))

(use-package elm-mode
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm)
  (define-key elm-mode-map (kbd "C-c TAB") 'elm-mode-format-buffer)
  (add-hook 'elm-mode-hook 'yas-minor-mode))

(use-package npm-mode
  :diminish npm-mode
  :config
  (npm-global-mode))

(use-package meghanada
  :config
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
  (add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (remove-hook 'before-save-hook 'meghanada-code-beautify-before-save))))

(use-package prettier-js
  :diminish prettier-js-mode
  :config
  (add-hook 'js2-jsx-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '("--print-width 80" "--single-quote" "--no-semi")))

(use-package json-mode
  :bind (:map json-mode-map
              ("C-c TAB" . json-pretty-print))
  :mode "\\.json\\'")

(use-package js2-mode
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill)
              ("C-c s" . replace-string)
              ("C-c C-s" . replace-string))
  :config
  ;; Activate toggle indent with tab
  (setq js2-bounce-indent-p t)
  (define-key js2-mode-map (kbd "C-c C-t") nil)
  ;; Set basic offset to 2
  (setq js2-basic-offset 2)
  (setq js-indent-level 2)
  (setq sgml-basic-offset 2)
  (setq sgml-at 2)
  (setq json-reformat:indent-width 2)

  ;; Highlight as much as possible
  (setq js2-highlight-level 3)

  ;; Do not warn about missing semicolons
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-highlight-external-variables nil)
  (setq js2-strict-trailing-comma-warning nil)
  (setq js2-ignored-warnings '("msg.no.side.effects", "msg.no.paren", "msg.no.semi.stmt", "msg.unterminated.re.lit"))
  (js2-imenu-extras-setup)
  ;; Add externals
  (setq js2-global-externs '("describe", "it", "expect", "beforeEach", "sinon", "require"))
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (use-package js2-refactor
    :diminish js2-refactor-mode
    :config
    (js2r-add-keybindings-with-prefix "C-c C-j"))
  (use-package xref-js2)

  (defun er/add-js2-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(er/mark-html-attribute
                                er/mark-inner-tag
                                er/mark-outer-tag
                                ))))

  (add-hook 'js2-mode-hook 'er/add-js2-mode-expansions))

(use-package web-mode
  :mode "\\.html\\'"
  :config
  (setq web-mode-code-indent-offset 2)
  ;; No auto quote in html tags
  (setq web-mode-enable-auto-quoting nil)

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (eval-after-load 'web-mode
    '(define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file))

  (eval-after-load 'web-mode
    '(define-key web-mode-map (kbd "C-c RET") 'mc/mark-all-like-this-dwim))

  (define-key web-mode-map (kbd "C-;") 'company-web-html)
  (add-hook 'web-mode-hook (lambda ()
                             (set (make-local-variable 'company-backends) '(company-web-html company-files))
                             (company-mode t)))
  (add-hook 'web-mode-hook #'yas-minor-mode))

(use-package rjsx-mode
  :mode
  ("\\.js\\'" . rjsx-mode)
  :interpreter "node"
  :config
  (add-hook 'rjsx-mode-hook 'yas-minor-mode)
  (add-hook 'rjsx-mode-hook 'js2-refactor-mode)
  (add-hook 'rjsx-mode-hook 'rainbow-mode))

(use-package purescript-mode
  :mode "\\.purs\\'")

(use-package package-lint)
(use-package flycheck-package)
(use-package log4j-mode)

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
         ("C-c o" . org-iswitchb)
         ("M-p" . org-move-subtree-up)
         ("M-n" . org-move-subtree-down)
         (:map org-mode-map
               ("C-'" . avy-goto-char)))
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
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")))

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
            (todo "TODO")))
          ("k" "Klaka"
           ((todo "ToDo")
            (todo "Doing")
            (todo  "Waiting")
            (todo "Review")))))

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
  (add-hook 'org-mode-hook (lambda ()
                             (flyspell-mode)
                             (define-key flyspell-mode-map (kbd "C-.") 'counsel-imenu)))
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'org-bullets-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (defun rac-completion-hook ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'rac-completion-hook)
  (define-key dired-mode-map (kbd "M-o") nil))

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

(use-package org-trello
 :config
 (custom-set-variables '(org-trello-files '("~/Dropbox/org/Klaka.org"))))

(use-package org-gcal
  :config
  (add-hook 'org-agenda-mode-hook #'org-gcal-sync))

(use-package demo-it)
(use-package org-tree-slide)
(use-package fancy-narrow)

(use-package osx-location)

(use-package grab-mac-link
  :init
  (bind-key "s-f"  (lambda () (interactive) (insert (grab-mac-link 'firefox))))
  (bind-key "s-c"  (lambda () (interactive) (insert (grab-mac-link 'chrome)))))

(use-package google-this
  :bind (("s-g" . google-this)))

(load-file "~/.emacs.d/mail.el")
(require 'mail)


