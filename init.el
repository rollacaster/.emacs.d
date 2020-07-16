;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

;; Refresh packages if archives do not exist yet
(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
        (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-compute-statistics nil)

(use-package use-package-ensure-system-package)

(use-package emacs
  :ensure nil
  :ensure-system-package
  (("/Users/thomas/Library/Fonts/FiraCode-Retina.otf" . "brew tap homebrew/cask-fonts; brew cask install font-fira-code")
   ("/Applications/TeamViewer.app" . "brew cask install teamviewer")
   ("/Applications/Kap.app" . "brew cask install kap")
   (aws . awscli)
   mu
   isync
   node
   (fuck . "npm i -g fuck-you")
   (internal-ip . "npm i -g internal-ip-cli")
   (kill-port . "npm i -g kill-port")
   (psql . postgresql))
  :config
  ;; Disable UI Elements
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

  (global-hl-line-mode 1)
  (mac-auto-operator-composition-mode)
  (set-face-attribute 'default nil :height 150)
  (set-frame-name "🐦")

  (setq inhibit-startup-message t)
  (add-to-list 'load-path "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp/mu/mu4e/"))

(set-frame-font "-*-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; load paths
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "defuns" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'sane-defaults)
(require 'key-bindings)

(use-package s)

(use-package dash)

(use-package diminish)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package moody
  :custom
  (x-underline-at-descent-line  t)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package org-beautify-theme
  :config
  (load-theme 'org-beautify t))

;; Hack Reload solazired to override beautify font colors
(load-theme 'solarized-light t)

;; Setup env variables on mac
(use-package exec-path-from-shell
  :defer t
  :if (memq window-system '(mac ns)))

(use-package expand-region
  :bind (("M-2" . er/expand-region)
         ("M-1" . er/contract-region)))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook prog-mode)

(use-package emojify
  :bind (("C-c e" . emojify-insert-emoji))
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles (quote (unicode))))

(use-package hydra)

(use-package smartparens
  :diminish smartparens-mode
  :hook ((js2-mode web-mode json-mode) . smartparens-mode))

(use-package wgrep
  :bind (:map grep-mode-map
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit))
  :custom
  (wgrep-auto-save-buffer t))

(use-package avy
  :bind (( "C-c g" . avy-goto-line)
         ( "C-'" . avy-goto-char)
         ( "C-c ," . avy-copy-line)
         ( "C-c m" . avy-move-line)
         ( "C-c ." . avy-copy-region)))

(use-package paredit
  :diminish paredit-mode
  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode eval-expression-minibuffer-setup) . paredit-mode)
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)
         ("M-i" . paredit-splice-sexp-killing-backward)
         ("M-k" . paredit-splice-sexp-killing-forward)))

(use-package company-restclient)

(use-package company
  :diminish company-mode
  :bind (("C-;" . company-complete))
  :custom
  (company-show-numbers t)
  :config
  (global-company-mode)
  (add-to-list 'company-backends 'company-restclient))

(use-package company-web)
(use-package company-emoji
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
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

(use-package flycheck-clj-kondo)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode org-mode sql-interactive-mode mu4e-compose-mode) . yas-minor-mode)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; Manually define indents
  (yas-indent-line 'fixed)
  :config
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

(use-package npm-mode
  :ensure-system-package
  ((nodemon . "npm i -g nodemon")
   yarn
   watchman
   (eslint . "npm i -g eslint")
   (n . "npm i -g n"))
  :diminish npm-mode
  :config
  (npm-global-mode))

(use-package meghanada
  :mode ("\\.java\\'" . java-mode)
  :ensure-system-package
  ("/usr/bin/java" . "brew cask install java")
  :custom
  (jdee-server-dir "~/Projects/jdee-server/target")
  :config
  (add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (remove-hook 'before-save-hook 'meghanada-code-beautify-before-save))))

(use-package prettier-js
  :ensure-system-package prettier
  :diminish prettier-js-mode
  :custom
  (prettier-js-show-errors 'echo)
  (prettier-js-args '("--print-width 80" "--single-quote" "--no-semi"))
  :hook ((js2-mode web-mode) . prettier-js-mode))

(use-package json-mode
  :bind (:map json-mode-map
              ("C-c TAB" . json-pretty-print))
  :mode "\\.json\\'")

(use-package js2-mode
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill)
              ("C-c s" . replace-string)
              ("C-c C-s" . replace-string)
              ("C-)" . paredit-forward-slurp-sexp)
              ("C-(" . paredit-backward-slurp-sexp)
              ("C-c t" . rac-string-to-template))
  :custom
  (js2-bounce-indent-p t)
  ;; Set basic offset to 2
  (js2-basic-offset 2)
  (js-indent-level 2)
  (sgml-basic-offset 2)
  (sgml-at 2)
  (json-reformat:indent-width 2)
  ;; Highlight as much as possible
  (js2-highlight-level 3)
  ;; Do not warn about missing semicolons
  (js2-strict-missing-semi-warning nil)
  (js2-highlight-external-variables nil)
  (js2-strict-trailing-comma-warning nil)
  (js2-ignored-warnings '("msg.no.side.effects", "msg.no.paren", "msg.no.semi.stmt", "msg.unterminated.re.lit"))
  (js2-global-externs '("describe", "it", "expect", "beforeEach", "sinon", "require"))
  :config
  (define-key js2-mode-map (kbd "C-c C-t") nil)
  (js2-imenu-extras-setup)
  :interpreter ("node" . js2-jsx-mode))

(use-package js2-refactor
  :diminish js2-refactor-mode
  :hook (rjsx-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-j"))

(use-package web-mode
  :bind (:map web-mode-map
              ("C-c C-v" . browse-url-of-file)
              ("C-c RET" . mc/mark-all-like-this-dwim)
              ("C-;" . company-web-html))
  :mode "\\.html\\'"
  :custom
  (web-mode-code-indent-offset 2)
  ;; No auto quote in html tags
  (web-mode-enable-auto-quoting nil)
  :config
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  (set (make-local-variable 'company-backends) '(company-web-html company-files)))

(use-package rjsx-mode
  :bind
  ( "M-[" . rjsx-jump-opening-tag)
  ( "M-]" . rjsx-jump-closing-tag)
  :mode
  ("\\.js\\'" . rjsx-mode)
  :interpreter "node")

(use-package package-lint)
(use-package flycheck-package)

(use-package alert
  :commands (alert)
  :custom
  (alert-default-style 'notifier))

(use-package beginend
  :diminish beginend-global-mode
  :diminish beginend-prog-mode
  :diminish beginend-magit-status-mode
  :diminish beginend-org-mode
  :diminish beginend-outline-mode
  :bind (("M-<" . beginend-prog-mode-goto-beginning)
         ("M->" . beginend-prog-mode-goto-end))
  :config
  (beginend-global-mode))

(use-package vlf
  :config
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

(use-package pomidor
 :bind (("<f10>" . pomidor))
 :custom (pomidor-play-sound-file . nil))
(use-package counsel-projectile)
(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  (projectile-switch-project-action 'magit-status)
  (magit-display-buffer-function
   (lambda (buffer)
     (display-buffer
      buffer
      (cond ((and (derived-mode-p 'magit-mode)
                  (eq (with-current-buffer buffer major-mode)
                      'magit-status-mode))
             nil)
            ((memq (with-current-buffer buffer major-mode)
                   '(magit-process-mode
                     magit-revision-mode
                     magit-diff-mode
                     magit-stash-mode))
             nil)
            (t
             '(display-buffer-same-window))))))
  :config
  (projectile-global-mode)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "elm-stuff"))

(use-package magit
  :ensure-system-package git
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind (( "C-x g" . magit-status)
         ( "C-x M-g" . magit-dispatch-popup)
         (:map magit-mode-map
               ("M-2" . er/expand-region)
               ("M-1" . er/contract-region))))

(use-package git-timemachine)

(use-package subword
  :diminish subword-mode
  :ensure nil)

(use-package flyspell
  :ensure-system-package hunspell
  :diminish flyspell-mode
  :ensure nil
  :config
  (setq ispell-current-dictionary "en_US")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  (defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "de_DE_frami") "en_US" "de_DE_frami")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)))

  (global-set-key (kbd "<f9>")   'fd-switch-dictionary))

(use-package noflet)

(use-package org
  :ensure-system-package
  (("/Applications/Dropbox.app" . "brew cask install dropbox")
   (latex . "brew cask install mactex-no-gui"))
  :mode ("\\.org" . org-mode)
  :bind (("C-c c" . org-capture)
         ("C-c s" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c o" . org-switchb)
         ("M-p" . org-move-subtree-up)
         ("M-n" . org-move-subtree-down)
         :map org-mode-map
         ("C-'" . avy-goto-char)
         :map flyspell-mode-map
         ("C-." . counsel-imenu))
  :custom
  ;; Org capture templates to add todos or learn actions
  (org-capture-templates '(("i" "Inbox" entry (file "~/org/Inbox.org")
                            "* %?  %i\n %a")
                           ("t" "Todo" entry (file+headline "~/org/Todo.org" "TODOs")
                            "* TODO %?")
                           ("m" "Maybe" entry (file "~/org/Maybe.org")
                            "* %?\n")
                           ("r" "Read" entry (file "~/org/Inbox.org")
                            "* %? %^L" :prepend t)
                           ("j" "Journal" entry (file+datetree "~/org/Journal.org")
                            "* %?%i\n%U\n")))
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")))
  (org-tag-alist '(("Work" . ?w) ("Online" . ?o) ("Home" . ?h) ("Phone" . ?p)))
  (org-ellipsis "⤵")
  (org-folder "~/org")
  (org-agenda-files (seq-filter
                     (lambda (file) (and
                                (not (string-match-p "archive" file))
                                (not (string-match-p "organice" file))
                                (string-match-p "\\.org" file)))
                     (nthcdr 2 (directory-files org-folder t))))
  ;; babel
  (org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (shell . t)
     (clojure . t)))
  (org-confirm-babel-evaluate nil)

  (org-list-indent-offset 2)

  ;; logging
  (org-log-into-drawer t)

  ;; export
  (org-export-with-toc nil)

  ;; Ask if work time should be substracted after 15 minutes
  (org-clock-idle-time 15)

  ;; Parent TODOs cannot be resolved when a child has a TODO state
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)

  (org-refile-allow-creating-parent-nodes (quote confirm))
  (org-refile-targets
   (quote
    ((org-agenda-files :level . 1)
     (org-agenda-files :level . 2)
     (org-agenda-files :level . 3))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file
                               org-outline-path-complete-in-steps nil)

  ;; images
  (org-image-actual-width '(150))

  ;; Ignore schedule & deadline items in TODO agenda
  (org-agenda-todo-ignore-scheduled t)
  (org-agenda-todo-ignore-deadlines t)
  (org-agenda-tags-todo-honor-ignore-options t)

  (org-agenda-custom-commands
   '(("h" "Agenda"
      ((agenda "")
       (todo "TODO")))
     ("k" "Klaka"
      ((todo "ToDo")
       (todo "Doing")
       (todo  "Waiting")
       (todo "Review")))))
  :init
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

  ;; (defun rac-completion-hook ()
  ;;   (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  ;; (add-hook 'org-mode-hook #'rac-completion-hook)
  ;; (define-key dired-mode-map (kbd "M-o") nil)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Org tags*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2))))

(use-package org-pdfview
  :defer t)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-download
  :config
  (setq org-download-screenshot-method "screencapture -i %s"))
  ;; (use-package ox-jira)

(use-package ox-gfm)

(use-package try)

(use-package docker
  :defer t
  :ensure-system-package (docker . "brew cask install docker"))

(use-package pcre2el
  :diminish pcre-mode
  :config
  (pcre-mode))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package uuidgen)

(use-package f)

(use-package org-tree-slide)

(use-package osx-location)

(use-package google-this
  :ensure-system-package
  (("/Applications/Google Chrome.app" . "brew cask install google-chrome")
   ("/Applications/Firefox.app" . "brew cask install firefox"))
  :bind (("s-g" . google-this)))

(use-package elcontext
  :diminish elcontext-global-mode
  :config
  (elcontext-global-mode))

(use-package ob-restclient)

(use-package vmd-mode
  :ensure-system-package (vmd . "npm i -g vmd"))

(use-package eldoc
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) . turn-on-eldoc-mode))

(use-package cider
  :bind (:map cider-mode-map
              ("C-c RET" . mc/mark-all-like-this-dwim))
  :hook ((cider-repl-mode cider-mode) . cider-company-enable-fuzzy-completion)
  :custom
  (cider-print-quota 10485)
  (cider-print-options '(("length" 100) ("level" 10) ("print-length" 100) ("print-level" 10)))
  (cider-figwheel-main-default-options ":dev")
  (cider-repl-history-file "~/.emacs.d/cider-history")
  (cider-repl-wrap-history t)
  (cider-prompt-for-symbol nil)
  (cider-show-error-buffer nil)
  (cider-auto-select-error-buffer nil)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-use-pretty-printing t))

(use-package clj-refactor
  :defer t
  :custom
  (cljr-warn-on-eval nil)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r"))

(use-package rainbow-delimiters
  :diminish rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package clojure-mode
  :ensure-system-package (((java . "brew tap AdoptOpenJDK/openjdk && brew cask install adoptopenjdk11"))
                          clojure
                          (lein . leiningen)
                          (heroku . "brew tap heroku/brew && brew install heroku")
                          (jet . borkdude/brew/jet))
  :bind (:map clojure-mode-map
              ("C-c w" . rac-start-sketch)
              ("C-c q" . rac-exit-sketch)
              ("C-c s" . replace-string)
              ("C-c C-s" . replace-string))
  :config
  (require 'flycheck-clj-kondo)
  (use-package clojure-mode-extra-font-locking)
  ;; Use clojure mode for other extensions
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode)))

(use-package dictcc)

(use-package dired-details
  :load-path "dired-details.el"
  :hook (dired-mode . dired-hide-details-mode)
  (framemove-hook-into-windmove t))


(use-package pdf-tools
  :defer t
  :config
  (pdf-loader-install))

(use-package ob-clojurescript)

(use-package visual-fill-column
  :hook
  (visual-line-mode . visual-fill-column-mode))


(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . (lambda ()
                      (face-remap-add-relative 'variable-pitch :family "Helvetica"
                             :height 1.0))))
(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package calfw
  :custom
  (calendar-week-start-day 1)
  (cfw:fchar-junction ?╋)
  (cfw:fchar-vertical-line ?┃)
  (cfw:fchar-horizontal-line ?━)
  (cfw:fchar-left-junction ?┣)
  (cfw:fchar-right-junction ?┫)
  (cfw:fchar-top-junction ?┯)
  (cfw:fchar-top-left-corner ?┏)
  (cfw:fchar-top-right-corner ?┓)
  :config
  (use-package calfw-org)
  (use-package calfw-ical))

(use-package typescript-mode
    :mode ("\\.ts\\'"
           "\\.tsx\\'"))

(use-package tide
  :custom
  ;; aligns annotation to the right hand side
  (company-tooltip-align-annotations t)
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  :hook (typescript-mode . setup-tide-mode)
  :config
  ;; formats the buffer before saving
  (remove-hook 'before-save-hook 'tide-format-before-save)

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package german-holidays
  :config
  (setq calendar-holidays holiday-german-BY-holidays))

(use-package ivy
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ( "C-x b" . ivy-switch-buffer))
  :custom-face
  (ivy-highlight-face ((t nil)))
  :custom
  (ivy-initial-inputs-alist nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-height 15)
  (ivy-count-format "%d/%d ")
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure-system-package (rg . ripgrep)
  :bind (("C-x m" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-M-i" . complete-symbol)
         ("C-." . counsel-imenu)
         ("C-c 8" . counsel-unicode-char)
         ( "C-h f" . counsel-describe-function)
         ( "C-h v" . counsel-describe-variable)
         ( "C-h l" . counsel-find-library)
         ( "C-h u" . counsel-unicode-char)
         ( "C-c f" . counsel-rg))
  :custom
  (counsel-yank-pop-separator "\n------------\n")
  (counsel-rg-base-command "rg --no-heading --line-number --color never %s .")
  (counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never %s"))

(use-package counsel-osx-app
    :bind (("C-c x" . counsel-osx-app)))

(use-package swiper)

(use-package smex)

(use-package eshell
  :ensure nil
  :bind (("M-r" . counsel-esh-history)
         :map eshell-mode-map
         ("M-r" . counsel-esh-history)))

;; Load auto-revert-mode for log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(use-package multi-term
  :bind
  (( "C-x c" . term-char-mode)
   ( "C-x l" . term-line-mode)
   ( "C-x t" . multi-term)
   ( "C-c t" . multi-term))
  :custom
  (multi-term-program "/bin/zsh"))

(when (file-exists-p "~/.emacs.d/mail.el")
  (progn
    (load-file "~/.emacs.d/mail.el")
    (require 'mail)))

(when (file-exists-p "~/.emacs.d/work.el")
  (load-file "~/.emacs.d/work.el"))

(when (file-exists-p "~/.emacs.d/calendar.el")
  (load-file "~/.emacs.d/calendar.el"))

(use-package mu4e
  :ensure nil
  :custom
  (mu4e-context-policy 'ask)
  (mu4e-hide-index-messages t)
  (message-send-mail-function  'smtpmail-send-it)
  (smtpmail-stream-type  'starttls)
  (smtpmail-smtp-service 587)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-images t)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 600)
  (mu4e-compose-signature-auto-include nil)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-bookmarks
  `( ,(make-mu4e-bookmark
	:name  "Unread messages"
	:query "flag:unread AND NOT flag:trashed AND NOT subject:JIRA*"
	:key ?u)
     ,(make-mu4e-bookmark
	:name "Today's messages"
	:query "date:today..now"
	:key ?t)
     ,(make-mu4e-bookmark
	:name "Last 7 days"
	:query "date:7d..now"
	:key ?w)))
  (mu4e-headers-skip-duplicates t)
  (message-kill-buffer-on-exit t)
  (mu4e-headers-fields
      '((:human-date . 12)
        (:flags . 6)
        (:mailing-list . 8)
        (:from-or-to . 25)
        (:thread-subject . 30)
        (:maildir . 40)))
  (mu4e-change-filenames-when-moving t)
  (smtpmail-queue-mail nil  ;; start in normal mode
      smtpmail-queue-dir   "~/Maildir/queue/cur")
  :config
  (require 'smtpmail)
  (require 'org-mu4e)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types)))

(when (file-exists-p "~/.emacs.d/dbs.el")
  (load-file "~/.emacs.d/dbs.el"))

(load-file "~/.emacs.d/frame-move.el")

(use-package slack
  :commands (slack-start)
  :ensure-system-package ("/Applications/Slack.app" . "brew cask install slack")
  :hook (slack-mode . (lambda () (set-input-method "german-postfix")))
  :bind (:map slack-mode-map
              ("@" . slack-message-embed-mention)
              ("#" . slack-message-embed-channel))
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t)
  (slack-typing-visibility 'never))

(when (file-exists-p "~/.emacs.d/slack.el")
  (load-file "~/.emacs.d/slack.el"))

(use-package counsel-spotify
  :ensure-system-package
  ("/Applications/Spotify.app" . "brew cask install spotify"))

(use-package inf-clojure)

(setq ivy-initial-inputs-alist nil)
(use-package pandoc-mode
  :hook markdown-mode
  :ensure-system-package pandoc)

(use-package csv-mode
  :hook (csv-mode . csv-align-mode)
  :mode ("\\.csv\\'" . csv-mode))

(use-package forge)

(use-package ox-jira)


(use-package quelpa-use-package)
(require 'quelpa-use-package)

(use-package so-long
  :quelpa (so-long :url "https://raw.githubusercontent.com/emacs-mirror/emacs/master/lisp/so-long.el" :fetcher url)
  :config (global-so-long-mode 1))

(use-package grab-mac-link)

