;; set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

;; load paths
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)
(load custom-file)

;; add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; write backup to own directories
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; make always backup files
(setq vc-make-backup-files t)

;; save point position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Mac test
(setq is-mac (equal system-type 'darwin))


(require 'sane-defaults)

;; setup packages
(require 'setup-package)

(defun init-install-packages ()
  (packages-install
   '(

     rainbow-mode
     use-package
     )
   )
  )

(condition-case nil
    (init-install-packages)
  (error
   (package-refresh-contents)
   (init-install-packages)
   )
  )

(require 'appearance)
(require 'key-bindings)

;; enable forbidden commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)


(package-initialize)

;; Setup env variables on mac
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; map files to modes
(require 'mode-mappings)


;; setup extensions
;; (require 'setup-conkeror)
(require 'setup-use-package)
(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind (( "C-x g" . magit-status)
         ( "C-x M-g" . magit-dispatch-popup)))
(eval-after-load 'dired '(require 'setup-dired))
(use-package noflet)
(require 'setup-org)
(use-package neotree
  :bind (([f8] . neotree-toggle)))
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))
(use-package crux
  :bind ( ("C-c r" . crux-rename-buffer-and-file)
          ("C-c TAB" . crux-cleanup-buffer-or-region)
          ("C-x C-e" . crux-eval-and-replace)
          ("C-c C-e" . eval-last-sexp)))
(use-package company
  :diminish company-mode
  :bind (("C-;" . company-complete)))
(use-package company-web)
(use-package company-emoji
  :config
  (add-to-list 'company-backends 'company-emoji))
(use-package harvest)
(require 'setup-mu)
(require 'setup-tide)
(use-package paredit
  :bind (:map paredit-mode-map
              ("C-M-s" . paredit-forward-slurp-sexp)
              ("C-M-b" . paredit-forward-slurp-sexp))
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
(use-package emojify
  :bind (("C-c e" . emojify-insert-emoji)))
(use-package elm-mode
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm)
  (define-key elm-mode-map (kbd "C-c TAB") 'elm-mode-format-buffer))
(use-package flycheck
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
  (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode))

(use-package framemove
  :config
  (setq framemove-hook-into-windmove t))

(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))
(use-package all-the-icons)
(use-package all-the-icons-dired)

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

(use-package powerline
  :config
  (powerline-default-theme))

(use-package projectile
  :config
  (projectile-global-mode)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "elm-stuff")
  (setq projectile-completion-system 'ivy))

(use-package smartparens
  :bind (:map smartparens-mode-map
              ("C-M-s" . sp-forward-slurp-sexp)
              ("C-M-b" . sp-forward-barf-sexp))
  :config
  (smartparens-global-mode 1))

(use-package yasnippet
  :config
  ;; Activate yasnippet
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  ;; Manually define indents
  (setq yas-indent-line 'fixed)

  ;; Load snippets
  (yas-reload-all))

(use-package npm-mode
  :config
  (npm-global-mode))
(use-package ox-jira)
(use-package ox-gfm)
(use-package meghanada)
(use-package restclient)

(use-package prettier-js
  :config
  (add-hook 'js2-jsx-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '("--single-quote" "--no-semi")))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (eval-after-load 'grep
    '(define-key grep-mode-map
       (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

  (eval-after-load 'wgrep
    '(define-key grep-mode-map
       (kbd "C-c C-c") 'wgrep-finish-edit)))

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
  (require 'ivy-hydra)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))

  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  (setq ivy-count-format "%d/%d ")
  (setq counsel-yank-pop-separator "\n------------\n")

  (use-package counsel-osx-app
    :bind (("C-c x" . counsel-osx-app))))

(use-package counsel-projectile
  :config
  (counsel-projectile-on))

(use-package which-key
  :config
  (which-key-mode))

(use-package avy
  :bind (( "C-c g" . avy-goto-line)
         ( "C-c v" . avy-goto-char)
         ( "C-c ," . avy-copy-line)
         ( "C-c m" . avy-move-line)
         ( "C-c ." . avy-copy-region)))
(use-package json-mode
  :mode "\\.json\\'")
(use-package multi-term
  :bind
  (( "C-x t" . multi-term)
   ( "C-c t" . multi-term)))
(use-package solarized-theme
  :config
  (require 'solarized-light-theme))

(use-package js2-mode
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill))
  :config
  ;; Activate toggle indent with tab
  (setq js2-bounce-indent-p t)

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
                             (company-mode t))))

(use-package auctex
  :defer t
  :config
  ;; Make AUCTex aware of style files and multi-files
  (setq TeX-parse-self t))

(use-package rjsx-mode)

(use-package beginend
  :config
  (beginend-global-mode))

(use-package org-download)

