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
   '(smartparens
     js2-mode
     dired-details
     popup
     texinfo
     auctex
     ispell
     multi-term
     projectile
     restclient
     diminish
     csv-mode
     avy
     json-mode
     web-mode
     helm
     helm-projectile
     helm-spotify
     solarized-theme
     elm-yasnippets
     js2-refactor
     rainbow-mode
     kurecolor
     ggtags
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
(require 'setup-helm)
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
(use-package magit)
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'dired '(require 'setup-dired))
(require 'setup-yasnippet)
(require 'setup-smartparens)
(require 'setup-auctex)
(require 'setup-web-mode)
(require 'setup-org)
(require 'setup-projectile)
(use-package neotree)
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))
(use-package crux)
(use-package company)
(use-package company-web)
(use-package company-emoji
  :config
  (add-to-list 'company-backends 'company-emoji))
(use-package harvest)
(require 'setup-mu)
(require 'setup-tide)
(use-package paredit
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
  (flycheck-add-mode 'javascript-standard 'js2-jsx-mode))

(use-package framemove
  :config
  (setq framemove-hook-into-windmove t))

(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
  (setq guide-key/highlight-command-regexp
        '("rectangle"
          ("register" . font-lock-type-face)
          ("bookmark" . "hot pink")))
  (guide-key-mode 1))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
(use-package all-the-icons)
(use-package all-the-icons-dired)

(use-package hydra
  :config
  (eval-after-load 'css-mode '(defhydra hydra-color (css-mode-map "C-c C-c")
                                "color"
                                ("u" kurecolor-increase-hue-by-step "increase hue")
                                ("j" kurecolor-decrease-hue-by-step "decreasee hue")
                                ("i" kurecolor-increase-saturation-by-step "increase saturation")
                                ("k" kurecolor-decrease-saturation-by-step "decrease saturation")
                                ("o" kurecolor-increase-brightness-by-step "increase brightness")
                                ("l" kurecolor-decrease-brightness-by-step "decrease brightness"))))

(use-package powerline
  :config
  (powerline-default-theme))
