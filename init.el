;; set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

(setq theme-dir
      (expand-file-name "themes" user-emacs-directory))

(setq custom-file 
      (expand-file-name "custom.el" user-emacs-directory))

;; load paths
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path theme-dir)
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
   '(magit
     yasnippet
     smartparens
     js2-mode
     dired-details
     popup
     guide-key
     texinfo
     auctex
     ispell
     multi-term
     projectile
     restclient
     diminish
     csv-mode
     framemove
     powerline
     avy
     flycheck
     json-mode
     web-mode
     helm
     helm-projectile
     helm-spotify
     solarized-theme
     company
     crux
     undo-tree
     company-tern
     tern
     mocha
     elm-mode
     elm-yasnippets
     js2-refactor
     rainbow-mode
     kurecolor
     hydra
     alert
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
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'dired '(require 'setup-dired))
(require 'setup-yasnippet)
(require 'setup-smartparens)
(require 'setup-auctex)
(require 'setup-web-mode)
(require 'setup-flyspell)
(require 'setup-org)
(require 'setup-projectile)
(require 'setup-framemove)
(require 'setup-powerline)
(require 'neotree)
(require 'setup-jsdoc)
(require 'setup-flycheck)
(when (not is-mac)
  (require 'setup-pdf))
(require 'undo-tree)
(global-undo-tree-mode)
(require 'crux)
(require 'setup-mocha)
(require 'setup-tern)
(require 'setup-guidekey)
(require 'setup-hydras)
(require 'setup-emoji)
(require 'setup-company)
(eval-after-load 'elm-mode '(require 'setup-elm))
(require 'setup-alert)
(require 'setup-harvest)
