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

(require 'appearance)

;; add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
  (add-to-list 'load-path project)))

;; write backup to own directories
(setq backup-directory-alist
      `(("." . ,(expand-file-name
           (concat user-emacs-directory "backups")))))

;; make always backup files
(setq vc-make-backup-files t)

;; save point position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


(require 'sane-defaults)
(require 'key-bindings)

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
     tern
     auto-complete
     tern-auto-complete
     guide-key
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

;; enable forbidden commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; setup extensions
;; (require 'setup-conkeror)
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'dired '(require 'setup-dired))
(require 'setup-yasnippet)
(require 'setup-autocomplete)
;;(require 'setup-guide-key)
(require 'setup-smartparens)
(require 'setup-tern)
;; (require 'setup-web-mode)
(require 'setup-ido)
(require 'setup-smex)

;; map files to modes
(require 'mode-mappings)




