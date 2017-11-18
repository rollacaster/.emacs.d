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

;; load paths
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
;; add external projects to load path
(dolist (project (directory-files (expand-file-name "site-lisp" user-emacs-directory) t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'appearance)
(require 'sane-defaults)
(require 'key-bindings)

(require 'utils-packages)
(require 'appearance-packages)
(require 'editing-packages)
(require 'programming-packages)
(require 'mail-packages)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))
