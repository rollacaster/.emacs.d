;; JSX Mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-hook 'js2-jsx-mode 'yas-minor-mode)

;; LaTex mode
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; JS2 Mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'yas-minor-mode)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;; Org mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-capture 'auto-fill-mode)
(add-hook 'org-capture 'flyspell-mode)
(add-hook 'org-mode 'flyspell-mode)
(add-hook 'org-mode 'auto-fill-mode)
(add-hook 'org-mode 'org-bullets-mode)

;; Rest-Client mode
(add-to-list 'auto-mode-alist '("\\.api\\'" . restclient-mode))

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'mode-mappings)
