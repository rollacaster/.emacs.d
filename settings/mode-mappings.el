;; Web-Mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook 'yas-minor-mode)
(add-hook 'web-mode-hook
          #'(lambda ()
              (define-key web-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key web-mode-map "@" 'js-doc-insert-tag)))

;; LaTex mode
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; JS2 Mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'yas-minor-mode)
(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key web-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key web-mode-map "@" 'js-doc-insert-tag)))
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;; Org mode
(add-hook 'org-capture 'auto-fill-mode)
(add-hook 'org-capture 'flyspell-mode)
(add-hook 'org-mode 'flyspell-mode)
(add-hook 'org-mode 'auto-fill-mode)

;; Rest-Client mode
(add-to-list 'auto-mode-alist '("\\.api\\'" . restclient-mode))

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'mode-mappings)
