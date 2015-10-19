;; Web-Mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook (lambda () (tern-mode t)))
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

;; Org mode
(add-hook 'org-capture 'auto-fill-mode)
(add-hook 'org-capture 'flyspell-mode)
(add-hook 'org-mode 'flyspell-mode)
(add-hook 'org-mode 'auto-fill-mode)

;; Rest-Client mode
(add-to-list 'auto-mode-alist '("\\.api\\'" . restclient-mode))

(provide 'mode-mappings)
