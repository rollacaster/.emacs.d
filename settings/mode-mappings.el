;; JSX Mode
(rename-modeline "js2-mode" js2-jsx-mode "JSX")
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-hook 'js2-jsx-mode 'yas-minor-mode)
(add-hook 'js2-jsx-mode #'js2-refactor-mode)
(add-hook 'js2-jsx-mode 'rainbow-mode)

;; LaTex mode
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; JS2 Mode
(rename-modeline "js2-mode" js2-mode "JS2")
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'js2-mode-hook #'yas-minor-mode)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)
(add-hook 'typescript-mode-hook #'js2-refactor-mode)

;; Rest-Client mode
(add-to-list 'auto-mode-alist '("\\.api\\'" . restclient-mode))

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Elm mode
(add-hook 'elm-mode-hook #'yas-minor-mode)

;; CSS-mode
(add-hook 'css-mode-hook 'rainbow-mode)

(provide 'mode-mappings)
