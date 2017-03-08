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
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-hook 'web-mode-hook #'yas-minor-mode)

;; JS2 Mode
(rename-modeline "js2-mode" js2-mode "JS2")
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'js-jsx-mode-hook #'yas-minor-mode)
(add-hook 'js-jsx-mode-hook #'js2-refactor-mode)
(add-hook 'js-jsx-mode-hook 'rainbow-mode)
(add-hook 'js-jsx-mode-hook 'rainbow-delimiters-mode)
(add-hook 'js2-mode-hook #'yas-minor-mode)
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

;; dired-mode
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; java mode
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

(provide 'mode-mappings)
