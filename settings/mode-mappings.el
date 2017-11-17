;; LaTex mode
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-hook 'web-mode-hook #'yas-minor-mode)

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
            (remove-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

(provide 'mode-mappings)
