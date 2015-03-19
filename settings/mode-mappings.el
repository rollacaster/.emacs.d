;; Web-Mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook (lambda () (tern-mode t)))

;; LaTex mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(provide 'mode-mappings)
