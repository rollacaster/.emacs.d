(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-to-list 'company-backends 'company-elm)

(define-key elm-mode-map (kbd "C-c TAB") 'elm-mode-format-buffer)

(provide 'setup-elm)
