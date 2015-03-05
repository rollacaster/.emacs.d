(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
)

(setq js2-highlight-level 3)

(add-hook 'js2-mode-hook 'ac-js2-mode)

(provide 'setup-js2-mode)
