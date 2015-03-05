(require 'yasnippet)

;; Activate yasnippet
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Manually define indents
(setq yas-indent-line 'fixed)


(provide 'setup-yasnippet)
