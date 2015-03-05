(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs/ac-dict")
(ac-config-default)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(provide 'setup-autocomplete)
