(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(setq guide-key/highlight-command-regexp
      '("rectangle"
        ("register" . font-lock-type-face)
        ("bookmark" . "hot pink")))
(guide-key-mode 1)
(provide 'setup-guide-key)
