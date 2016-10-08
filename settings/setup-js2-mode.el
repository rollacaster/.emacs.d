;; Activate toggle indent with tab
(setq js2-bounce-indent-p t)

;; Set basic offset to 2
(setq js2-basic-offset 2)
(setq js-indent-level 2)
(setq sgml-basic-offset 2)
(setq sgml-at 2)

;; Highlight as much as possible
(setq js2-highlight-level 3)

;; Do not warn about missing semicolons
(setq js2-strict-missing-semi-warning nil)
(setq js2-highlight-external-variables nil)
(setq js2-strict-trailing-comma-warning nil)

;; Add externals
(setq js2-global-externs '("describe", "it", "expect", "beforeEach", "sinon", "require"))

(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-j")

(provide 'setup-js2-mode)
