;; Activate toggle indent with tab
(setq js2-bounce-indent-p t)

;; Set basic offset to 2
(setq js2-basic-offset 2)

;; Highlight as much as possible
(setq js2-highlight-level 3)

;; Do not warn about missing semicolons
(setq js2-strict-missing-semi-warningl nil)

;; Add externals
(setq js2-global-externs '("describe", "it", "expect" "beforeEach", "sinon"))

(provide 'setup-js2-mode)
