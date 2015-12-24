(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
)

(setq js2-highlight-level 3)
(setq js2-strict-missing-semi-warning nil)
(setq js2-global-externs '("describe", "it", "expect" "beforeEach", "sinon"))

(provide 'setup-js2-mode)
