(use-package elm-mode
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm)
  (define-key elm-mode-map (kbd "C-c TAB") 'elm-mode-format-buffer)
  (add-hook 'elm-mode-hook 'yas-minor-mode))

(use-package npm-mode
  :diminish npm-mode
  :config
  (npm-global-mode))

(use-package meghanada
  :config
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
  (add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (remove-hook 'before-save-hook 'meghanada-code-beautify-before-save))))

(use-package prettier-js
  :diminish prettier-js-mode
  :config
  (add-hook 'js2-jsx-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '("--print-width 80" "--single-quote" "--no-semi")))

(use-package json-mode
  :bind (:map json-mode-map
              ("C-c TAB" . json-pretty-print))
  :mode "\\.json\\'")

(use-package js2-mode
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill)
              ("C-c s" . replace-string)
              ("C-c C-s" . replace-string))
  :config
  ;; Activate toggle indent with tab
  (setq js2-bounce-indent-p t)
  (define-key js2-mode-map (kbd "C-c C-t") nil)
  ;; Set basic offset to 2
  (setq js2-basic-offset 2)
  (setq js-indent-level 2)
  (setq sgml-basic-offset 2)
  (setq sgml-at 2)
  (setq json-reformat:indent-width 2)

  ;; Highlight as much as possible
  (setq js2-highlight-level 3)

  ;; Do not warn about missing semicolons
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-highlight-external-variables nil)
  (setq js2-strict-trailing-comma-warning nil)
  (setq js2-ignored-warnings '("msg.no.side.effects", "msg.no.paren", "msg.no.semi.stmt", "msg.unterminated.re.lit"))
  (js2-imenu-extras-setup)
  ;; Add externals
  (setq js2-global-externs '("describe", "it", "expect", "beforeEach", "sinon", "require"))
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (use-package js2-refactor
    :diminish js2-refactor-mode
    :config
    (js2r-add-keybindings-with-prefix "C-c C-j"))
  (use-package xref-js2)

  (defun er/add-js2-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(er/mark-html-attribute
                                er/mark-inner-tag
                                er/mark-outer-tag
                                ))))

  (add-hook 'js2-mode-hook 'er/add-js2-mode-expansions))

(use-package web-mode
  :mode "\\.html\\'"
  :config
  (setq web-mode-code-indent-offset 2)
  ;; No auto quote in html tags
  (setq web-mode-enable-auto-quoting nil)

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (eval-after-load 'web-mode
    '(define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file))

  (eval-after-load 'web-mode
    '(define-key web-mode-map (kbd "C-c RET") 'mc/mark-all-like-this-dwim))

  (define-key web-mode-map (kbd "C-;") 'company-web-html)
  (add-hook 'web-mode-hook (lambda ()
                             (set (make-local-variable 'company-backends) '(company-web-html company-files))
                             (company-mode t)))
  (add-hook 'web-mode-hook #'yas-minor-mode))

(use-package rjsx-mode
  :mode
  ("\\.js\\'" . rjsx-mode)
  :interpreter "node"
  :config
  (add-hook 'rjsx-mode-hook 'yas-minor-mode)
  (add-hook 'rjsx-mode-hook 'js2-refactor-mode)
  (add-hook 'rjsx-mode-hook 'rainbow-mode))

(use-package purescript-mode
  :mode "\\.purs\\'")

(use-package package-lint)
(use-package flycheck-package)

(provide 'programming-packages)
