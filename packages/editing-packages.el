(use-package hideshow
  :bind (("C-\\" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((java-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil)))))

(use-package expand-region
  :bind (("M-2" . er/expand-region)
         ("M-1" . er/contract-region)))

(use-package rainbow-mode
  :diminish rainbow-mode)

(use-package emojify
  :bind (("C-c e" . emojify-insert-emoji)))

(use-package hydra
  :config
  (use-package kurecolor
    :config
    (eval-after-load 'css-mode '(defhydra hydra-color (css-mode-map "C-c C-c")
                                  "color"
                                  ("u" kurecolor-increase-hue-by-step "increase hue")
                                  ("j" kurecolor-decrease-hue-by-step "decreasee hue")
                                  ("i" kurecolor-increase-saturation-by-step "increase saturation")
                                  ("k" kurecolor-decrease-saturation-by-step "decrease saturation")
                                  ("o" kurecolor-increase-brightness-by-step "increase brightness")
                                  ("l" kurecolor-decrease-brightness-by-step "decrease brightness")))))

(use-package smartparens
  :diminish smartparens-mode
  :init (smartparens-global-mode 1)
  :bind (:map smartparens-mode-map
              ("C-)" . sp-forward-slurp-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-{" . sp-backward-barf-sexp)))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (eval-after-load 'grep
    '(define-key grep-mode-map
       (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

  (eval-after-load 'wgrep
    '(define-key grep-mode-map
       (kbd "C-c C-c") 'wgrep-finish-edit)))

(use-package avy
  :bind (( "C-c g" . avy-goto-line)
         ( "C-'" . avy-goto-char)
         ( "C-c ," . avy-copy-line)
         ( "C-c m" . avy-move-line)
         ( "C-c ." . avy-copy-region)))

(use-package auctex
  :mode "\\.tex\\'"
  :defer t
  :config
  ;; Make AUCTex aware of style files and multi-files
  (setq TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(use-package paredit
  :diminish paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package company
  :diminish company-mode
  :bind (("C-;" . company-complete))
  :init (global-company-mode)
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-web)
(use-package company-emoji
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  :config
  ;; Show list of flycheck errors on the bottom in a small window
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2)))

  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  ;; Activate yasnippet
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  ;; Manually define indents
  (setq yas-indent-line 'fixed)

  ;; Load snippets
  (yas-reload-all))

(use-package restclient
 :mode ("\\.api\\'" . restclient-mode))

(use-package multiple-cursors
  :bind (( "C-c RET" . mc/mark-all-like-this-dwim)
         ( "M-3" . mc/mark-next-like-this)
         ( "M-4" . mc/mark-previous-like-this)
         ( "M-#" . mc/unmark-next-like-this)
         ( "M-$" . mc/unmark-previous-like-this)))

(provide 'editing-packages)
