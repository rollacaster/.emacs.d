(require 'web-mode)

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

(provide 'setup-web-mode)
