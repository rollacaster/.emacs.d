(require 'flycheck)

;; Show list of flycheck errors on the bottom in a small window
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.2)))

(flycheck-add-mode 'html-tidy 'web-mode)
(flycheck-add-mode 'javascript-standard 'js2-jsx-mode)

(provide 'setup-flycheck)
