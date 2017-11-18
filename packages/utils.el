(use-package s)

;; Setup env variables on mac
(when (equal system-type 'darwin)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(use-package framemove
  :config
  (setq framemove-hook-into-windmove t))

(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package multi-term
  :bind
  (( "C-x t" . multi-term)
   ( "C-c t" . multi-term)))

(use-package beginend
  :diminish beginend-global-mode
  :diminish beginend-prog-mode
  :diminish beginend-magit-status-mode
  :config
  (beginend-global-mode))

(use-package vlf   :config
  (require 'vlf-setup))

(use-package hideshow
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-\\" . hs-toggle-hiding)
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

(provide 'utils-packages)
