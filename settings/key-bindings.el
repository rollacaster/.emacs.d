;; Avoid quitting emacs by accident
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Change keys for faster editing
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c g") 'goto-line)

;; Use hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Helm
(global-set-key (kbd "C-c i") 'helm-command-prefix)
(global-set-key (kbd "C-c i o") 'helm-occur)
(global-set-key (kbd "C-c i g") 'helm-google-suggest)

(global-set-key (kbd "C-c i s") 'helm-spotify)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; Org-Mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c s") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; Windmove
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

;; Avy
(global-set-key (kbd "C-c n") 'avy-goto-char)
(global-set-key (kbd "C-c C-n") 'avy-goto-char)
(global-set-key (kbd "C-c ,") 'avy-copy-line)
(global-set-key (kbd "C-c m") 'avy-move-line)
(global-set-key (kbd "C-c .") 'avy-copy-region)

;; Neo
(global-set-key [f8] 'neotree-toggle)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; crux
(global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
(global-set-key (kbd "C-c TAB") 'crux-cleanup-buffer-or-region)
(global-set-key (kbd "C-x C-e") 'crux-eval-and-replace)
(global-set-key (kbd "C-c C-e") 'eval-last-sexp)

;; terminals
(global-set-key (kbd "C-x t") 'multi-term)
(global-set-key (kbd "C-c t") 'multi-term)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Replace String
(global-set-key (kbd "C-c s") 'replace-string)
(global-set-key (kbd "C-c C-s") 'replace-string)

(provide 'key-bindings)
