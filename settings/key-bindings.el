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
(global-set-key (kbd "C-c n") 'avy-goto-char-2)
(global-set-key (kbd "C-c m") 'avy-copy-line)
(global-set-key (kbd "C-c ,") 'avy-move-line)
(global-set-key (kbd "C-c .") 'avy-copy-region)

;; Neo
(global-set-key [f8] 'neotree-toggle)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(provide 'key-bindings)
