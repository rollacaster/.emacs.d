;; Change keys for faster editing
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-c g") 'goto-line)

;; Use hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Smart M-x
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x m") 'smex)

;; Org-Mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c s") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; Windmove
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c l")  'windmove-down)

;; Avy
(global-set-key (kbd "C-c n") 'avy-goto-char-2)
(global-set-key (kbd "C-c m") 'avy-copy-line)
(global-set-key (kbd "C-c ,") 'avy-move-line)
(global-set-key (kbd "C-c .") 'avy-copy-region)

;; Neo
(global-set-key [f8] 'neotree-toggle)

(provide 'key-bindings)
