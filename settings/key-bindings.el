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

(global-set-key (kbd "C-x s") 'helm-spotify)
(global-set-key (kbd "C-x a") 'helm-execute-kmacro)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; Org-Mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c s") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-x o") 'rac-find-org-project)

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

;; Move
(global-set-key "\M-9" 'backward-sexp)
(global-set-key "\M-0" 'forward-sexp)

;; Expand mark
(global-set-key "\M-2" 'er/expand-region)

;; Multiple cursors
(global-set-key (kbd "C-c RET") #'mc/mark-all-like-this-dwim)
(global-set-key (kbd "M-3") #'mc/mark-next-like-this)
(global-set-key (kbd "M-4") #'mc/mark-previous-like-this)
(global-set-key (kbd "M-#") #'mc/unmark-next-like-this)
(global-set-key (kbd "M-$") #'mc/unmark-previous-like-this)

(global-set-key (kbd "s-p") 'projectile-switch-project)
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-d") 'projectile-find-dir)
(global-set-key (kbd "s-g") 'projectile-grep)
(global-set-key (kbd "s-s") 'projectile-ag)
(global-set-key (kbd "s-k") 'projectile-kill-buffers)

; Move-text
(global-set-key (kbd "M-P") 'move-text-up)
(global-set-key (kbd "M-N") 'move-text-down)

; Company mode
(global-set-key (kbd "C-;") 'company-complete)

; Change keys for mac port
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(provide 'key-bindings)
