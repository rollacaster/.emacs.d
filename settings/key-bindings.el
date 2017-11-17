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

;; Org-Mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c s") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c o") 'org-iswitchb)

;; Windmove
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Replace String
(global-set-key (kbd "C-c s") 'replace-string)
(global-set-key (kbd "C-c C-s") 'replace-string)

;; Expand region
(global-set-key "\M-2" 'er/expand-region)
(global-set-key "\M-1" 'er/contract-region)

;; Multiple cursors
(global-set-key (kbd "C-c RET") #'mc/mark-all-like-this-dwim)

; Move-text
(global-set-key (kbd "M-P") 'move-text-up)
(global-set-key (kbd "M-N") 'move-text-down)

; Change keys for mac port
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

; Pomidor
(global-set-key (kbd "<f10>") #'pomidor)

; Dired
(global-set-key (kbd "C-x C-j") 'dired-jump)

; Beginend
(global-set-key (kbd "s-<") 'beginend-prog-mode-goto-beginning)
(global-set-key (kbd "s->") 'beginend-prog-mode-goto-end)

;; Narrowing
(global-set-key (kbd "C-x C-n") #'eos/narrow-or-widen-dwim)


(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-decrease)
(provide 'key-bindings)
