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

;; Windmove
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

;; Replace String
(global-set-key (kbd "C-c s") 'replace-string)
(global-set-key (kbd "C-c C-s") 'replace-string)

; Move-text
(global-set-key (kbd "M-P") 'move-text-up)
(global-set-key (kbd "M-N") 'move-text-down)

; Change keys for mac port
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

; Dired
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; Narrowing
(global-set-key (kbd "C-x C-n") #'eos/narrow-or-widen-dwim)

;; Unfill
(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Scale
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-decrease)

;; Move downladed file to current folder
(global-set-key (kbd "C-c C-h") 'bjm/move-file-here)

;; Copy full path
(global-set-key (kbd "C-c C-p") 'copy-file-path)

(global-set-key (kbd "C-c i") (lambda () (interactive)(find-file "~/.emacs.d/init.el")))

;; Refresh buffer
(global-set-key (kbd "<f5>") (lambda () (interactive)(revert-buffer nil t)))

;; browse-url
(global-set-key (kbd "C-c u") 'browse-url)

(provide 'key-bindings)
