;;; package --- Summary

;;; Commentary:

;;; Code:

;; Avoid quitting emacs by accident
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Change keys for faster editing
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
;; (global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c g") 'goto-line)

;; Use hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)

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
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

; Dired
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; Narrowing
(global-set-key (kbd "C-x C-n") #'eos/narrow-or-widen-dwim)

;; Unfill
(global-set-key (kbd "s-q") 'unfill-paragraph)

;; Scale
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-decrease)

;; Move downladed file to current folder
(global-set-key (kbd "C-c C-h") 'bjm/move-file-here)

;; Copy full path
(global-set-key (kbd "C-c C-p") 'copy-file-path)

;; goto config
(global-set-key (kbd "C-c i") (lambda () (interactive)(find-file "~/.emacs.d/init.el")))

;; Refresh buffer
(global-set-key (kbd "<f5>") (lambda () (interactive)(revert-buffer nil t)))

;; browse-url
(global-set-key (kbd "C-c u") 'browse-url)

;; eshell
(global-set-key (kbd "C-c t") (lambda () (interactive (eshell 'N))))
(global-set-key (kbd "C-x t") (lambda () (interactive (eshell 'N))))

;; switch-git
(global-set-key (kbd "C-x p") 'rac-switch-git)
(global-set-key (kbd "C-x C-p") 'rac-switch-git)

;; Connect to db
(global-set-key (kbd "C-x C-d") 'rac-connect-db)

(global-set-key (kbd "C-x e") 'explain-emacs)

;; TODO Call mac switch to locale
(global-set-key (kbd "C-z") (lambda () (interactive) (message "Switch the locale!")))

;; Calendar
(global-set-key (kbd "C-x j") 'rac-open-calendar)

;; Open localhost
(global-set-key (kbd "C-x o") 'rac-open-localhost)

;; grab mac-link
(global-set-key (kbd "s-c") 'grab-mac-link)

;; vterm-toggle
(global-set-key (kbd "C-c C-z") 'vterm-toggle)

;; Clocking
(global-set-key (kbd "C-c C-x i") '(lambda () (interactive) (org-clock-in '(4))))
(global-set-key (kbd "C-c C-x TAB") '(lambda () (interactive) (org-clock-in '(4))))
(global-set-key (kbd "C-c C-x o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "s-s") (lambda () (interactive) (insert "ß")))
(global-set-key (kbd "s-a") (lambda () (interactive) (insert "ä")))
(global-set-key (kbd "s-o") (lambda () (interactive) (insert "ö")))
(global-set-key (kbd "s-u") (lambda () (interactive) (insert "ü")))


(provide 'key-bindings)

;;; key-bindings.el ends here
