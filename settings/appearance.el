(setq custom-theme-directory 
      (concat user-emacs-directory "themes"))

(add-to-list 'custom-theme-load-path custom-theme-directory)

(require 'color-theme-tomorrow)
(load-theme 'tomorrow-night-eighties t)

;; No splash screen
(setq inhibit-startup-message t)

;; Disable UI Elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Cutom faces
(custom-set-faces
 '(highlight ((t (:background "dim gray"))))
 '(magit-item-highlight ((t (:inherit highlight))))
 '(sp-pair-overlay-face ((t (:background "chartreuse" :foreground "black")))))

(provide 'appearance)
