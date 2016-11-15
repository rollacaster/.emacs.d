(require 'solarized-light-theme)

;; No splash screen
(setq inhibit-startup-message t)

;; Disable UI Elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(provide 'appearance)
