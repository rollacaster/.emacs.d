;; No splash screen
(setq inhibit-startup-message t)

;; Disable UI Elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-hl-line-mode 1)

;; Allows to use fonts with ligatures
(mac-auto-operator-composition-mode)
(set-frame-font "-*-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-attribute 'default nil :height 140)

(provide 'appearance)
