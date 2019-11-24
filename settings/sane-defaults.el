;;; package --- Summary

;;; Commentary:

;;; Code:

;; When both the source file and its compiled bytecode are present load the
;; source file first.
(setq load-prefer-newer t)

;; disable sounds
(setq ring-bell-function 'ignore)

;; Auto refresh buffers and dired
(global-auto-revert-mode 1)
(setq global-auto-revert-none-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Show paren mode
(show-paren-mode 1)

;; Move to trash when deleting
(setq delete-by-moving-to-trash t)

;; No use of shift for marking
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show more results for apropos
(setq apropos-do-all t)

;; Remove text in active region when typing
(delete-selection-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Activate recent mode and set max saved item to 100
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo redo window configuration with C-c left/right
(winner-mode 1)

;; Show empty lines at buffer end
(set-default 'indent-tabs-mode nil)

;; Navigate thourgh CamelCase words
(global-subword-mode 1)

;; Truncate long lines
(setq-default truncate-lines t)

;; Increase gc memory size
(setq gc-cons-threshold 20000000)

;; Sentences do not need double space to end
(set-default 'sentence-end-double-space nil)

;; 80 chars width
(set-default 'fill-column 80)

;; Use uniquify
(setq uniquify-buffer-name-style 'forward)

;; No electric indent
(setq electric-indent-mode nil)

;; Kill process without asking
(setq kill-buffer-query-functions nil)

;; Allow to read from minibuffer while in minibuffer.
(setq enable-recursive-minibuffers t)

;; Show the minibuffer depth (when larger than 1)
(minibuffer-depth-indicate-mode 1)

;; Change recenter order for C-l
(setq recenter-positions '(top middle bottom))

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; write backup to own directories
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; make always backup files
(setq vc-make-backup-files t)

;; save point position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; enable forbidden commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; case search
(setq case-fold-search t)

;; Move files between split panes
(setq dired-dwim-target t)

(put 'dired-find-alternate-file 'disabled nil)

;; Save all pastes in kill ring
(setq save-interprogram-paste-before-kill t)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Non blinking cursor
(blink-cursor-mode 0)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(setq explicit-shell-file-name "/bin/bash")

;; German
(setq default-input-method "german-postfix")
(setq input-method-highlight-flag nil)
(add-hook 'text-mode-hook (lambda () (set-input-method "german-postfix")))
(set-input-method "german-postfix")

;; Browse
(setq browse-url-browser-function 'browse-url-default-browser)

;; add js to rgrep
;; (add-to-list 'grep-files-aliases '("js" . "*.js"))

;; Add week numbers to calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(provide 'sane-defaults)
;;; sane-defaults.el ends here
