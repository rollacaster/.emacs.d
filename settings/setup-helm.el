(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(define-key helm-map (kbd "M-v")  'helm-previous-source) ; list actions using C-z
(define-key helm-map (kbd "C-v")  'helm-next-source) ; list actions using C-z


(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t ; Use `recentf-list' instead of `file-name-history' in `helm-find-files'
      helm-ff-auto-update-initial-value     t ; Try to auto update
      helm-ff-find-url-at-point             t ; Open URLs at point
      helm-ff-newfile-prompt-p            nil
      helm-autoresize-mode                  t
      ;;Enable fuzzymatching
      helm-M-x-fuzzy-match                  t 
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-apropos-fuzzy-match              t)

(helm-mode 1)
(helm-autoresize-mode t)

(provide 'setup-helm)
