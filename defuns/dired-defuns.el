;; dired
(require 'dired)

(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))

;; Easier delte in dired
(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
(define-key dired-mode-map (kbd "k") 'dired-do-delete)

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)
(eval-after-load "wdired"
     '(define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files))
