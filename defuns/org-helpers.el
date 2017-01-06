(require 's)

(defun rac-find-org-project ()
  "Switch to corresponding Project in Projects.org"
  (interactive)
  (let ((project (car (last (s-split "/" (s-trim (s-chop-suffix "/" (projectile-project-root))))))))
    (find-file "~/Dropbox/org/Projects.org")
    (widen)
    (beginning-of-buffer)
    (search-forward project)
    (org-narrow-to-subtree)
    (org-clock-display)))
