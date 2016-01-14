(require 'projectile)
(projectile-global-mode)
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(diminish 'projectile-mode (format " [%s]" (projectile-project-name)))

(provide 'setup-projectile)
