(use-package org-beautify-theme
  :config
  (require 'org-beautify-theme))

(use-package solarized-theme
  :config
  (require 'solarized-light-theme))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(provide 'appearance-packages)
