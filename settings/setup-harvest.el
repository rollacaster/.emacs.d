(use-package harvest
    :ensure t
    :config
    (add-hook 'org-clock-in-hook 'harvest)
    (add-hook 'org-clock-out-hook 'harvest-clock-out))

(provide 'setup-harvest)
