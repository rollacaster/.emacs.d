(require 'company-emoji)
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-to-list 'company-backends 'company-emoji)
(setq company-tern-property-marker "")

(provide 'setup-company)

