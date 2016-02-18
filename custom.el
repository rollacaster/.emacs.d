(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((mocha-options . "--compilers js:./test/compiler;")
     (mocha-project-test-directory . "src/**/*.spec.js?(x)")
     (mocha-project-test-directory . "**/*.spec.js")
     (mocha-options . "--compilers --require babel-core/register")
     (mocha-project-test-directory . "'src/**/*.spec.js?(x)'")
     (mocha-options . "--require babel-core/register")
     (mocha-environment-variables . "NODE_ENV=test")
     (mocha-which-node . "/Users/thomas/.nvm/versions/node/v4.2.4/bin/node")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
