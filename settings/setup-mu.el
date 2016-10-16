(add-to-list 'load-path "/Users/thomas/.emacs.d/site-lisp/mu/mu4e")

(require 's)
(require 'mu4e)

(setq mu4e-context-policy 'ask)

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Home"
           :enter-func (lambda () (mu4e-message "Switch to Home"))
           :match-func (lambda (msg)
                         (if msg
                             (let ((maildir (mu4e-message-field-raw msg :maildir)))
                               (s-contains? "Web" maildir))
                           nil))
           :vars '((user-mail-address . "thsojka@web.de")
                   (user-full-name . "Thomas Sojka")
                   (mu4e-compose-signature . "VG Thomas")
                   (mu4e-drafts-folder . "/Web/Drafts")
                   (mu4e-sent-folder   . "/Web/Sent")
                   (mu4e-trash-folder  ."/Web/Trash")
                   (mu4e-refile-folder .  "/Web/Archives")
                   (mu4e-sent-messages-behavior . sent)))
         , (make-mu4e-context
            :name "comSysto"
            :enter-func (lambda () (mu4e-message "Switch to comSysto"))
            :match-func (lambda (msg)
                         (if msg
                             (let ((maildir (mu4e-message-field-raw msg :maildir)))
                               (s-contains? "Gmail" maildir))
                           nil))
            :vars '((user-mail-address . "thomas.sojka@comsysto.com")
                    (user-full-name . "Thomas Sojka")
                    (mu4e-compose-signature . "VG Thomas")
                    (mu4e-drafts-folder . "/Gmail/[Gmail].Drafts")
                    (mu4e-sent-folder   . "/Gmail/[Gmail].Sent Mail")
                    (mu4e-trash-folder  ."/Gmail/[Gmail].Trash")
                    (mu4e-refile-folder .  "/Gmail/Archives")
                    (mu4e-sent-messages-behavior . delete)))))

(setq mu4e-maildir-shortcuts
      '( ("/Gmail/INBOX"               . ?a)
         ("/Gmail/[Gmail].Sent Mail"   . ?s)
         ("/Gmail/Archives"   . ?d)
         ("/Gmail/[Gmail].Trash"       . ?f)
         ("/Gmail/[Gmail].All Mail"    . ?r)
         ("/Web/INBOX"               . ?q)
         ("/Web/Sent"   . ?w)
         ("/Web/Archives"       . ?e)
         ("/Web/Trash"       . ?r)))

;; This sets `mu4e-user-mail-address-list' to the concatenation of all
;; `user-mail-address' values for all contexts.
(setq mu4e-user-mail-address-list
      (delq nil
            (mapcar (lambda (context)
                      (when (mu4e-context-vars context)
                        (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
                    mu4e-contexts)))

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-use-fancy-chars t)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; Get mail every 10 minutes
(setq mu4e-update-interval 600)

;; Signature
(setq mu4e-compose-signature-auto-include nil)

;; Do not reply to myself
(setq mu4e-compose-dont-reply-to-self t)

;; Set bookmarks
(add-to-list 'mu4e-bookmarks
             '("list:fcb@comsysto.com" "FCB" ?f))

                                        ; Skip duplicate mails
(setq mu4e-headers-skip-duplicates t)


;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)

;; Setup for sending mails
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(setq qmu4e-headers-fields
      '((:human-date . 12)
        (:flags . 6)
        (:mailing-list . 8)
        (:from-or-to . 25)
        (:thread-subject . 30)
        (:maildir . 9)))

(provide 'setup-mu)