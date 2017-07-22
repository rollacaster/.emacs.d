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
                   (mu4e-compose-signature . "")
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
                               (message maildir)
                               (and (s-contains? "Gmail" maildir) (not (s-contains? "integritynext" maildir))))
                           nil))
            :vars '((user-mail-address . "thomas.sojka@comsysto.com")
                    (user-full-name . "Thomas Sojka")
                    (mu4e-compose-signature . "
Thomas Sojka
Software Engineer

comSysto

comSysto GmbH
Tumblingerstr. 23
80337 München

Tel: +49 176 422 353 77
Fax: +49 89 550 605 90

http://comsysto.com/

Geschäftsführer:
Daniel Bartl
Tomislav Zorc

Handelsregister:
Amtsgericht München
HRB 147101

Wichtiger Hinweis: Diese E-Mail und etwaige Anlagen enthalten vertrauliche oder rechtlich geschützte Informationen. Wenn Sie nicht der vorgesehene Adressat dieser E-Mail oder dessen Vertreter sein sollten, so beachten Sie bitte, dass jede Form der Kenntnisnahme, Veröffentlichung, Vervielfältigung oder Weitergabe des Inhalts dieser E-Mail unzulässig ist. Wir bitten Sie, sich in diesem Fall mit dem Absender der E-Mail in Verbindung zu setzen.

Important Note: This e-mail and any attachment are confidential or protected by law. Access to this email by anyone else is unauthorized. If you are not the intended recipient, any form of disclosure, reproduction, distribution or any action taken or refrained from in reliance on it, is prohibited and may be unlawful. Please notify the sender immediately.
")
                    (mu4e-drafts-folder . "/Gmail/[Gmail].Drafts")
                    (mu4e-sent-folder   . "/Gmail/[Gmail].Sent Mail")
                    (mu4e-trash-folder  ."/Gmail/[Gmail].Trash")
                    (mu4e-refile-folder .  "/Gmail/Archives")
                    (mu4e-sent-messages-behavior . delete)))
           ,(make-mu4e-context
            :name "IntegrityNext"
            :enter-func (lambda () (mu4e-message "Switch to IntegrityNext"))
            :match-func (lambda (msg)
                         (if msg
                             (let ((maildir (mu4e-message-field-raw msg :maildir)))
                               (s-contains? "integritynext" maildir))
                           nil))
            :vars '((user-mail-address . "thomas.sojka@integritynext.com")
                    (user-full-name . "Thomas Sojka")
                    (mu4e-compose-signature . "")
                    (mu4e-drafts-folder . "/integritynext/[Gmail].Drafts")
                    (mu4e-sent-folder   . "/integritynext/[Gmail].Sent Mail")
                    (mu4e-trash-folder  ."/integritynext/[Gmail].Bin")
                    (mu4e-refile-folder .  "/integritynext/Archives")
                    (mu4e-sent-messages-behavior . delete)))))

(setq mu4e-maildir-shortcuts
      '( ("/Gmail/INBOX"               . ?a)
         ("/Gmail/[Gmail].Sent Mail"   . ?s)
         ("/Gmail/Archives"   . ?d)
         ("/Gmail/[Gmail].Trash"       . ?f)
         ("/Gmail/[Gmail].All Mail"    . ?g)
         ("/integritynext/INBOX"               . ?z)
         ("/integritynext/[Gmail].Gesendet"   . ?x)
         ("/integritynext/Archives"   . ?c)
         ("/integritynext/[Gmail].Trash"       . ?v)
         ("/integritynext/[Gmail].All Mail"    . ?b)
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
