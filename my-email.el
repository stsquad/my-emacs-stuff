;; Email set-up
;
; Current experimenting with notmuch, mu4e and Gnus so expect
; this to be a bit of a mish-mash  of stuff for now.
;

;; Signature
(defun my-sig-function ()
  "Generate a signature"
  (concat "Alex Bennée"))

;; Global email details
;
(setq
 user-mail-address "ajb@cbnl.com"
 user-full-name  "Alex Bennée"
 message-signature 'my-sig-function)

;; SMTP setup
;
; To save messing with passwords and other complications I just
; use a port-forward to my main email server which doesn't ask
; too many questions for localhost punts only

; emacs24 only
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "localhost"
      smtpmail-smtp-server "localhost"
      smtpmail-smtp-service 2500)

;; mu4e setup
;
; Currently in the running as:
;   - fastest indexer
;   - doesn't crap out on my work email

(let ((mu4e-path "/home/ajb/src/emacs/mu.git/mu4e"))
  (when (file-exists-p mu4e-path)
    (add-to-list 'load-path mu4e-path)
    (setq mu4e-show-images t
          mu4e-html2text-command "html2text -utf8 -width 72")
    (require 'mu4e)))