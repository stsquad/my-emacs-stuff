;; Email set-up
;
; Current experimenting with notmuch, mu4e and Gnus so expect
; this to be a bit of a mish-mash  of stuff for now.
;

;; Signature
(defun my-sig-function ()
  "Generate a signature"
  (interactive)
  (concat "Alex Bennée"))

;; Global email details
;
(setq
 user-mail-address "alex.bennee@linaro.org"
 user-full-name  "Alex Bennée"
 mail-signature '(insert (concat "\n--\n" (my-sig-function)))
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

(let ((mu4e-path (concat (getenv "HOME") "/src/emacs/mu.git/mu4e")))
  (when (file-exists-p mu4e-path)
    (add-to-list 'load-path mu4e-path)
    (setq mu4e-show-images t
          mu4e-headers-skip-duplicates t
          mu4e-header-include-related t
          mu4e-html2text-command "html2text -utf8 -width 72"
          mu4e-view-fields
          '(:from :to :cc :subject :flags :date :tags :attachments :signature)
          mu4e-maildir-shortcuts
          '( ("/Inbox"     . ?i)
             ("/qemu"      . ?q)
             ("/kvm"       . ?k)
             ("/mythreads" . ?m)
             ("/sent"      . ?s)))
    (autoload 'mu4e "mu4e")
    (global-set-key (kbd "C-c m") 'mu4e)
    (eval-after-load "mu4e"
      '(progn
         (add-to-list
	  'mu4e-bookmarks
	  '("\(to:alex.bennee or cc:alex.bennee\) and \( \(reviewed AND by\) OR \(signed AND off AND by\) \)"
	    "Mail addressed to me with git tags" ?g))
         (add-to-list
	  'mu4e-bookmarks
	  '("\(to:alex.bennee or cc:alex.bennee\) and \(flag:unread or flag:flagged\)"
	    "Flagged or unread posts addressed to me" ?m))
	 (add-to-list
	  'mu4e-bookmarks
	  '("from:linaro.org and flag:unread"
	    "Latest unread Linaro posts" ?l))
	 (add-to-list
	  'mu4e-bookmarks
	  '("maildir:\"/qemu\" and flag:unread"
	    "Latest QEMU posts" ?q))
	 (add-to-list
	  'mu4e-bookmarks
	  '("flag:flagged" "Flagged and Starred posts" ?f))))))
