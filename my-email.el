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
(setq message-send-mail-function 'smtpmail-send-it)

(eval-after-load "smtpmail"
  (setq smtpmail-default-smtp-server "localhost"
        smtpmail-smtp-server "localhost"
        smtpmail-smtp-service 2500))

;; Utility functions for email

(defun my-snip-region (beg end)
  "A wrapper around kill region that inserts a <snip> tag to
yanked text if it started as a quoted email"
  (interactive (list (point) (mark)))
  (kill-region beg end)
  (when (string-prefix-p ">" (car kill-ring))
    (insert "<snip>\n")))

;; mu4e setup
;
; Currently in the running as:
;   - fastest indexer
;   - doesn't crap out on my work email

(let ((mu4e-path (concat (getenv "HOME") "/src/emacs/mu.git/mu4e")))
  (when (file-exists-p mu4e-path)
    (add-to-list 'load-path mu4e-path)
    (setq mail-user-agent 'mu4e-user-agent)
    (setq mu4e-show-images t
          mu4e-headers-skip-duplicates t
          mu4e-header-include-related t
          mu4e-html2text-command "html2text -utf8 -width 72"
          mu4e-view-fields
          '(:from :to :cc :subject :flags :date :tags :attachments :signature)
          mu4e-maildir-shortcuts
          '( ("/linaro/Inbox"     . ?i)
             ("/linaro/qemu"      . ?q)
             ("/linaro/kvm"       . ?k)
             ("/linaro/mythreads" . ?m)
             ("/developer/emacs"  . ?e)
             ("/sent"             . ?s)))
    (autoload 'mu4e "mu4e")
    (global-set-key (kbd "C-c m") 'mu4e)
    (eval-after-load "mu4e"
      '(progn
         ; key-bindings
         (define-key mu4e-compose-mode-map (kbd "C-w") 'my-snip-region)
         ; pre-canned searches
         (add-to-list
          'mu4e-headers-actions
          '("gapply git patches" . mu4e-action-git-apply-patch) t)
         (add-to-list
          'mu4e-view-actions
          '("gapply git patch" . mu4e-action-git-apply-patch) t)
         (add-to-list
	  'mu4e-bookmarks
	  '("\(to:alex.bennee or cc:alex.bennee\) and \( \(reviewed ADJ by\) OR \(signed ADJ off ADJ by\) \)"
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
	  '("maildir:\"/linaro/qemu\" and flag:unread"
	    "Latest QEMU posts" ?q))
	 (add-to-list
	  'mu4e-bookmarks
	  '("flag:flagged" "Flagged and Starred posts" ?f))))))
