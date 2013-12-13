;;; my-email.el --- Email set-up
;;
;;; Commentary:
;; After with notmuch, mu4e and Gnus I went with mu4e mainly due to
;; speed and apparent simplicity of customisation.
;;
;;; Code:

(require 'my-vars)
(require 'message)

; Currently I compile my own...
(let ((mu4e-path (concat (getenv "HOME") "/src/emacs/mu.git/mu4e")))
  (when (file-exists-p mu4e-path)
    (add-to-list 'load-path mu4e-path)))

(require 'mu4e nil t)


;; Signature
(defun my-sig-function ()
  "Generate a signature."
  (interactive)
  (concat "Alex Bennée"))

;; Global email details
;
(setq
 user-mail-address "alex.bennee@linaro.org"
 user-full-name  "Alex Bennée"
 mail-signature '(insert (concat "\n--\n" (my-sig-function)))
 message-signature 'my-sig-function)

; Magic handling for multiple email addrsses
(defvar my-email-address-alist
  '( ("Maildir/developer" . "kernel-hacker@bennee.com")
     ("Maildir/linaro" . "alex.bennee@linaro.org")
     ("Maildir/personal" . "alex@bennee.com") )
  "List of mappings from Maildir->email address")

(defun my-choose-mail-address ()
  "Pick a new value of `user-mail-address' based on the parent
email. Sadly this is not a local variable as at the time of the
hook we are not yet in the compose buffer."
  (when mu4e-compose-parent-message
    (setq
     user-mail-address
     (assoc-default
      (plist-get mu4e-compose-parent-message :path)
      my-email-address-alist 'string-match))))

(add-hook 'mu4e-compose-pre-hook 'my-choose-mail-address)

;; SMTP setup
;
; To save messing with passwords and other complications I just
; use a port-forward to my main email server which doesn't ask
; too many questions for localhost punts only

; emacs24 only
(setq message-send-mail-function 'smtpmail-send-it)

(eval-after-load "smtpmail"
  (setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
        smtpmail-queue-dir   "~/Maildir/queue/cur"
        smtpmail-default-smtp-server "localhost"
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


(defun my-reset-mu4e-caches ()
  "Reset some of mu4e's caches to avoid restarting."
  (interactive)
  (setq mu4e~maildir-list nil))

(setq mu4e-compose-complete-only-personal nil)
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-view-show-images t
      mu4e-use-fancy-chars t
      mu4e-headers-skip-duplicates t
      mu4e-headers-include-related t
      mu4e-view-fill-headers nil
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

(when (or I-am-at-work I-am-at-home)
  (setq mu4e-get-mail-command
        "mbsync linaro-sync developer-sync"
        mu4e-update-interval 600))

(when I-am-on-pixel
  (setq mu4e-get-mail-command "true"))

(when I-am-at-work
  (setq mu4e-user-mail-address-list '("alex.bennee@linaro.org")
        mu4e-compose-complete-only-after "2013-11-01"))

(autoload 'mu4e "mu4e")
(global-set-key (kbd "C-c m") 'mu4e)
(eval-after-load "mu4e"
  '(progn
                                        ; key-bindings
     (define-key mu4e-compose-mode-map (kbd "C-w") 'my-snip-region)
                                        ; mode hooks
     (add-hook 'mu4e-headers-mode-hook
               '(lambda () (yas-minor-mode -1)))
                                        ; pre-canned searches
     (add-to-list
      'mu4e-headers-actions
      '("gapply git patches" . mu4e-action-git-apply-patch) t)
     (add-to-list
      'mu4e-view-actions
      '("mgit am patch" . mu4e-action-git-apply-mbox) t)
     (add-to-list
      'mu4e-bookmarks
      '("\(to:alex.bennee or cc:alex.bennee\) and \( \(reviewed ADJ by\) OR \(signed ADJ off ADJ by\) \)"
        "Mail addressed to me with git tags" ?g))
     (add-to-list
      'mu4e-bookmarks
      '("\(to:alex.bennee or cc:alex.bennee\) AND flag:unread"
        "Unread posts addressed to me" ?m))
     (add-to-list
      'mu4e-bookmarks
      '("from:alex.bennee"
        "Mail sent by me" ?s))
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
      '("maildir:\"/linaro/qemu\" AND (aarch64 OR arm64)"
        "QEMU ARM64 posts" ?a))
     (add-to-list
      'mu4e-bookmarks
      '("flag:flagged" "Flagged and Starred posts" ?f))))

(defun my-insert-pull-request ()
  "Insert basic pull request into buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (insert
     (shell-command-to-string
      (format
       "git request-pull %s http://github.com/stsquad/qemu.git HEAD"
       (ido-completing-read
        "Commit start:" '("HEAD~" "origin/master") 'nil))))))

(provide 'my-email)
;;; my-email.el ends here
