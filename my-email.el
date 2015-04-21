;;; my-email.el --- Email set-up
;;
;;; Commentary:
;;
;; After with notmuch, mu4e and Gnus I went with mu4e mainly due to
;; speed and apparent simplicity of customisation.
;;
;;; Code:

(require 'use-package)
(require 'my-vars)

(use-package smtpmail
  :commands smtpmail-send-queued-mail
  :config
  (setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
        smtpmail-queue-dir   "~/Maildir/queue/cur"
        smtpmail-default-smtp-server "localhost"
        smtpmail-smtp-server "localhost"
        smtpmail-smtp-service 2500))


;; Signature
(defun my-sig-function ()
  "Generate a signature."
  (interactive)
  (concat "Alex Bennée"))

;; Simple mail-mode and message-mode hooks.
;;
;; Ostensibly they both do the same thing however message-mode (and
;; the derived mu4e-compose-mode) assume they are sending from within
;; emacs. So I'll use the convention that I'll use mail-mode for
;; edit-server spawned mails and message-mode for the rest

(defun my-common-mail-tweaks ()
  "Enable common mail tweaks for sending messages."
  (interactive)
  (turn-on-flyspell)
  (turn-on-auto-fill))

(defun my-mail-mode-tweaks()
  "Customise mail-mode stuff"
  (interactive)
  (my-common-mail-tweaks)
  (when (and
         buffer-file-name
         (or
          (string-match "/tmp/mutt" buffer-file-name)
          (string-match "gitsend" buffer-file-name)))
    (define-key (current-local-map) (kbd "C-c C-c") 'server-edit)
    (define-key (current-local-map) (kbd "C-c C-s") 'server-edit)))

(use-package mail-mode
  ;; Enable mail-mode for mutt spawned files
  :mode (("/tmp/mutt-*" . mail-mode)
         ("0000-cover-letter.patch" . mail-mode)
         (".*/\.git/\.gitsendemail.MSG.*" . mail-mode))
  :config (add-hook 'mail-mode-hook 'my-mail-mode-tweaks))

(use-package message-mode
  :commands message-mode
  :config (add-hook 'message-mode-hook 'my-common-mail-tweaks))

;;
;; Finally the mu4e configuration
;;
;; This is my main work horse for day to day email.
;;

(use-package mu4e
  :commands mu4e
  :preface
  (progn
    ;; Switch function
    (defun my-switch-to-mu4e ()
      "Smart dwim switch to mu4e."
      (interactive)
      (if (get-buffer "*mu4e-headers*")
          (progn
            (switch-to-buffer "*mu4e-headers*")
            (delete-other-windows))
        (mu4e)))
    ;; Set default directory when viewing messages
    (defvar my-mailing-list-dir-mapping
      '( ("qemu-devel.nongnu.org" . "~/lsrc/qemu/qemu.git/")
         ("kvmarm.lists.cs.columbia.edu" . "~/lsrc/kvm/linux.git/") )
      "Mapping from mailing lists to source tree.")
    (defvar my-maildir-mapping
      '( ("linaro/virtualization/.qemu" . "~/lsrc/qemu/qemu.git/")
         ("linaro/kernel" . "~/lsrc/kvm/linux.git/") )
      "Mapping from maildirs to source tree.")
    (defun my-set-view-directory ()
      "Switch the `default-directory' depending on the mailing list we
  are in."
      (interactive)
      (let ((list (mu4e-message-field (mu4e-message-at-point)
                                      :mailing-list))
            (maildir (mu4e-message-field (mu4e-message-at-point)
                                         :maildir)))
        (setq default-directory
              (expand-file-name
               (or
                (assoc-default list my-mailing-list-dir-mapping)
                (assoc-default maildir my-maildir-mapping 'string-match)
                "~")))))
  :bind ("C-c m" . my-switch-to-mu4e)
  :config
  (progn
    (require 'mu4e-vars)
    ;; config options
    (setq
     ;; generic mail options
     user-mail-address "alex.bennee@linaro.org"
     user-full-name  "Alex Bennée"
     mail-signature '(insert (concat "\n--\n" (my-sig-function)))
     mail-user-agent 'mu4e-user-agent
     ;; message functions
     message-signature 'my-sig-function
     message-send-mail-function 'smtpmail-send-it
     ;; mu4e functions
     ;; mail fetch
     mu4e-get-mail-command "mbsync linaro-sync"
     mu4e-update-interval 600
     ;; navigate options
     mu4e-use-fancy-chars t
     mu4e-headers-skip-duplicates t
     mu4e-headers-include-related t
     ;; compose options
     mu4e-compose-signature 'my-sig-function
     mu4e-compose-complete-only-personal t
     mu4e-user-mail-address-list
     '("alex.bennee@linaro.org"
       "alex@bennee.com"
       "kernel-hacker@bennee.com")
     mu4e-compose-complete-only-after "2013-11-01"
     ;; view options
     mu4e-view-show-images t
     mu4e-view-show-addresses t
     mu4e-view-fill-headers nil
     mu4e-html2text-command "html2text -utf8 -width 72"
     mu4e-view-fields
     '(:from :to :cc :subject :flags :date :tags :attachments :signature)
     mu4e-maildir-shortcuts
     '( ("/linaro/Inbox"     . ?i)
        ("/linaro/mythreads" . ?m)
        ("/linaro/team"      . ?t)
        ("/linaro/virtualization/.qemu" . ?q)
        ("/sent"             . ?s)))
    ;; key-bindings
    (when (keymapp mu4e-compose-mode-map)
      (define-key mu4e-compose-mode-map (kbd "C-w") 'my-snip-region))
    ;; mode hooks
    (add-hook 'mu4e-headers-mode-hook
              '(lambda () (yas-minor-mode -1)))
    (add-hook 'mu4e-compose-pre-hook 'my-choose-mail-address)
    (add-hook 'mu4e-view-mode-hook 'my-set-view-directory)
    (add-hook 'mu4e-compose-mode-hook 'my-set-view-directory)
    ;; Header actions
    (setq mu4e-headers-actions
          (delete-dups
           (append
            mu4e-headers-actions
            '(("gapply git patches" . mu4e-action-git-apply-patch)
              ("mgit am patch" . mu4e-action-git-apply-mbox)
              ("crun checkpatch script" . my-mu4e-action-run-check-patch)))))
    ;; Message actions
    (setq mu4e-view-actions
          (delete-dups
           (append
            '(("gapply git patches" . mu4e-action-git-apply-patch)
              ("mgit am patch" . mu4e-action-git-apply-mbox)
              ("crun checkpatch script" . my-mu4e-action-run-check-patch)))))
    ;; Bookmarks
    (setq mu4e-bookmarks
          '(
            ;; Personal bookmarks
            ("\(to:alex.bennee or cc:alex.bennee\) NOT m:/linaro/misc/ AND flag:unread "
             "Unread posts addressed to me" ?M)
            ("\(to:alex.bennee or cc:alex.bennee\) AND flag:list AND flag:unread "
             "Unread list email addressed to me" ?m)
            ("\(to:alex.bennee or cc:alex.bennee\) and \( \(reviewed ADJ by\) OR \(signed ADJ off ADJ by\) \)"
             "Mail addressed to me with git tags" ?g)
            ("\(from:alex.bennee OR from:bennee.com\)"
             "Mail sent by me" ?s)
            ("flag:flagged" "Flagged and Starred posts" ?f)
            ("to:alex.bennee@linaro.org AND (from:agustin OR from:christoffer.dall@linaro.org)"
             "From my various bosses" ?B)
            ;; Virt related
            ("list:qemu-devel.nongnu.org and flag:unread"
             "Latest QEMU posts" ?q)
            ("list:qemu-devel.nongnu.org AND (aarch64 OR arm64 OR A64)"
             "QEMU ARM64 posts" ?a)
            ("to:mttcg@listserver.greensocs.com"
             "Multi-threaded QEMU posts" ?T)
            ("list:android-emulator-dev.googlegroups.com OR (list:qemu-devel.nongnu.org AND subject:android)"
             "Android related emails" ?A)
            ("list:kvmarm.lists.cs.columbia.edu and flag:unread"
             "Latest ARM KVM posts" ?k)
            ("list:virtualization.linaro.org and flag:unread"
             "Linaro Virtualization List" ?v)
            ("maildir:\"/linaro/virtualization/*\" AND flag:list AND flag:unread"
             "All unread Virtualization email" ?V)
            ;; Linaro Specific
            ("list:linaro-dev.lists.linaro.org AND flag:unread"
             "Latest Linaro-Dev emails" ?d)
            ("list:tech.lists.linaro.org AND flag:unread"
             "Latest Linaro-Tech emails" ?t)
            ("\(to:lists.linaro.org OR cc:lists.linaro.org\) AND flag:list AND flag:unread"
             "Unread work mailing lists (lists.linaro.org)" ?l)
            ("from:linaro.org and flag:unread"
             "Latest unread Linaro posts from Linaro emails" ?L)
            ;; Emacs
            ("list:emacs-devel.gnu.org and flag:unread"
             "Latest unread Emacs developer posts" ?E)
            ("list:help-gnu-emacs.gnu.org and flag:unread"
             "Latest unread Emacs user posts" ?e)
            ("list:emacs-orgmode.gnu.org and flag:unread"
             "Latest unread org-mode posts" ?o)))))


;; Magic handling for multiple email addrsses
(defvar my-email-address-alist
  '( ("Maildir/developer" . "kernel-hacker@bennee.com")
     ("Maildir/linaro" . "alex.bennee@linaro.org")
     ("Maildir/personal" . "alex@bennee.com") )
  "List of mappings from Maildir->email address.")

(defun my-choose-mail-address ()
  "Pick new `user-mail-address' based on the parent email.
Sadly this is not a local variable as at the time of the
hook we are not yet in the compose buffer."
  (let ((email
         (when mu4e-compose-parent-message
           (assoc-default
            (plist-get mu4e-compose-parent-message :path)
            my-email-address-alist 'string-match))))
    (setq user-mail-address
          (if email
              email
            (cond
             (I-am-at-work "alex.bennee@linaro.org")
             (t "alex@bennee.com"))))))

;; Utility functions for email

(defun my-snip-region (beg end)
  "Kill the region BEG to END and replace with <snip> tag."
  (interactive (list (point) (mark)))
  (kill-region beg end)
  (when (string-prefix-p ">" (car kill-ring))
    (insert "<snip>\n")))

;; Reset the cache of the directory list
(defun my-reset-mu4e-caches ()
  "Reset some of mu4e's caches to avoid restarting."
  (interactive)
  (setq mu4e~maildir-list nil))

;;
;; Checkpatch in emails
;;
(defvar my-checkpatch-script-history nil
  "History of checkpatch invocations.")

(defun my-mu4e-action-run-check-patch (msg)
  "Run checkpatch against the [patch] `MSG'."
  (let*
      ((ido-work-file-list my-checkpatch-script-history)
       (script (ido-read-file-name
                "Checkpatch Script: " (directory-file-name (or (car
                                                                ido-work-file-list)
                                                               default-directory)))))
    (setf my-checkpatch-script-history
          (cons script (delete script my-checkpatch-script-history)))
    (let ((proc-name "checkpatch")
          (buff-name (format "*checkpatch*")))
      (start-process-shell-command
       proc-name
       buff-name
       (format "cat %s | %s -" (mu4e-message-field msg :path) script))
    (switch-to-buffer buff-name)
    (goto-char (point-min))
    (compilation-minor-mode))))


;; WIP: Pull requests
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
