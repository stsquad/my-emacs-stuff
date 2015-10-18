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
        smtpmail-smtp-service 25))


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

(use-package sendmail
  ;; Enable mail-mode for mutt spawned files
  :mode (("/tmp/mutt-*" . mail-mode)
         ("0000-cover-letter.patch" . mail-mode)
         (".*/\.git/\.gitsendemail.MSG.*" . mail-mode))
  :config (add-hook 'mail-mode-hook 'my-mail-mode-tweaks))

(use-package message
  :commands message-mode
  :config (add-hook 'message-mode-hook 'my-common-mail-tweaks))

;; BBDB
(use-package bbdb
  :config (progn
            (setq bbdb-add-aka t
                  bbdb-layout 'one-line
                  bbdb-mua-auto-update-p 'query)
            (bbdb-initialize 'mu4e 'rmail 'gnus 'message)))

;;
;; Finally the mu4e configuration
;;
;; This is my main work horse for day to day email.
;;

;; Switch function
(defun my-switch-to-mu4e ()
  "Smart dwim switch to mu4e."
  (interactive)
  (let ((candidate
         (or
          ;; unsent emails
          (car (--filter
                (with-current-buffer it
                  (and
                   (eq major-mode 'mu4e-compose-mode)
                   (not message-sent-message-via)))
                (buffer-list)))
          ;; current search
          (get-buffer "*mu4e-headers*")
          ;; current view
          (get-buffer "*mu4e-view*"))))
    (if candidate
        (progn
          (switch-to-buffer candidate)
          (delete-other-windows))
      (mu4e))))

;; Jump to current thread
(defun my-switch-to-thread ()
  "Switch to headers view of current thread."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (id (or (mu4e-message-field-raw msg :in-reply-to)
                 (mu4e-message-field-raw msg :message-id))))
    (when (> (length id) 0)
      (mu4e-headers-search (format "i:%s" (s-replace-all '(("<" . "")
                                                           (">" . ""))
                                                         id))))))

;; Set default directory when viewing messages
(defvar my-mailing-list-dir-mapping
  '( ("qemu-devel.nongnu.org" . "~/lsrc/qemu/qemu.git/")
     ("kvmarm.lists.cs.columbia.edu" . "~/lsrc/kvm/linux.git/") )
  "Mapping from mailing lists to source tree.")

(defvar my-maildir-mapping
  '( ("linaro/virtualization/qemu" . "~/lsrc/qemu/qemu.git/")
     ("linaro/virtualization/qemu-multithread" . "~/lsrc/qemu/qemu.git/")
     ("linaro/kernel" . "~/lsrc/kvm/linux.git/") )
  "Mapping from maildirs to source tree.")

(defun my-set-view-directory ()
  "Switch the `default-directory' depending on the mailing list we
  are in."
  (interactive)
  (let ((msg (mu4e-message-at-point t)))
    (when msg
      (let ((list (mu4e-message-field msg :mailing-list))
            (maildir (mu4e-message-field msg :maildir)))
        (setq default-directory
              (expand-file-name
               (or
                (assoc-default list my-mailing-list-dir-mapping)
                (assoc-default maildir my-maildir-mapping 'string-match)
                "~")))))))

(use-package mu4e-compose
  :commands mu4e-compose-mode
  :config (progn
            ;; key-bindings
            (when (keymapp mu4e-compose-mode-map)
              (define-key mu4e-compose-mode-map (kbd "C-w") 'my-snip-region))
            (add-hook 'mu4e-compose-mode-hook 'my-set-view-directory)
            (add-hook 'mu4e-compose-pre-hook 'my-choose-mail-address)))

(use-package mu4e-headers
  :commands mu4e-headers-mode
  :config (progn
            ;; My mode bindings
            (define-key mu4e-headers-mode-map (kbd "C-c l") 'org-store-link)
            (define-key mu4e-headers-mode-map (kbd "C-c t")
              'my-switch-to-thread)
            (add-hook 'mu4e-headers-mode-hook
                      '(lambda () (yas-minor-mode -1)))
            (add-hook 'mu4e-headers-mode-hook 'my-set-view-directory)))

(use-package mu4e-view
  :commands mu4e-view
  :config (progn
            ;; My mode bindings
            (define-key mu4e-view-mode-map (kbd "C-c l") 'org-store-link)
            (define-key mu4e-view-mode-map (kbd "C-c t") 'my-switch-to-thread)
            ;; mode hooks
            (add-hook 'mu4e-view-mode-hook 'my-set-view-directory)))

(use-package mu4e
  :commands mu4e
  ;; Bindings
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
     mu4e-get-mail-command
     (cond
      (I-am-at-work "mbsync linaro-sync")
      (t "true"))
     mu4e-update-interval 600
     ;; navigate options
     mu4e-use-fancy-chars t
     mu4e-headers-skip-duplicates t
     mu4e-headers-include-related t
     ;; compose options
     mu4e-compose-signature 'my-sig-function
     mu4e-compose-complete-addresses nil
     mu4e-compose-complete-only-personal t
     mu4e-user-mail-address-list
     (cond
      (I-am-at-work  '("alex.bennee@linaro.org"
                       "alex@bennee.com"
                       "kernel-hacker@bennee.com"))
      (t '("alex@bennee.com")))
     mu4e-compose-complete-only-after "2013-11-01"
     ;; view options
     mu4e-view-show-images t
     mu4e-view-show-addresses t
     mu4e-view-fill-headers nil
     mu4e-html2text-command "html2text -utf8 -width 72"
     mu4e-view-fields
     '(:from :to :cc :subject :flags :date :tags :attachments
             :signature)
     mu4e-maildir-shortcuts
     (cond
      (I-am-at-work
       '( ("/linaro/Inbox"     . ?i)
          ("/linaro/mythreads" . ?m)
          ("/linaro/team"      . ?t)
          ("/linaro/virtualization/.qemu" . ?q)
          ("/linaro/virtualization/.kvm-arm" . ?k)
          ("/sent"             . ?s) ))
      (t
       '( ("/"     . ?i)
          ("/.Spam" . ?s)
          ("/.Oldmail" . ?o) ))))

    ;; Header markers
    (defvar my-mu4e-patches nil
      "List of mu4e-messages snagged by the (Patches) actions.")

    (defun my-mu4e-apply-marked-mbox-patches ()
      "Apply patches in order."
      (interactive)
      (let ((patches
             (sort
              my-mu4e-patches
              #'(lambda(a b)
                  (string<
                   (mu4e-message-field-raw
                    a :subject)
                   (mu4e-message-field-raw
                    b :subject))))))
        (mapc 'mu4e-action-git-apply-mbox patches)))
    
    (add-to-list
     'mu4e-headers-custom-markers
     '("Patches"
       ;; Match function
       (lambda (msg parent-id)
         (when
             (and
              (string-match
               parent-id
               (or
                (mu4e-message-field-raw msg :in-reply-to)
                ""))
              (string-match "^\\[" (mu4e-message-field-raw msg
                                                           :subject)))
           (add-to-list 'my-mu4e-patches msg)))
       ;; Param function
       (lambda ()
         (setq my-mu4e-patches nil)
         (let ((msg (mu4e-message-at-point)))
           (mu4e-message-field-raw msg :message-id)))))
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
          (cond
           (I-am-at-work
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
              ("to:alex.bennee@linaro.org AND from:christoffer.dall@linaro.org"
               "From my boss" ?B)
              ("date:1h..now"
               "In the last hour" ?h)
              ("date:1h..now AND flag:unread"
               "In the last hour (unread)" ?H)
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
              ("list:conf.lists.linaro.org AND flag:unread"
               "Latest Conf emails" ?c)
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
               "Latest unread org-mode posts" ?o)))
           (I-am-on-server
            '(
              ;; Personal bookmarks
              ("\(to:alex@bennee.com or cc:alex@bennee.com\) AND flag:unread "
               "Unread posts addressed to me" ?M)
              ("flag:list AND flag:unread "
               "Unread list/notification email" ?m)
              ("\(from:alex.bennee OR from:bennee.com\)"
               "Mail sent by me" ?s)
              ("from:eileen OR from:nigel"
               "From parents" ?P)
              ("to:bugzilla@bennee.com" "Bug Mail" ?B)))))))

              
(use-package helm-mu
  :commands helm-mu
  :if (and (string-match "zen" (system-name))
           (locate-library "helm-mu"))
  :config (progn
            (define-key mu4e-headers-mode-map (kbd "C-s") 'helm-mu)))

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
