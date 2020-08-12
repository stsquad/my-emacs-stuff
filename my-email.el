;;; my-email.el --- Email set-up -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; After with notmuch, mu4e and Gnus I went with mu4e mainly due to
;; speed and apparent simplicity of customisation.
;;
;;; Code:

(require 'use-package)
(require 'my-vars)
(require 'my-libs)
(require 'my-hydra)
(require 'dash)

(use-package smtpmail
  :commands smtpmail-send-queued-mail
  :config
  (setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
        smtpmail-queue-dir   "~/Maildir/queue/cur"
        smtpmail-default-smtp-server "localhost"
        smtpmail-smtp-server "localhost"
        smtpmail-stream-type  'plain
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

;;
;; Finally the mu4e configuration
;;
;; This is my main work horse for day to day email.
;;

(let ((local-mu4e (my-return-path-if-ok
                   "~/src/emacs/mu/install/share/emacs/site-lisp/mu4e/")))
  (setq mu4e-mu-binary (or
                        (my-return-path-if-ok "/usr/bin/mu")
                        (my-return-path-if-ok "~/bin/mu")))
  (when local-mu4e
    (add-to-list 'load-path local-mu4e)))


(defun my-rig-mu4e-for-idle-running ()
  "Setup more comprehensive indexing when Emacs is idle."
  (setq mu4e-index-lazy-check nil   ; more comprehensive
        mu4e-index-cleanup t        ; check msgs still in store
        mu4e-get-mail-command "mbsync  -V -Dm linaro-slow-sync"))


(defun my-rig-mu4e-for-active-running ()
  "Setup faster indexing for when I'm actively using Emacs mail."
  (setq mu4e-index-lazy-check t     ; faster sync
        mu4e-index-cleanup nil     ; skip checking the whole store
        mu4e-get-mail-command "mbsync  -V -Dm linaro-sync")
  (when (not (-contains? (--map (aref it 5) timer-idle-list) 'my-rig-mu4e-for-idle-running))
    (run-with-idle-timer 600 nil 'my-rig-mu4e-for-idle-running)))

;; Switch function
(defun my-switch-to-mu4e (&optional prefix)
  "Smart dwim switch to mu4e."
  (interactive "P")
  (when I-am-at-work
    (my-rig-mu4e-for-active-running))
  (if prefix
      (mu4e)
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
        (mu4e)))))

;; ivy powered switch function
(defun my-ivy-switch-to-mu4e (&optional prefix)
  "Ivy based switch to mu4e.

Instead of the heuristics of `my-switch-to-mu4e' we build a list of
all mu4e buffers and allow ivy selection of them.
"
  (interactive "P")
  (when I-am-at-work
    (my-rig-mu4e-for-active-running))
  (if (or prefix (not (get-buffer " *mu4e-main*")))
      (mu4e)
    (let (collection)
      ;; Go backwards in priority (as add to list prepends by default)
      ;; The main menu
      (push (propertize (format "mu4e menu")
                        'buffer (get-buffer " *mu4e-main*")) collection)
      ;; What are we reading
      (let ((view (or (get-buffer "*mu4e-view*") (get-buffer "*Article*"))))
        (when view
          (push (propertize
                 (format "reading:%s"
                         (with-current-buffer view
                           (mu4e-message-field-at-point :subject)))
                 'buffer view) collection)))
      ;; What are we searching
      (let ((headers (get-buffer "*mu4e-headers*")))
        (when headers
          (push (propertize
                 (format "mu4e headers:%s"
                         (with-current-buffer headers
                           mu4e~headers-last-query))
                 'buffer headers) collection)))

      ;; What are we composing
      (--each (buffer-list)
        (with-current-buffer it
          (when (and (eq major-mode 'mu4e-compose-mode)
                     (not message-sent-message-via))
            (push (propertize
                   (format "composing:%s"
                           (or (message-fetch-field "subject")
                               "No subject"))
                   'buffer it) collection))))
      ;;
      (switch-to-buffer
       (get-text-property 0 'buffer
                          (if (< 1 (length collection))
                              (ivy-read "mu4e:"
                                        collection)
                            (car collection))))
      (delete-other-windows))))

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
  '( ("qemu-devel.gnu.org" . "~/lsrc/qemu.git/")
     ("qemu-devel.nongnu.org" . "~/lsrc/qemu.git/")
     ("kvmarm.lists.cs.columbia.edu" . "~/lsrc/linux.git/")
     ("kvm.vger.kernel.org" . "~/lsrc/linux.git/")
     ("virtualization.lists.linux-foundation.org" . "~/lsrc/linux.git/") )
  "Mapping from mailing lists to source tree.")

(defvar my-maildir-mapping
  '( ("linaro/virtualization/qemu" . "~/lsrc/qemu.git/")
     ("linaro/virtualization/qemu-arm" . "~/lsrc/qemu.git/")
     ("linaro/virtualization/qemu-multithread" . "~/lsrc/qemu.git/")
     ("linaro/kernel" . "~/lsrc/linux.git/") )
  "Mapping from maildirs to source tree.")

(defvar my-mail-address-mapping
  ' (
     ("qemu-devel@gnu.org" . "~/lsrc/qemu.git/")
     ("qemu-devel@nongnu.org" . "~/lsrc/qemu.git/")
      ("kvmarm@lists.cs.columbia.edu" . "~/lsrc/linux.git/") )
    "Mapping from target address to source tree.
Useful for replies and drafts")

(defun my-get-code-dir-from-email ()
  "Return the associated code directory depending on email."
  (let ((msg (mu4e-message-at-point t)))
    (if (not msg)
        default-directory
      (let ((list (mu4e-message-field msg :mailing-list))
            (maildir (mu4e-message-field msg :maildir))
            (addresses (-map 'cdr (append (mu4e-message-field msg :to)
                                          (mu4e-message-field msg :cc)))))
        (expand-file-name
         (or
          (assoc-default list my-mailing-list-dir-mapping)
          (assoc-default maildir my-maildir-mapping 'string-match)
          (assoc-default (-first
                          #'(lambda (mail)
                              (assoc-default mail my-mail-address-mapping))
                          addresses) my-mail-address-mapping)
          "~"))))))

(defun my-set-view-directory ()
  "Switch the `default-directory' depending mail contents."
  (interactive)
  (let ((this-buffer (current-buffer)))
    (if (mu4e-message-at-point t)
        (setq default-directory (my-get-code-dir-from-email))
      ;; *hack* if mu4e-message-at-point isn't ready yet
      (run-with-idle-timer
       0 nil (lambda()
               (with-current-buffer this-buffer
                 (setq default-directory (my-get-code-dir-from-email))))))))

;; We don't have the benefit of mu4e-message-at-point here so we do
;; things by hand using message-fetch-field.
(defun my-set-compose-directory ()
  "Switch the `default-directory' when composing an email."
  (interactive)
  (let ((cc (message-fetch-field "cc"))
        (to (message-fetch-field "to"))
        (all))
    (when (stringp cc)
      (nconc all (s-split ", " cc)))
    (when (stringp to)
      (nconc all (s-split ", " to)))
    (let ((dir (cdr
                (assoc
                 (--first
                  (assoc-default it my-mail-address-mapping)
                  all)
                 my-mail-address-mapping))))
    (when dir (setq default-directory dir)))))

(defun my-search-code-from-email ()
  "Search code depending on email."
  (interactive)
  (my-project-find (my-get-code-dir-from-email)))


(defun my-strip-msg-to-sig ()
  "Delete everything from point to my signature."
  (interactive)
  (let ((start (point)))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "--")
        (kill-region start (- (match-beginning 0) 1))))))


(defvar my-bad-addresses
  '( "docs.google.com"
     "\\(?:confirm.*@\\)"
     "\\(?:\\(?:not?\\)-?reply\\)"
     "\\(?:@bugs\\.launchpad\\.net\\)"
     "\\(?:bounces[^@]*@\\)"
     "\\(?:(via[^)]+)\\)"
     "richard.hendreson@linaro.org")
     "List of regexs to clean contact list.")

(defun my-mu4e-contact-cleaner (addr)
  "Clean out junk emails from contacts"
  (if (--any (string-match-p it addr) my-bad-addresses)
      nil
    addr))

(use-package mu4e-compose
  :hook ((mu4e-compose-mode . my-set-compose-directory)
          (mu4e-compose-pre . my-choose-mail-address))
  :bind (:map mu4e-compose-mode-map
              ("C-w" . my-snip-region)
              ("<f5>". my-search-code-from-email))
  :config (setq mu4e-compose-signature 'my-sig-function
                mu4e-compose-complete-addresses t
                mu4e-compose-complete-only-personal nil ;; personal seems to miss things

                mu4e-compose-complete-only-after "2013-11-01"
                mu4e-contact-process-function #'my-mu4e-contact-cleaner))


(defun my-update-async-jobs (ignored)
  "Flush the command queue."
  (when (and (fboundp 'shell-command-queue-run)
             shell-command-queue)
    (shell-command-queue-run)))

(use-package mu4e-headers
  :commands mu4e-headers-mode
  :bind (:map mu4e-headers-mode-map
              ("C-c C-l" . org-store-link)
              ("C-c t" . my-switch-to-thread)
              ("C-c d" . my-set-view-directory)
              ("C-x n l" . my-narrow-to-list)
              ("C-c A" . my-mu4e-apply-marked-mbox-patches))
  :hook ((mu4e-headers-mode . my-yas-local-disable)
         (mu4e-headers-found . my-set-view-directory)
         (mu4e-headers-search . my-update-async-jobs))
  :config (setq mu4e-headers-time-format "%H:%M:%S"
                mu4e-headers-date-format "%a %d/%m/%y"
                mu4e-headers-skip-duplicates t
                mu4e-headers-include-related t
                ;; mu4e-headers-hide-predicate 'my-mu4e-headers-hide-muted-p
                mu4e-headers-actions '(("gapply git patches" . mu4e-action-git-apply-patch)
                                       ("mgit am patch" . mu4e-action-git-apply-mbox)
                                       ("rrun checkpatch script" . my-mu4e-action-run-check-patch)
                                       ("sMark SPAM" . my-mu4e-register-spam-action)
                                       ("hMark HAM" . my-mu4e-register-ham-action)
                                       ("MMute Thread" . my-mu4e-headers-hide-muted-p)
                                       ("GCheck if merged" . my-mu4e-action-check-if-merged))))

(defvar my-mu4e-line-without-quotes-regex
  (rx (: bol (not (any ">"))))
  "Match start of line without any quotes or whitespace.")

(defhydra my-mu4e-view-toggle (:hint nil :color blue :timeout 5)
  (concat "_c_itation function:%`mu4e-compose-cite-function ")
  ;; toggle citation mode
  ("c" (lambda()
         (interactive)
         (if (eq mu4e-compose-cite-function
                 'message-cite-original-without-signature)
             (setq mu4e-compose-cite-function 'message-cite-original)
           (setq mu4e-compose-cite-function
                 'message-cite-original-without-signature))))
  ("g" (lambda() (interactive)
         (setq mu4e-view-use-gnus (not mu4e-view-use-gnus))) "toggle GNUs viewer")
  ("t" my-hydra-toggle/body "main toggles"))

(defvar my-gnus-cc-hiding-overlay nil
  "Overlay used to hide over long CC lines")
(make-variable-buffer-local 'my-gnus-cc-hiding-overlay)

(defun my-gnus-article-toggle-long-cc ()
  "Fold message headers."
  (interactive)
  (if (overlayp my-gnus-cc-hiding-overlay)
      (progn
        (delete-overlay my-gnus-cc-hiding-overlay)
        (setq my-gnus-cc-hiding-overlay nil))
    (save-restriction
      (goto-char (point-min))
      (gnus-article-goto-header (rx (or "CC" "Cc" "cc")))
      (mail-header-narrow-to-field)
      (let ((nlpos nil)
            (cclen (- (point-max) (point-min)))
            (winlen (window-width)))
        (save-excursion
          (setq nlpos (re-search-forward (rx eol) (point-max) t)))
        (setq my-gnus-cc-hiding-overlay
              (cond
               ((< nlpos (point-max))
                (make-overlay (1- nlpos) (point-max)))
               ((> cclen winlen)
                (make-overlay (+ (point-min) (- winlen 8))
                              (- (point-max) 4)))
               (t nil)))
        (when my-gnus-cc-hiding-overlay
          (overlay-put my-gnus-cc-hiding-overlay 'display "...")
          (overlay-put my-gnus-cc-hiding-overlay 'evaporate t))))))

(defun my-mu4e-kill-message-id ()
  "Snarf the message-id into the kill ring."
  (interactive)
  (kill-new (mu4e-message-field-at-point :message-id)))

(use-package mu4e-view
  :commands mu4e-view
  :bind (:map mu4e-view-mode-map
              ("C-c C-l". org-store-link)
              ("C-c f" . my-mu4e-search-from)
              ("C-c t" . my-switch-to-thread)
              ("C-c i" . my-mu4e-kill-message-id)
              ("C-x n l" . my-narrow-to-list)
              ("C-x t" . my-mu4e-view-toggle/body)
              ("h"     . my-gnus-article-toggle-long-cc))
  :hook (mu4e-view-mode . my-set-view-directory)
  :config (setq mu4e-view-show-images t
                mu4e-view-show-addresses t
                mu4e-view-fill-headers nil
                mu4e-view-fields
                '(:from :to :cc
                        :subject :flags
                        :date :tags :attachments :signature)
                mu4e-view-use-gnus t))

;; spam learning: ionice -c 3 sa-learn --progress --spam
;; ~/Maildir/.Spam/cur/*

(use-package shell-command-queue
  :if (file-exists-p "~/.emacs.d/shell-command-queue.el")
  :load-path "~/.emacs.d/")

;; loosely hacked from mu4e-control.el HEAD
(defvar my-mu4e-register-spam-cmd
  "sa-learn --spam --sync "
  "Command for invoking spam processor to register message as spam.")

(defvar my-mu4e-register-ham-cmd
  "sa-learn --ham --sync "
  "Command for invoking spam processor to register message as ham.")


(defun my-mu4e-next-if-at-point (msgid &optional delete)
  "Simple helper for bulk tagging operations.

Move next if the message at point is what we have just processed."
  (let ((msgid-at-point (mu4e-message-field-at-point :message-id)))
    (when (and msgid-at-point
               (string= msgid-at-point msgid))
      (when delete
        (mu4e-mark-at-point 'delete nil))
      (mu4e-headers-next))))

(defun my-mu4e-register-action (msg tag cmd)
  "Mark `MSG' as with `TAG' and `CMD'."
  (unless (-contains?
           (mu4e-message-field msg :tags) tag)
    (mu4e-action-retag-message msg (concat "+" tag))
    (shell-command-queue-add
       (concat "Learn " tag)
       (concat cmd
               (shell-quote-argument (mu4e-message-field msg :path))))))

(defun my-mu4e-register-spam-action (msg)
  "Mark `MSG' as spam."
  (interactive)
  (my-mu4e-register-action msg "spam" my-mu4e-register-spam-cmd)
  (my-mu4e-next-if-at-point (mu4e-message-field msg :message-id) t))

(defun my-mu4e-register-ham-action (msg)
  "Mark `MSG' as ham."
  (interactive)
  (my-mu4e-register-action msg "ham" my-mu4e-register-ham-cmd)
  (mu4e-action-retag-message msg "-spam")
  (my-mu4e-next-if-at-point (mu4e-message-field msg :message-id) t))

;; Check if patch merged into a given tree
;;
;; Subject: [Qemu-devel] [PATCH 1/2] tcg: Allow constant pool entries in the prologue
(defvar my-extract-patch-title
  (rx (:
       (or "[PATCH" "[RFC") (zero-or-more print) "]"
       (zero-or-more space)
       (group (one-or-more print))))
  "Regex to extract patch title from email subject.")

(defun my-mu4e-action-check-if-merged (msg)
  "Check if `MSG' is in your tree."
  (let ((subj (mu4e-message-field-at-point :subject)))
    (when (string-match my-extract-patch-title subj)
      (let ((title (match-string-no-properties 1 subj))
            (default-directory (read-directory-name "Project:")))
        (let ((result (magit-git-string
                       "log" "origin/master" "--no-merges" "--oneline"
                       "--grep" title)))
          (if result
              (when (yes-or-no-p (format "Visit:%s?" result))
                (magit-show-commit (car (split-string result))))
            (message "no commit found :-(")))))))

(defun my-mu4e-search-for-id ()
  "Find the Message-Id in the buffer and search for it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward my-capture-msgid-re)
      (mu4e-headers-search (format "i:%s" (match-string-no-properties 1))))))


(defun my-mu4e-search-from (&optional edit)
  "Find msgs from author of the message, optionally EDIT the search."
  (interactive "P")
  (let ((from (mu4e-message-field-at-point ':from)))
    (when from
      (mu4e-headers-search
       (format "f:%s" (cdr (car from))) nil edit))))

(use-package mu4e
  :commands mu4e
  ;; Bindings
  :bind ("C-c m" . my-ivy-switch-to-mu4e)
  :config
  (progn
    (require 'mu4e-vars)
    ;; config options
    (setq
     ;; generic mail options
     user-mail-address
     (cond
      (I-am-at-work  "alex.bennee@linaro.org")
      (t "alex@bennee.com"))
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
      (I-am-at-work "mbsync  -V -Dm linaro-sync")
      (t "true"))
     mu4e-update-interval 800
     mu4e-hide-index-messages t
     mu4e-change-filenames-when-moving t ; keep mbsync happy
     mu4e-index-lazy-check t             ; faster sync
     mu4e-index-cleanup nil              ; should toggle this
     ;; completion
     mu4e-completing-read-function 'completing-read
     ;; navigate options
     mu4e-use-fancy-chars t
     mu4e-user-mail-address-list
     (cond
      (I-am-at-work  '("alex.bennee@linaro.org"))
      (t '("alex@bennee.com")))
     mu4e-maildir-shortcuts
     (cond
      (I-am-at-work
       '( ("/linaro/Inbox"     . ?i)
          ("/linaro/mythreads" . ?m)
          ("/linaro/archived" . ?A)
          ("/linaro/team"      . ?t)
          ("/linaro/kernel/lkml"      . ?l)
          ("/linaro/virtualization/qemu" . ?q)
          ("/linaro/virtualization/qemu-arm" . ?a)
          ("/linaro/virtualization/qemu-multithread" . ?M)
          ("/linaro/virtualization/kvm-arm" . ?k)
          ("/sent"             . ?s) ))
      (t
       '( ("/"     . ?i)
          ("/.Spam" . ?s)
          ("/.Oldmail" . ?o) ))))

    ;; Header markers
    (defvar my-mu4e-patches nil
      "List of mu4e-messages snagged by the (Patches) actions.")
    (defvar my-mu4e-applied-patches nil
      "List of mu4e-messages successfully applied by the (Patches)
    actions.")
    (make-variable-buffer-local 'my-mu4e-patches)
    (make-variable-buffer-local 'my-mu4e-applied-patches)

    (defun my-mu4e-get-patch-number (msg)
      "Return patch number from a message."
      (let ((subject (mu4e-message-field msg :subject)))
        (when
            (string-match
             (rx (: (group-n 1 (one-or-more (any "0-9"))) (any "/")
                    (one-or-more (any "0-9"))))
             subject)
          (match-string 1 subject))))

    (defun my-mu4e-remaining-patches ()
    "Return a sorted list of patches left to apply"
    (--sort
     (string<
       (my-mu4e-get-patch-number it)
       (my-mu4e-get-patch-number other))
     (-difference my-mu4e-patches
                  my-mu4e-applied-patches)))

    ;; from latest mu4e
    (defvar mu4e~patch-directory-history nil
      "History of directories we have applied patches to.")

    ;; This essentially works around the fact that read-directory-name
    ;; can't have custom history.
    (defun mu4e~read-patch-directory (&optional prompt)
      "Read a `PROMPT'ed directory name via `completing-read' with history."
      (unless prompt
        (setq prompt "Target directory:"))
      (file-truename
       (completing-read prompt 'read-file-name-internal #'file-directory-p
                        nil nil 'mu4e~patch-directory-history)))

    (defun mu4e-action-git-apply-mbox (msg &optional signoff)
      "Apply `MSG' a git patch with optional `SIGNOFF'.

If the `default-directory' matches the most recent history entry don't
bother asking for the git tree again (useful for bulk actions)."
      (my-git-apply-mbox (mu4e-message-field msg :path) signoff))

    (defun my-mu4e-apply-marked-mbox-patches (&optional arg)
      "Apply patches in order. With PREFIX include signoff"
      (interactive "P")
      (let ((applied-or-skipped
             (--take-while
              (let ((docid (plist-get it :docid)))
                (if (mu4e-mark-docid-marked-p docid)
                    (if (= 0 (mu4e-action-git-apply-mbox it arg))
                        (when (mu4e~headers-goto-docid docid)
                          (mu4e-mark-set 'unmark) t)
                      ; failed to apply, stop
                      (switch-to-buffer "*Shell Command Output*")
                      nil)
                  ; not marked, skip
                  t))
              (my-mu4e-remaining-patches))))
        (setq my-mu4e-applied-patches
              (-union my-mu4e-applied-patches applied-or-skipped))

        (message (format "Applied %d (%d)/%d patches"
                         (length applied-or-skipped)
                         (length my-mu4e-applied-patches)
                         (length my-mu4e-patches)))))

    ;; The following two functions are custom marker functions
    ;; Match function
    (defun my-mu4e-patch-match (msg parent-id)
      "Match any patches related to the parent-id. Add them
to `my-mu4e-patches' for later processing."
      (when
          (and (string-match parent-id
                             (or
                              (mu4e-message-field-raw msg :in-reply-to)
                              (mu4e-message-field-raw msg :message-id)))
               (string-match
                (rx
                 (: bol "["
                    (minimal-match (zero-or-more (not (any "/"))))
                    (or (in "1-9")
                        (: (any "1-9") (one-or-more (any "0-9"))))
                    "/"))
                (mu4e-message-field-raw msg :subject)))
        (add-to-list 'my-mu4e-patches msg)))


    ;; Param function
    (defun my-mu4e-patch-setup ()
      "Reset the patch list and extract parent-id for `my-mu4e-patch-match'"
      (setq my-mu4e-patches nil
            my-mu4e-applied-patches nil)
      (let ((msg (mu4e-message-at-point)))
        (mu4e-message-field-raw msg :message-id)))

    (when (boundp 'mu4e-marks)
      (add-to-list
       'mu4e-marks
       '(patch
         :char ("#" . "#")
         :prompt "Patch")))

    (add-to-list
     'mu4e-headers-custom-markers
     '("Patches" my-mu4e-patch-match my-mu4e-patch-setup))
    ;; Message actions
    (setq mu4e-view-actions
          (delete-dups
           (append
            '(("gapply git patches" . mu4e-action-git-apply-patch)
              ("mgit am patch" . mu4e-action-git-apply-mbox)
              ("crun checkpatch script" . my-mu4e-action-run-check-patch)
              ("MCheck if merged" . my-mu4e-action-check-if-merged)))))
    ;; Bookmarks
    (setq mu4e-bookmarks
          (cond
           (I-am-at-work
            '(
              ;; Personal bookmarks
              ("recip:alex.bennee flag:unread "
               "Unread posts addressed to me" ?M)
              ("recip:alex.bennee flag:list flag:unread "
               "Unread list email addressed to me" ?m)
              ("recip:alex.bennee AND \( \(reviewed ADJ by\) OR \(signed ADJ off ADJ by\) \)"
               "Mail addressed to me with git tags" ?g)
              ("\(from:alex.bennee OR from:bennee.com\)"
               "Mail sent by me" ?s)
              ("recip:alex.bennee s:Re NOT flag:seen"
               "Mail sent by me (unread replied)" ?S)
              ("\(from:alex.bennee OR from:bennee.com\) AND s:PATCH NOT s:Re"
               "My patches" ?p)
              ("\
(from:alex.bennee OR from:bennee.com\) AND \
\(b:\"Reviewed\" OR b:\"Tested\"\)" "My tags" ?r)
              ("s:PULL \(b:Bennée OR b:Bennee\)" "Pull Reqs with my name" ?P)
              ("flag:flagged" "Flagged and starred posts" ?f)
              ("flag:flagged NOT flag:seen" "Unread flagged and starred posts" ?F)
              ("to:alex.bennee@linaro.org AND from:maxim.kuvyrkov@linaro.org"
               "From my boss" ?B)
              ("date:1h..now"
               "In the last hour" ?h)
              ("date:1h..now AND flag:unread"
               "In the last hour (unread)" ?H)
              ("from:alex.bennee date:1w..now"
               "My emails in the last week" ?w)
              ;; Bugs and Notifications
              ("recip:alex.bennee AND (f:bugs.debian.org OR f:bugs.launchpad.net)" "Bugs" ?b)
              ("maildir:\"/linaro/Inbox\" f:travis OR f:lava OR f:shippable" "Notifications" ?n)
              ("f:vandersonmr2@gmail.com" "GSoC" ?G)
              ;; Virt related
              ("list:qemu-devel* and flag:unread"
               "Latest QEMU posts" ?q)
              ("((list:qemu-devel* AND (s:aarch64 OR s:arm OR s:A64)) OR list:qemu-arm*)"
               "QEMU ARM posts" ?a)
              ("((list:qemu-devel* OR list:qemu-arm*) AND (s:fpu OR s:softfloat OR s:float))"
               "QEMU Softfloat Posts" ?o)
              ("list:android-emulator-dev.googlegroups.com OR (list:qemu-devel* AND subject:android)"
               "Android related emails" ?A)
              ("list:kvmarm.lists.cs.columbia.edu and flag:unread"
               "Latest ARM KVM posts" ?k)
              ;; Linaro Specific
              ("list:linaro-toolchain.lists.linaro.org OR maildir:/linaro/linaro-list/linaro-tcwg"
               "Linaro public TCWG posts" ?T)
              ("to:tcwg@linaro.org"
               "Linaro private TCWG posts" ?t)
              ("list:conf.lists.linaro.org AND flag:unread"
               "Latest Conf emails" ?c)
              ("list:linaro-dev.lists.linaro.org AND flag:unread"
               "Latest Linaro-Dev emails" ?d)
              ;; ("list:tech.lists.linaro.org AND flag:unread"
              ;;  "Latest Linaro-Tech emails" ?t)
              ("\(to:lists.linaro.org OR cc:lists.linaro.org\) AND flag:list AND flag:unread"
               "Unread work mailing lists (lists.linaro.org)" ?l)
              ("from:linaro.org and flag:unread"
               "Latest unread Linaro posts from Linaro emails" ?L)
              ;; Distro and others
              ;; Emacs
              ("list:emacs-devel.gnu.org and flag:unread"
               "Latest unread Emacs developer posts" ?E)
              ("list:help-gnu-emacs.gnu.org and flag:unread"
               "Latest unread Emacs user posts" ?e)
              ("list:emacs-orgmode.gnu.org and flag:unread"
               "Latest unread org-mode posts" ?O)))
           (I-am-on-server
            '(
              ;; Personal bookmarks
              ("recip:alex@bennee.com AND flag:unread "
               "Unread posts addressed to me" ?M)
              ("flag:unread AND NOT recip:alex@bennee.com"
               "Unread posts addressed to aliases" ?a)
              ("flag:list AND flag:unread "
               "Unread list/notification email" ?m)
              ("\(from:alex.bennee OR from:bennee.com\)"
               "Mail sent by me" ?s)
              ("from:eileen OR from:nigel"
               "From parents" ?P)
              ("tag:spam" "Tagged Spam" ?t)
              ("to:bugzilla@bennee.com" "Bug Mail" ?B)))))))


(when (locate-library "mu4e")
  (use-package mu4e-alert
    :disabled t
    :config (progn
              (setq mu4e-alert-interesting-mail-query
                    "recip:alex.bennee flag:unread date:7d..now AND NOT flag:trashed")
              (mu4e-alert-enable-mode-line-display))))

(use-package mu4e-patch
  :load-path (lambda () (my-return-path-if-ok
                         "~/src/emacs/mu4e-patch.git"))
  :config (add-hook 'mu4e-view-mode-hook #'mu4e-patch-highlight))


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

(defun my-mu4e-action-run-check-patch (msg)
  "Call checkpatch against `MSG'."
  (checkpatch-run-against-patch-file (mu4e-message-field msg :path)))

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
