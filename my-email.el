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
  (concat "Alex Bennée\n"
          "Virtualisation Tech Lead @ Linaro"))

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
                   "~/src/emacs/install/share/emacs/site-lisp/mu4e/")))
  (setq mu4e-mu-binary (or
                        (my-return-path-if-ok "/usr/bin/mu")
                        (my-return-path-if-ok "~/src/emacs/install/bin/mu")))
  (when local-mu4e
    (add-to-list 'load-path local-mu4e)))

(defun my-return-most-recent-mu4e-contacts ()
  "Return the most recent contacts for completion."
  (split-string
   (ansi-color-filter-apply
    (shell-command-to-string
     (concat mu4e-mu-binary
             " find -n 500 "
             "--sortfield=date --reverse "
             "--fields f "
             "recip:alex.bennee | sort | uniq")))
    "\n"))

;; Switch function
(defun my-switch-to-mu4e (&optional prefix)
  "Smart dwim switch to mu4e."
  (interactive "P")
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
  "Ivy based switch to mu4e, with PREFIX switch directly to main menu.

Instead of the heuristics of `my-switch-to-mu4e' we build a list of
all mu4e buffers and allow ivy selection of them.
"
  (interactive "P")
  (if (or prefix (not (get-buffer "*mu4e-main*")))
      (mu4e)
    (let (collection)
      ;; Go backwards in priority (as add to list prepends by default)
      ;; The main menu
      (push (propertize (format "mu4e menu")
                        'buffer (get-buffer "*mu4e-main*")) collection)
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
                           mu4e--search-last-query))
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
     ("virtualization.lists.linux-foundation.org" ."~/lsrc/linux.git/"))
  "Mapping from mailing lists to source tree.")

(defvar my-maildir-mapping
  '( ("virtualization/qemu" . "~/lsrc/qemu.git/")
     ("virtualization/qemu-arm" . "~/lsrc/qemu.git/")
     ("virtualization/qemu-multithread" . "~/lsrc/qemu.git/")
     ("kernel" . "~/lsrc/linux.git/") )
  "Mapping from maildirs to source tree.")

(defvar my-mail-address-mapping
  ' (
     ("qemu-devel@gnu.org" . "~/lsrc/qemu.git/")
     ("qemu-devel@nongnu.org" . "~/lsrc/qemu.git/")
     ("kvmarm@lists.cs.columbia.edu" . "~/lsrc/linux.git/")
     ("xen-devel@lists.xenproject.org" . "~/lsrc/xen/xen.git"))
    "Mapping from target address to source tree.
Useful for replies and drafts")

(defun my-get-code-dir-from-email ()
  "Return the associated code directory depending on email."
  (let ((msg (mu4e-message-at-point t)))
    (if (not msg)
        default-directory
      (let ((list (mu4e-message-field msg :list))
            (maildir (mu4e-message-field msg :maildir))
            (addresses (--map (plist-get it :email)
                              (append (mu4e-message-field msg :to)
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
        (to (message-fetch-field "to")))
    (let ((dir (cdr
                (assoc
                 (--first
                  (assoc-default it my-mail-address-mapping)
                  (nconc
                   (when (stringp cc) (s-split ", " cc))
                   (when (stringp to) (s-split ", " to))))
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
     "\\(?:\\(?:[nN]o?t?\\)[-_ ]?[rR]eply\\)"
     "\\(?:@bugs\\.launchpad\\.net\\)"
     "\\(?:bounces[^@]*@\\)"
     "\\(?:(via[^)]+)\\)"
     "\\(?:via [^<]+<[^>]+>\\)"
     "notifications@github.com"
     "reply@github.com"
     "reply.github.com"
     "incoming.gitlab.com"
     "nongu.org" ; so many misspellings !
     "nongdu.org"
     "nongun.org"
     "nongnu.orh"
     "nongnu.orgn"
     "nongnu.orgd"
     "nongnu.-rg"
     "nonngnu.org"
     "nognu.org"
     "@linaro.atlassian.net" ; notifications
     "projects@linaro.org"
     "linaor.org"
     "linrao.org"
     "liaro.org"
     "linar.org"
     "rehat.com"
     "richard.hendreson@linaro.org"
     "Peter.maydel@linaro.org"
     "vincent.guitto@linaro.org"
     "vincent.guitttot@linaro.org")
     "List of regexs to clean contact list.")

(defun my-mu4e-contact-cleaner (addr)
  "Clean out junk emails from contacts."
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
                mu4e-contact-process-function #'my-mu4e-contact-cleaner
                mu4e-sent-messages-behavior 'delete))


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
  :hook ((mu4e-headers-found . my-set-view-directory)
         (mu4e-search . my-update-async-jobs))
  :config (setq mu4e-headers-time-format "%H:%M:%S"
                mu4e-headers-date-format "%a %d/%m/%y"
                mu4e-headers-skip-duplicates t
                mu4e-headers-include-related t
                ;; Fancy chars
                mu4e-use-fancy-chars        t
                mu4e-headers-draft-mark     '("D" . "💈")
                mu4e-headers-flagged-mark   '("F" . "📍")
                mu4e-headers-new-mark       '("N" . "🔥")
                mu4e-headers-passed-mark    '("P" . "❯")
                mu4e-headers-replied-mark   '("R" . "❮")
                mu4e-headers-seen-mark      '("S" . "☑")
                mu4e-headers-trashed-mark   '("T" . "💀")
                mu4e-headers-attach-mark    '("a" . "📎")
                mu4e-headers-encrypted-mark '("x" . "🔒")
                mu4e-headers-signed-mark    '("s" . "🔑")
                mu4e-headers-unread-mark    '("u" . "📨")
                mu4e-headers-list-mark      '("s" . "🔈")
                mu4e-headers-personal-mark  '("p" . "👨")
                mu4e-headers-calendar-mark  '("c" . "📅")
                ;; which flags for the above
                mu4e-headers-visible-flags '(draft flagged unread seen passed replied trashed attach encrypted signed)
                ;; mu4e-headers-hide-predicate 'my-mu4e-headers-hide-muted-p
                mu4e-search-results-limit 1000
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

(defun my-mu4e-kill-message-id (&optional no-kill)
  "Snarf the message-id into the kill ring unless NO-KILL prefix."
  (interactive "P")
  (let ((refs))
    ;; References
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward my-capture-msgid-re nil t)
        (let ((id (match-string-no-properties 1)))
          (push (propertize (format "reference: %s" id)
                            'id id) refs))))
    ;; Headers
    (when (and (derived-mode-p 'gnus-article-mode) (mu4e-message-at-point))
      (let ((id (mu4e-message-field-at-point :message-id)))
        (push (propertize (format "header: %s" id)
                          'id id) refs)))
    ;; Gnus


    ;; do it
    (let ((final
           (get-text-property 0 'id
                              (if (< 1 (length refs))
                                  (ivy-read "select reference:" refs)
                                (car refs)))))
      (message "message-id: %s" final)
      (unless no-kill
        (kill-new final))
      final)))

(defun my-mu4e-jump-to-commit ()
  "Jump to a referenced commit in the message."
  (interactive)
  (let ((refs))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx (group (>= 7 hex))
                  (one-or-more space)
                  (zero-or-one "(" (group (one-or-more (not ")"))) ")"))
              nil t)
        (let ((id (match-string-no-properties 1))
              (extra (match-string-no-properties 2)))
          (push (propertize (if extra
                                (format "commit: %s - %s" id extra)
                              (format "commit: %s" id)) 'id id)
                refs))))
    ;; do it
    (let ((final
           (get-text-property 0 'id
                              (if (< 1 (length refs))
                                  (ivy-read "select commit:" (nreverse refs))
                                (car refs)))))
      (magit-show-commit final))))

(defun my-mu4e-view-copy-reference ()
  "Grab the headers needed to find this message into the kill ring."
  (interactive)
  (let ((path (mu4e-message-readable-path))
             (headers))
         (with-temp-buffer
           (insert-file-contents path)
           (goto-char (point-min))
           (while (re-search-forward
                   (rx (or "From:"
                           "Subject:"
                           "Date:"
                           (: "Message-" (or "Id" "ID") ":" ))) nil t)
             (push (buffer-substring-no-properties (match-beginning 0) (line-end-position)) headers)))
         (kill-new (mapconcat 'identity headers "\n"))))

(or (require 'mu4e-view-gnus nil t) (require 'mu4e-view))

(use-package mu4e-view
  :commands mu4e-view
  :bind (:map mu4e-view-mode-map
              ("C-c C-l". org-store-link)
              ;; ("C-c c" . my-mu4e-jump-to-commit) aliases with
              ;; compile command
              ("C-c f" . my-mu4e-search-from)
              ("C-c t" . my-switch-to-thread)
              ("C-c i" . my-mu4e-kill-message-id)
              ("C-c w" . my-mu4e-view-copy-reference)
              ("C-x n l" . my-narrow-to-list)
              ("C-x t" . my-mu4e-view-toggle/body)
              ("h"     . my-gnus-article-toggle-long-cc))
  :hook (mu4e-view-mode . my-set-view-directory)
  :config (setq mu4e-view-fields
                '(:from :to :cc
                        :subject :flags
                        :date :tags :attachments :signature)))

;; spam learning: ionice -c 3 sa-learn --progress --spam
;; ~/Maildir/.Spam/cur/*

;;(use-package shell-command-queue
;;  :if (file-exists-p "~/.emacs.d/shell-command-queue.el")
;;  :load-path "~/.emacs.d/")

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
;; [PATCH v3 0/7] silence the compiler warnings
;; [PATCH v3 1/7] target/i386: silence the compiler warnings in gen_shiftd_rm_T1
;; [PATCH v3 for 5.2 3/7] more stuff for a patch
;; [PATCH v12 for 5.1-rc1 5/7] even more
;; [PATCH for 5.2] random patch
;; [RFC PATCH] fix thing
;; TODO: move to my-vars, add ert tests

(defvar my-extract-patch-title
  (rx (:
       (or "[PATCH" "[RFC") (zero-or-more print) "]"
       (zero-or-more space)
       (group-n 1 (one-or-more print))))
  "Regex to extract patch title. Simple version that doesn't take into
  account series and re-roll formats.")

(defvar my-extract-patch-title-from-series
  (rx (: (or "[PATCH" "[RFC")
         (zero-or-more space)
         (group-n 2 (zero-or-one (: (in "vV") (one-or-more digit))))
         (minimal-match (zero-or-more print))
         (group-n 3 (: (in "1-9") (zero-or-more digit)))
         "/" (zero-or-more print) "]"
         (zero-or-more space)
         (group-n 1 (one-or-more print))))
  "Regex to extract patch title (and other bits) from email subject.
Groups: 1:subject, 2:revision, 3: patch number. ")

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
  (mu4e-headers-search (format "i:%s" (my-mu4e-kill-message-id 't))))

(defun mu4e-action-git-apply-b4 (msg)
  "Find the Message-Id in the buffer and pass to b4"
  (interactive)
  (my-git-fetch-and-apply-via-b4 (my-mu4e-kill-message-id 't)))

(defun mu4e-action-setup-reword-b4 (msg)
  "Find the Message-Id in the buffer and pass to b4 to learn commits"
  (interactive)
  (my-set-reword-commits-from-b4 (my-mu4e-kill-message-id 't)))

(defun my-mu4e-search-from (&optional edit)
  "Find msgs from author of the message, optionally EDIT the search."
  (interactive "P")
  (let ((from (mu4e-message-field-at-point ':from)))
    (when from
      (mu4e-headers-search
       (format "f:%s" (plist-get (car from) :email)) nil edit))))

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
     ;; debug
     mu4e-mu-debug t
     ;;
     mu4e-update-interval 800
     mu4e-hide-index-messages t
     mu4e-change-filenames-when-moving t ; keep mbsync happy
     mu4e-index-lazy-check nil           ; faster sync when t
     mu4e-index-cleanup t                ; faster cleanup when nil
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
       '( ("/Inbox"     . ?i)
          ("/mythreads" . ?m)
          ("/team"      . ?t)
          ("/linaro-list/stratos-dev"      . ?S)
          ("/linaro-list/linaro-team-toolchain"  . ?T)
          ("/linaro-list/linaro-tcwg"  . ?c)
          ("/kernel/lkml"      . ?l)
          ("/virtualization/qemu" . ?q)
          ("/virtualization/qemu-arm" . ?a)
          ("/virtualization/qemu-multithread" . ?M)
          ("/virtualization/kvm-arm" . ?k)
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
      (let ((pid (or (mu4e-message-field-raw msg :in-reply-to)
                     (mu4e-message-field-raw msg :message-id)))
            (subj (mu4e-message-field-raw msg :subject)))
        (when (and (string-match parent-id pid)
                   subj
                   (string-match my-extract-patch-title-from-series subj))
          (add-to-list 'my-mu4e-patches msg))))

    (defun my-mu4e-unapplied-patch-match (msg parent-id)
      "Same at `my-mu4e-patch-match' but only selecting un-applied
patches."
      (let ((subj (mu4e-message-field-raw msg :subject)))
        (when (and (string-match parent-id
                                 (or
                                  (mu4e-message-field-raw msg :in-reply-to)
                                  (mu4e-message-field-raw msg :message-id)))
                 (string-match my-extract-patch-title-from-series
                               subj))
          (message "Checking: %s" subj)
          (unless (my-magit-check-if-subject-merged
                   (match-string-no-properties 1 subj) "HEAD"
                   default-directory)
            (add-to-list 'my-mu4e-patches msg)))))

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

    (setq mu4e-headers-custom-markers
          (delete-dups
           (append mu4e-headers-custom-markers
                   '(("Patches" my-mu4e-patch-match my-mu4e-patch-setup)
                     ("Unapplied patches" my-mu4e-unapplied-patch-match my-mu4e-patch-setup)))))
    ;; Message actions
    (setq mu4e-view-actions
          (delete-dups
           (append
            '(("gapply git patches" . mu4e-action-git-apply-patch)
              ("mgit am patch" . mu4e-action-git-apply-mbox)
              ("bb4 am patch" . mu4e-action-git-apply-b4)
              ("ssetup reword list with b4" . mu4e-action-setup-reword-b4)
              ("crun checkpatch script" . my-mu4e-action-run-check-patch)
              ("MCheck if merged" . my-mu4e-action-check-if-merged)))))
    ;; Bookmarks
    (setq mu4e-bookmarks
          (cond
           (I-am-at-work
            '(
              ;; Work bookmarks
              (:name "Unread posts addressed to me"
               :query "recip:alex.bennee@linaro.org flag:unread"
               :key ?M)
              (:name "Unreplied posts addressed to me"
               :query "to:/alex.bennee/ and not (flag:replied or flag:trashed or s:Re)"
               :key ?U)
              (:name "Unread list email addressed to me"
               :query "recip:alex.bennee@linaro.org flag:unread AND NOT maildir:\"/Inbox\""
               :key ?m)
              (:name "Mail addressed to me with git tags"
               :query "recip:/alex.bennee/ AND \( \(reviewed ADJ by\) OR \(signed ADJ off ADJ by\) \)"
               :hide-unread t
               :key ?g)
              (:name "Mail sent by me (unread replied)"
               :query "(flag:replied OR (recip:/alex.bennee/ AND s:Re)) NOT flag:seen"
               :key ?S)
              (:name "Mail sent by me"
               :query "f:/alex.bennee/ and not flag:draft"
               :key ?s)
              (:name "My Patches"
               :query "\(from:/alex.bennee/ OR from:/bennee.com/\) AND s:PATCH NOT s:Re"
               :hide-unread t
               :key ?p)
              (:name "My tags"
               :query "from:/alex.bennee/ AND \ \(b:\"Reviewed\" OR b:/Tested-by/ OR b:/Acked-by/ \)"
               :hide-unread t
               :key ?r)
              (:name "Pull Reqs with my name"
               :query "s:PULL \(b:Bennée OR b:Bennee\)"
               :hide-unread t
               :key ?P)
              (:name "Flagged and starred posts"
               :query "flag:flagged"
               :key ?f)
              (:name "Unread flagged and starred posts"
               :query "flag:flagged NOT flag:seen"
               :key ?F)
              (:name "From my boss"
               :query "to:alex.bennee@linaro.org AND (from:/maxim.kuvyrkov/ OR f:/mike.holmes/)"
               :key ?B)
              (:name "From my Engineers"
               :query "recip:alex.bennee@linaro.org \
AND (\
f:takahiro.akashi@linaro.org OR \
f:viresh.kumar@linaro.org OR \
f:peter.maydell@linaro.org OR \
f:richard.henderson@linaro.org OR\
f:philmd@linaro.org \
) AND NOT (flag:list OR recip:nongnu.org)"
               :key ?e)
              (:name "In the last hour"
               :query "date:1h..now"
               :hide-unread t
               :key ?h)
              (:name "In the last hour (unread)"
               :query "date:1h..now AND flag:unread"
               :hide-unread t
               :key ?H)
              (:name "My emails in the last week"
               :query "from:/alex.bennee/ date:1w..now"
               :key ?w)
              ;;:name "Bugs" and CI stuff
              (:name "Bugs and Notifications"
               :query "recip:/alex.bennee/ AND (f:bugs.debian.org OR f:bugs.launchpad.net OR f:gitlab.com)"
               :key ?b)
              (:name "CI Notifications"
               :query "f:noreply@shippable.com OR f:builds@travis-ci.org"
               :key ?C)
              ;; Virt related
              (:name "Latest QEMU posts (unread)"
               :query "(list:/qemu-devel.*/ OR recip:qemu-devel@nongnu.org) and flag:unread"
               :hide-unread t
               :key ?q)
              (:name "Latest QEMU Maintainer Posts"
               :query "recip:/alex.bennee/ AND recip:/qemu-devel/ AND (b:/fpu/ OR b:\"tests/fp\" OR b:\"tests/tcg/multiarch\" OR b:gdbstub OR b:plugin OR b:semihosting OR b:docker OR b:\"tests/vm\" OR b:gitlab OR b:\"tests/tcg/Makefile\" OR b:gitdm)"
               :hide-unread t
               :key ?Q)
              (:name "QEMU ARM posts"
               :query "((list:qemu-devel* OR recip:qemu-devel@nongnu.org) AND (s:aarch64 OR s:arm OR s:A64)) OR (list:qemu-arm* OR recip:qemu-arm@nongnu.org)"
               :hide-unread t
               :key ?a)
              (:name "virtio-dev posts"
               :query "recip:virtio-dev"
               :key ?V)
              (:name "QEMU Softfloat Posts"
               :query "((list:qemu-devel* OR list:qemu-arm* OR recip:qemu-*) AND (s:fpu OR s:softfloat OR s:float))"
               :hide-unread t
               :key ?o)
              (:name "Project Stratos"
               :query "maildir:/linaro-list/stratos-dev OR recip:stratos-dev@op-lists.linaro.org"
               :key ?R)
              (:name "Xen Devel (unread)"
               :query "(list:xen-devel* OR recip:xen-devel@lists.xenproject.org) and flag:unread"
               :key ?x)
              (:name "Valgrind"
               :query "list:valgrind*"
               :hide-unread t
               :key ?v)
              ;; Linaro Specific
              (:name "Linaro public TCWG posts"
               :query "list:linaro-toolchain.lists.linaro.org OR maildir:/linaro-list/linaro-tcwg"
               :hide-unread t
               :key ?T)
              (:name "Linaro private TCWG posts"
               :query "to:tcwg@linaro.org"
               :hide-unread t
               :key ?t)
              (:name "Latest Conf emails"
               :query "list:conf.lists.linaro.org AND flag:unread"
               :hide-unread t
               :key ?c)
              (:name "Latest Linaro-Dev emails"
               :query "list:linaro-dev.lists.linaro.org AND flag:unread"
               :hide-unread t
               :key ?d)
              (:name "Latest Linaro List emails"
               :query "(list:linaro* OR recip:op-lists.linaro.org) AND flag:unread"
               :hide-unread t
               :key ?L)
              (:name "Unread work mailing lists (lists.linaro.org)"
               :query "from:linaro.org and flag:unread"
               :hide-unread t
               :key ?l)
              ;; Distro and others
              ;; Emacs
              (:name "Latest unread Emacs developer posts"
               :query "list:emacs-devel.gnu.org and flag:unread"
               :hide-unread t
               :key ?E)
              (:name "Latest unread Emacs user posts"
               :query "list:help-gnu-emacs.gnu.org and flag:unread"
               :hide-unread t
               :key ?e)
              (:name "Latest unread org-mode posts"
               :query "list:emacs-orgmode.gnu.org and flag:unread"
               :hide-unread t
               :key ?O)))
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

(defun my-defer-mu4e-patch-highlight ()
  "Set-up to highlight patch once we are idle."
  (run-with-idle-timer
     0.1 nil
     (lambda (buffer)
       (with-current-buffer buffer
         (mu4e-patch-highlight))) (current-buffer)))

(use-package mu4e-patch
  :load-path (lambda () (my-return-path-if-ok
                         "~/src/emacs/mu4e-patch.git"))
  :config (add-hook 'mu4e-view-mode-hook #'my-defer-mu4e-patch-highlight))


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
