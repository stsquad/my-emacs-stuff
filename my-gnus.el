;;; my-gnus --- Basic GNUs setup -*- lexical-binding: t -*-
;;
;; Copyright (C) 2014 Alex Bennée
;;
;; Author: Alex Bennée <alex@bennee.com>
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;;; Commentary:
;;
;; I'm using mu4e for reading my email.  However it is occasionally
;; useful to read archives of projects I'm not subscribed to.
;;
;;; Code:

(eval-when-compile (require 'use-package))
(require 'transient)

(use-package my-git)

;; GNUS Article Mode Helpers

(defun my-gnus-apply-article-patch ()
  "Take the current article and apply it as a patch."
  (interactive)
  (let ((buffer-file-name
         (make-temp-file "gnus-article" nil ".patch")))
    ;; (set-buffer-modified-p t)
    (save-buffer 0)
    (my-git-apply-mbox buffer-file-name)
    (delete-file buffer-file-name)))

;;
;; This was not as obvious as I hoped, however
;; "<87sjxrxo2q.fsf@member.fsf.org>" pointed the way in the gnus-user
;; group.
(defun my-gnus-fetch-message-id ()
  "Fetch the message-id"
  (mail-header-message-id
   (gnus-data-header
    (gnus-data-find
     (gnus-summary-article-number)))))

(defun my-gnus-b4-article ()
  "Take the current article id and pass to b4"
  (interactive)
  (my-git-fetch-and-apply-via-b4 (my-gnus-fetch-message-id)))

(defvar my-gnus-group-dir-mapping
  '( ("org.nongnu.qemu-devel" . "~/lsrc/qemu.git/")
     ("qemu-devel.nongnu.org" . "~/lsrc/qemu.git/")
     ("kvmarm.lists.cs.columbia.edu" . "~/lsrc/linux.git/")
     ("kvm.vger.kernel.org" . "~/lsrc/linux.git/")
     ("virtualization.lists.linux-foundation.org" . "~/lsrc/linux.git/") )
  "Mapping from newgroups to source tree.")

(defun my-gnus-article-set-dir ()
  "Set the default-directory of the new article buffer."
  (with-current-buffer gnus-article-buffer
    (setq default-directory
          (or (assoc-default gnus-newsgroup-name
                             my-gnus-group-dir-mapping)
              default-directory))))

(defvar my-gnus-group-email-mapping
  '( ("org.nongnu.qemu-devel" . "qemu-devel@nongnu.org")
     ("qemu-devel.nongnu.org" . "qemu-devel@nongnu.org")
     ("virtualization.lists.linux-foundation.org" . "~/lsrc/linux.git/") )
  "Mapping from newgroups to reply address.")

(defvar my-gnus-current-group-list-address nil
  "Current group address, set by query helper")

(defun my-gnus-reply-find-group ()
  "Set `my-current-group-list-address' based on current newsgroup."
  (let ((email
         (assoc-default gnus-newsgroup-name
                        my-gnus-group-email-mapping)))
    (setq my-gnus-current-group-list-address email)))

(defun my-gnus-reply-to-list-address ()
  "Reply to the mailing list address mapped for the current group."
  (interactive)
  (my-gnus-reply-find-group)
  (if my-gnus-current-group-list-address
      (progn
        (gnus-article-reply-with-original)
        (with-current-buffer gnus-article-copy
          (message-add-header "To" my-gnus-current-group-list-address)))
    (user-error "No list address mapped for this group")))

;; Transient wrapper functions for persistent article navigation
(defun my-gnus-article-close-and-quit-dispatch ()
  "Close the current article buffer and quit the article transient."
  (interactive)
  (quit-window)
  (transient-quit-one))

(defun my-gnus-article-toggle-headers-subject-only ()
  "Toggle between showing all headers and only the Subject header."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (if (and (local-variable-p 'gnus-visible-headers)
             (string= gnus-visible-headers "^Subject:"))
        (kill-local-variable 'gnus-visible-headers)
      (setq-local gnus-visible-headers "^Subject:")))
  (gnus-summary-show-article))

(defun my-gnus-article-thread-mark-read-and-next ()
  "Mark the current thread as read and move to the next thread."
  (interactive)
  (gnus-summary-top-thread)
  (gnus-summary-kill-thread)
  (my-gnus-article-next-thread))

(defun my-gnus-article-next-thread ()
  "Move to the next thread and open its first unread article."
  (interactive)
  (gnus-summary-top-thread)
  (gnus-summary-next-thread 1)
  (gnus-summary-display-article (gnus-summary-article-number)))

(defun my-gnus-article-prev-thread ()
  "Move to the previous thread and open its first unread article."
  (interactive)
  (gnus-summary-top-thread)
  (gnus-summary-prev-thread 1)
  (gnus-summary-display-article (gnus-summary-article-number)))


;; Define Gnus Article Mode Transient
(transient-define-prefix my-gnus-article-dispatch ()
  "Transient interface for Gnus Article operations.
Stays open while navigating articles."
  :transient-suffix 'transient--do-stay
  [:description "Gnus Article Operations"
   ["Navigation (Paging)"
    ("SPC" "Next Page"         gnus-summary-next-page)
    ("DEL" "Prev Page"         gnus-summary-prev-page)
    ("n"   "Next Article"      gnus-summary-next-article)
    ("p"   "Prev Article"      gnus-summary-prev-article)
    ("u"   "Next Unread"       gnus-summary-next-unread-article)]
   ["Thread"
    ("N"   "Next Thread"       my-gnus-article-next-thread)
    ("P"   "Prev Thread"       my-gnus-article-prev-thread)
    ("k"   "Mark Read & Next"  my-gnus-article-thread-mark-read-and-next)]
   ["Development Actions"
    ("a"   "Apply Patch"       my-gnus-apply-article-patch)
    ("B"   "Apply (b4)"        my-gnus-b4-article)]
   ["Reply"
    ("R"   "Wide Reply"        gnus-article-wide-reply-with-original :transient nil)
    ("r"   "Reply to List"     my-gnus-reply-to-list-address :transient nil)]
   ["View Options"
    ("v s" "Toggle Subject"    my-gnus-article-toggle-headers-subject-only)
    ("v r" "Raw Source"        gnus-summary-show-raw-article)
    ("v h" "Toggle Header"     gnus-summary-toggle-header)
    ("v g" "Wash HTML"         gnus-article-wash-html)]
   ["Marking"
    ("!"   "Tick (Save)"       gnus-summary-tick-article-forward)
    ("d"   "Mark as Read/Del"  gnus-summary-mark-as-read-forward)]
   ["Quit"
    ("C-g" "Quit Transient"    transient-quit-one :transient nil)
    ("q"   "Quit Article"      my-gnus-article-close-and-quit-dispatch :transient nil)
    ("Q"   "Quit Gnus"         gnus-group-exit :transient nil)]])

;; `gnus-art` package configuration
(use-package gnus-art
  :hook ((gnus-article-prepare . my-gnus-article-set-dir)
         (gnus-article-prepare . my-gnus-article-dispatch)) ; Auto-invoke transient on article entry
  :bind (:map gnus-article-mode-map
              ("C-x t" . my-gnus-article-dispatch)
              ("q"     . my-gnus-article-close-and-quit-dispatch)) ; Override 'q' to use our wrapper
  :config (setq gnus-treat-strip-cr t))


;; Gnus Agent (no changes needed)
(use-package gnus-agent
  :config (setq gnus-agent-synchronize-flags 'ask))

;; Gnus Message (no changes needed)
(use-package gnus-msg
  :config (setq gnus-posting-styles
                '((my-gnus-reply-find-group
                   ("Bcc" my-gnus-current-group-list-address)))))


;; Gnus Summary Mode Helpers
(defun my-gnus-fetch-whole-thread ()
  "like `A R' `T o' `A T' in the summary buffer."
  (interactive)
  (gnus-summary-refer-references)
  (gnus-summary-top-thread)
  (gnus-summary-refer-thread))

;; Define Gnus Summary Mode Transient
(transient-define-prefix my-gnus-summary-dispatch ()
  "Transient interface for Gnus Summary operations.
Stays open for navigation and filtering."
  :transient-suffix 'transient--do-stay
  [:description "Gnus Summary Operations"
   ["Navigation"
    ("RET" "Open Article"    gnus-summary-read-article :transient nil)
    ("n"   "Next Unread"     gnus-summary-next-unread-article)
    ("p"   "Previous Article" gnus-summary-prev-article)
    ("N"   "Next Thread"     gnus-summary-next-thread)
    ("P"   "Prev Thread"     gnus-summary-prev-thread)
    ("^"   "Parent Article"  gnus-summary-refer-parent-article)]
   ["Threading/Viewing"
    ("t"   "Fetch Whole Thread" my-gnus-fetch-whole-thread)
    ("v r" "Raw Source"        gnus-summary-show-raw-article)
    ("v h" "Toggle Header"     gnus-summary-toggle-header)
    ("v g" "Wash HTML"         gnus-article-wash-html)]
   ["Filter/Limit"
    ("/ a" "Author"            gnus-summary-limit-to-author)
    ("/ s" "Subject"           gnus-summary-limit-to-subject)
    ("/ /" "Subject (Regexp)"  gnus-summary-limit-to-subject)
    ("/ w" "Pop Limit"         gnus-summary-pop-limit)]
   ["Marking"
    ("!"   "Tick (Save)"       gnus-summary-tick-article-forward)
    ("d"   "Mark as Read/Del"  gnus-summary-mark-as-read-forward)
    ("c"   "Catchup All"       gnus-summary-catchup-and-exit :transient nil)]
   ["Development"
    ("a"   "Apply Patch"       my-gnus-apply-article-patch)
    ("B"   "Apply (b4)"        my-gnus-b4-article)]
   ["Article Actions"
    ("R"   "Wide Reply"        gnus-article-wide-reply-with-original :transient nil)]
   ["Quit"
    ("q"   "Quit Group"        gnus-summary-exit :transient nil)]])

;; `gnus-summary` package configuration
(use-package gnus-summary
  :bind (:map gnus-summary-mode-map
              ("C-x t" . my-gnus-summary-dispatch))
  :config
  (setq-default
   gnus-summary-line-format "%0{%U%R%z%} %3t %3{│%} %1{%d%} %5k %-20,20n%3{│%} %B%S\n"
   gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M"))
   ;; use references to gather (so patch series are correct)
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   ;; fancy formatting for the thread view
   gnus-sum-thread-tree-root ""
   gnus-sum-thread-tree-false-root ""
   gnus-sum-thread-tree-indent " "
   gnus-sum-thread-tree-leaf-with-other "├► "
   gnus-sum-thread-tree-single-leaf "╰► "
   gnus-sum-thread-tree-vertical "│"))

;; Define Gnus Group Mode Transient
(transient-define-prefix my-gnus-group-dispatch ()
  "Transient interface for Gnus Group operations.
Stays open until explicitly quit."
  :transient-suffix 'transient--do-stay
  [:description "Gnus Group Operations"
   ["Navigation"
    ("RET" "Enter Group"       gnus-group-read-group :transient nil)
    ("l"   "List Subscribed"   gnus-group-list-groups)
    ("L"   "List All"          gnus-group-list-all-groups)
    ("A"   "List Active"       gnus-group-list-active)
    ("/"   "Search Group"      gnus-group-read-ephemeral-search-group)
    ("^"   "Servers"           gnus-group-enter-server-mode :transient nil)]
   ["Manage Groups"
    ("s"   "Subscribe"         gnus-group-subscribe)
    ("u"   "Unsubscribe"       gnus-group-unsubscribe)
    ("g"   "Get New News"      gnus-group-get-new-news)
    ("G"   "Check Current"     gnus-group-get-new-news-this-group)]
   ["Quit"
    ("q"   "Quit Menu"         transient-quit-one)
    ("Q"   "Quit Gnus"         gnus-group-exit :transient nil)]])

;; `gnus-group` package configuration
(use-package gnus-group
  :ensure nil
  :bind (:map gnus-group-mode-map
              ("C-x t" . my-gnus-group-dispatch)))

;; Main `gnus` package configuration
(use-package gnus
  :commands gnus
  :config
  (setq
   gnus-select-method '(nntp "nntp.lore.kernel.org")
   gnus-view-pseudo-asynchronously t
   gnus-activate-level 3
   ;; Scoring
   gnus-use-adaptive-scoring t
   gnus-decay-scores t

   ;; Thread behaviour
   gnus-build-sparse-threads 'some
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   gnus-thread-hide-subtree t

   ;; Thread sorting (primary function is the last)
   gnus-thread-sort-functions
   '(gnus-thread-sort-by-total-score
     gnus-thread-sort-by-number
     gnus-thread-sort-by-most-recent-date)
   gnus-read-active-file nil)

  ;; Have the article view split horizontally thanks to my wide monitors
  (gnus-add-configuration '(article
                            (horizontal 1.0
                                        (summary 0.25 point)
                                        (article 1.0)))))

(provide 'my-gnus)
;;; my-gnus.el ends here
