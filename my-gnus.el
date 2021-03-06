;;; my-gnus --- Basic GNUs setup (for reading actual news)
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

(require 'use-package)
(use-package my-git)

;; GNUS Article Mode

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

(use-package gnus-art
  :hook ((gnus-article-prepare . my-gnus-article-set-dir))
  :bind (:map gnus-article-mode-map
              ("q" . delete-window)
              ("C-c a" . my-gnus-apply-article-patch)
              ("C-c b" . my-gnus-b4-article)
              ("R" . gnus-article-wide-reply-with-original))
  :config (setq gnus-treat-strip-cr t))


(defvar my-gnus-group-email-mapping
  '( ("org.nongnu.qemu-devel" . "qemu-devel@nongnu.org")
     ("qemu-devel.nongnu.org" . "qemu-devel@nongnu.org")
     ("virtualization.lists.linux-foundation.org" . "~/lsrc/linux.git/") )
  "Mapping from newgroups to reply address.")

(defvar my-gnus-current-group-list-address "foo@bar.com"
  "Current group address, set by query helper")

(defun my-gnus-reply-find-group ()
  "Set `my-current-group-list-address' and reply true if we know it"
  (let ((email
         (assoc-default gnus-newsgroup-name
                        my-gnus-group-email-mapping)))
    (if email
        (setq my-gnus-current-group-list-address email)
      (setq my-gnus-current-group-list-address nil))))

;;
;; Package configuration, split into sections
;;
;; Gnus is like really big man, let try and keep config together in
;; one place.
;;

(use-package gnus-agent
  :config (setq gnus-agent-synchronize-flags 'ask))

(use-package gnus-msg
  :config (setq gnus-posting-styles
                '((my-gnus-reply-find-group
                   ("Bcc" my-gnus-reply-find-group)))))

(defun my-gnus-fetch-whole-thread ()
  "like `A R' `T o' `A T' in the summary buffer."
  (interactive)
  (gnus-summary-refer-references)
  (gnus-summary-top-thread)
  (gnus-summary-refer-thread))

(use-package gnus-summary
  :bind (:map gnus-summary-mode-map
              ("C-c t" . my-gnus-fetch-whole-thread)
              ("R" . gnus-article-wide-reply-with-original))
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

;; Everything else

(use-package gnus
  :commands gnus
  :config
  (progn
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
    gnus-read-active-file nil)))


(provide 'my-gnus)
;;; my-gnus.el ends here
