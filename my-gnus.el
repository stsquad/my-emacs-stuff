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

;; GNUS Article Mode
(use-package gnus-art
  :config (define-key gnus-article-mode-map (kbd "q") 'delete-window))

(use-package gnus
  :commands gnus
  :config
  (progn
   (setq-default
    gnus-summary-line-format "%U%R%d %5i %B%-80,80S %-20,20n (%k, %I/%t)\n"
    gnus-user-date-format-alist '((t . "%d-%m-%Y %H:%M"))
    ;; use references to gather (so patch series are correct)
    gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
    ;; fancy formatting for the thread view
    gnus-sum-thread-tree-root ""
    gnus-sum-thread-tree-false-root ""
    gnus-sum-thread-tree-indent " "
    gnus-sum-thread-tree-leaf-with-other "├► "
    gnus-sum-thread-tree-single-leaf "╰► "
    gnus-sum-thread-tree-vertical "│")
   (setq
    gnus-select-method
    '(nnimap "imap.gmail.com"
             (nnimap-inbox "INBOX")
             (nnimap-split-methods default)
             (nnimap-expunge t)
             (nnimap-stream ssl))
    gnus-thread-hide-subtree t
    ;; Thread sorting (primary function is the last)
    gnus-thread-sort-functions
    '(gnus-thread-sort-by-most-recent-number
      gnus-thread-sort-by-total-score
      gnus-thread-sort-by-most-recent-date))))



(provide 'my-gnus)
;;; my-gnus.el ends here

