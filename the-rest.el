;;; the-rest --- Dumping ground for un-cleaned up code
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
;; I'm cleaning up my init.el to make it lean and sleak and
;;use-package friendly. Rather than nest everything in there I'm
;;dumping it here when I can clean it up piece by piece.
;;
;;; Code:

(message "Doing the-rest")

;;; Miscellaneous functions

(require 'my-utils)


;; my-find-binary
;
; Handy for dumping objdump into a buffer
(when (locate-library "my-find-binary")
    (autoload 'find-binary-file "my-find-binary"))


;; Dired stuff
(add-hook 'dired-mode-hook
          (lambda ()
            (setq truncate-lines t)))


;;
;; Simple mail-mode and message-mode hooks.
;;
;; Ostensibly they both do the same thing however message-mode (and
;; the derived mu4e-compose-mode) assume they are sending from within
;; emacs. So I'll use the convention that I'll use mail-mode for
;; edit-server spawned mails and message-mode for the rest
;;

;; Enable mail-mode for mutt spawned files
(add-to-list 'auto-mode-alist '("/tmp/mutt-*" . mail-mode))
(add-to-list 'auto-mode-alist '("0000-cover-letter.patch" . mail-mode))
(add-to-list 'auto-mode-alist '(".*/\.git/\.gitsendemail.MSG.*" . mail-mode))

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
         buffer-file-name;
         (or
          (string-match "/tmp/mutt" buffer-file-name)
          (string-match "gitsend" buffer-file-name)))
    (define-key (current-local-map) (kbd "C-c C-c") 'server-edit)
    (define-key (current-local-map) (kbd "C-c C-s") 'server-edit)))

(add-hook 'mail-mode-hook 'my-mail-mode-tweaks)
(add-hook 'message-mode-hook 'my-common-mail-tweaks)

;; Python Mode
;

(message "Done various programming modes")




;;
;; Tcl (and expect)
;;
(add-to-list 'auto-mode-alist '("\\.expect\\'" . tcl-mode))



(provide 'the-rest)
;;; the-rest.el ends here

