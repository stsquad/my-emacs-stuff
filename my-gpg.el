;;; my-gpg --- GPG related stuff
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
;; Mainly we are just dealing with faff for access paths and X/non-X environments
;;
;;; Code:

(require 'use-package)

;; enable EasyPG handling
; gpg-agent confuses epa when getting passphrase
(defun my-squash-gpg (&rest ignored-frame)
  "Kill any GPG_AGENT_INFO in our environment."
  (setenv "GPG_AGENT_INFO" nil))

;; Keychain access
(when (file-exists-p "~/src/emacs/keychain-environment.git")
  (use-package keychain-environment
    :load-path "~/src/emacs/keychain-environment.git"
    :if (or I-am-at-work I-am-at-home)
    :commands keychain-refresh-environment
    :defer 60
    :config (setq keychain-should-inherit t
                  keychain-be-quick t)))

;; Fix up the frame so we don't send pinentry to the wrong place
(defun my-fixup-gpg-agent (&optional frame)
  "Tweak DISPLAY and GPG_TTY environment variables as appropriate to
`FRAME'."
  (when (not frame)
    (setq frame (selected-frame)))
  (if (display-graphic-p frame)
      (setenv "DISPLAY" (terminal-name frame))
    (setenv "GPG_TTY" (terminal-name frame))
    (setenv "DISPLAY" nil)))

(defun my-grab-ssh-agent-from-tmux ()
  "Grab the SSH config from tmux."
  (interactive)
  (let ((ssh (shell-command-to-string "tmux showenv | grep -v '^-'")))
    (list (and ssh
               (string-match "SSH_AUTH_SOCK=\\(.*?\\)$" ssh)
               (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh))))))

(when (getenv "DISPLAY")
  (add-hook 'after-make-frame-functions 'my-fixup-gpg-agent)
  (add-hook 'focus-in-hook 'my-fixup-gpg-agent))

(use-package epa-file
  :if (string-match "socrates" (system-name))
  :commands epa-file-enable
  :init (epa-file-enable)
  :config
  (progn
    (add-hook 'after-make-frame-functions 'my-squash-gpg t)
    (my-squash-gpg)
    (epa-file-enable)))

(use-package auth-source-pass
  :ensure t
  :if (file-exists-p "~/.password-store")
  :config (auth-source-pass-enable))

(provide 'my-gpg)
;;; my-gpg.el ends here

