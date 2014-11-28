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

;; Keychain access
(use-package keychain-environment
  :idle
  :config
  (keychain-refresh-environment))

;; enable EasyPG handling
; gpg-agent confuses epa when getting passphrase
(defun my-squash-gpg (&rest ignored-frame)
  "Kill any GPG_AGENT_INFO in our environment."
  (setenv "GPG_AGENT_INFO" nil))

(use-package epa-file
  :if (string-match "socrates" (system-name))
  :config
  (progn
    (add-hook 'after-make-frame-functions 'my-squash-gpg t)
    (epa-file-enable)))

(provide 'my-gpg)
;;; my-gpg.el ends here

