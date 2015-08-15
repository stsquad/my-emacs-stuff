;;; my-basic-modes --- Common global modes
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
;; This enables all the common built-in modes I use in Emacs.
;; Specifically these are modes enabled at start and not on demand.
;;
;;; Code:

(require 'use-package)

;; Automagically decompress files
(auto-compression-mode t)

;; Save history
(use-package savehist
  :defer 60
  :init (savehist-mode))

;; Don't prompt me to revert something
(use-package autorevert
  :defer 240
  :init (global-auto-revert-mode 1))
  
;; Keep track of my key-presses
(use-package keyfreq
  :if (daemonp)
  :defer 240
  :config
  (progn
    (keyfreq-mode)
    (keyfreq-autosave-mode)))

;; Simple access to the calculator
(use-package calculator
  :bind ("C-<return>" . calculator))

;; Recentf
;;
;; This is mainly for the benefit of helm-mini
(use-package recentf
  :commands recentf-mode
  :config (setq recentf-max-saved-items nil
                recentf-exclude '("Maildir/.*")))

;; Help+
(use-package help-fns+
  :commands describe-keymap)

(provide 'my-basic-modes)
;;; my-basic-modes.el ends here
