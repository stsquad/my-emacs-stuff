;;; my-diff --- Centralised configuration of diff/patch handling
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
;; This is an attempt to reduce the mess that is my patch handling
;;stuff. This includes my first derrived mode "my-diff-mode" which was
;;useful (and may still be) for dealing with lots of patchs by hand.
;;
;;; Code:

(require 'use-package)

;; ediff
;;
;; Still needs some loving care to setup properly

(use-package ediff
  :commands (ediff-buffers ediff-files)
  :config
  (progn
    (setq diff-switches               "-ub"
          ediff-custom-diff-options   "-U3"
          ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain)
    (when (display-graphic-p)
      (add-hook 'ediff-before-setup-hook 'new-frame)
      (add-hook 'ediff-quit-hook 'delete-frame)
      (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
      (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
      (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display))))

;; I'm just going to assume I have my-diff-mode
;; We want to find files like
;;
;; 0001-Remove-old-pre-SIGNAL_MANAGER-code.patch
;; mypatch.patch
;; adiff.diff
;; .dotest/0001
;;
;; But not files like
;;  ~/.mozilla/firefox/adxbrp73.default/itsalltext/www.bennee.com.3022372z35.txt

(use-package my-diff-mode
  :commands my-diff-mode
  :mode (("\.diff$" . my-diff-mode)
         ("\.patch$" . my-diff-mode)
         ("\.rej$"  . my-diff-mode)
         ("\\.dotest/[0123456789][0123456789][0123456789][0123456789]". my-diff-mode)))

(provide 'my-diff)
;;; my-diff.el ends here

