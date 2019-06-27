;;; my-editing --- editing helpers
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
;; 
;;
;;; Code:

;; Require prerequisites
(require 'use-package)
(require 'my-compat)
(require 'my-hydra)

;; Variables

;; Code

;; Nice for jumping about windows.
(use-package avy
  :ensure t
  :bind ("C-x j" . avy-goto-word-or-subword-1))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :commands mc/mark-next-like-this
  :bind (:map mc/keymap
              ("C-n" . mc/mark-next-like-this)))

;; Expand region
(defun my-mark-or-expand-dwim (&optional arg)
  "Set the mark (with prefix `ARG') or if mark already set call expand-region."
  (interactive "P")
  (if arg
      (let ((current-prefix-arg nil))
        (setq transient-mark-mode t)
        (call-interactively #'set-mark-command))
    (if (or (use-region-p)
            (and mark-active
                 (eq (point) (mark))))
        (call-interactively #'er/expand-region)
      (call-interactively #'set-mark-command))))

(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :bind (("C-SPC" . my-mark-or-expand-dwim)
         ("C-@" . my-mark-or-expand-dwim)
         ("C-=" . er/expand-region)))

(defun my-next-mc-or-line-dwim (&optional arg)
  "Do `next-line' or `mc/mark-next-like-this' intelligently (with prefix `ARG').
If the region is less than a line long assume I want to mark the next
  mc entry.  Otherwise treat it as a new line."
  (interactive "P")
  (if (= (line-number-at-pos (region-beginning))
         (line-number-at-pos (region-end)))
      (unless multiple-cursors-mode
        (call-interactively #'mc/mark-next-like-this))
    (call-interactively #'next-line)))

(when (locate-library "region-bindings-mode")
  (use-package region-bindings-mode
    ; not in stable  :ensure t
    :commands (region-bindings-mode-enable)
    :bind (:map region-bindings-mode-map
                ("C-n" . my-next-mc-or-line-dwim))
    :init (region-bindings-mode-enable)))

(use-package ws-butler
  :ensure t
  :defer 120
  :config (ws-butler-global-mode))

(provide 'my-editing)
;;; my-editing.el ends here

