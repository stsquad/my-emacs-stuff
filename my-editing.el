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
(use-package ace-jump-mode
  :disabled t
  :bind ("C-x j" . ace-jump-mode))

(use-package avy
  :ensure t
  :bind ("C-x j" . avy-goto-word-or-subword-1))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :commands (mc/edit-lines
             mc/mark-all-like-this mc/mark-all-like-this-dwim
             mc/mark-next-like-this mc/mark-previous-like-this
             mc/skip-to-next-like-this mc/skip-to-previous-like-this
             mc/unmark-next-like-this mc/unmark-previous-like-this)
  :init
  (with-eval-after-load 'hydra
    (global-set-key
     (kbd "C-x ;")
     (defhydra my-hydra-mc
       (:hint nil)
       "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_/_;_] Mark all, DWIM
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  (";" mc/mark-all-like-this-dwim :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil)))))

;; Expand region
(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :bind (("C-@" . my-mark-or-expand-dwim)
         ("C-=" . er/expand-region)))

(defun my-mark-or-expand-dwim (&optional arg)
  "Set the mark (with prefix `ARG') or if mark already set call expand-region."
  (interactive "P")
  (if (or (use-region-p)
          (and mark-active
               (eq (point) (mark))))
      (call-interactively #'er/expand-region)
    (call-interactively #'set-mark-command)))

(defun my-next-mc-or-line-dwim (&optional arg)
  "Do `next-line' or `mc/mark-next-like-this' intelligently (with prefix `ARG').
If the region is less than a line long assume I want to mark the next
  mc entry.  Otherwise treat it as a new line."
  (interactive "P")
  (if (= (line-number-at-pos (region-beginning))
         (line-number-at-pos (region-end)))
      (call-interactively #'mc/mark-next-like-this)
    (call-interactively #'next-line)))

(use-package region-bindings-mode
  :ensure t
  :bind (:map region-bindings-mode-map
              ("C-n" . my-next-mc-or-line-dwim))
  :config (region-bindings-mode-enable))

(use-package ws-butler
  :ensure t
  :defer 120
  :config (ws-butler-global-mode))

(provide 'my-editing)
;;; my-editing.el ends here

