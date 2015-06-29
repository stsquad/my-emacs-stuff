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
(require 'my-hydra)

;; Variables

;; Code

;; Nice for jumping about windows.
(use-package ace-jump-mode
  :bind ("C-x j" . ace-jump-mode))

;; Multiple cursors
(use-package multiple-cursors
  :bind (( "C->" . mc/mark-next-like-this)
         ( "C-<" . mc/mark-previous-like-this)
         ( "C-x ;" . mc/mark-all-like-this-dwim)
         ( "C-+" . mc/mark-all-like-this-dwim)
         ( "M-+" . mc/edit-lines))
  :config
  (with-eval-after-load 'hydra
    (global-set-key
     (kbd "C-x ;")
     (defhydra my-hyrda-mc
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
  :bind ("C-=" . er/expand-region))

(provide 'my-editing)
;;; my-editing.el ends here

