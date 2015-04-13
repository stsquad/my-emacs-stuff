;;; my-hydra --- Hydra configurations
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
;; I'm currently experimenting with Hydras as an entry point
;;
;;; Code:

;; Require prerequisites
(require 'use-package)
(require 'my-display)
(require 'my-toggles)
(require 'my-org)

(use-package hydra
  :config
  (progn
    ;; Multiple cursors
    (when (fboundp 'mc/mark-all-like-this-dwim)
      (global-set-key
       (kbd "C-x ;")
       (defhydra my-hyrda-mc (:color blue)
         "Multiple cursors"
         (";" mc/mark-all-like-this-dwim "mark-all-like-this-dwim")
         ("n" mc/mark-next-like-this "mark-next-like-this" :color
          red))))
    ;; org-mode hydra
    (when (fboundp 'org-agenda)
      (global-set-key
       (kbd "C-c C-o")
       (defhydra my-hydra-org (:color blue)
         "Access org-mode"
         ("a" org-agenda "org-agenda")
         ("c" org-capture "org-capture"))))
    ;; Toggles with hydra
    (require 'whitespace)
    (global-set-key
     (kbd "C-x t")
     (defhydra my-hydra-toggle (:color blue)
       (concat "toggles:\n"
               "debug-on-error: %`debug-on-error debug-on-quit: %`debug-on-quit"
               " auto-fill-mode: %`auto-fill-function"
               " truncate-lines: %`truncate-lines\n"
               "whitespace-mode: %`whitespace-mode"
               " visual-line-mode: %`visual-line-mode\n")
       ;; Debugging
       ("d" toggle-debug-on-error "d-o-e")
       ("q" toggle-debug-on-quit "d-o-q")
       ;; Narrowing, region selection
       ("n" my-narrow-or-widen-dwim "arrow-or-w")
       ("e" er/expand-region "xpand-r")
       ;; Fill, whitespace and other display modes
       ("f" auto-fill-mode "auto-fill")
       ("w" whitespace-mode "whitespace")
       ("t" toggle-truncate-lines "truncate-l")
       ("l" visual-line-mode "visual-line")
       ;; quit the hydra
       ("q" nil "quit" :color red)))))

(provide 'my-hydra)
;;; my-hydra.el ends here

