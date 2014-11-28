;;; my-elisp --- elisp related customisation
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
;; Not much in here.
;;
;;; Code:

(require 'use-package)

(defun my-elisp-compile-buffer ()
  "Compile the current buffer."
  (interactive)
  (byte-compile-file (buffer-file-name)))

(use-package eldoc
  :commands eldoc-mode
  :diminish "")

(defun my-elisp-hook-functions ()
  "A few quick elisp hook customisation."
  (setq mode-name "elisp")
  (eldoc-mode t)
  (local-set-key (kbd "C-c C-c") 'my-elisp-compile-buffer)
  (turn-on-auto-fill))

(use-package lisp-mode
  :commands emacs-lisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'my-elisp-hook-functions))

(use-package macrostep
  :ensure macrostep
  :commands macrostep-expand
  :init (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))

(provide 'my-elisp)
;;; my-elisp.el ends here

