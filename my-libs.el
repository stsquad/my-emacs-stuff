;;; my-libs --- demand loading of common libraries
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
;; Where I use 3rd party libraries in multiple places I put their
;;demand loading code in here.
;;
;;; Code:

;; Require prerequisites
(eval-when-compile (require 'use-package))

;; Variables

;; Code

;; Magnar's handy libs
(use-package dash
  :ensure t)

(use-package s
  :ensure t
  :commands s-suffix? s-contains? s-replace-all s-chop-suffix s-trim s-prefix-p)

(use-package fn
  :ensure t)

;; Helpers for use-package
(use-package bind-key
  :ensure t)

(use-package diminish
  :ensure t)

(use-package async
  :ensure t)

;; Up GC threshold from 800k to 2Mb
(use-package emacs-gc-stats
  :ensure t
  :config (setq gc-cons-threshold (* 2 1024 1024))
  :init (emacs-gc-stats-mode))

;; Other helpers
(defun my-return-path-if-ok (path)
  "Return an expanded file-path if it exists."
  (let ((ep (expand-file-name path)))
    (when (file-exists-p ep)
      ep)))

;;
;; Some helper predicates
;;
(defun my-current-frame-is-portrait-p ()
  "Return t if the current frame is in portrait mode."
  (< (/ (float (frame-width)) (frame-height)) 1.5))

(provide 'my-libs)
;;; my-libs.el ends here
