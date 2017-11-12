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

;; Magnar's string handling library
(use-package s
  :ensure t
  :commands s-contains? s-replace-all s-chop-suffix s-trim)

(use-package fn
  :ensure t)

;; Helpers for use-package
(use-package bind-key
  :ensure t)

(use-package diminish
  :ensure t)

(provide 'my-libs)
;;; my-libs.el ends here

