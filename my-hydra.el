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
;; This really just configures hydra.  Actual hydras should be defined
;; in the respective modules.  You can wrap the defhydra with a:
;;
;; (with-eval-after-load 'hydra
;;   (global-set-key
;;    (kbd "C-c C-o")
;;    ...)
;;
;;; Code:

;; Require prerequisites
(eval-when-compile (require 'use-package))

(use-package hydra
  :ensure t
  :commands defhydra)

(use-package use-package-hydra
  :ensure t)

(provide 'my-hydra)
;;; my-hydra.el ends here

