;;; my-keyhelp --- Helpers for keys and other help
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
;; Variables

;; Code

(use-package which-key
  :ensure t
  :commands which-key-mode
  :defer 60
  :diminish ""
  :config
  (which-key-mode))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)))

(provide 'my-keyhelp)
;;; my-keyhelp.el ends here

