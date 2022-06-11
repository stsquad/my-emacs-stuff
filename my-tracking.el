;;; my-tracking --- Configuration for tracking buffers
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
;; I had duplicated various use-package settings around the place
;; which didn't work well. So I've drawn them all here.
;;
;;; Code:

;; Require prerequisites
(require 'use-package)

;; More tracking config
(use-package tracking
  :ensure t
  :commands (tracking-mode tracking-add-buffer)
  :init (tracking-mode)
  :config (setq tracking-sort-faces-first t
                tracking-most-recent-first t
                tracking-position 'end))


(provide 'my-tracking)
;;; my-tracking.el ends here

