;;; my-find --- General searching configuration
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
;; This provides a very basic find front-end for f5/f6. It mostly
;; defers to packages and leaves f5 to be over-ridden when running
;; under a project mode.
;;
;;; Code:

;; Require prerequisites

;; Variables

;; Code

(require 'use-package)

(use-package ack-and-a-half
  :commands ack-and-a-half
  :init (global-set-key (kbd "<f6>") 'ack-and-a-half))

(use-package helm-ag
  :commands helm-ag
  :init (global-set-key (kbd "<f6>") 'helm-do-ag))

(use-package swiper
  :bind ("C-s" . swiper))

(defun my-project-find (prefix)
  "Search within the default-directory. This is mainly for
  the benefit of things like mail. Pass the prefix to the ultimate
  function."
  (interactive "P")
  (if (and (file-directory-p (format "%s.git/" default-directory))
           (functionp 'helm-git-grep))
      (call-interactively #'helm-git-grep)
    (call-interactively #'helm-do-ag default-directory)))

(global-set-key (kbd "<f5>") 'my-project-find)

(provide 'my-find)
;;; my-find.el ends here
