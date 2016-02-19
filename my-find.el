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

(use-package flx)

;; ivy is a general completion framework, I only use it for swiper ATM
;; C-o enters options via ivy-hydra
(use-package ivy
  :config
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

;; See swiper-map for extra keys
(use-package swiper
  :bind ("C-s" . swiper))

(defun my-project-find (&optional directory)
  "Search within `DIRECTORY' using various search helpers.
If no directory is passed try and infer from the `default-directory' or
variable `buffer-file-name'."
  (interactive)

  ;; Use default-directory or buffer-name if nothing passed
  (when (not directory)
    (setq directory (or default-directory buffer-file-name)))

  (let ((is-git-dir (locate-dominating-file directory ".git")))
    (if (and
         is-git-dir
         (file-directory-p is-git-dir)
         (functionp 'helm-git-grep))
        (let ((default-directory directory))
          (call-interactively #'helm-git-grep))
    (call-interactively #'helm-do-ag directory))))

(global-set-key (kbd "<f5>") 'my-project-find)

(provide 'my-find)
;;; my-find.el ends here
