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

(eval-when-compile (require 'use-package))
(use-package my-helm)

(use-package helm-git-grep
  :disabled t
  :commands helm-git-grep
  :config (setq helm-git-grep-candidate-number-limit nil))

;; from https://github.com/abo-abo/swiper/issues/1068
(defun my-ivy-with-thing-at-point (cmd &optional dir)
  "Wrap a call to CMD with setting "
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (if dir
        (let ((default-directory dir))
          (funcall cmd))
      (funcall cmd))))

(defun my-counsel-ag-from-here (&optional dir)
  "Start ag but from the directory the file is in (otherwise I would
be using git-grep)."
  (interactive "D")
  (my-ivy-with-thing-at-point
   'counsel-ag
   (or dir (file-name-directory (buffer-file-name)))))

(defun my-counsel-git-grep ()
  (interactive)
  (my-ivy-with-thing-at-point
   'counsel-git-grep))

(global-set-key (kbd "<f6>") 'my-counsel-ag-from-here)

(use-package wgrep-helm
  :ensure t)

(use-package flx
  :ensure t)

;; See swiper-map for extra keys
(defun my-swoop-with-swiper ()
  "Replicate helm-swoop but with ivy. C-c C-o into occur C-c C-p into wgrep."
  (interactive)
  (swiper (thing-at-point 'symbol)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-c o" . my-swoop-with-swiper)))

(defun my-ivy-rich-switch-buffer-project (candidate)
  "Find the project compile root of a CANDIDATE."
  (with-current-buffer (get-buffer candidate)
    default-directory))

;; additional transformers for ivy mode (e.g. make ivy-switch-buffer
;; more like helm-mini)
(use-package ivy-rich
  :ensure t
  :init
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (my-ivy-rich-switch-buffer-project (:width 50 :face success)))
           :predicate
           (lambda (cand) (get-buffer cand)))))
  (ivy-rich-mode))

(defun my-counsel-mini ()
  "Emulate helm-mini with my own preferences."
  (interactive)
  (let (collection)
    ;; Go backwards in priority (as add to list prepends by default)
    (setq collection
          (-concat
           ;; the buffer list
           (--map (propertize (format "buf: %s" (buffer-name it))
                             'action 'switch-to-buffer
                             'value it)
                  (buffer-list))
           ;; The recentf files
           (--map (propertize (format "rf: %s" it)
                             'action 'find-file
                             'value it)
                  recentf-list)
           ;; The bookmarks
           (--map
            (propertize (format "bkm: %s" it)
                        'action 'bookmark-jump
                        'value it)
            (bookmark-all-names))))
    ;; Finally
    (let ((result (ivy-read "counsel-mini:" collection)))
      (apply
       (get-text-property 0 'action result)
       (list (get-text-property 0 'value result))))))


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
         (functionp 'counsel-git-grep))
        (let ((default-directory directory))
          (call-interactively #'counsel-git-grep t (vector (thing-at-point 'symbol))))
    (call-interactively #'my-counsel-ag-from-here directory))))

(global-set-key (kbd "<f5>") 'my-project-find)

(provide 'my-find)
;;; my-find.el ends here
