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

(defun my-get-initial-string ()
  "Return substring from thing-at-point or empty string."
  (substring-no-properties (or (thing-at-point 'symbol) "")))


;; from https://github.com/abo-abo/swiper/issues/1068
(defun my-ivy-with-thing-at-point (cmd &optional dir)
  "Wrap a call to CMD with setting "
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (my-get-initial-string)))))
    (funcall cmd nil dir)))

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

(use-package counsel
  :disabled t
  :config (setq counsel-ag-base-command "ag --vimgrep %s")
  :bind ("<f6>" . my-counsel-ag-from-here))

(defun my-consult-rg-from-here (&optional dir)
  "Start rg but from the directory the file is in (otherwise I would be using git-grep)."
  (interactive "D")
  (funcall #'consult-ripgrep
           (or dir (file-name-directory (buffer-file-name)))
           (my-get-initial-string)))

(use-package consult
  :bind ("<f6>" . my-consult-rg-from-here))

(use-package flx
  :ensure t)

;; See swiper-map for extra keys
(defun my-swoop-with-swiper ()
  "Replicate helm-swoop but with ivy. C-c C-o into occur C-c C-p into wgrep."
  (interactive)
  (swiper (or (and (use-region-p) (buffer-substring (mark) (point)))
              (thing-at-point 'symbol))))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("C-c o" . my-swoop-with-swiper)))

(defun my-ivy-rich-switch-buffer-project (candidate)
  "Find the project compile root of a CANDIDATE."
  (with-current-buffer (get-buffer candidate)
    default-directory))

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
    (if (and is-git-dir (file-directory-p is-git-dir))
        (let ((default-directory directory)
              (initial (my-get-initial-string)))
          (cond
           ((functionp 'consult-git-grep)
            (funcall-interactively #'consult-git-grep is-git-dir initial))
           ((functionp 'counsel-git-grep)
            (call-interactively #'my-counsel-git-grep t (vector initial)))
           (t
            (call-interactively #'vc-git-grep initial))))
      (call-interactively (key-binding (kbd "<f6>")) t (vector directory)))))

(global-set-key (kbd "<f5>") 'my-project-find)

(provide 'my-find)
;;; my-find.el ends here
