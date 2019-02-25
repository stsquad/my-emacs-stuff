;;; the-rest --- Dumping ground for un-cleaned up code
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
;; I'm cleaning up my init.el to make it lean and sleak and
;;use-package friendly. Rather than nest everything in there I'm
;;dumping it here when I can clean it up piece by piece.
;;
;;; Code:

(message "Doing the-rest")

;;; Miscellaneous functions
(require 'use-package)
(require 'my-utils)



(use-package esup
  :commands esup
  :config
  (progn
    (when (string-match "dumb" (getenv "TERM"))
      (setenv "TERM" "xterm-256color")
      (setq esup-run-as-batch-p t))))

;; my-find-binary
;
                                        ; Handy for dumping objdump into a buffer
(use-package my-find-binary
  :commands find-binary-file)

;; Dired stuff
(add-hook 'dired-mode-hook
          (lambda ()
            (setq truncate-lines t)))

;; remove log4j-mode from auto-mode-alist
;; it just chokes on large files
(setq auto-mode-alist (delq (rassoc 'log4j-mode auto-mode-alist)
                            auto-mode-alist))

;; Re-implement https://www.codesections.com/blog/vim-timestamped/
(defun my-insert-timestamp-or-ret (&optional arg)
  "Insert a simple timestamp if at bol, else call org-return."
  (interactive "P")
  (if (bolp)
      (let ((current-prefix-arg '(16)))
        (call-interactively 'org-time-stamp))
    (org-return arg)))

(define-minor-mode org-simple-timestamps-mode
  "Don's require prefixed timestamps, useful for meetings."
  :lighter " SimpleTS"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'my-insert-timestamp-or-ret)
            map))

(provide 'the-rest)
;;; the-rest.el ends here

