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


;; my-find-binary
;
                                        ; Handy for dumping objdump into a buffer
(use-package my-find-binary
  :commands find-binary-file)

;; Dired stuff
(add-hook 'dired-mode-hook
          (lambda ()
            (setq truncate-lines t)))


(provide 'the-rest)
;;; the-rest.el ends here

