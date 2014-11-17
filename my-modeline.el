;;; my-modeline --- Modeline stuff
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
;; Split from my-display
;;
;;; Code:

(require 'use-package)

;;
;;
;;
;; want to reduce the amount of white space in the mode-line
(setq global-mode-string
      '("" org-mode-line-string))

(setq-default
 ;; format
 mode-line-format '("-"
                    mode-line-mule-info
                    mode-line-modified
                    " "
                    mode-line-buffer-identification
                    " "
                    "%l/%c "
                    "%[("
                    mode-name
                    mode-line-process
                    minor-mode-alist
                    "%n"
                    ")%]"
                    "--"
                    global-mode-string
                    "--")
 ;; Reduce white space
 mode-line-buffer-identification '("%b"))



;; Let's shrink the minor-mode-alist down to size.
(setcdr (assq 'abbrev-mode minor-mode-alist) '(" Ab"))
(setcdr (assq 'auto-fill-function minor-mode-alist) '(" Fl"))

;; Not added until the relevant mode is loaded.
(setq minor-mode-alist (cons '(compilation-in-progress nil)
                             minor-mode-alist))

;; Uses a separate variable. Isn't that nice?
(setq eldoc-minor-mode-string nil)

;; (display-time) is needed for appt to display in the mode-line, but
;; we don't want the time taking up precious space.
(use-package time
  :commands display-time-mode
  :idle (display-time-mode)
  :config
  (setq display-time-interval 20
        display-time-format 'nil
        display-time-string-forms '( 24-hours ":" minutes )))

;; Displays current function() in programming modes.
(use-package which-func
  :idle (which-function-mode))

;; Smart Mode line
(use-package smart-mode-line
  :idle (sml/setup))

;; More tracking config
(use-package tracking
  :idle (tracking-mode)
  :config (setq tracking-most-recent-first t))

(provide 'my-modeline)
;;; my-modeline.el ends here

