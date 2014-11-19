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
(require 'my-tracking)

;; want to reduce the amount of white space in the mode-line
(setq global-mode-string
      '("" org-mode-line-string))

;; Smart Mode line
(use-package smart-mode-line
  :idle (sml/setup)
  :config
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
   mode-line-buffer-identification '("%b")))


(use-package diminish
  :config
  (progn
    (diminish 'auto-fill-function "Fl")
    (diminish 'abbrev-mode "Ab")))

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

(provide 'my-modeline)
;;; my-modeline.el ends here

