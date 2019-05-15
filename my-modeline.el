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
;; (setq global-mode-string
;;       '("" org-mode-line-string))

(use-package powerline
  :ensure t)

;; Smart Mode line
(use-package smart-mode-line
  :ensure t
  :commands (sml/setup sml/apply-theme)
  :config
  (progn
    (setq-default
     ;; format
     mode-line-format
     '( "%e"
        mode-line-front-space
        mode-line-mule-info
        mode-line-modified
        mode-line-remote
        mode-line-frame-identification
        " "
        mode-line-buffer-identification
        " "
        "%l/%c "
        mode-line-misc-info
        "%[("
        mode-name
        minor-mode-alist
        mode-line-process
        "%n"
        ")%] "
        mode-line-end-spaces
        )
     sml/theme 'dark)
    (sml/setup)
    (sml/apply-theme 'respectful)))

(use-package diminish
  :commands diminish
  :init
  (progn
    (diminish 'auto-fill-function "Fl")
    (diminish 'abbrev-mode "Ab")))

;; (display-time) is needed for appt to display in the mode-line, but
;; we don't want the time taking up precious space.
(use-package time
  :commands display-time-mode
  :init (display-time-mode)
  :config
  (setq display-time-interval 20
        display-time-format 'nil
        display-time-string-forms '( 24-hours ":" minutes )))

;; Displays current function() in programming modes.
(use-package which-func
  :commands which-function-mode
  :init (which-function-mode))

(provide 'my-modeline)
;;; my-modeline.el ends here

