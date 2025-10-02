;;; my-modeline --- Modeline stuff -*- lexical-binding: t -*-
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

;; override mood-line's default squashing of faces
(defun my-mood-line-segment-misc-info ()
  "Return the current value of `mode-line-misc-info'."
  (let ((misc-info (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p misc-info)
      (propertize (string-trim misc-info)))))

;; not in latest elpa stable release
(defun my-mood-line-segment-project ()
  "Return project name from project.el or Projectile, if any."
  (or
   (and (fboundp 'project-name)
        (project-current)
        (project-name (project-current)))
   (and (fboundp 'projectile-project-name)
        (projectile-project-name))))

(use-package mood-line
  :ensure t
  :config (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-unicode)
  (mood-line-format '((" "
                       (mood-line-segment-modal)
                       " "
                       (or
                        (mood-line-segment-buffer-status)
                        (mood-line-segment-client)
                        " ")
                       " "
                       (my-mood-line-segment-project)
                       "/"
                       (mood-line-segment-buffer-name)
                       "  "
                       (mood-line-segment-anzu)
                       "  "
                       (mood-line-segment-multiple-cursors)
                       "  "
                       (mood-line-segment-cursor-position)
                       ""
                       #(":" 0 1
                         (face mood-line-unimportant))
                       (mood-line-segment-cursor-point)
                       " "
                       (mood-line-segment-region)
                       " "
                       (mood-line-segment-scroll)
                       "")
                      ((mood-line-segment-indentation)
                       "  "
                       (mood-line-segment-eol)
                       "  "
                       (mood-line-segment-encoding)
                       "  "
                       (mood-line-segment-vc)
                       "  "
                       (mood-line-segment-major-mode)
                       "  "
                       (my-mood-line-segment-misc-info)
                       "  "
                       (mood-line-segment-checker)
                       "  "
                       (mood-line-segment-process)
                       "  " " "))))

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

