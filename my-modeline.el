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

(unless I-am-on-pixelbook
  (use-package doom-modeline
    :ensure t
    :config
    (setq doom-modeline-icon (or (display-graphic-p) (I-am-at-work))
          doom-modeline-major-mode-icon t
          doom-modeline-major-mode-color-icon t
          doom-modeline-project-detection 'project
          doom-modeline-lsp t
          ;; IRC notifications are handled separately by tracking.el
          doom-modeline-irc nil
          doom-modeline-irc-buffers nil
          doom-modeline-modal-icon t
          doom-modeline-mu4e t
          doom-modeline-minor-modes nil
          doom-modeline-buffer-state-icon t
          doom-modeline-buffer-modification-icon t)
    :hook (after-init . doom-modeline-mode)))

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

