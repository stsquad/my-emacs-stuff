;;; my-config --- Simple config settings
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
;; This contains the basic config settings of the system *only*.
;;
;;; Code:

;; Variables

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Default mode is text-mode,
(setq-default major-mode 'text-mode)

;; Don't truncate message buffer. For debugging reasons.
(setq message-log-max t)

;; Less verbosity
(fset 'yes-or-no-p 'y-or-n-p)

;; Obey local variables set in -*- type things
(setq enable-local-variables t)

;; Silently add trailing newline to file
(setq require-final-newline t)

;; ^K deletes line, not delete to EOL
(setq-default kill-whole-line t)

;; Searches are case sensitive
(setq-default case-fold-search nil)

;; Use .el if it's newer
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

;; Moar history
(setq history-length 1000)

;; Seriously the kernel TAGS is >10Mb
(setq large-file-warning-threshold 40000000)

;; also large files tend to upset the warnings
;;(add-to-list 'warning-suppress-types 'undo)
;;(add-to-list 'warning-suppress-types 'discard-info)

;; Stop popping up the file dialog, very annoying when compile-mode
;; want to find an error in a non-existent file
(setq use-file-dialog nil
      use-dialog-box nil)

;; X selection magic
(setq x-select-enable-primary t) ; ensure killed text goes to primary

;; Move the custom file out of init.el
(setq custom-file "~/.emacs.d/my-custom.el")

(provide 'my-config)
;;; my-config.el ends here


