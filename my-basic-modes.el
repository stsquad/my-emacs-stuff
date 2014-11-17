;;; my-basic-modes --- Common global modes
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
;; This enables all the common built-in modes I use in Emacs.
;; Specifically these are modes enabled at start and not on demand.
;;
;;; Code:

(require 'use-package)

;; Automagically decompress files
(auto-compression-mode t)

;; Save history
(savehist-mode)

;; Nice window sizing
(use-package golden-ratio
  :idle
  :config
  (golden-ratio))

;; Keep track of my key-presses
(use-package keyfreq
  :idle
  :config
  (progn
    (keyfreq-mode)
    (keyfreq-autosave-mode)))

(provide 'my-basic-modes)
;;; my-basic-modes.el ends here

