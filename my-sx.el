;;; my-sx --- Stack Exchange Access
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
;; Access to stack Exchange sites
;;
;;; Code:

;; Require prerequisites

;; Variables

(require 'use-package)

;; Code

(use-package sx
  :commands sx-search)


(provide 'my-sx)
;;; my-sx.el ends here

