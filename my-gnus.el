;;; my-gnus --- Basic GNUs setup (for reading actual news)
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
;; I'm using mu4e for reading my email. However it is occasionally
;; useful to read archives of projects I'm not subscribed to.
;;
;;; Code:

(require 'use-package)

(use-package gnus
  :commands gnus
  :config (setq gnus-select-method '(nntp "news.gmane.org")))

(provide 'my-gnus)
;;; my-gnus.el ends here

