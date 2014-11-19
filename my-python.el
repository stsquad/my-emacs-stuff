;;; my-python --- Python configuration
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
;; There are two python modes, one supplied with Emacs (python.el) and
;; a Python community one called python-mode.el. elpy works with the
;; Emacs version. Annoyingly both these modes clash so hi-jinks can
;; ensue when your not sure what you are running.
;;
;;; Code:

(require 'use-package)

(use-package elpy
  :commands elpy-enable
  :idle (elpy-enable)
  :config
  (progn
    (setq elpy-rpc-backend "jedi"
          elpy-rpc-project-specific 't)
    (when (fboundp 'flycheck-mode)
      (setq elpy-modules (delete 'elpy-module-flymake elpy-modules)))))

(provide 'my-python)
;;; my-python.el ends here
