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
(require 'my-flycheck)

(use-package python
  :config
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i"))

(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  (setq elpy-rpc-backend "jedi"
        elpy-rpc-project-specific 't
        elpy-rpc-python-command "python3")
  (eval-after-load 'flycheck-mode
    (when (fboundp 'flycheck-tip-cycle)
      (define-key elpy-mode-map (kbd "C-c C-n")
        'flycheck-tip-cycle))))

;; elpy can be enabled better on demand surely?
;; TODO - automode alist
;;(add-hook 'python-mode-hook #'(lambda () (require 'my-python-mode)))

(provide 'my-python)
;;; my-python.el ends here
