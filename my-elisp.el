;;; my-elisp --- elisp related customisation
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
;; Not much in here.
;;
;;; Code:

(require 'use-package)

(defun my-elisp-compile-buffer ()
  "Compile the current buffer."
  (interactive)
  (let ((bfn (buffer-file-name)))
    (when bfn
        (byte-compile-file bfn))))

(use-package eldoc
  :commands eldoc-mode
  :diminish "")

;; see https://github.com/joddie/macrostep/issues/11
(defun my-macrostep-expand-wrapper ()
  "Workaround `macrostep-expand' not liking white-space after a sexp."
  (interactive)
  (when (and (= ?\n (char-after))
             (= (point) (cdr (bounds-of-thing-at-point 'sexp))))
    (backward-char))
  (macrostep-expand))

(use-package macrostep
  :ensure t
  :commands macrostep-expand)

(defun my-elisp-hook-functions ()
  "A few quick elisp hook customisation."
  (setq mode-name "elisp")
  (eldoc-mode t)
  (when buffer-file-name
    (local-set-key (kbd "C-c C-c") 'my-elisp-compile-buffer)))

(use-package lisp-mode
  :commands emacs-lisp-mode
  :hook (emacs-lisp-mode . my-elisp-hook-functions)
  :mode ((".*\.el.gz" . emacs-lisp-mode))
  :bind (:map emacs-lisp-mode-map
              ("C-x e" . my-macrostep-expand-wrapper)
              ("C-x i" . ielm))
  :config
  (add-to-list 'safe-local-variable-values
               '(lisp-indent-function . common-lisp-indent-function)))

;; This seems to cause problems for booting up emacs daemon
(use-package ert-async
  :after ert
  :config (remove-hook 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)
  :hook (emacs-lisp-mode . ert-async-activate-font-lock-keywords))


(provide 'my-elisp)
;;; my-elisp.el ends here

