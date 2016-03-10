;;; my-web --- Web related hooks and hacks
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
;; I've tried to separate this from the spawling my-devel bit
;;
;;; Code:

(require 'use-package)

;; BROKEN?
(defun my-wordpress-hook ()
  "Hook function for editing Wordpress code."
  (interactive)
  (setq tab-width 4
	indent-tabs-mode 't))

(defvar my-php-hooks-alist
  (mapc
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   '(
     (".*/wp-content.*php$" . my-wordpress-hook )
     (".*/wordpress.*php$" . my-wordpress-hook )))
  "A list of reg-ex to php-mode hooks.")

;;
;; Web stuff, use web-mode
;;

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (visual-line-mode)
  (flyspell-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.php$" . web-mode)
  :config
  (progn
    (add-hook 'web-mode-hook 'my-web-mode-hook)))

;;
;; js2-mode, override javascript-mode
;;
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (defalias 'javascript-mode 'js2-mode "js2-mode is aliased to javascript mode"))

;;
;; restclient, make REST calls from Emacs
;;
(use-package restclient
  :if (locate-library "restclient")
  :commands restclient-mode)

(provide 'my-web)
;;; my-web.el ends here

