;;; the-rest --- Dumping ground for un-cleaned up code
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
;; I'm cleaning up my init.el to make it lean and sleak and
;;use-package friendly. Rather than nest everything in there I'm
;;dumping it here when I can clean it up piece by piece.
;;
;;; Code:

(message "Doing the-rest")

;;; Miscellaneous functions

(require 'my-utils)

;; ediff
;
; Need to setup properly
;
;? Also need to find a way to restore it all on
;  resume. This stuff is all far from bullet-proof.

(eval-when-compile
  (defvar ediff-custom-diff-options)
  (defvar ediff-split-window-function)
  (defvar ediff-window-setup-function))

(eval-after-load "ediff"
  '(progn
     (message "doing ediff customisation")
     (setq diff-switches               "-u"
           ediff-custom-diff-options   "-U3"
           ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain)
     (add-hook 'ediff-before-setup-hook 'new-frame)
     (add-hook 'ediff-quit-hook 'delete-frame)
     (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
     (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
     (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display)))

;; diff-mode and its derivitives
;
; Here we have some tweaks to the diff-mode for testing a series of
; patchs and/or applying a whole patch
;

(when (locate-library "diff-mode")
  (if (locate-library "my-diff-mode")
      (progn
        (message "Hooking in my-diff-mode")
        (autoload 'my-diff-mode "my-diff-mode")
        (defalias 'dmode-alias 'my-diff-mode))
    (autoload 'diff-mode "diff-mode")
    (defalias 'dmode-alias 'diff-mode))

  ; Which ever version we have we need to set the
  ; automode up so it loads when we need it
  (setq auto-mode-alist (append (list
                                 (cons "\.diff$"  'dmode-alias)
                                 (cons "\.patch$" 'dmode-alias)
                                 (cons "\.rej$" 'dmode-alias)
                                 (cons "\.dotest/0.*"
                                       'dmode-alias))
                                auto-mode-alist)))


;; calculator
;
; If we have the calculator library available lets load it in
;

(when (locate-library "calculator")
  (autoload 'calculator "calculator"
    "Run the Emacs calculator." t)
  (global-set-key [(control return)] 'calculator))


;; my-find-binary
;
; Handy for dumping objdump into a buffer
(when (locate-library "my-find-binary")
    (autoload 'find-binary-file "my-find-binary"))

; I like to use .git/.bzr etc in my directory names
(setq completion-ignored-extensions
      (remove ".git/"
              (remove ".bzr/"
                      (remove ".svn/" completion-ignored-extensions))))

;; Dired stuff
(add-hook 'dired-mode-hook
          (lambda ()
            (setq truncate-lines t)))


;;
;; Simple mail-mode and message-mode hooks.
;;
;; Ostensibly they both do the same thing however message-mode (and
;; the derived mu4e-compose-mode) assume they are sending from within
;; emacs. So I'll use the convention that I'll use mail-mode for
;; edit-server spawned mails and message-mode for the rest
;;

;; Enable mail-mode for mutt spawned files
(add-to-list 'auto-mode-alist '("/tmp/mutt-*" . mail-mode))
(add-to-list 'auto-mode-alist '("0000-cover-letter.patch" . mail-mode))
(add-to-list 'auto-mode-alist '(".*/\.git/\.gitsendemail.MSG.*" . mail-mode))

(defun my-common-mail-tweaks ()
  "Enable common mail tweaks for sending messages."
  (interactive)
  (turn-on-flyspell)
  (turn-on-auto-fill))

(defun my-mail-mode-tweaks()
  "Customise mail-mode stuff"
  (interactive)
  (my-common-mail-tweaks)
  (when (and
         buffer-file-name;
         (or
          (string-match "/tmp/mutt" buffer-file-name)
          (string-match "gitsend" buffer-file-name)))
    (define-key (current-local-map) (kbd "C-c C-c") 'server-edit)
    (define-key (current-local-map) (kbd "C-c C-s") 'server-edit)))

(add-hook 'mail-mode-hook 'my-mail-mode-tweaks)
(add-hook 'message-mode-hook 'my-common-mail-tweaks)

;; Python Mode
;
; TODO - automode alist
(add-hook 'python-mode-hook #'(lambda () (require 'my-python-mode)))

(message "Done various programming modes")


(when I-am-at-work
  (setenv "DEBEMAIL" "alex.bennee@linaro.org")
  (setenv "DEBFULLNAME" "Alex Bennée"))


;;
;; Tcl (and expect)
;;
(add-to-list 'auto-mode-alist '("\\.expect\\'" . tcl-mode))



(provide 'the-rest)
;;; the-rest.el ends here

