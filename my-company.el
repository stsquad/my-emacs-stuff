;;; my-company --- My personal tweaks for company-mode
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
;; I've been using autocomplete-mode so far so this is currently
;; experimental.  I thought I'd have a look after elpy moved to using
;; company-mode.
;;
;;; Code:

;; Require prerequisites
(eval-when-compile (require 'use-package))

(defvar my-disabled-company-modes
  '(company-bbdb company-eclim company-xcode company-semantic
                 company-clang)
  "Backends I disable/blacklist.")

(use-package company
  :ensure t
  :commands (global-company-mode company-complete-common)
  :init (add-hook 'prog-mode-hook 'global-company-mode)
  :diminish "Cpy"
  :config
  (progn
    ;; Variables
    (setq company-selection-wrap-around t
          tab-always-indent 'complete)
    ;; Remove backends I'll never use
    (setq company-backends
          (--remove
           (-contains-p my-disabled-company-modes it)
           company-backends))
    ;; Keys
    ;; keys active while completing
    (define-key company-active-map (kbd "TAB") 'company-complete)
    (define-key company-active-map [tab] 'company-complete)
    (define-key company-active-map (kbd "<right>") 'company-complete-common)
    ;; keys active in the global minor mode
    (define-key company-mode-map (kbd "M-/") 'company-complete-common)
    (define-key company-mode-map [remap indent-for-tab-command]
      'company-indent-for-tab-command)))

(when (and have-melpa (version<= "25.1" emacs-version))
  (use-package company-lsp
    :ensure t
    :after company
    :config (push 'company-lsp company-backends)))

;; company-irony uses clang, but it should be before company-clang in
;; the company-backends list or it will never get the chance to complete
(use-package company-irony
  :ensure t
  :after irony
  :commands (company-irony company-irony-setup-begin-commands)
  :config (progn
            (add-to-list 'company-backends #'company-irony))
            (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package company-statistics
  :defer 60
  :config (company-statistics-mode))

;; company-yasnippet must be at the end of the list
;; (when (require 'company-yasnippet nil t)
;;   (add-to-list 'company-backends 'company-yasnippet t))
;(delete 'company-yasnippet company-backends)

(defvar completion-at-point-functions-saved nil
  "Saved copy of the `completion-at-point-functions'.")

;; Code
; Wrappers for company-mode to integrate with tab-always-indent's
; complete function.

(defun company-complete-common-wrapper ()
  "A simple wrapper."
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete-common)))

(defun company-indent-for-tab-command (&optional arg)
  "A company-mode wrapper for `indent-for-tab-command'.
`ARG' is passed to `indent-for-tab-command'."
  (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
        (completion-at-point-functions '(company-complete-common-wrapper)))
    (indent-for-tab-command arg)))

(provide 'my-company)
;;; my-company.el ends here

