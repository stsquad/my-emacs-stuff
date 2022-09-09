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

(add-to-list 'completion-category-overrides '((email (basic orderless))))

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

;; this does raise a question about company sorting when it shouldn't,
;; for example when mu4e provides the list of emails...
(use-package company-statistics
  :ensure t
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

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))


(provide 'my-company)
;;; my-company.el ends here

