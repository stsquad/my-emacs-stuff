;;; my-cperl-mode --- Perl related bindings
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
;; Currently an ill thought out dumping ground
;;
;;; Code:

(require 'use-package)

;; CPerl-mode
(use-package cperl-mode
  :mode ((cons "\\.\\([pP][Llm]\\|al\\)\\'" 'cperl-mode)
         (cons "\\.plx\\'" 'cperl-mode)
         (cons "\\.cgi\\'" 'cperl-mode)
         (cons "\\.pod\\'" 'cperl-mode)
         (cons ".*/perl/.*" 'cperl-mode))
  :interpreter (("perl" . cperl-mode)
                ("perl5" . cperl-mode)
                ("miniperl" . cperl-mode))
  :config
  (progn
    ;; settings
    (setq cperl-info-on-command-no-prompt nil
          cperl-clobber-lisp-bindings     nil
          cperl-electric-parens           nil
          cperl-electric-keywords         nil)
    ;; cperl-mode tries to load abrev before running the hook
    (add-hook 'cperl-mode-hook
              '(lambda ()
                 (cperl-set-style "BSD")
                 (setq cperl-hairy                                 nil
                       cperl-merge-trailing-else                   nil
                       cperl-tab-always-indent                     nil
                       cperl-auto-newline                          nil
                       cperl-electric-lbrace-space                 nil
                       cperl-electric-linefeed                     t
                       cperl-electric-parens                       nil
                       cperl-electric-keywords                     nil
                       cperl-lazy-help-time                        1
                       cperl-extra-newline-before-brace            t
                       cperl-extra-newline-before-brace-multiline  t
                       cperl-max-help-size                         50)
                 (turn-on-auto-fill)
                 (if (not cperl-lazy-installed)	; Only toggle if it's
                     (cperl-toggle-autohelp))	; not already set
                 (if (locate-library "mode-compile")
                     (define-key cperl-mode-map "\C-cr" 'mode-compile))
                 (define-key cperl-mode-map "\C-cc" 'cperl-check-syntax)
                 (define-key cperl-mode-map "\C-j"  'cperl-linefeed)
                 (message "Ran cperl-mode hook")))))

(provide 'my-cperl-mode)
;;; my-cperl-mode.el ends here

