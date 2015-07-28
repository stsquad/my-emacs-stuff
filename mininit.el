;;; mininit.el --- Just enough init
;;
;;; Commentary:
;;
;; A very minimal init for debugging.
;;
;;; Code:

; debugging weird start-up issues.
;(setq debug-ignored-errors (remq 'user-error debug-ignored-errors))
(setq debug-on-error 't)

;; Manually load this (as paths not yet set)
;(eval-when-compile (defvar god-local-mode))
(declare-function my-add-config-paths "~/.emacs.d/my-elisp/my-paths" t t)
(when (load-library "~/.emacs.d/my-elisp/my-paths")
  (my-add-config-paths))

(require 'my-package)

(provide 'mininit)
;;; mininit.el ends here
