;;; mininit.el --- Just enough init
;;
;;; Commentary:
;;
;; A very minimal init for debugging.
;;
;;; Code:

; debugging weird start-up issues.
(setq debug-on-error 't)

;; Manually load this (as paths not yet set)
(when (load "~/.emacs.d/my-elisp/my-paths.el" t t)
  (my-add-config-paths)
  (my-add-git-project-paths))

(require 'my-package)

(provide 'mininit)
;;; mininit.el ends here
