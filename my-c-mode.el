;;; my-c-mode.el ---  My C Mode Customisations
;;
;;; Commentary:
;;
;; I have declared c-mode bankruptcy and removed all customisations
;; and style guessing code from here. This is better handled by things
;; like .dir-locals or editorconfig fles in each project.
;;
;;; Code:

(require 'use-package)
(require 'my-vars)

(use-package cc-mode
  :commands c-mode)

;;
;; End of c-mode customisations
;;

(provide 'my-c-mode)
