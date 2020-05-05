;;; my-text --- Text and Documentation Mode related tweaks
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package rst
  :mode ((rx (: ".rst" (zero-or-one ".inc") eol)) . rst-mode))

(provide 'my-text)
;;; my-text.el ends here
