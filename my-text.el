;;; my-text --- Text and Documentation Mode related tweaks
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package rst
  :mode ((rx (: ".rst" (zero-or-one ".inc") eol)) . rst-mode))

(use-package tex-mode
  :config (setq tex-verbatim-environments
                '("verbatim" "verbatim*" "Verbatim" "lstlisting")
                tex-fontify-script nil))

(provide 'my-text)
;;; my-text.el ends here
