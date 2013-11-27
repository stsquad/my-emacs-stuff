;;; my-flycheck.el --- flycheck configuration
;;
;;; Commentary:
;;
;; Just basic stuff for now
;;
;;; Code:
;;

(require 'flycheck)

; settings
(setq flycheck-emacs-lisp-initialize-packages t
      flycheck-highlighting-mode 'lines
      flycheck-emacs-lisp-load-path (list "~/.emacs.d/"))

(global-flycheck-mode)

(provide 'my-flycheck)
;;; my-flycheck.el ends here
