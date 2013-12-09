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
      flycheck-highlighting-mode 'lines)


(defun my-flycheck-elisp-dirs ()
  "Ensure flycheck has set search directories."
  (when (and (eq major-mode 'emacs-lisp-mode)
             buffer-file-name)
    (setq flycheck-emacs-lisp-load-path (list (file-name-directory
                                                (file-chase-links
                                                 buffer-file-name))))))

(add-hook 'flycheck-mode-hook 'my-flycheck-elisp-dirs)

(global-flycheck-mode)

(provide 'my-flycheck)
;;; my-flycheck.el ends here
