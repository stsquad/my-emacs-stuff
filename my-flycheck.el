;;; my-flycheck.el --- flycheck configuration
;;
;;; Commentary:
;;
;; Just basic stuff for now
;;
;;; Code:
;;

(require 'use-package)

(use-package flycheck
  :commands global-flycheck-mode
  :idle (global-flycheck-mode)
  :config
  (progn
    ;; Other pkgs
    (use-package flycheck-tip
      :commands 'flycheck-tip-cycle)

    (define-key flycheck-mode-map (kbd "C-c C-n") 'flycheck-tip-cycle)
    
    ;; Settings
    (setq-default flycheck-emacs-lisp-initialize-packages t
                  flycheck-highlighting-mode 'lines)
    ;; Fixups
    (add-hook 'flycheck-mode-hook 'my-flycheck-elisp-dirs)))

(defun my-flycheck-elisp-dirs ()
  "Ensure flycheck has set search directories."
  (when (and (eq major-mode 'emacs-lisp-mode)
             buffer-file-name)
    (setq flycheck-emacs-lisp-load-path (list (file-name-directory
                                               (file-chase-links
                                                buffer-file-name))))))

(provide 'my-flycheck)
;;; my-flycheck.el ends here
