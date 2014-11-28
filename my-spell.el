;;; my-spell.el --- Spelling customisation
;;
;;; Commentary:
;;
;; I prefer using aspell as it is utf-8 safe
;;
;;; Code:

(require 'my-utils)

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (progn
    ;; Add hooks to enable flyspell
    (add-hook 'text-mode-hook 'turn-on-flyspell)
    (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
    (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (use-package ispell
      :config
      (let ((spell-path (which-lookup '("aspell" "ispell")))) ; aspell is preferred
        (when spell-path
          (setq ispell-program-name spell-path
                ispell-dictionary "british"))))))
  
(defun turn-on-flyspell ()
  "Force 'flyspell-mode' on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

(provide 'my-spell)
;;; my-spell.el ends here
