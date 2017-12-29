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
  :diminish "fs"
  :init
  (progn
    ;; Add hooks to enable flyspell
    (add-hook 'text-mode-hook 'turn-on-flyspell)
    (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
    (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
    ;; Hooks for non-possessive its
    (add-hook 'text-mode-hook 'my-hightlight-non-possessive-its)
    (add-hook 'prog-mode-hook 'my-hightlight-non-possessive-its))
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

(use-package hi-lock
  :commands hi-lock-face-phrase-buffer
  :config (eval-after-load "hi-lock"
            '(assq-delete-all 'hi-lock-mode minor-mode-map-alist)))

(defun my-hightlight-non-possessive-its ()
  "Turn on hi-lock mode for any (potentially incorrect) usage of it's"
  (interactive)
  (hi-lock-face-phrase-buffer "it's"))

(provide 'my-spell)
;;; my-spell.el ends here
