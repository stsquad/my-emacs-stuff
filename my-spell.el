;;; my-spell.el --- Spelling customisation
;;
;;; Commentary:
;;
;; I prefer using aspell as it is utf-8 safe
;;
;;; Code:

(require 'my-utils)

(use-package ispell
  :config
  (let ((spell-path (which-lookup '("aspell" "ispell")))) ; aspell is preferred
    (when spell-path
      (setq ispell-program-name spell-path
            ispell-dictionary "british"))))

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :after ispell
  :diminish "fs"
  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))

(defun turn-on-flyspell ()
  "Force 'flyspell-mode' on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

(defun my-highlight-non-possessive-its ()
  "Turn on hi-lock mode for any (potentially incorrect) usage of it's"
  (interactive)
  (hi-lock-face-phrase-buffer "it's"))

(use-package hi-lock
  :commands hi-lock-face-phrase-buffer
  :hook ((text-mode-hook . my-highlight-non-possessive-its)
         (prog-mode-hook . my-highlight-non-possessive-its))
  :config (eval-after-load "hi-lock"
            '(assq-delete-all 'hi-lock-mode minor-mode-map-alist)))


(provide 'my-spell)
;;; my-spell.el ends here
