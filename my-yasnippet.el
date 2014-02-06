;;; my-yasnippet.el --- customisation and helper functions
;;
;;; Commentary:
;;
;; There isn't much here but I do want to add more Lisp type stuff.
;;
;;; Code:

(require 'yasnippet)

(when (file-exists-p "~/.emacs.d/my-snippets")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/my-snippets"))

(setq yas-prompt-functions
      '(yas-ido-prompt yas-completing-prompt yas-no-prompt))

;; Helper functions
(defvar my-yas-emails
  (mapc
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   '(
     (".*/lsrc/.*" . "alex.bennee@linaro.org")
     (".*/mysrc/.*" . "alex@bennee.com")))
  "A mapping from source location to email address.")

(defun my-yas-expand-email ()
  "Return the right email for the current source file."
  (cond
   ((derived-mode-p 'mail-mode 'mu4e-compose-mode)
    user-mail-address)
   ((buffer-file-name)
    (assoc-default (buffer-file-name) my-yas-emails 'string-match))
   (t "alex@....")))

(yas-global-mode)

(provide 'my-yasnippet.el)
;;; my-yasnippet.el ends here
