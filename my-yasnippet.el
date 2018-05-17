;;; my-yasnippet.el --- customisation and helper functions
;;
;;; Commentary:
;;
;; There isn't much here but I do want to add more Lisp type stuff.
;;
;;; Code:

(require 'use-package)
(require 'my-libs)

(use-package pkg-info
  :ensure t
  :commands pkg-info-package-version)

;; YASnippet itself
(use-package yasnippet
  :ensure t
  :commands (snippet-mode yas-global-mode)
  :defer 60
  :config
  (progn
    (when (file-exists-p "~/.emacs.d/my-snippets")
          (add-to-list 'yas-snippet-dirs "~/.emacs.d/my-snippets"))
    (yas-global-mode)
    (setq yas-prompt-functions
          '(yas-ido-prompt yas-completing-prompt yas-no-prompt))))

;; Helper functions
(defvar my-yas-emails
  (mapc
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   '(
     (".*/src/.*" . "alex.bennee@linaro.org")
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

(defun my-yas-pull-req-helper ()
  "Return a pull request string from a given directory."
  (interactive)
  (let ((base "origin/master")
        (repo "https://github.com/stsquad/qemu.git")
        (head (shell-command-to-string "git describe")))
    (shell-command-to-string
     (format "git request-pull %s %s %s" base repo head))))

(provide 'my-yasnippet)
;;; my-yasnippet.el ends here
