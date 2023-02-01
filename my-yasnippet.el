;;; my-yasnippet.el --- customisation and helper functions
;;
;;; Commentary:
;;
;; There isn't much here but I do want to add more Lisp type stuff.
;;
;;; Code:

(require 'use-package)
(require 'my-libs)

(defun my-disable-yas-if-no-snippets ()
  (when (and yas-minor-mode (null (yas--get-snippet-tables)))
    (yas-minor-mode -1)))

(add-hook 'yas-minor-mode-hook #'disable-yas-if-no-snippets)

;; YASnippet itself
(use-package yasnippet
  :ensure t
  :commands (snippet-mode yas-global-mode)
  :defer 60
  :hook (yas-minor-mode . #'my-disable-yas-if-no-snippets)
  :config (progn
            (when (file-exists-p "~/.emacs.d/my-snippets")
              (add-to-list 'yas-snippet-dirs "~/.emacs.d/my-snippets"))
            (yas-global-mode)
            (setq yas-prompt-functions
                  '(yas-ido-prompt yas-completing-prompt yas-no-prompt)))

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
   (t "alex.bennee@linaro.org")))

(defun my-yas-expand-copyright ()
  "Like `my-yas-expand-email' except just return the company name if
I'm in a work for hire directory."
  (if (s-contains-p "lsrc" (buffer-file-name))
      "Linaro Ltd"
    (my-yas-expand-email)))

(defun my-yas-pull-req-helper ()
  "Return a pull request string from a given directory."
  (interactive)
  (let ((base "origin/master")
        (repo "https://gitlab.com/stsquad/qemu.git")
        (head (shell-command-to-string "git describe")))
    (shell-command-to-string
     (format "git request-pull %s %s %s" base repo head))))

(defun my-yas-local-disable ()
  "Disable yas-snippet-mode in this local buffer"
  (yas-minor-mode -1))

(provide 'my-yasnippet)
;;; my-yasnippet.el ends here
