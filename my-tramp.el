;;; my-tramp --- Tramp Customisation
;;
;;; Commentary:
;;
;; Not much here apart from stopping vc getting happy.
;;
;;; Code:

(require 'use-package)

;; (setq tramp-verbose 10)

(use-package tramp
  :defer
  :config
  (progn
    (setq tramp-default-method "scp"
          ;; I tend to use magit anyway, but disable VC mode over TRAMP
          vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))
    ;; Auto-saves tramp files on our local file system
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp "~/.emacs.d/tramp-saves"))))


(provide 'my-tramp)
;;; my-tramp.el ends here
