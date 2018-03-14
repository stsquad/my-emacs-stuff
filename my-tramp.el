;;
;; Tramp Customisations/Tweaks
;;

(require 'use-package)

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
