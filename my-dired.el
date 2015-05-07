;;; my-dired --- tweaks for dired
;;
;;; Commentary:
;;
;; This syncs a number of minor tweaks to dired usage.
;;
;;; Code:

(require 'use-package)

(use-package dired
  :preface
  (progn
    (defun my-dired-enable-recursive-delete ()
      "Enable easy recursive delete for temporary directories."
      (when (-any?
             (lambda (dir) (string-match dir (expand-file-name default-directory)))
             '("/home/alex/tmp/" "/home/alex/Downloads/"))
        (set (make-local-variable 'dired-recursive-deletes) 'always))))
  :config (add-hook 'dired-mode-hook 'my-dired-enable-recursive-delete))

(use-package dired-async)

(provide 'my-dired)
;;; my-dired.el ends here

