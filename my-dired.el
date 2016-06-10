;;; my-dired --- tweaks for dired
;;
;;; Commentary:
;;
;; This syncs a number of minor tweaks to dired usage.
;;
;;; Code:

(eval-when-compile (require 'use-package))
(require 'my-hydra)

(defun my-dired-enable-recursive-delete ()
  "Enable easy recursive delete for temporary directories."
  (when (-any?
         (lambda (dir) (string-match dir (expand-file-name default-directory)))
         '("/home/alex/tmp/" "/home/alex/Downloads/" "/home/alex/torrent/"))
    (set (make-local-variable 'dired-recursive-deletes) 'always)))

(use-package dired
  :config (progn
            (add-hook 'dired-mode-hook
                      'my-dired-enable-recursive-delete)
            (define-key dired-mode-map
              (kbd "C-x t")
              (defhydra my-hydra-dired
                (:hint nil :color red :timeout 5)
                "
Number of marked items: %(length (dired-get-marked-files))
"
                ("m" dired-mark "mark")
                ("x" wdired-change-to-wdired-mode "wdired" :exit t)))))

(use-package dired-async)

(provide 'my-dired)
;;; my-dired.el ends here

