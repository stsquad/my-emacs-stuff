;;; my-dired --- tweaks for dired
;;
;;; Commentary:
;;
;; This syncs a number of minor tweaks to dired usage.
;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'my-utils)
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

(use-package dired-quick-sort
  :config (dired-quick-sort-setup))

(global-set-key
   (kbd "C-x d")
   (defhydra my-hydra-directory (:exit t :hint nil :color red :timeout 5)
     "
^Dired Browse^               ^Change default-directory^
----------------------------------------------------------------
_b_rowse (select dir)        Cur: %`default-directory
from _h_ome (~)              _s_et new default-directory
from _d_efault-directory     _l_ast set %`my-last-set-directory
"
     ;; Set
     ("s" my-set-default-directory)
     ("l" (lambda () (interactive) (my-set-default-directory my-last-set-directory)))
     ;; Browse
     ("b" ido-dired)
     ("d" (lambda () (interactive) (dired default-directory)))
     ("h" (lambda () (interactive) (dired "~")))))


(provide 'my-dired)
;;; my-dired.el ends here

