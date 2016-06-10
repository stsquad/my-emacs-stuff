;;; my-dired --- tweaks for dired
;;
;;; Commentary:
;;
;; This syncs a number of minor tweaks to dired usage.
;;
;;; Code:

(require 'use-package)
(require 'my-utils)
(require 'my-hydra)

(defun my-dired-enable-recursive-delete ()
  "Enable easy recursive delete for temporary directories."
  (when (-any?
         (lambda (dir) (string-match dir (expand-file-name default-directory)))
         '("/home/alex/tmp/" "/home/alex/Downloads/" "/home/alex/torrent/"))
    (set (make-local-variable 'dired-recursive-deletes) 'always)))

(use-package dired
  :config (add-hook 'dired-mode-hook 'my-dired-enable-recursive-delete))

(use-package dired-async)

(use-package dired-quick-sort
  :config (dired-quick-sort-setup))

(global-set-key
   (kbd "C-x d")
   (defhydra my-hydra-directory (:exit t :hint nil :color red :timeout 5)
     (concat "in %`default-directory "
      "dired _b_rowse, _s_et new default-directory, set to _l_ast dir %`my-last-set-directory")
     ;;
     ("b" ido-dired)
     ("s" my-set-default-directory)
     ("l" (lambda () (interactive) (my-set-default-directory my-last-set-directory)))))


(provide 'my-dired)
;;; my-dired.el ends here

