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
  :commands dired
  :config (progn
            (add-hook 'dired-mode-hook
                      'my-dired-enable-recursive-delete)
            (setq dired-dwim-target t)))

(use-package dired-async
  :ensure async)

(use-package dired-quick-sort
  :ensure t
  :config (dired-quick-sort-setup))

(defun my-dired-frame (directory)
  "Open up a dired frame which closes on exit."
  (interactive)
  (switch-to-buffer (dired directory))
  (local-set-key
   (kbd "C-x C-c")
   (lambda ()
     (interactive)
     (kill-this-buffer)
     (save-buffers-kill-terminal 't))))

;; Hydras
(with-eval-after-load 'dired
  (progn
    ;; Inside dired
    (define-key dired-mode-map
      (kbd "C-x t")
      (defhydra my-hydra-dired
        (:hint nil :color red :timeout 5)
        "
Number of marked items: %(length (dired-get-marked-files))
"
        ("m" dired-mark "mark")
        ("x" wdired-change-to-wdired-mode "wdired" :exit t)))

    ;; Global access to dired
    (global-set-key
     (kbd "C-x d")
     (defhydra my-hydra-directory (:exit t :hint nil :color red :timeout 5)
       "
^Dired Browse^   ^Change default-directory^
----------------------------------------------------------------
_f_rom         _c_urrent default-directory: %`default-directory
_b_rowse      _l_ast set default-directory: %`my-last-set-directory
_h_ome        _r_eset default-directory to: %(file-name-directory (or (buffer-file-name) default-directory))
               _s_et new default-directory
"
       ;; Set
       ("s" my-set-default-directory)
       ("l" (lambda () (interactive) (my-set-default-directory my-last-set-directory)))
       ("r" (lambda () (interactive) (my-set-default-directory (file-name-directory (buffer-file-name)))))
       ;; Browse
       ("f" (lambda () (interactive) (dired default-directory)))
       ("c" (lambda () (interactive) (dired default-directory)))
       ("b" ido-dired)
       ("h" (lambda () (interactive) (dired "~")))))))

(provide 'my-dired)
;;; my-dired.el ends here

