;;; my-flycheck.el --- flycheck configuration
;;
;;; Commentary:
;;
;; Just basic stuff for now. As flycheck needs a fairly recent Emacs
;; this shouldn't be loaded if (version<= "24.4" emacs-version)
;;
;;; Code:
;;

(eval-when-compile (require 'use-package))

(use-package flycheck
  :ensure t
  :if (version<= "24.4" emacs-version)
  :commands flycheck-mode
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq-default flycheck-emacs-lisp-initialize-packages 'auto
                flycheck-emacs-lisp-load-path 'inherit
                flycheck-highlighting-mode 'lines
                flycheck-disabled-checkers '(c/c++-gcc)))

;; Other pkgs
(use-package flycheck-tip
  :ensure t
  :commands 'flycheck-tip-cycle
  :after flycheck
  :bind (:map flycheck-mode-map
              ("C-c C-n" . flycheck-tip-cycle)))

(use-package flycheck-package
  :ensure t)

;; The upstream package is stale so we need to use the local fork
(use-package flycheck-checkpatch
  :load-path (lambda () (my-return-path-if-ok
                         "~/src/emacs/flycheck-checkpatch.git"))
  :config (flycheck-checkpatch-setup)
  :config (setq flycheck-checkers (delete 'checkpatch flycheck-checkers))
  :config (add-to-list 'flycheck-checkers 'checkpatch t))

(provide 'my-flycheck)
;;; my-flycheck.el ends here
