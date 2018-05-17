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

(use-package my-hydra)

(use-package flycheck
  :ensure t
  :if (version<= "24.4" emacs-version)
  :commands global-flycheck-mode
  :init (add-hook 'prog-mode-hook 'global-flycheck-mode)
  :config
  (progn
    ;; Settings
    (setq-default flycheck-emacs-lisp-initialize-packages t
                  flycheck-highlighting-mode 'lines
                  flycheck-check-syntax-automatically '(save)
                  flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
    ;; Fixups
    (defun my-flycheck-elisp-dirs ()
      "Ensure flycheck has set search directories."
      (when (and (eq major-mode 'emacs-lisp-mode)
		 buffer-file-name)
	(setq flycheck-emacs-lisp-load-path (list (file-name-directory
						   (file-chase-links
						    buffer-file-name))))))

    (add-hook 'flycheck-mode-hook 'my-flycheck-elisp-dirs)))

;; Other pkgs
(use-package flycheck-tip
  :ensure t
  :after flycheck
  :commands 'flycheck-tip-cycle
  :config (define-key flycheck-mode-map (kbd "C-c C-n") 'flycheck-tip-cycle))

;; mostly obsoleted by flycheck-irony
(use-package flycheck-clangcheck
  :if (locate-library "flycheck-clangcheck")
  :config
  (with-eval-after-load 'eproject
    (when (boundp 'kernel-project-file-visit-hook)
      (defun my-set-kernel-clangcheck-build-path ()
        "Set the build path to the latest kernel build tree."
        (when (my-eproject-is-type-p 'kernel)
          (setq
           flycheck-clangcheck-build-path
           (car (my-kernel-build-dirs (eproject-root))))))
      (add-hook 'kernel-project-file-visit-hook
                'my-set-kernel-clangcheck-build-path))))

(use-package flycheck-package
  :ensure t)

(use-package flycheck-checkpatch
  :config (flycheck-checkpatch-setup)
  :config (setq flycheck-checkers (delete 'checkpatch flycheck-checkers))
  :config (add-to-list 'flycheck-checkers 'checkpatch t))

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :config (flycheck-irony-setup))

(with-eval-after-load 'flycheck
  (define-key flycheck-command-map
   (kbd "t")
   (defhydra my-flycheck-toggle (:hint nil :color blue :timeout 5)
     "
Flycheck options: clangcheck _a_nalyse mode: %`flycheck-clangcheck-analyze"
     ;; Clangcheck
     ("a" (fn (setq flycheck-clangcheck-analyze (not flycheck-clangcheck-analyze)))))))


(provide 'my-flycheck)
;;; my-flycheck.el ends here
