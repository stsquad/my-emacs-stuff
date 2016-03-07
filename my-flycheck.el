;;; my-flycheck.el --- flycheck configuration
;;
;;; Commentary:
;;
;; Just basic stuff for now.
;;
;;; Code:
;;

(require 'use-package)

(use-package flycheck
  :ensure t
  :if (version<= "24.4" emacs-version)
  :commands global-flycheck-mode
  :init (add-hook 'prog-mode-hook 'global-flycheck-mode)
  :config
  (progn
    ;; Other pkgs
    (use-package flycheck-tip
      :if (locate-library "flycheck-tip")
      :commands 'flycheck-tip-cycle
      :init (define-key flycheck-mode-map (kbd "C-c C-n") 'flycheck-tip-cycle))
    
    ;; Settings
    (setq-default flycheck-emacs-lisp-initialize-packages t
                  flycheck-highlighting-mode 'lines
                  flycheck-check-syntax-automatically '(save))
    ;; Fixups
    (defun my-flycheck-elisp-dirs ()
      "Ensure flycheck has set search directories."
      (when (and (eq major-mode 'emacs-lisp-mode)
		 buffer-file-name)
	(setq flycheck-emacs-lisp-load-path (list (file-name-directory
						   (file-chase-links
						    buffer-file-name))))))

    (add-hook 'flycheck-mode-hook 'my-flycheck-elisp-dirs)))


(use-package flycheck-clangcheck
  :ensure t
  :config
  (when (boundp 'kernel-project-file-visit-hook)
    (defun my-set-kernel-clangcheck-build-path ()
      "Set the build path to the latest kernel build tree."
      (when (my-eproject-is-type-p 'kernel)
        (setq
         flycheck-clangcheck-build-path
         (car (my-kernel-build-dirs (eproject-root))))))
    (add-hook 'kernel-project-file-visit-hook
              'my-set-kernel-clangcheck-build-path)))


(provide 'my-flycheck)
;;; my-flycheck.el ends here
