;;; my-devel.el --- Central configuration for development hooks
;;
;;; Commentary:
;;
;; All other development modes should be triggered from here.
;;
;;; Code:

(require 'my-vars)
(require 'my-find)
(require 'use-package)

;; Currently I'm still unsettled about which project library to use
(cond
 ((require 'eproject nil t) (load-library "my-eproject"))
 ((require 'projectile nil t) (load-library "my-projectile"))
 (t (load-library "my-project")))

;;
;; Compile Mode
;;
(use-package compile
  :bind (("C-c c" . compile)
         ("C-c r" . recompile))
  :config
  (progn
    (setq
     compilation-auto-jump-to-first-error nil
     compilation-scroll-output t
     compilation-window-height 10)
    ;; lets not overtax the regex matcher on our huge compilation buffers
    (when I-am-at-work
      (setq compilation-error-regexp-alist '(gcc-include gnu)))
    ;; shortcut keybindings
    (define-key compilation-mode-map (kbd "n") 'compilation-next-error)
    (define-key compilation-mode-map (kbd "p") 'compilation-previous-error)))

;; Use tracking with compilation-mode if we have it
(use-package tracking
  :commands tracking-add-buffer
  :init
  (progn
    (defun my-hide-compilation-buffer (proc)
      "Hide the compile buffer"
      (delete-window (get-buffer-window "*compilation*")))

    (defun my-report-compilation-finished (buf exit-string)
      "Report the compilation buffer to tracker"
      (tracking-add-buffer buf)
      (when (fboundp 'global-flycheck-mode)
        (global-flycheck-mode 0))))
  :config
  (progn
    (add-hook 'compilation-start-hook 'my-hide-compilation-buffer)
    (add-hook 'compilation-finish-functions 'my-report-compilation-finished)))

;; Handle Makefile.blah 
(use-package files
  :mode ("Makefile\.*" . makefile-gmake-mode))

(provide 'my-devel)
;;; my-devel.el ends here
