;; Development Hooks
;;
;; All other development modes should be triggered from here.
;;

(require 'my-find)

(cond
 ((require 'eproject nil t) (load-library "my-eproject"))
 ((require 'projectile nil t) (load-library "my-projectile"))
 (t (load-library "my-project")))

;; Flycheck
(when (locate-library "flycheck")
  (require 'my-flycheck nil t))

;; Compilation mode tweaks
(when I-am-emacs-23+
    (setq compilation-auto-jump-to-first-error 'nil))

(setq compilation-scroll-output t
      compilation-window-height 10)

; lets not overtax the regex matcher on our huge compilation buffers
(when I-am-at-work
  (setq compilation-error-regexp-alist '(gcc-include gnu)))

;; currently errors in CI, must figure out why
;; (define-key compilation-mode-map (kbd "n") 'compilation-next-error)
;; (define-key compilation-mode-map (kbd "p") 'compilation-previous-error)

; Global keybindings for compiling
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

;; If we have the tracking library add compilation buffer when complete
(when (require 'tracking nil 'noerror)
  (defun my-hide-compilation-buffer (proc)
    "Hide the compile buffer"
    (delete-window (get-buffer-window "*compilation*")))

  (defun my-report-compilation-finished (buf exit-string)
    "Report the compilation buffer to tracker"
    (tracking-add-buffer buf))

  (add-hook 'compilation-start-hook 'my-hide-compilation-buffer)
  (add-hook 'compilation-finish-functions 'my-report-compilation-finished))

;; Makefiles
;
; The auto-mode-alist isn't quite set-up to handle Makefile.something
(setq auto-mode-alist
      (append (list (cons "Makefile\.*" 'makefile-gmake-mode))
	      auto-mode-alist))

;; PHP
;

(defun my-wordpress-hook ()
  "Hook function for editing Wordpress code"
  (interactive)
  (setq tab-width 4
	c-basic-offset 4
	indent-tabs-mode 't))

(defvar my-php-hooks-alist
  (mapc
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   '(
     (".*/wp-content.*php$" . my-wordpress-hook )
     (".*/wordpress.*php$" . my-wordpress-hook )))
  "A list of reg-ex to php-mode hooks")

;;
;; Web stuff, use web-mode
;;

;(when (featurep web-mode)

(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))

(provide 'my-devel)
