;;; my-devel.el --- Central configuration for development hooks
;;
;;; Commentary:
;;
;; All other development modes should be triggered from here.
;;
;;; Code:

(require 'use-package)
(require 'my-vars)
(require 'my-find)
(require 'my-tracking)

;; Currently I'm still unsettled about which project library to use
(cond
 ((require 'eproject nil t) (load-library "my-eproject"))
 ((require 'projectile nil t) (load-library "my-projectile"))
 (t (load-library "my-project")))

;;
;; Compile Mode
;;

(use-package rx
  :commands rx)

;; See: http://emacs.stackexchange.com/questions/3802/how-can-i-detect-compilation-mode-is-waiting-for-input/3807?noredirect=1#comment5796_3807
(defun my-compilation-mode-warn-about-prompt ()
  "Pop up a warning if we stall due to interactive config questions."
  (save-excursion
    (let ((re (rx "[" (one-or-more (any "n" "N" "m" "M" "Y" "y") "/") "?]"
                   (optional " (NEW)") (zero-or-more whitespace) buffer-end)))
      (when (re-search-backward re nil 'no-error)
        (lwarn 'emacs :warning "Compilation process in %s seems stalled!"
               (buffer-name))))))

(use-package compile
  :bind (("C-c c" . compile)
         ("C-c r" . recompile))
  :diminish ((compilation-in-progress . "*COM*"))
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
    (define-key
      compilation-mode-map (kbd "n") 'compilation-next-error)
    (define-key
      compilation-mode-map (kbd "p") 'compilation-previous-error)
    ;; Detect stalls
    (add-hook 'compilation-filter-hook
              #'my-compilation-mode-warn-about-prompt)
    ;; Add tracking to the compilation buffer
    (when (fboundp 'tracking-add-buffer)
      (defun my-hide-compilation-buffer (proc)
      "Hide the compile buffer"
      (delete-window (get-buffer-window "*compilation*")))

      (defun my-report-compilation-finished (buf exit-string)
        "Report the compilation buffer to tracker"
        (tracking-add-buffer buf)
        (when (fboundp 'global-flycheck-mode)
          (global-flycheck-mode 0)))

      (add-hook 'compilation-start-hook 'my-hide-compilation-buffer)
      (add-hook 'compilation-finish-functions 'my-report-compilation-finished))))

;; Handle Makefile.blah
(use-package make-mode
  :mode ("Makefile\.*" . makefile-gmake-mode))

;; Handle expect files
(use-package tcl
  :mode ("\\.expect\\'" . tcl-mode))

;; Smart Parens
(use-package smartparens
  :commands (smartparens-mode
             smartparens-global-mode
             show-smartparens)
  :init (smartparens-global-mode)
  :config
  (progn
    (require 'smartparens-config)
    ;; Filters
    (sp-local-pair '(mail-mode magit-commit-mode) "'" "'" :actions '(wrap))
    ;; Keymap Tweaks
    (defvar my-sp-map nil
      "My prefixed Smart Parens bindings.")
    (define-prefix-command 'my-sp-map)
    (define-key ctl-x-map "p" 'my-sp-map)
    (define-key my-sp-map "n" 'sp-next-sexp)
    (define-key my-sp-map "p" 'sp-previous-sexp)))

;; maybe (show-paren-mode 1) if no smartparens?


(provide 'my-devel)
;;; my-devel.el ends here
