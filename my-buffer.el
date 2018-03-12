;;; my-buffer.el --- buffer navigation tweaks
;;
;;; Commentary:
;;
;; I now only use lusty-explorer for file loading. C-x b buffer
;; switching will be helm-mini.
;;
;;; Code:

(require 'use-package)

;; midnight mode, clean-up unused buffers overnight
(use-package midnight
  :defer 1000
  :init (midnight-mode t))

;;
;; Lusty Explorer
;;
;; moved C-x b to helm-mini (see my-helm)
;; (global-set-key (kbd "C-x b") 'lusty-buffer-explorer)
(defun my-lusty-file-explorer()
  "Wrapper to launch, temporarily defaulting completing read."
  (interactive)
  (let ((completing-read-function 'completing-read-default))
    (lusty-file-explorer)))

(use-package lusty-explorer
  :if (locate-library "lusty-explorer")
  :commands lusty-file-explorer
  :bind (("C-x C-f" . my-lusty-file-explorer)))

;; ibuffer has been around for some time
(defun my-ibuffer-bs-show ()
  "Emulate `bs-show' from the bs.el package."
  (interactive)
  (ibuffer nil "*Ibuffer-my-bs*" '((filename . ".*")) nil t)
  (define-key (current-local-map) "a" 'ibuffer-bs-toggle-all))

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . my-ibuffer-bs-show)
  :config
  (progn
    (require 'ibuf-ext)
    (setq ibuffer-saved-filters
          (quote (("mysrc" ((filename . "~/mysrc/*")))
                  ("tramp" ((filename . "\\/ssh:")))
                  ("irc" ((mode . circe-mode)))
                  ("magit" ((mode . magit-status-mode)))
                  ("programming" ((or (mode . emacs-lisp-mode)
                                      (mode . cperl-mode)
                                      (mode . c-mode)
                                      (mode . java-mode)
                                      (mode . idl-mode)
                                      (mode . lisp-mode)))))))))

(provide 'my-buffer)
;;; my-buffer.el ends here
