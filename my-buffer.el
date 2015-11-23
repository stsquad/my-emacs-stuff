;;; my-buffer.el --- buffer navigation tweaks
;;
;;; Commentary:
;;
;; Use lusty-explorer if I can, otherwise leave it to ido-mode which
;; has been in emacs since version 22.
;;
;; Still have a bs-show "all" bound to C-x C-b for when I want to see
;; everything
;;
;;; Code:

(require 'use-package)

;; midnight mode, clean-up unused buffers overnight
(use-package midnight
  :defer 1000
  :config (setq midnight-mode t))

;; ido-mode - better buffer selection, although lusty does a lot of it
(use-package ido
  :commands ido-mode
  :defer 100
  :config (ido-mode t))

;;
;; Lusty Explorer
;;
;; moved C-x b to helm-mini (see my-helm)
;; (global-set-key (kbd "C-x b") 'lusty-buffer-explorer)
(use-package lusty-explorer
  :if (locate-library "lusty-explorer")
  :bind (("C-x C-f" . lusty-file-explorer)))


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
