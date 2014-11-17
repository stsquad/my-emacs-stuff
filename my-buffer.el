;;; my-buffer.el --- buffer naviagtion tweaks
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
  :idle
  :config
  (setq midnight-mode t))

;; ido-mode - better buffer selection, although lusty does a lot of it
(use-package ido
  :idle
  :config
  (ido-mode t))

;; CURRENT UNUSED
(defun my-lusty-buffer-explorer ()
  "Wrapper to select between `lusty-buffer-explorer' and `helm-buffers-list'.
This chooses helm when the buffer list gets a  bit too big."
  (interactive)
  (if (> (length (buffer-list)) 20)
      (helm-buffers-list)
    (lusty-buffer-explorer)))

;;
;; Lusty Explorer
;;
(use-package lusty-explorer
  :bind (("C-x C-f" . lusty-file-explorer)
         ("C-x b"   . lusty-buffer-explorer)))

;;
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

(message "Done Buffer Handling Tweaks")

(provide 'my-buffer)
;;; my-buffer.el ends here
