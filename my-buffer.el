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
  :commands midnight-mode
  :config (midnight-mode t))

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
  :disabled t
  :commands lusty-file-explorer
  :bind (("C-x C-f" . my-lusty-file-explorer)))

;; ibuffer has been around for some time
(defun my-ibuffer-bs-show ()
  "Emulate `bs-show' from the bs.el package."
  (interactive)
  (ibuffer nil "*Ibuffer-my-bs*" '((filename . ".*")) nil t)
  (define-key (current-local-map) "a" 'ibuffer-bs-toggle-all))

(if (version<= "26.3" emacs-version)
    (use-package bufler
      :ensure t
      :bind (("C-x C-b" . bufler))
      :config (bufler-defgroups
               (group
                ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
                (group-or "*Help/Info*"
                          (mode-match "*Help*" (rx bos "help-"))
                          (mode-match "*Info*" (rx bos "info-"))))
               (group
                ;; Subgroup collecting all special buffers (i.e. ones that are not
                ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
                ;; through to other groups, so they end up grouped with their project buffers).
                (group-and "*Special*"
                           (lambda (buffer)
                             (unless (or (funcall
                                          (mode-match "Magit" (rx bos "magit-" (or "status" "log")))
                                                  buffer)
                                         (funcall (mode-match "Dired" (rx bos "dired"))
                                                  buffer)
                                         (funcall (auto-file) buffer))
                               "*Special*")))
                (group
                 ;; Subgroup collecting these "special special" buffers
                 ;; separately for convenience.
                 (name-match "**Special**"
                             (rx bos "*" (or "Messages" "Warnings"
                                             "scratch" "Backtrace")
                                 "*"))))
               (group
                (group-or "IRC"
                          (mode-match "Channels" (rx "circe-channel-mode"))
                          (mode-match "DMs" (rx "circe-query-mode")))
               (group
                (auto-project))
               (auto-directory)
               (auto-mode))))
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
                                        (mode . lisp-mode))))))))))

(provide 'my-buffer)
;;; my-buffer.el ends here
