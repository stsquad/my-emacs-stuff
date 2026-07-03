;;; my-buffer.el --- buffer navigation tweaks -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; C-x b buffer switching is now consult-buffer
;;
;;; Code:

(require 'use-package)

;; midnight mode, clean-up unused buffers overnight
(use-package midnight
  :commands midnight-mode
  :config (midnight-mode t))

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
      :config
      (setq bufler-groups
            (bufler-defgroups
               (group
                ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
                (group-or "*Help/Info*"
                          (mode-match "*Help*" (rx bos (or "help" "helpful") "-"))
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
                                             "scratch" "Backtrace"
                                             "Async-native-compile-log")
                                 "*")))
                ;; IRC buffers
                (group
                 (group-or "IRC Servers"
                           (mode-match "Servers" (rx bos "circe-server-mode"))))
                (group
                 (group-or "IRC Messages"
                           (mode-match "Channels" (rx bos "circe-channel-mode"))
                           (mode-match "DMs" (rx bos "circe-query-mode"))))
                ;; Emails
                (group
                 (group-or "Incoming Emails"
                           (mode-match "Menu" (rx bos "mu4e-main-mode"))
                           (mode-match "Headers" (rx bos "mu4e-headers-mode"))
                           (mode-match "Viewing" (rx bos "mu4e-view-mode"))))
                (group
                 (group-or "Composed Emails"
                           (mode-match "Composing" (rx bos "mu4e-compose-mode"))))
                (group
                 (name-match "ECA" (rx bos "<eca"))))
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
