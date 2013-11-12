;;
;; Edit Server Tweaks
;;

(require 'edit-server)

;; Handy for Gmail
(when (require 'edit-server-htmlize nil t)
  (add-hook 'edit-server-start-hook
            'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook
            'edit-server-maybe-htmlize-buffer))

;; Handy for wiki editing
(when (require 'mediawiki nil t)
  (add-to-list 'edit-server-url-major-mode-alist '("mediawiki" .
                                                   mediawiki-mode)))


;; Detect when we are editing webmail
(add-to-list 'edit-server-url-major-mode-alist
             '("mail.google" . mail-mode))

;; Ensure edit-server is spawned after emacs starts
(add-hook 'emacs-startup-hook '(lambda ()
                                 (message "starting up edit-server")
                                 (edit-server-start)))

