;;; my-edit-server.el -- load up the Edit with Emacs edit server
;;
;;; Commentary:
;;
;; There is not much to do here but ensure the edit server is spawned
;; at the end of start-up and we have hooks in place for the various modes.
;;
;;; Code:

(require 'edit-server)

;; Handy for wiki editing
(when (require 'mediawiki nil t)
  (add-to-list 'edit-server-url-major-mode-alist '("mediawiki" .
                                                   mediawiki-mode))
  (add-to-list 'edit-server-url-major-mode-alist '("wikipedia" .
                                                   mediawiki-mode)))

;; Markdown sites
(when (require 'markdown-mode nil t)
  (add-to-list 'edit-server-url-major-mode-alist '("stackexchange" .
                                                   markdown-mode)))

(setq edit-server-edit-mode-hook nil)
(add-hook 'edit-server-edit-mode-hook 'flyspell-mode t)

;; (when (require 'ox nil t)
;;   (defun my-maybe-export-org-mode ()
;;     (when (derived-mode-p 'org-mode)
;;       (let ((post (org-export-as 'html nil nil t)))
;;         (delete-region (point-min) (point-max))
;;         (insert post))))
;;   (add-hook 'edit-server-done-hook 'my-maybe-export-org-mode)
;;   (add-to-list 'edit-server-url-major-mode-alist
;;   '("www.bennee.com/~alex/blog" . org-mode)))

(add-to-list 'edit-server-url-major-mode-alist
             '("www.bennee.com/~alex/blog" . web-mode))


;; Fallbacks for webmail
(unless (require 'gmail-message-mode nil t)
  (add-to-list 'edit-server-url-major-mode-alist
               '("mail.google" . mail-mode))
  ;; Rough and ready html munging
  (when (require 'edit-server-htmlize nil t)
    (add-hook 'edit-server-start-hook
              'edit-server-maybe-dehtmlize-buffer)
    (add-hook 'edit-server-done-hook
              'edit-server-maybe-htmlize-buffer)))

;; Ensure edit-server is spawned after emacs starts
(add-hook 'emacs-startup-hook '(lambda ()
                                 (message "starting up edit-server")
                                 (edit-server-start)))

(provide 'my-edit-server)
;;; my-edit-server.el ends here


