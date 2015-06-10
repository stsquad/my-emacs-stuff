;;; my-edit-server.el -- load up the Edit with Emacs edit server
;;
;;; Commentary:
;;
;; There is not much to do here but ensure the edit server is spawned
;; at the end of start-up and we have hooks in place for the various modes.
;;
;;; Code:

(require 'use-package)
(require 'my-vars)
(require 'edit-server)

;; Chromebook Support
(use-package chromebook
  :if I-am-on-pixel
  :config
  (when (crmbk-running-in-host-x11-p)
    (set-face-attribute 'default nil :height 250)
    (add-hook 'crmbk-frame-mode-hook 'crmbk-remap-search)
    (add-hook 'crmbk-frame-mode-hook 'crmbk-disable-touchpad)
    (define-key crmbk-frame-mode-map (kbd "<M-up>") 'scroll-down)
    (define-key crmbk-frame-mode-map (kbd "<M-down>") 'scroll-up)
    (when (boundp 'edit-server-new-frame-alist)
      (setq edit-server-new-frame-alist '((name . "Edit Server Frame")
                                          (fullscreen . 'fullboth))))))

;; Handy for wiki editing
(use-package mediawiki
  :commands mediawiki-mode
  :init
  (progn
    (add-to-list 'edit-server-url-major-mode-alist
                 '("mediawiki" . mediawiki-mode))
    (add-to-list 'edit-server-url-major-mode-alist
                 '("wikipedia" . mediawiki-mode))
    (add-to-list 'edit-server-url-major-mode-alist
                 '("wiki.qemu.org" . mediawiki-mode))))

(use-package moinmoin-mode
  :commands moinmoin-mode
  :init (add-to-list 'edit-server-url-major-mode-alist
                 '("wiki.linaro.org" . moinmoin-mode)))

;; Markdown sites
(use-package markdown-mode
  :config
  (progn
    (setq markdown-reference-location 'end)
    (add-to-list 'edit-server-url-major-mode-alist
                 '("stackexchange" . markdown-mode))
    (add-to-list 'edit-server-url-major-mode-alist
                 '("github.com" . markdown-mode))))

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

(provide 'my-edit-server)
;;; my-edit-server.el ends here
