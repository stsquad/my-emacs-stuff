;;; my-atomic-chrome --- support for Atomic Chrome integration
;;
;;; Commentary:
;;
;; This is similar to my-edit-server (which uses my extension) but has
;;the advantage of supporting bi-directional editing.
;;
;;; Code:

(defun my-close-buffer-and-frame ()
  "Close the current buffer and the frame it is displayed on."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-frame))

(use-package atomic-chrome
  :commands atomic-chrome-start-server
  :init (if after-init-time
            (atomic-chrome-start-server)
          (add-hook 'after-init-hook
                    #'(lambda() (atomic-chrome-start-server))))
  ;; :bind-keymap ("C-c C-c"  . my-close-buffer-and-frame)
  :config (setq atomic-chrome-buffer-open-style 'frame))

;; This is essentially a duplicate of my-edit-server.el

;; Handy for wiki editing
(use-package mediawiki
  :ensure t
  :commands mediawiki-mode)

(use-package moinmoin-mode
  :commands moinmoin-mode)

;; Defer setting up handlers until after atomic-chrome has loaded
(with-eval-after-load 'atomic-chrome
  (add-to-list 'atomic-chrome-url-major-mode-alist
               '("mediawiki" . mediawiki-mode))
  (add-to-list 'atomic-chrome-url-major-mode-alist
               '("wikipedia" . mediawiki-mode))
  (add-to-list 'atomic-chrome-url-major-mode-alist
               '("wiki.qemu.org" . mediawiki-mode))
  (when (package-installed-p 'moinmoin-mode)
    (add-to-list 'atomic-chrome-url-major-mode-alist
                 '("wiki.linaro.org" . moinmoin-mode)))
  (add-to-list 'atomic-chrome-url-major-mode-alist
               '("www.bennee.com/~alex/blog" . web-mode)))

(provide 'my-atomic-chrome)
;;; my-atomic-chrome.el ends here
