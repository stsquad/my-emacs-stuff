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
  :init (add-hook 'after-init-hook
                  #'(lambda() (atomic-chrome-start-server)))
  :config (progn
            (setq atomic-chrome-buffer-open-style 'frame)
            (define-key atomic-chrome-edit-mode-map (kbd "C-c C-c") 'my-close-buffer-and-frame)))

(provide 'my-atomic-chrome)
;;; my-atomic-chrome.el ends here
