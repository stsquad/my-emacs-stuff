;;; my-git.el --- My git custiomisations
;;
;;; Commentary:
;;
;; This is mostly magit configuration stuff
;;
;;; Code:

(require 'my-vars)
(require 'use-package)

; work-around stale shells
(when I-am-at-work
  (setenv "GIT_AUTHOR_EMAIL" "alex.bennee@linaro.org"))

(use-package magit
  :commands magit-status
  :diminish ((magit-auto-revert-mode . "MR"))
  :bind ("C-x g" . my-magit-start)
  :config
  '(progn
     (add-hook 'magit-mode-hook #'(lambda() (yas-minor-mode -1)))
     (add-hook 'magit-commit-mode-hook #'(lambda() (auto-fill-mode 1)))
     (add-hook 'magit-log-edit-mode-hook #'(lambda() (auto-fill-mode 1)))
     (add-hook 'magit-log-mode-hook 'my-magit-add-checkpatch-hook)
     (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)
     (setq
      magit-status-buffer-switch-function 'switch-to-buffer
      magit-rewrite-inclusive 'nil)))

(defun my-magit-start ()
  "My personal start magit from anywhere function."
  (interactive)
  (if buffer-file-name
      (magit-status (file-name-directory (file-chase-links buffer-file-name)))
    (magit-status default-directory)))

(global-set-key (kbd "C-x g") 'my-magit-start)

;;;
;; Run checkpatch.pl if we can
;;
(defvar magit-checkpatch-script nil
  "Path to local checkpatch script, if it exists.")
(make-variable-buffer-local 'magit-checkpatch-script)
(put 'magit-checkpatch-script 'permanent-local t)

(use-package compile
  :commands compilation-mode
  :config
  (progn
    ;; Match checkpatch.pl output
    (add-to-list
     'compilation-error-regexp-alist-alist
     '(checkpatch
       "\\(WARNING\\|ERROR\\).*\n#.*FILE: \\([^:]+\\):\\([^:digit:]+\\).*\n.*"
       2 ; file
       3 ; line
       ))
    ;; add reference
    (add-to-list 'compilation-error-regexp-alist 'checkpatch)))

(defun my-magit--do-run-checkpatch (commit)
  "Run the checkpatch script against `COMMIT'."
  (let ((proc-name "checkpatch")
        (buff-name (format "checkpatch-%s" commit)))
    (start-process-shell-command
     proc-name
     buff-name
     (format "git show %s | %s -" commit magit-checkpatch-script))
    (switch-to-buffer buff-name)
    (goto-char (point-min))
    (compilation-minor-mode)))

(defun my-magit-run-checkpatch ()
  "Run a checkpatch script against current commit."
  (interactive)
  (when (derived-mode-p 'magit-log-mode)
    (magit-section-action checkpatch (info)
      (commit
       (my-magit--do-run-checkpatch info)))))

(defun my-magit-add-checkpatch-hook ()
  "Add the ability to run a checkpatch script if we can."
  (let ((script (concat default-directory "scripts/checkpatch.pl")))
    (when (file-exists-p script)
      (setq magit-checkpatch-script script)
      (local-set-key (kbd "C") 'my-magit-run-checkpatch))))


;;;
;;; Additional GIT bits
;;;
(use-package git
  :commands git-status)

(use-package git-blame
  :commands git-blame-mode)

(use-package git-messenger
  :commands git-messenger:popup-message
  :bind ("C-h g" . git-messenger:popup-message)
  :init
  (setq git-messenger:show-detail t))

(provide 'my-git)
;;; my-git.el ends here
