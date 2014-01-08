;;; my-git.el --- My git custiomisations
;;
;;; Commentary:
;;
;; This is mostly magit configuration stuff
;;
;;; Code:

(require 'magit)

(message "Setting up GIT bits")

; if we have magit then stop VC mode messing about
(setq vc-handled-backends (remq 'Git vc-handled-backends))
(autoload 'magit-status "magit" "magit front end" t)

; work-around stale shells
(when I-am-at-work
  (setenv "GIT_AUTHOR_EMAIL" "alex.bennee@linaro.org"))

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

;; Match checkpatch.pl output
(add-to-list
 'compilation-error-regexp-alist-alist
 '(checkpatch
   "\\(WARNING\\|ERROR\\).*\n#.*FILE: \\([^:]+\\):\\([^:digit:]+\\).*\n.*"
   2 ; file
   3 ; line
   ))

(add-to-list 'compilation-error-regexp-alist 'checkpatch)

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
    (magit-section-action (item info "run checkpatch" t)
      ((commit)
       (message "doing commit check for %s" info)
       (my-magit--do-run-checkpatch info)))))

(defun my-magit-add-checkpatch-hook ()
  "Add the ability to run a checkpatch script if we can."
  (let ((script (concat default-directory "scripts/checkpatch.pl")))
    (when (file-exists-p script)
      (setq magit-checkpatch-script script)
      (local-set-key (kbd "C") 'my-magit-run-checkpatch))))

; hook into magit-log-mode to check for checkpatch scripts
; (add-hook 'magit-log-mode-hook 'my-magit-add-checkpatch-hook)

(eval-after-load "magit"
  '(progn
     (add-hook 'magit-mode-hook #'(lambda() (yas-minor-mode -1)))
     (add-hook 'magit-commit-mode-hook #'(lambda() (auto-fill-mode 1)))
     (add-hook 'magit-log-edit-mode-hook #'(lambda() (auto-fill-mode
  1)))
     (add-hook 'magit-log-mode-hook 'my-magit-add-checkpatch-hook)
     (setq
      magit-status-buffer-switch-function 'switch-to-buffer
      magit-rewrite-inclusive 'nil)))
  
; Also the git-blame and git-status stuff
(when (locate-library "git")
  (autoload 'git-status "git"
    "Git Status" t))

(when (locate-library "git-blame")
  (autoload 'git-blame-mode "git-blame"
    "Minor mode for incremental blame for Git." t))

(when (locate-library "git-messenger")
  (global-set-key (kbd "C-h g") 'git-messenger:popup-message)
  (eval-after-load "git-messenger"
    (setq git-messenger:show-detail 't)))

(message "Done GIT hooks")

(provide 'my-git)
;;; my-git.el ends here
