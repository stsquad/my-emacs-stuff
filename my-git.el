;;
;; Magit customisations
;;

(require 'magit)

(message "Setting up GIT bits")

; if we have magit then stop VC mode messing about
(setq vc-handled-backends (remq 'Git vc-handled-backends))
(autoload 'magit-status "magit" "magit front end" t)

(defun my-magit-start ()
  "My personal start magit from anywhere function"
  (interactive)
  (if buffer-file-name
      (magit-status (file-name-directory (file-chase-links buffer-file-name)))
    (magit-status default-directory)))

(global-set-key (kbd "C-x g") 'my-magit-start)

(eval-after-load "magit"
  '(progn
     (add-hook 'magit-mode-hook #'(lambda() (yas-minor-mode -1)))
     (add-hook 'magit-commit-mode-hook #'(lambda() (auto-fill-mode 1)))
     (add-hook 'magit-log-edit-mode-hook #'(lambda() (auto-fill-mode 1)))
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
