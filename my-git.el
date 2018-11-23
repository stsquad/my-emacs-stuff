;;; my-git.el --- My git customisation
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

;; magit-file-mode is a minor mode for files that are under
;; magit control (i.e. in git repos). It is a handy place to override
;; global bindings - like the octopus like vc mode.
(use-package magit-files
  :commands global-magit-file-mode
  :bind (:map magit-file-mode-map
              ("C-x v l" . magit-log-buffer-file)
              ("C-x v d" . magit-diff-buffer-file))
  :defer 2
  :config (global-magit-file-mode))

;; I only really use git, stamp on vc-mode....
(with-eval-after-load 'vc
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (remove-hook 'find-file-hook 'vc-refresh-state)
  (setq vc-handled-backends nil))

(use-package magit-popup
  :ensure t
  :pin melpa-stable)

(use-package magit
  :ensure t
  :pin melpa-stable
  :commands magit-status
  :init
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0"))
  :config
  (progn
    (add-hook 'magit-mode-hook #'(lambda() (yas-minor-mode -1)))
    (add-hook 'magit-log-edit-mode-hook #'(lambda() (auto-fill-mode
                                                     1)))
    (setq
     ;; tweak magit
     magit-patch-arguments '("--cover-letter")
     magit-auto-revert-immediately 't)
    ;; Hydra for modes
    (with-eval-after-load 'hydra
      (define-key magit-mode-map
        (kbd "C-x t")
        (defhydra my-git-mode-hydra (:hint nil :color blue :timeout 10)
          (concat "Tweak Buffer State : _l_ock buffer:%`magit-buffer-locked-p "
                  "Navigate History   : _p_revious/_b_ack history: %(nth 3 (car help-xref-stack)) _n_ext/_f_orward history: %(nth 3 (car help-xref-forward-stack))")
          ;; Lock/unlock
          ("l" (magit-toggle-buffer-lock))
          ;; Navigation
          ("b" (magit-go-backward) nil :color red)
          ("p" (magit-go-backward) nil :color red)
          ("f" (magit-go-forward) nil :color red)
          ("n" (magit-go-forward)  nil :color red)
          ;; Main toggles
          ("t" my-hydra-toggle/body nil)
          )))))


;; Tweaks to git-commit-mode
;;
;; Mainly hooks for automation

(defun my-commit-mode-check-and-apply-tags ()
  "Run in git-commit-mode to check for any archived tags."
  (interactive)
  (let ((title))
    (save-excursion
      (goto-char (point-min))
      (setq title (chomp (substring-no-properties (thing-at-point
                                                   'line)))))
    (when title
      (let ((tags (my-org-find-review-tags title "DONE")))
        (when tags
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward my-dco-tag-re (point-max) t))
            (beginning-of-line 2)
            (insert (mapconcat 'identity tags "\n"))
            (message "Added %d tags to buffer" (length tags))))))))

(use-package git-commit
  :config
  (define-key git-commit-mode-map
    (kbd "C-x C-x") 'my-commit-mode-check-and-apply-tags))

;(add-hook 'git-commit-mode 'my-commit-mode-check-for-tags)

;; Re-base workflow
;;
;; When re-basing I want to tag for re-wording anything that needs
;; s-o-b and r-b tags applying. I also want to mark for editing
;; anything that has outstanding review comments.

(defvar my-rebase-match-re
  (rx (: bol
         (one-or-more letter) ; rebase action
         " "
         (>= 7 hex)            ; commitish
         " "
         (group (one-or-more nonl)))) ; summary
  "Regexp to match summary lines in rebase summary.")

(defun my-mark-rebase-commits-for-tagging ()
  "Set any commits in a re-base buffer to if tagging required."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward my-rebase-match-re (point-max) t)
      (let ((msg (match-string-no-properties 1)))
        (when (my-org-find-review-tags msg "TODO")
          (git-rebase-reword))))))

(defun my-mark-rebase-commits-for-editing ()
  "Set any commits in the re-base buffer to edit if comments."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward my-rebase-match-re (point-max) t)
      (let ((msg (match-string-no-properties 1)))
        (when (my-org-find-review-comments msg)
          (git-rebase-edit))))))

;;;
;;; Additional GIT bits
;;;

(use-package git-messenger
  :ensure t
  :commands git-messenger:popup-message
  :bind ("C-h g" . git-messenger:popup-message)
  :init
  (setq git-messenger:show-detail t)
  :config
  (progn
    ;; As we have magit we may as well use it instead of
    ;; git-messenger's own stuff.
    (defun my-git-messenger-show ()
      (interactive)
      (magit-show-commit git-messenger:last-commit-id)
      (git-messenger:popup-close))
    (define-key git-messenger-map (kbd "d") 'my-git-messenger-show)
    (define-key git-messenger-map (kbd "s") 'my-git-messenger-show)
    (define-key git-messenger-map (kbd "S") 'my-git-messenger-show)))


;;; Git Time Machine
;; via: http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
(defun my-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        (git-timemachine-show-revision rev)))))

(defun my-git-timemachine ()
  "Open git snapshot with the selected version.  Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'my-git-timemachine-show-selected-revision))

(use-package git-timemachine)

(provide 'my-git)
;;; my-git.el ends here
