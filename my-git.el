;;; my-git.el --- My git customisation
;;
;;; Commentary:
;;
;; This is mostly magit configuration stuff
;;
;;; Code:

(require 'my-vars)
(require 'use-package)
(require 'my-find)

; work-around stale shells
(when I-am-at-work
  (setenv "GIT_AUTHOR_EMAIL" "alex.bennee@linaro.org"))

(with-eval-after-load 'magit
  (defun my-magit-file-bindings ()
    "Setup my file bindings"
    (local-set-key (kbd "<f5>") 'my-counsel-git-grep))
  (add-hook 'magit-find-file-hook 'my-magit-file-bindings))

;; I only really use git, stamp on vc-mode....
(with-eval-after-load 'vc
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (remove-hook 'find-file-hook 'vc-refresh-state)
  (setq vc-handled-backends nil))

;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(defun my-git-project-finder (dir)
  "Integrate .git project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
                     (expand-file-name dir))))
    (and dotgit
         (cons 'transient (file-name-directory dotgit)))))

(add-hook 'project-find-functions 'my-git-project-finder)

(defun my-magit-dispatch (&optional prefix)
  "My personal preference for magit-dispatch.
While magit-file-dispatch is cool, falling back to magit-dispatch is
not, I'd rather just go to magit-status. Lets make it so."
  (interactive "P")
  (if (or prefix
          (not (buffer-file-name))
          (not (functionp 'magit-file-dispatch)))
      (magit-status)
    (magit-file-dispatch)))


(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-x g" . my-magit-dispatch)
         :map magit-hunk-section-map
         ("<rebind> magit-visit-thing" . magit-diff-visit-file-worktree))
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
            (insert "\n")
            (message "Added %d tags to buffer" (length tags))))))))

;; requires the showfix alias in .gitconfig
(defun my-commit-mode-add-fixes (commit)
  "Insert a Fixes line, selecting a COMMIT."
  (interactive (list (magit-read-range-or-commit "Fixes commit:")))
  (insert (magit-git-str "showfix" commit)))

(use-package git-commit
  :bind (:map git-commit-mode-map
              ("C-c x" . my-commit-mode-check-and-apply-tags)
              ("C-c f" . my-commit-mode-add-fixes)))

;;
;;
;; Checking if merged workflow
;;
(defun my-magit-check-if-subject-merged (subj branch &optional dir)
  "Check if `SUBJ' is in your `BRANCH' under `DIR'."
  (interactive)
  (unless branch
    (setq branch "origin/master"))
  (unless dir
    (setq dir (read-directory-name "Project:")))
  (let ((default-directory dir))
    (magit-git-string
     "log" branch "--no-merges" "--oneline" "--grep" subj)))

;; Applying Patches Workflow
;;
;; Now we can apply patches from many places. Lets keep the common
;; stuff here and then GNUS and mu4e can munge things as needed.
;;

(defvar my-patch-directory-history nil
  "History of directories we have applied patches to.")

(defun my-read-patch-directory (&optional prompt)
  "Read a `PROMPT'ed directory name via `completing-read' with history."
  (unless prompt
    (setq prompt "Target directory:"))
  (file-truename
    (completing-read prompt 'read-file-name-internal #'file-directory-p
      nil nil 'my-patch-directory-history)))

(defun my-git-apply-mbox (file &optional signoff)
  "Apply `FILE' a git patch with optional `SIGNOFF'.

If the `default-directory' matches the most recent history entry don't
bother asking for the git tree again (useful for bulk actions)."

  (let ((cwd (substring-no-properties
              (or (car my-patch-directory-history)
                  "not-a-dir"))))
        (unless (and (stringp cwd) (string= default-directory cwd))
          (setq cwd (my-read-patch-directory "Target directory: ")))
        (let ((default-directory cwd))
          (shell-command
           (format
            "git am --reject %s %s"
            (if signoff "--signoff" "")
            (shell-quote-argument file))))))

(defun my-git-fetch-and-apply-via-b4 (id)
  "Fetch `id' via the b4 tool and apply it."
  (interactive "sMessage-id:")
  (with-temp-buffer
    (call-process "b4" nil t t "am" id)
    (goto-char 0)
    (when (re-search-forward
           (rx (: "git am "
                  (group (one-or-more (not space)) ".mbx"))))
      (let ((mbox (match-string-no-properties 1))
            (need-sob nil))
        (with-temp-buffer
          (insert-file-contents mbox)
          (goto-char 0)
          (unless (re-search-forward
                   (format "Signed-of-by: Alex Bennée <%s>"
                           user-mail-address) nil t)
                   (setq need-sob 't)))
        (my-git-apply-mbox mbox need-sob)
        (message "applied %s" mbox)
        (delete-file mbox)))))

(defun my-git-apply-region (beg end)
  "Apply region as a patch."
  (interactive "r")
  (let ((patch (buffer-substring-no-properties beg end))
        (file (make-temp-file "region-as-patch")))
    (with-temp-file file
      (insert patch))
    (my-git-apply-mbox file)))

;; Re-base workflow
;;
;; When re-basing I want to tag for re-wording anything that needs
;; s-o-b and r-b tags applying. I also want to mark for editing
;; anything that has outstanding review comments.

(defvar my-rebase-match-re
  (rx (: bol
         (one-or-more letter) ; rebase action
         " "
         (group (>= 7 hex))            ; commitish
         " "
         (group (one-or-more nonl)))) ; summary
  "Regexp to match summary lines in rebase summary.")

(defvar my-rebase-reword-commits
  nil
  "List of commits that need re-wording in the current re-base.")

(defvar my-rebase-edit-commits
  nil
  "List of commits that need re-wording in the current re-base.")

(defun my-rebase-auto-tag-commits ()
  "Mark commits as needing reword/edit depending on the appropriate
variables."
  (when my-rebase-reword-commits
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward my-rebase-match-re (point-max) t)
        (let ((msg (match-string-no-properties 1)))
          (when (--any? (s-contains-p msg it) my-rebase-reword-commits)
            (git-rebase-reword)))))
    (setq my-rebase-reword-commits nil)))

(defun my-mark-rebase-commits-for-tagging ()
  "Set any commits in a re-base buffer to if tagging required."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward my-rebase-match-re (point-max) t)
      (let ((msg (match-string-no-properties 2)))
        (when (my-org-find-review-tags msg "TODO")
          (git-rebase-reword))))))

(defun my-mark-rebase-commits-for-editing ()
  "Set any commits in the re-base buffer to edit if comments."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward my-rebase-match-re (point-max) t)
      (let ((msg (match-string-no-properties 2)))
        (when (my-org-find-review-comments msg)
          (git-rebase-edit))))))

(use-package git-rebase
  :commands git-rebase-mode
  :hook (git-rebase-mode . my-rebase-auto-tag-commits)
  :bind (:map git-rebase-mode-map
              ("C-x t" . my-mark-rebase-commits-for-tagging)
              ("C-x e" . my-mark-rebase-commits-for-editing)))

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

(use-package git-timemachine
  :ensure t)

(provide 'my-git)
;;; my-git.el ends here
