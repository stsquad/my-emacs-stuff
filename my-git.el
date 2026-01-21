;;; my-git.el --- My git customisation -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This is mostly magit configuration stuff
;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package my-vars)
(use-package my-find)
(use-package my-hydra)
(use-package my-org
  :autoload my-org-find-review-tags my-org-find-review-comments)

; work-around stale shells
(when I-am-at-work
  (setenv "GIT_AUTHOR_EMAIL" "alex.bennee@linaro.org"))

;; I only really use git, stamp on vc-mode....
(with-eval-after-load 'vc
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (remove-hook 'find-file-hook 'vc-refresh-state)
  (setq vc-handled-backends nil))

;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(defun my-git-project-finder (dir)
  "Integrate .git from DIR into project roots."
  (let ((dotgit (locate-dominating-file dir ".git")))
    (when dotgit
         (cons 'transient
               (file-name-directory (expand-file-name dotgit))))))

(add-hook 'project-find-functions 'my-git-project-finder)

(defun my-magit-dispatch (&optional prefix)
  "My personal magit-dispatch with `PREFIX' forcing `magit-status'.

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
  :pin melpa-stable
  :commands magit-status
  :bind (("C-x g" . my-magit-dispatch)
         :map magit-hunk-section-map
         ("<rebind> magit-visit-thing" . magit-diff-visit-file-worktree)
         :map magit-revision-mode-map
         ("C-c i" . my-commit-kill-message-id)
         :map magit-mode-map
         ("C-x t" . hydra-magit/body))
  :hook (magit-log-edit-mode . auto-fill-mode)
  :hydra (hydra-magit (:hint nil :color blue :timeout 10)
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
          ("t" my-hydra-toggle/body nil))
  :config
  (setq
   ;; tweak magit
   magit-patch-arguments '("--cover-letter")
   magit-auto-revert-immediately 't)
  (when window-system           ;; fancy icons in diff
    (setq magit-format-file-function #'magit-format-file-nerd-icons)))



;; we use magit-git-string ourselves but there is no autoload
(use-package magit-git
  :requires magit
  :commands (magit-git-string magit-git-lines))

(use-package magit-process
  :requires magit
  :commands magit-process-git)

(use-package magit-files
  :requires magit
  :commands magit-file-dispatch)

;; DCO helpers
;;

(defun my-dco-address ()
  "Return my DCO address."
  (format "Alex Bennée <%s>" user-mail-address))

(defun my-check-for-my-signoff ()
  "Return t if my signoff is found in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (format "Signed-off-by: %s" (my-dco-address))
     nil t)))

(defvar my-b4-message-id-history nil
  "History of b4 message-id's processed.")

(defvar my-b4-current-results-buffer nil
  "Buffer of the current b4 results.")

;; Tweaks to git-commit-mode
;;
;; Mainly hooks for automation

(defun my-commit-mode-check-and-apply-tags (&optional prefix)
  "Run in git-commit-mode to check for any archived tags. When
called with `PREFIX' we include Message-Id tags."
  (interactive)
  (let ((title))
    (save-excursion
      (goto-char (point-min))
      (setq title (chomp (substring-no-properties (thing-at-point
                                                   'line)))))
    (when my-b4-current-results-buffer
      (my-commit-update-with-b4 t prefix))
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

(defun my-git-check-for-updates-with-b4 (subject &optional id inc-msg-id)
  "Get tags for `SUBJECT' from message `ID' or `my-b4-current-results-buffer'."
  (unless (or id my-b4-current-results-buffer)
    (user-error "Need a Message-ID or active b4 buffer to continue"))
  (let ((tags)
        ;; might be nice not to sweep up msg-ids sometimes
        (valid-tags (if inc-msg-id
                        (rx bol (group (or my-bare-dco-tag-rx
                                           my-msgid-rx)))
                      (rx bol (group my-bare-dco-tag-rx))))
        ;; tweak subject for wrapped lines which b4 may have added
        (tweaked-subject (replace-regexp-in-string
                          " " "[[:space:]]+" subject)))
    (with-temp-buffer
      (if id
          (call-process "b4" nil t t "am" "-S" "-t" id "-o" "-")
        (insert-buffer-substring-no-properties my-b4-current-results-buffer))
      (goto-char 0)
      (when (re-search-forward
             (concat
              (rx "Subject: [" (zero-or-more nonl) "] ") tweaked-subject)
             nil t)
        (forward-line)
        (beginning-of-line)
        (let ((end-of-subj
               (save-excursion
                 (re-search-forward "---"))))
          (while (re-search-forward valid-tags end-of-subj t)
            (push (match-string-no-properties 1) tags)
            (forward-line)))))
    (message "found %d tags" (length tags))
    tags))

(defun my-check-for-b4-results-with-subject (subject)
  "Check if SUBJ appears in the results buffer."
  (when my-b4-current-results-buffer
    (with-current-buffer my-b4-current-results-buffer
      (save-excursion
        (re-search-forward subject nil t)))))

(defun my-commit-update-with-b4 (&optional prefix include-msg-id)
  "Check if the current commit has tags from its last posting.

This works by looking for a message-id in the buffer or prompting for
  one. If none can be found it might still be able to apply manually
  from `my-b4-current-results-buffer'. Interactively setting `PREFIX'
  forces this mode. Unless `include-msg-id' set skip those tags."
  (interactive "P")
  (let ((subj) (id))
    (save-excursion
      (goto-char 0)
      (setq subj (chomp (substring-no-properties (thing-at-point 'line))))
      (setq id (cond
                ;; prefix forces a b4 lookup
                (prefix nil)
                ;; if subject exists we know we are good
                ((my-check-for-b4-results-with-subject subj) nil)
                ;; if we can find an id use that
                ((re-search-forward my-capture-msgid-re nil t)
                 (match-string-no-properties 1))
                ;; finally fall back to query
                (t (completing-read "Message-ID:" my-b4-message-id-history)))))
    (when (or subj id)
      (let ((tags (my-git-check-for-updates-with-b4 subj id include-msg-id)))
        (--map
         (save-excursion
           (goto-char 0)
           (when (not (re-search-forward it nil t))
             (or (re-search-forward my-capture-msgid-re nil t)
                 (re-search-forward my-bare-dco-tag-re nil t))
             (beginning-of-line)
             (insert it ?\n))) tags)))))


;; requires the showfix alias in .gitconfig
(defun my-commit-mode-add-fixes (commit)
  "Insert a Fixes line, selecting a COMMIT."
  (interactive (list (magit-read-range-or-commit "Fixes commit:")))
  (insert (magit-git-string "showfix" commit)))


(defun my-commit-kill-message-id (&optional no-kill)
  "Snarf a message-id from the commit message into the kill ring."
  (interactive "P")
  (let* ((ids (my-capture-msgids))
         (refs (--map (propertize (format "reference: %s" it) 'id
                                  it) ids))
         (final-id
          (get-text-property
           0 'id
           (cond ((= (length refs) 1)
                  (car refs))
                 ((> (length refs) 1)
                  (ivy-read "select reference:" refs))
                 (t nil)))))
    (when final-id
      (message "message-id: %s" final-id)
      (unless no-kill
        (kill-new final-id)))
    final-id))

(defun my-commit-kill-extra-msgids ()
  "Kill extraneous msgids keeping the most recent one.

This assumes they all have the same domain with datestring at front of
the message id itself. If the commit has multiple ids from different
domains then you'd best edit it yourself."
  (interactive)
  (let ((matches '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward my-msgid-re nil t)
        (push (match-string-no-properties 1) matches)))
    (let ((sorted (seq-sort 'string< matches)))
      (while (> (length sorted) 1)
        (let ((to-kill (pop sorted)))
          (save-excursion
            (goto-char (point-min))
            (when (search-forward to-kill nil t)
              (kill-whole-line))))))))

(use-package git-commit
  ;; now part of magit: https://github.com/magit/magit/blob/main/CHANGELOG#v410----2024-09-01
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c b" . my-commit-update-with-b4)
              ("C-c i" . my-commit-kill-message-id)
              ("C-c x" . my-commit-mode-check-and-apply-tags)
              ("C-c f" . my-commit-mode-add-fixes)
              ("C-c k" . my-commit-kill-extra-msgids))
  :config (setq git-commit-summary-max-length 50))

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
  "Read a `PROMPT'ed directory name via `completing-read' with
  history.

If the `default-directory' matches the most recent history entry don't
bother asking for the git tree again (useful for bulk actions)."
  (unless prompt
    (setq prompt "Target directory:"))
  (let ((cwd (car my-patch-directory-history)))
    (unless (and cwd (stringp cwd)
                     (string=
                      (file-truename default-directory)
                      (file-truename cwd)))
      (setq cwd (file-truename
                 (completing-read prompt 'read-file-name-internal #'file-directory-p
                                  nil nil
                                  'my-patch-directory-history))))
    cwd))


(defun my-git-apply-mbox (file &optional signoff)
  "Apply `FILE' a git patch with optional `SIGNOFF'."
  (let ((default-directory
         (my-read-patch-directory "Target directory: ")))
    (shell-command
     (format
      "git am --reject %s %s"
      (if signoff "--signoff" "")
      (shell-quote-argument file)))))

(defun my-git-fetch-and-apply-via-b4 (id)
  "Fetch `id' via the b4 tool and apply it."
  (interactive "sMessage-id:")
  (with-temp-buffer
    (call-process "b4" nil t t "am" "-i" "-S" id)
    (goto-char 0)
    (when (re-search-forward
           (rx (: "git am "
                  (group (one-or-more (not space)) ".mbx"))))
      (let ((mbox (match-string-no-properties 1))
            (need-sob nil))
        (with-temp-buffer
          (insert-file-contents mbox)
          (setq need-sob (not (my-check-for-my-signoff))))
        (my-git-apply-mbox mbox need-sob)
        (message "applied %s" mbox)
        (delete-file mbox)))))

(defun my-grab-lore-mbox-from-b4 (id)
  (interactive "sMessage-id:")
  (let ((mbox))
  (with-temp-buffer
    (call-process "b4" nil t t "mbox" id)
    (goto-char 0)
    (when (re-search-forward
           (rx (: (group (one-or-more (not space)) ".mbx"))))
      (setq mbox (match-string-no-properties 1))))
  (switch-to-buffer (get-buffer-create (format "*%s*" id)))
  (erase-buffer)
  (call-process "clean-mbox.py" nil t t mbox)
  (goto-char (point-min))))


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
  "List of commits that need editing in the current re-base.")

(defun my-rebase-auto-tag-commits ()
  "Mark commits as needing reword/edit depending on the appropriate
variables."
  (when (or my-rebase-reword-commits my-rebase-edit-commits)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward my-rebase-match-re (point-max) t)
        (let ((commit (match-string-no-properties 1))
              (subject (match-string-no-properties 2)))
          (when (or (--any?
                     (s-contains-p commit it)  my-rebase-reword-commits)
                    (--any?
                     (s-contains-p subject it)  my-rebase-reword-commits))
            (git-rebase-reword))
          (when (--any? (s-contains-p commit it) my-rebase-edit-commits)
            (git-rebase-edit)))))
    (setq my-rebase-reword-commits nil)
    (setq my-rebase-edit-commits nil)))


(defun my-set-reword-commits-from-b4 (&optional id)
  "Run b4 to collect a list of subjects that have updated tags and
fill in `my-rebase-reword-commits'. This can be used to manually
prepare for a re-base where we are not rebuilding the tree from
  scratch. Each commit can then be updated with
  `my-commit-update-with-b4'."
  (interactive (list (completing-read "Root Message-Id:" nil)))
  (let ((add-dco (rx (: "+ " (group (regexp my-bare-dco-tag-re)))))
        (subjects))
    (with-temp-buffer
      (call-process "b4" nil t t "am" "-S" "-t" id "-o" "-")
      (save-excursion
        (goto-char 0)
        (while (re-search-forward add-dco nil t)
          (save-excursion
            (beginning-of-line)
            (forward-line -1)
            (when (re-search-forward "] " nil t)
              (push
               (buffer-substring-no-properties (point) (point-at-eol))
               subjects)))))
      (setq my-b4-current-results-buffer
            (generate-new-buffer "*b4 run*"))
      (copy-to-buffer my-b4-current-results-buffer (point-min) (point-max)))
    (if (not subjects)
        (message "No additional DCOs found")
      (add-to-list 'my-b4-message-id-history id)
      (message "Added %d subjects to my-rebase-reword-commits"
               (length (setq my-rebase-reword-commits subjects))))))



(defun my-mark-rebase-commits-for-tagging ()
  "Set any commits in a re-base buffer to if tagging required."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (re-search-forward my-rebase-match-re (point-max) t)
        (let ((msg (match-string-no-properties 2)))
          (when (my-org-find-review-tags msg "TODO")
            (git-rebase-reword)
            (setq count (+ 1 count)))))
      (message "%d commits marked for tagging" count))))


(defun my-mark-rebase-commits-for-editing ()
  "Set any commits in the re-base buffer to edit if comments."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (re-search-forward my-rebase-match-re (point-max) t)
        (let ((msg (match-string-no-properties 2)))
          (when (my-org-find-review-comments msg)
            (git-rebase-edit)
            (setq count (+ 1 count)))))
      (message "%d commits marked for editing" count))))

(use-package git-rebase
  :commands git-rebase-mode
  :hook (git-rebase-mode . my-rebase-auto-tag-commits)
  :bind (:map git-rebase-mode-map
              ("C-x t" . my-mark-rebase-commits-for-tagging)
              ("C-x e" . my-mark-rebase-commits-for-editing)))

;;
;; Add forge support
;;
(when I-am-at-work
  (use-package forge
    :ensure t
    :after magit)

  (use-package code-review
    :ensure t
    :after forge
    :load-path (lambda () (my-return-path-if-ok
                           "~/src/emacs/code-review.git/"))
    :config
    (define-key
     forge-pullreq-section-map (kbd "R")
     'code-review-forge-pr-at-point)))


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
