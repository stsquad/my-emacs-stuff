;;; my-org --- org-mode related configuration bits
;;
;;; Commentary:
;;
;; Not much in here as I'm not yet a heavy user of org-mode
;;
;;; Code:

(require 'my-vars)
(require 'my-email)
(require 'use-package)

(defvar ajb-work-org-file
  (when I-am-at-work "/home/alex/org/index.org")
  "The location of my main work scratchpad.")

(defvar my-org-babel-hashes nil
  "List of known babel hashes to prevent re-asking every bloodly time...")

(use-package org
  :commands org-agenda
  :requires (org-clock)
  :bind ("C-c C-o" . org-capture)
  :init
  (progn
    (setq
     ;; General navigation
     org-return-follows-link t
     ;; Agenda locations
     org-agenda-files '("~/org/")
     org-refile-targets '((nil :maxlevel . 2)
                          (org-agenda-files :maxlevel . 2))
     ;; Capture Temapltes
     org-directory "~/org"
     org-capture-templates
     '(("r" "Review Comment (email)"
        checkitem
        (file+headline "review.org" "Review Comments")
        "  - [ ] %A%?")
       ("R" "Review Comment (region)"
        checkitem
        (file+headline "review.org" "Review Comments")
        "  - [ ] %i%?")
       ("t" "Add TODO task"
        entry
        (file+headline "team.org" "Tasks")
        "** TODO %i%?"))
     ;; Clocking behaviour
     org-clock-persist 't
     org-clock-in-resume 't                 ; resume currently open clock
     org-clock-persist-query-resume 'nil    ; don't ask me about it
     org-log-into-drawer 't                 ; roll clocks up into drawers
     org-clock-idle-time 'nil
     ;; Mode line tweaks for clock
     org-clock-mode-line-total 'current
     org-clock-clocked-in-display 'frame-title
     ;; TODO Hierarchy
     org-provide-todo-statistics t
     org-checkbox-hierarchical-statistics nil
     org-hierarchical-todo-statistics nil
     ;; Export settings
     org-export-allow-bind-keywords t)
    (when I-am-at-work
      (setq
       org-publish-project-alist
       '(
         ("org-notes"
          :base-directory "~/org/"
          :base-extension "org"
          :publishing-directory "~/public_html/org/"
          :recursive nil
          :publishing-function org-html-publish-to-html
          :headline-levels 4             ; Just the default for this project.
          :auto-preamble t
          )
         ("org-presentations"
          :base-directory "~/org/presentations"
          :base-extension "html\\|css\\|js\\|png\\|jpg\\|gif\\|svg\\|pdf\\|mp3\\|ogg\\|swf"
          :publishing-directory "~/public_html/org/presentations/"
          :recursive t
          :publishing-function org-publish-attachment
          )
         ("org-static"
          :base-directory "~/org/"
          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
          :publishing-directory "~/public_html/org/"
          :recursive t
          :publishing-function org-publish-attachment
          )
         ("org" :components ("org-notes" "org-presentations"
                             "org-static")))))
    ;; Mail integration
    (use-package org-mu4e
      :init (add-to-list 'org-modules 'org-mu4e t))
    ;; Mode keys
    (define-key org-mode-map (kbd "M-[ c") 'org-demote-subtree)
    (define-key org-mode-map (kbd "M-[ d") 'org-promote-subtree)
    (define-key org-mode-map (kbd "C-f") nil) ; I use C-x t f for auto-fill-mode
    (org-clock-persistence-insinuate)))

; summarise TODOs
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Org Babel configurations
(setq org-src-fontify-natively t)
(ignore-errors
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (ditaa . t)
     (makefile . t)
     (python . t)
     (sh . t)
     (risu . t))))

;; See http://emacs.stackexchange.com/questions/499/finding-and-executing-org-babel-snippets-programatically
(defun my-babel-hashed-confirm (lang body)
  "Check against known hashes before prompting for confirmation.
See `org-confirm-babel-evaluate'."
  (let ((check (list lang (md5 body))))
    ;; If not hashed, prompt
    (if (not (member (list lang (md5 body)) my-org-babel-hashes))
        ;; Ask if you want to hash
        (if (yes-or-no-p "Store hash for block? ")
            ;; Hash is added, proceed with evaluation
            (progn
              (add-to-list 'my-org-babel-hashes check)
              'nil)
          ;; Return 't to prompt for evaluation
          't))))

(setq org-confirm-babel-evaluate 'my-babel-hashed-confirm)

(defun my-invoke-babel-named (name)
  "Evaluate named babel block"
  (interactive)
  (save-excursion
    (org-babel-goto-named-src-block name)
    (org-babel-execute-src-block-maybe)))

;; not used atm
;; ORG JIRA
(use-package org-jira
  :disabled t
  :if I-am-at-work
  :config
  (progn
    (setq jiralib-url "https://cards.linaro.org/"
          org-jira-working-dir (expand-file-name "~/org/jira"))
    (add-to-list 'org-jira-serv-alist
                 '(linaro .
                          (:url "https://cards.linaro.org/"
                                :username "alex.bennee@linaro.org"
                                :password #'(lambda () (my-pass-password "linaro")))))))

(provide 'my-org)
;;; my-org.el ends here
