;;; my-org --- org-mode related configuration bits
;;
;;; Commentary:
;;
;; I'm slowly using org-mode more and more although there seem to be
;; some poor interactions with use-package. As a result I need to
;; split up the various sub-modes of org-mode.
;;
;;; Code:

;; (add-to-list 'org-export-options-alist '(:date "DATE" nil nil nil))
;; (add-to-list 'org-export-options-alist '(:title "TITLE" nil nil nil))


(require 'use-package)

(require 'my-vars)
(require 'my-email)
(require 'my-basic-modes)
(require 'my-hydra)

(defvar ajb-work-org-file
  (when I-am-at-work "/home/alex/org/index.org")
  "The location of my main work scratchpad.")

(defvar my-org-babel-hashes nil
  "List of known babel code hashes.
This prevents org re-asking every time I restart.")
(add-to-list 'savehist-additional-variables 'my-org-babel-hashes)

(defvar my-org-default-code-block nil
  "Default code block to run on `org-ctrl-c-ctrl-c'.

This is used by my-org-run-default-block which is added to
`org-ctrl-c-ctrl-c-final-hook'")
(make-variable-buffer-local 'my-org-default-code-block)

(defun my-org-run-default-block ()
  "Evaluate the code block `my-org-default-code-block' if it exists."
  (interactive)
  (when my-org-default-code-block
    (cond
     ((stringp my-org-default-code-block)
      (save-excursion
        (org-babel-goto-named-src-block my-org-default-code-block)
        (org-babel-execute-src-block-maybe)))
     ((functionp my-org-default-code-block)
      (funcall my-org-default-code-block))
     (t (error "what to do with: %s" my-org-default-code-block)))))

;; (add-to-list 'org-ctrl-c-ctrl-c-final-hook 'my-org-run-default-block)

(use-package org-agenda
  :commands org-agenda
  :config
  (setq 
   ;; Agenda locations
   org-agenda-files '("~/org/")
   org-refile-targets '((nil :maxlevel . 2)
                        (org-agenda-files :maxlevel . 2))))

(use-package org-src
  :commands org-edit-src-code
  :config (setq org-src-window-setup 'current-window))

(use-package org-capture
  :commands org-capture
  :config (setq
           org-capture-templates
           '(("lr" "Review Comment (email)"
              checkitem
              (file+headline "review.org" "Review Comments")
              "  - [ ] %a%?")
             ("R" "Review Comment (region)"
              checkitem
              (file+headline "review.org" "Review Comments")
              "  - [ ] %i%?")
             ("t" "Add TODO task"
              entry
              (file+headline "team.org" "Tasks")
              "** TODO %i%?")
             ("T" "Add TODO task with mail reference"
              entry
              (file+headline "team.org" "Tasks")
              "** TODO %i%?\nSee %a%?"))))

(use-package ox-publish
  :commands org-publish
  :config
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
                             "org-static"))))))
;; Mail integration
(use-package org-mu4e
  :if (locate-library "org-mu4e")
  :config (add-to-list 'org-modules 'org-mu4e t))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (progn
    (message "org init:")
    (setq
     ;; General navigation
     org-return-follows-link t))
  :config
  (progn
    (message "org config:")
    (setq
     ;; General navigation
     org-return-follows-link t
     ;; Agenda locations
     org-agenda-files '("~/org/")
     org-refile-targets '((nil :maxlevel . 2)
                          (org-agenda-files :maxlevel . 2))
     ;; Capture Templates
     org-directory "~/org"
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

    ;; Add my special handler.
    (add-to-list 'org-ctrl-c-ctrl-c-final-hook
                 'my-org-run-default-block)
    
    ;; Ditta
    (let ((ditta-path "/usr/share/ditaa/ditaa.jar"))
      (when (file-exists-p ditta-path)
        (setq org-ditaa-jar-path ditta-path)))
    
    ;; Mode keys
    ;; (define-key org-mode-map (kbd "M-[ c") 'org-demote-subtree)
    ;; (define-key org-mode-map (kbd "M-[ d") 'org-promote-subtree)
    (when (fboundp 'helm-org-agenda-files-headings)
      (define-key org-mode-map (kbd "C-f")
        'helm-org-agenda-files-headings))
    (with-eval-after-load 'hydra
      (global-set-key
       (kbd "C-c C-o")
       (defhydra my-hydra-org (:color blue)
         "Access org-mode"
         ("a" org-agenda "org-agenda")
         ("c" org-capture "org-capture")
         ("h" helm-org-agenda-files-headings "org-headings (helm)")
         ("r" (org-capture nil "r") "org-capture-email-review"))))
    (org-clock-persistence-insinuate)))

;; Mail integration
(use-package org-mu4e
  :config (add-to-list 'org-modules 'org-mu4e t))

;; Org reveal
(use-package ox-reveal)

; summarise TODOs
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Org Babel configurations
(ignore-errors
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (ditaa . t)
     (makefile . t)
     (python . t)
     (sh . t)
     (risu . t)
     (text . t))))
;;      (C . t)
;;      (asm . t))))

(use-package org-src
  :config
  (progn
    (setq org-src-fontify-natively t)))

(use-package graphiz-dot-mode
  :config (progn
            (let ((cust-install
                   (format "%s/src/graphviz/install"
                           (getenv "HOME"))))
              (when (file-exists-p cust-install)
                (my-add-world-to-env cust-install)))
            (add-to-list 'org-src-lang-modes
                         '("dot" . graphviz-dot))))

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
