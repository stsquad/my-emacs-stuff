;;; my-org --- org-mode related configuration bits
;;
;;; Commentary:
;;
;; Not much in here as I'm not yet a heavy user of org-mode
;;
;;; Code:

(require 'my-vars)

(require 'org)
(require 'org-clock)
(require 'org-capture nil t)
(require 'org-element nil t)
(require 'org-list nil t)
(require 'ox nil t)
(require 'ox-reveal nil t)

(defvar ajb-work-org-file
  (when I-am-at-work "/home/alex/org/index.org")
  "The location of my main work scratchpad.")

(defvar my-org-babel-hashes nil
  "List of known babel hashes to prevent re-asking every bloodly time...")

;; General navigation
(setq org-return-follows-link t)

;; Clocking behaviour
(setq org-clock-persist 't
      org-clock-in-resume 't                 ; resume currently open clock
      org-clock-persist-query-resume 'nil    ; don't ask me about it
      org-log-into-drawer 't                 ; roll clocks up into drawers
      org-clock-idle-time 'nil)

(org-clock-persistence-insinuate)

;; Mode line tweaks
(setq org-clock-mode-line-total 'current
      org-clock-clocked-in-display 'frame-title)

;; TODO Hierarchy
(setq org-provide-todo-statistics t
      org-checkbox-hierarchical-statistics nil
      org-hierarchical-todo-statistics nil)

;; Export settings
(setq org-export-allow-bind-keywords t)

;; ORG JIRA
(when (and I-am-at-work (require 'org-jira nil t))
  (setq jiralib-url "https://cards.linaro.org/")
  (setq org-jira-working-dir (expand-file-name "~/org/jira"))
  (add-to-list 'org-jira-serv-alist
               '(linaro .
                        (:url "https://cards.linaro.org/"
                              :username "alex.bennee@linaro.org"
                              :password #'(lambda () (my-pass-password "linaro"))))))

(when I-am-at-work
  (setq
   org-agenda-files '("~/org/")
   org-refile-targets '((nil :maxlevel . 2)
                        (org-agenda-files :maxlevel . 2))
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
     ("org" :components ("org-notes" "org-presentations" "org-static")))))

; summarise TODOs
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(defun ajb-get-trac-summary (id)
  "Fetch the bug summary directly from trac."
  (let ((url (format "http://engbot/bugs/ticket/%d?format=csv" id)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (forward-line)
      (re-search-forward (format "%d," id))
      (buffer-substring (point) (- (re-search-forward ",") 1)))))

(defun ajb-format-trac-item (type &optional id)
  "Format a ORG TODO item for a given trac item."
  (interactive "nTrac ID:")
  (when (not id)
    (setq id (read-number "Trac ID:")))
  (format
   "*** TODO %s [[http://engbot/bugs/ticket/%d][%d]] - %s\n"
   type id id (ajb-get-trac-summary id)))

;; Capture Templates
(setq org-capture-templates
      '(("b" "Bug" entry (file+headline ajb-work-org-file "Bug Fixing")
             "%(ajb-format-trac-item \"Bug\")" :clock-in)
	("f" "Feature" entry (file+headline ajb-work-org-file "Development")
             "%(ajb-format-trac-item \"Feature\")" :clock-in)))

(global-set-key (kbd "C-c C-o") 'org-capture)

; add binding to bury-buffer... C-x k?
(define-key org-mode-map (kbd "M-[ c") 'org-demote-subtree)
(define-key org-mode-map (kbd "M-[ d") 'org-promote-subtree)
(define-key org-mode-map (kbd "C-f") nil) ; I use C-x t f for auto-fill-mode

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


(provide 'my-org)
;;; my-org.el ends here
