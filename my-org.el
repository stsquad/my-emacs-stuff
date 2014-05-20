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

(when I-am-at-work
  (setq
   org-agenda-files '("~/org/")
   org-publish-project-alist
   '(
     ("org-notes"
      :base-directory "~/org/"
      :base-extension "org"
      :publishing-directory "~/public_html/org/"
      :recursive t
      :publishing-function org-html-publish-to-html
      :headline-levels 4             ; Just the default for this project.
      :auto-preamble t
      )
     ("org-static"
      :base-directory "~/org/"
      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
      :publishing-directory "~/public_html/org/"
      :recursive t
      :publishing-function org-publish-attachment
      )
     ("org" :components ("org-notes" "org-static")))))

; summarise TODOs
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(defun my-switch-to-org ()
  "Bring my default org buffer to the current window."
  (interactive)
  (switch-to-buffer
   (find-file ajb-work-org-file)
   (org-agenda-file-to-front)))

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

;(global-set-key (kbd "C-x o") 'my-switch-to-org)
; add binding to bury-buffer... C-x k?


;; Org Babel configurations
(ignore-errors
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (ditaa . t)
     (makefile . t)
     (python . t)
     (sh . t))))


(provide 'my-org)
;;; my-org.el ends here
