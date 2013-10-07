;; org-mode related configuration bits
;;

(require 'org-element)

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
(setq org-provide-todo-statistics 'ALL-HEADLINES
      org-hierarchical-checkbox-statistics 'nil
      org-hierarchical-todo-statistics 'nil)

; summarise TODOs
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq ajb-work-org-file "/ssh:alex@bennee.com:/home/alex/doc/org/work.org")

(defun my-switch-to-org ()
  "Bring my default org buffer to the current window"
  (interactive)
  (switch-to-buffer
   (find-file ajb-work-org-file)
   (org-agenda-file-to-front)))

(defun ajb-get-trac-summary (id)
  "Fetch the bug summary directly from trac"
  (let ((url (format "http://engbot/bugs/ticket/%d?format=csv" id)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-line 1)
      (re-search-forward (format "%d," id))
      (buffer-substring (point) (- (re-search-forward ",") 1)))))

(defun ajb-format-trac-item (type &optional id)
  "Format a ORG TODO item for a given trac item"
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


;; From: Mark Belmont  http://code.google.com/p/marcshacks/source/browse/elisp/personal/marcshacks.el

(defun org-time-delta-seconds (time-string seconds)
  (format-time-string
   "%Y-%m-%d"
   (seconds-to-time
    (+ (org-float-time (apply 'encode-time
                              (org-parse-time-string time-string)))
       seconds))))
(defun get-day-time-string (time-string)
  (format-time-string
   "%u"
   (seconds-to-time
    (org-float-time
     (apply 'encode-time (org-parse-time-string time-string))))))

(defun org-list-clocks-current-item (check-n-days)
  "Extract clocked hours per day of current item"
  (save-restriction
    (org-narrow-to-subtree)
    (let* ((next-day-secs (* 3600 25))
           (ts (format-time-string "%Y-%m-%d" (seconds-to-time (- (org-float-time (current-time)) (* check-n-days 3600 24)))))
           (te (org-time-delta-seconds ts next-day-secs))
           (res '())
           (dates '()))
      (while (< (org-float-time
                 (apply 'encode-time (org-parse-time-string ts)))
                (org-float-time (current-time)))
        (org-clock-sum ts te) ; slow
        (setq res (cons (/ org-clock-file-total-minutes 60.0) res)
              dates (cons (if (equal (get-day-time-string ts) "1")
                              "M"
                            (substring ts 8 10)) dates)
              ts te
              te (org-time-delta-seconds ts next-day-secs)))
      (list dates res))))

(defun org-chart-clocks-current-item ()
  "Request a Google Chart with the clocked time of the current item."
  (interactive)
  (let* ((x (org-list-clocks-current-item 40))
	 (dates (reverse (car x)))
	 (res (reverse (cadr x)))
	 (active-days (length (remove-if (lambda (x) (= x 0)) res)))
	 (sum-hours (reduce (lambda (a b) (+ a b)) res)))
    (browse-url (format "http://chart.apis.google.com/chart?chxl=1:|%s&chxr=0,0,8|1,0,105&chxt=y,x&chbh=15&chs=1000x300&cht=bvg&chco=80C65A&chds=0,8&chd=t:%s&chg=0,12.5&chma=|5,10&chtt=Clocked+Activity+(%d+days,+%d+hours)&chm=h,FF0000,0,%f,1"
			(mapconcat (lambda (x) x) dates "|")
			(mapconcat (lambda (x) (format "%s" x)) res  ",")
			active-days
			sum-hours
			(/ sum-hours active-days 8)))))

;; Org Babel configurations
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (sh . t)))
