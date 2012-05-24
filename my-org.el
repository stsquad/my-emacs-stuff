;; org-mode related configuration bits
;;
;;

;; Clocking behaviour
(setq org-clock-persist 't
      org-clock-in-resume 't                 ; resume currently open clock
      org-clock-persist-query-resume 'nil    ; don't ask me about it
      org-log-into-drawer 't                 ; roll clocks up into drawers
      org-clock-idle-time 10)

(org-clock-persistence-insinuate)

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

(when (and (daemonp) I-am-at-work)
  (setq
   org-agenda-files (quote ("/ssh:alex@bennee.com:/home/alex/doc/org/work.org")))
  (find-file "/ssh:alex@bennee.com:/home/alex/doc/org/work.org")
  (org-agenda-file-to-front))

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
