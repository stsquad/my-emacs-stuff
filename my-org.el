;; org-mode related configuration bits
;;
;;

(setq org-clock-persist 't
      org-clock-idle-time 10)

(org-clock-persistence-insinuate)

(when I-am-at-work
  (find-file "~/Documents/org/clock.org")
  (org-agenda-file-to-front))
