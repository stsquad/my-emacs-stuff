;;; my-matrix --- Matrix handling
;;
;;; Commentary:
;;
;; Currently using ement
;;
;;; Code:

;; while running ement from a checkout we need to ensure
;; some of its dependencies
(use-package taxy-magit-section
  :ensure t)

(use-package plz
  :ensure t)

(use-package ement
  :requires (plz taxy taxy-magit-section)
  :load-path (lambda () (my-return-path-if-ok
                         "~/src/emacs/ement.el.git"))
  :custom
  ;; :NOTE| Notifications ought to be limited to @mentions only!
  (ement-notify-notification-predicates '(ement-notify--event-mentions-session-user-p
                                          ement-notify--event-mentions-room-p))
  (ement-room-images t)
  (ement-room-message-format-spec "%S>%L %B%r%R[%t]")
  (ement-room-send-message-filter #'ement-room-send-org-filter)
  (ement-save-sessions t))

(provide 'my-matrix)
;;; my-matrix.el ends here
