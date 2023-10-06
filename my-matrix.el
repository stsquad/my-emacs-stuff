;;; my-matrix --- Matrix handling
;;
;;; Commentary:
;;
;; Currently using ement
;;
;;; Code:

(use-package ement
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
