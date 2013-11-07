;;
;; Customisations for ERC
;;
(require 'erc)
(require 'tls)


; aliases for various sound files as I have different ones on random machines
(defvar my-erc-msg-sound
  (find-valid-file
   '("/usr/share/sounds/purple/receive.wav"
     "/usr/share/sounds/ekiga/newmessage.wav"
     "/usr/share/sounds/generic.wav")))

(defvar my-erc-alert-sound
  (find-valid-file
   '("/usr/share/sounds/purple/alert.wav"
     "/usr/share/sounds/ekiga/voicemail.wav"
     "/usr/share/sounds/question.wav")))

(defun my-erc-sound-play (file)
  "Play a sound for erc, but do it asynchronously"
  (start-process-shell-command "erc-sound"
			       nil
			       (concat "paplay " file)))

(defun my-erc-text-match (match-type nickuserhost message)
  "My text matching hook for ERC, mainly for alert sounds"
  (cond
   ((eq match-type 'current-nick)
    (when my-erc-msg-sound
	(my-erc-sound-play my-erc-msg-sound)))
   ((eq match-type 'keyword)
    (when my-erc-alert-sound
      (my-erc-sound-play my-erc-alert-sound)))))

(add-hook 'erc-text-matched-hook 'my-erc-text-match)

; Define some useful servers
(setq erc-server-history-list '("engbot"
				"irc.freenode.org"
				"irc.gnome.org"))

(erc-track-mode t)
(erc-autojoin-mode nil)

(setq erc-autojoin-channels-alist '(("irc.pl0rt.org" "#blue")
				    ("engbot" "#eng"))
      erc-hide-list '("JOIN" "PART"
		      "QUIT" "NICK"))

(defun my-erc-pl0rt()
  "Connect to pl0rt"
  (interactive)
  (erc-tls :server "irc.pl0rt.org" :port 6697
	   :nick "ajb" :full-name "Alex"))

(defun my-erc-work()
  "Connect to engbot"
  (interactive)
  (erc :server "engbot"
       :nick "ajb" :full-name "Alex"))
