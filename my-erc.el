;;
;; Customisations for ERC
;;

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

(defun my-erc-text-match (match-type nickuserhost message)
  "My text matching hook for ERC, mainly for alert sounds"
  (cond
   ((eq match-type 'current-nick)
    (if my-erc-msg-sound
	(play-sound-file my-erc-msg-sound)
      (erc-beep-on-match match-type nickuserhost message)))
   ((eq match-type 'keyword)
    (if my-erc-alert-sound
	(play-sound-file my-erc-alert-sound)
      (erc-beep-on-match match-type nickuserhost message)))))

(add-hook 'erc-text-matched-hook 'my-erc-text-match)

(erc-track-mode t)
(erc-autojoin-mode 'nil)

(setq erc-beep-match-types '(current-nick keyword)
      erc-autojoin-channels-alist
      '(("irc.pl0rt.org" "#blue")
	("irc.srcf.ucam.org" "#mage"))
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477")
      erc-hide-list '("JOIN" "PART"
		      "QUIT" "NICK"))

