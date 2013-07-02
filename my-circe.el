;;
;; Circe Config Options
;;

(require 'circe)

;; Don't spam me with JOIN/QUIT etc messages
(setq circe-reduce-lurker-spam t)
(circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))

;; Some defaults
(setq circe-network-options
      `(("Freenode"
	 :host "chat.freenode.net"
         :nick "ajb-mark-tools"
         :channels ("#emacs" "#emacs-circe")
         )
	("Pl0rt"
	 :host "irc.pl0rt.org"
         :nick "ajb"
	 :service "6697"
	 :tls 't
         :channels ("#blue")
         )
	("engbot"
	 :nick "ajb"
	 :channels ("#eng")
	 :service "6667"
	)
	("bitlbee"
	 :nick "ajb"
	 :channels ("&bitlbee")
	 :host "localhost"
	 :service "6667"
	 :lagmon-disabled t
	)))

;;
;; Auto pasting
(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

; spell checking
(when (fboundp 'turn-on-flyspell)
  (add-hook 'circe-channel-mode-hook 'turn-on-flyspell))

;; Mode line tweaks
(add-to-list 'global-mode-string
	     'tracking-mode-line-buffers
	     t)


;; Auto join everything
(defun my-irc-login ()
  "Login into my usual IRCs"
  (interactive)
  (circe "Freenode")
  (circe "Pl0rt")
  (circe "engbot"))
