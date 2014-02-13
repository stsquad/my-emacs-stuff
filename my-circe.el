;;; my-circe --- Configuartion for Circe IRC client
;;
;;; Commentary:
;;
;; This contains basic account configuration for Circe.  I do use
;; the tracking utility for things like compile buffers as well.
;;
;;; Code:

(require 'circe)
(require 'my-utils)

;; Don't spam me with JOIN/QUIT etc messages
(setq circe-reduce-lurker-spam t)
(circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))

(defun my-bitlbee-password (server)
  "Return the password for the `SERVER'."
  (shell-command-to-string (format "pass bitlbee")))

(defun my-znc-password (server)
  "Return the password for the `SERVER'."
  (format "ajb-linaro:%s"
          (chomp (shell-command-to-string (format "pass znc")))))

;; Some defaults
(setq circe-network-options
      `(("Freenode"
	 :host "chat.freenode.net"
         :nick "ajb-mark-tools"
         :channels ("#emacs" "#emacs-circe")
         )
	("OFTC"
	 :host "irc.oftc.net"
	 :port "6697"
	 :tls 't
         :nick "stsquad"
         :channels ("#qemu")
         )
	("znc"
	 :host "ircproxy.linaro.org"
	 :port "6697"
         :pass my-znc-password
         :channels ("#linaro" "#linaro-virtualization")
	 :tls 't
         )
	("Pl0rt"
	 :host "irc.pl0rt.org"
         :nick "ajb"
	 :service "6697"
	 :tls 't
         :channels ("#blue")
         )
	("bitlbee"
	 :nick "ajb"
         :pass my-bitlbee-password
         :nickserv-mask "\\(bitlbee\\|root\\)!\\(bitlbee\\|root\\)@"
         :nickserv-identify-challenge "use the \x02identify\x02 command to identify yourself"
         :nickserv-identify-command "PRIVMSG &bitlbee :identify {password}"
         :nickserv-identify-confirmation "Password accepted, settings and accounts loaded"
	 :channels ("&bitlbee")
	 :host "localhost"
	 :service "6667"
	 :lagmon-disabled t
	)))

;;
;; Auto pasting
(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

(defun lui-autopaste-service-linaro (text)
  "Paste TEXT to (private) pastebin.linaro.org and return the paste url."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")
           ("Referer" . "https://pastebin.linaro.org")))
        (url-request-data (format "text=%s&language=text&webpage=&private=on"
                                  (url-hexify-string text)))
        (url-http-attempt-keepalives nil))
    (let ((buf (url-retrieve-synchronously
                "https://pastebin.linaro.org/api/create")))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (if (re-search-forward "\\(https://pastebin.linaro.org/view/.*\\)" nil t)
                (match-string 1)
              (error "Error during pasting to pastebin.linaro.org")))
        (kill-buffer buf)))))

; spell checking
(when (fboundp 'turn-on-flyspell)
  (add-hook 'circe-channel-mode-hook 'turn-on-flyspell))

;; Mode line tweaks
(add-to-list 'global-mode-string
	     'tracking-mode-line-buffers
	     t)

;; Auto join everything
(defun my-irc-login ()
  "Login into my usual IRCs."
  (interactive)
  (circe "Freenode")
  (circe "Pl0rt")
  (circe "OFTC")
  (circe "znc"))


(provide 'my-circe)
;;; my-circe.el ends here
