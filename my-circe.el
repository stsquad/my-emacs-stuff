;;; my-circe --- Configuartion for Circe IRC client
;;
;;; Commentary:
;;
;; This contains basic account configuration for Circe.  I do use
;; the tracking utility for things like compile buffers as well.
;;
;;; Code:

(require 'use-package)
(require 'my-vars)
(require 'my-utils)
(require 'my-spell)
(require 'tls)
(require 'nsm)

(defun my-bitlbee-password (server)
  "Return the password for the `SERVER'."
  (my-pass-password "bitlbee"))

(defun my-znc-freenode-password (server)
  "Return the password for the `SERVER'."
  (format "ajb-linaro/Freenode:%s"
          (my-pass-password "znc")))

(defun my-znc-oftc-password (server)
  "Return the password for the `SERVER'."
  (format "ajb-linaro/oftc:%s"
          (my-pass-password "znc")))

(defun my-znc-libera-password (server)
  "Return the password for the `SERVER'."
  (format "ajb-linaro/libera:%s"
          (my-pass-password "znc")))

;; These are both "personal" nicks for non-ZNC bounced connections
(defun my-freenode-nick-password (server)
  "Return the password for the `SERVER'."
  (my-pass-password "freenode-nick"))

(defun my-oftc-nick-password (server)
  "Return the password for the `SERVER'."
  (my-pass-password "stsquad@irc.oftc.net"))

(defun my-libera-nick-password (server)
  "Return the password for the `SERVER'."
  (my-pass-password "stsquad@irc.libera.chat"))

(defun my-gitter-password (server)
  "Return the password for the `SERVER'."
  (my-pass-password "gitter-irc"))


;; Logging
(use-package lui-logging
  :commands enable-lui-logging)

(defvar my-logged-chans
  '("#qemu@znc-oftc" "#qemu-gsoc@znc-oftc" "#linaro-virtualization@znc-libera" "#linaro-tcwg@znc-freenode")
  "List of channels which I log")

(defun my-maybe-log-channel ()
  "Maybe start logging the an IRC channel."
  (when (-contains? my-logged-chans (buffer-name))
    (enable-lui-logging)))

(defun my-check-for-meeting ()
  "Check for start/endmeeting and enable appropriately."
  (unless (-contains? my-logged-chans (buffer-name))
    (save-excursion
      (goto-char 0)
      (if (re-search-forward "#startmeeting" nil t)
          (enable-lui-logging)
        (goto-char 0)
        (if (and (re-search-forward "#endmeeting" nil t) lui-logging-timer)
            (disable-lui-logging))))))

;; Lui
(use-package lui
  :config (setq lui-flyspell-p t)
  :hook (lui-pre-input . my-check-for-meeting))

;; Auto pasting
(use-package lui-autopaste
  :commands enable-lui-autopaste)

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

;; Auto join everything
(defvar my-irc-login-timer
  nil
  "Timer for login.")

(defun my-irc-login ()
  "Login into my usual IRCs."
  (interactive)
  (circe-lagmon-mode)
  (when I-am-at-work
    (circe "znc-libera")
    (circe "znc-oftc")
    (circe "bitlbee"))
  (circe "Pl0rt"))

(defun my-disable-irc-login ()
  "Disable an idle login."
  (interactive)
  (when my-irc-login-timer
    (cancel-timer my-irc-login-timer)))

;; Find and capture logins
(defun my-ssh-to-server ()
  "Login to a server mentioned in buffer."
  (interactive)
  (let ((host (my-capture-login)))
    (when host
      (shell-command-to-string
       (format "tmux new-window -a -n %s 'ssh %s'" host host)))))


(use-package circe
  :ensure t
  :commands (circe circe-set-display-handler)
  :bind (:map circe-query-mode-map
              ("C-c C-c" . my-ssh-to-server))
  :hook ((circe-channel-mode . enable-lui-autopaste)
         (circe-channel-mode . my-maybe-log-channel))
  :requires my-tracking
  :init (when (and I-am-at-work
                   (daemonp)
                   (not I-am-root)
                   (not my-irc-login-timer))
          ;; override cert check for pl0rt
          (let ((id (nsm-id "irc.pl0rt.org" 6697)))
            (setq nsm-temporary-host-settings
                  (list (list :id id :conditions '(:no-host-match :expired :invalid :verify-cert)))))
          (setq my-irc-login-timer (run-with-idle-timer 120 nil 'my-irc-login)))
  :config
  (progn
    (when I-am-on-server
      (setq circe-default-nick "stsquad"
            circe-default-user "stsquad"
            circe-default-realname "stsquad"))
    ;; override cert check for pl0rt
    (let ((id (nsm-id "irc.pl0rt.org" 6697)))
      (setq nsm-temporary-host-settings
            (list (list :id id :conditions '(:no-host-match :expired :invalid :verify-cert)))))
    (setq circe-reduce-lurker-spam t
          circe-network-options
          `(("Freenode"
             :host "chat.freenode.net"
             :server-buffer-name "⇄ Freenode"
             :port "6697"
             :tls t
             :nick "stsquad"
             :nickserv-password my-freenode-nick-password
             )
            ("OFTC"
             :host "irc.oftc.net"
             :server-buffer-name "⇄ OFTC"
             :port "6697"
             :tls t
             :nick "stsquad"
             :nickserv-password my-oftc-nick-password
             :channels ("#debian-devel" "#debian-cross")
             )
            ("Libera Chat"
             :host "irc.libera.chat"
             :server-buffer-name "⇄ Libera Chat"
             :port "6697"
             :tls t
             :nick "stsquad"
             :nickserv-password my-libera-nick-password
             )
            ("znc-freenode"
             :host "ircproxy.linaro.org"
             :server-buffer-name "⇄ Freenode (ZNC)"
             :port "6697"
             :tls t
             :pass my-znc-freenode-password
             ;; NickServ is handled by ZNC and the SASL login
             :channels ("#linaro" "#linaro-virtualization")
             )
            ("znc-oftc"
             :host "ircproxy.linaro.org"
             :server-buffer-name "⇄ OFTC (ZNC)"
             :port "6697"
             :tls t
             :pass my-znc-oftc-password
             ;; NickServ is handled by ZNC and the SASL login
             :channels ("#qemu" "#qemu-gsoc")
             )
            ("znc-libera"
             :host "ircproxy.linaro.org"
             :server-buffer-name "⇄ Libera Chat (ZNC)"
             :port "6697"
             :tls t
             :pass my-znc-libera-password
             ;; NickServ is handled by ZNC and the SASL login
             )
            ("gitter"
             :host "irc.gitter.im"
             :server-buffer-name "⇄ Gitter (irc gateway)"
             :port "6697"
             :nick "stsquad"
             :pass my-gitter-password
             :tls t
             )
            ("Pl0rt"
             :host "irc.pl0rt.org"
             :server-buffer-name "⇄ Pl0rt"
             :port "6697"
             :nick "ajb"
             :use-tls t
             :channels ("#blue")
             )
            ("bitlbee"
             :nick "ajb"
             :server-buffer-name "⇄ bitlbee"
             ;; :pass my-bitlbee-password
             :nickserv-password my-bitlbee-password
             :nickserv-mask "\\(bitlbee\\|root\\)!\\(bitlbee\\|root\\)@"
             :nickserv-identify-challenge "use the \x02identify\x02 command to identify yourself"
             :nickserv-identify-command "PRIVMSG &bitlbee :identify {password}"
             :nickserv-identify-confirmation "Password accepted, settings and accounts loaded"
             :channels ("&bitlbee")
             ; does this set circe-lagmon-disabled in the server buffer?
             :lagmon-disabled t
             :host "localhost"
             :service "6667")
            ("Slack"
             :nick "alex.bennee"
             :server-buffer-name "⇄ Slack"
             :host "localhost"
             :service "9007")))))

(use-package circe-chanop
  :after circe)

(use-package circe-color-nicks
  :after circe
  :init (enable-circe-color-nicks))

(provide 'my-circe)
;;; my-circe.el ends here
