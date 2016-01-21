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

;; Auto pasting

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
(defun my-irc-login ()
  "Login into my usual IRCs."
  (interactive)
  (when I-am-at-work
    (circe "znc-freenode")
    (circe "znc-oftc")
    (circe "bitlbee"))
  (circe "Freenode")
  (circe "Pl0rt"))

(use-package circe
  :commands (circe circe-set-display-handler)
  ;; :diminish ((circe-channel-mode . "CirceChan")
  ;;            (circe-server-mode . "CirceServ"))
  :requires my-tracking
  :init (when (and I-am-at-work (daemonp))
          (run-with-idle-timer 120 nil 'my-irc-login))
  :config
  (progn
    (require 'tls)
    ;; Paste Support
    (use-package lui-autopaste
      :commands enable-lui-autopaste
      :init
      (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste))
    ;; spell checking
    (add-hook 'circe-channel-mode-hook 'turn-on-flyspell)
    ;; Colour nicks
    (enable-circe-color-nicks)
    ;; Mode line tweaks
    ;; Channel configurations
    (setq circe-reduce-lurker-spam t
          circe-network-options
          `(("Freenode"
             :host "chat.freenode.net"
             :server-buffer-name "⇄ Freenode"
             :nick "stsquad"
             :channels ("#emacs" "#emacs-circe")
             )
            ("OFTC"
             :host "irc.oftc.net"
             :server-buffer-name "⇄ OFTC"
             :port "6697"
             :tls t
             :nick "stsquad"
             :channels ("#qemu" "#qemu-gsoc")
             )
            ("znc-freenode"
             :host "ircproxy.linaro.org"
             :server-buffer-name "⇄ Freenode (ZNC)"
             :port "6697"
             :pass my-znc-freenode-password
             :channels ("#linaro" "#linaro-virtualization")
             :tls 't
             )
            ("znc-oftc"
             :host "ircproxy.linaro.org"
             :server-buffer-name "⇄ OFTC (ZNC)"
             :port "6697"
             :pass my-znc-oftc-password
             :channels ("#qemu" "#qemu-gsoc")
             :tls 't
             )
            ("Pl0rt"
             :host "irc.pl0rt.org"
             :server-buffer-name "⇄ Pl0rt"
             :nick "ajb"
             :service "6697"
             :tls 't
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
             :host "localhost"
             :service "6667"
             )))))

(provide 'my-circe)
;;; my-circe.el ends here
