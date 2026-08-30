;;; my-circe --- Configuartion for Circe IRC client -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This contains basic account configuration for Circe.  I do use
;; the tracking utility for things like compile buffers as well.
;;
;;; Code:

(eval-when-compile (require 'use-package))
(require 'dash)
(require 'url-http)
(require 'my-vars)
(require 'my-utils)
(require 'my-spell)
(require 'my-circe-secrets nil t)

;; Logging
(use-package lui-logging
  :commands enable-lui-logging disable-lui-logging)

(defvar my-logged-chans
  '("#qemu@irccloud (oftc)" "#qemu-gsoc@irccloud (oftc)" "#linaro-virtualization@irccloud (libera)")
  "List of channels which I log.")

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

;; Lui - Circe's Line User Interface
(use-package lui
  :config (setq lui-flyspell-p t)
  :hook (lui-pre-input . my-check-for-meeting))

;; https://paste.centos.org/view/6747518d
;; https://paste.debian.net/hidden/b9b43e07
;;

(defun my-create-pastebin-regex (host path alpha)
  "Return a regex to extract pastebin result URLs from text.
HOST is the pastebin host, PATH the url path and ALPHA indicates if
the string contains alpha characters as well as numbers."
  (rx-to-string
   `(:
     (group
      "https://"
      ,host
      "/"
      ,path
      "/"
      ,(if alpha
           '(one-or-more hex-digit)
         '(one-or-more digit)))) t))

(defun lui-autopaste-service-private-linaro (text)
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

(defun lui-autopaste-service-linaro (text)
  "Paste TEXT to pastebin.linaro.org and return the paste url."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")
           ("Referer" . "https://pastebin.linaro.org")))
        (url-request-data (format "text=%s&language=text"
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

(defvar my-find-paste-url
  (rx (:
       ">"
       (group (: "paste.debian.net/" (one-or-more (in digit))))
       "</a>"))
  "Regexp to extract result URL from response.
We need to fish it out from the returned page like savages.")

(defun lui-autopaste-service-debian (text)
  "Paste TEXT to paste.debian.net return the paste url."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")
           ("Referer" . "https://paste.debian.net")))
        (url-request-data (format "code=%s&lang=text&expire=604800"
                                  (url-hexify-string text)))
        (url-http-attempt-keepalives nil))
    (let ((buf (url-retrieve-synchronously
                "https://paste.debian.net")))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (if (re-search-forward my-find-paste-url nil t)
                (format "http://%s" (match-string 1))
              (error "Error during pasting to paste.debian.net")))
        (kill-buffer buf)))))

(defun lui-autopaste-service-ubuntu (text)
  "Paste TEXT to pastebin.ubuntu.com and return the paste url."
  (let* ((user "alex.bennee@linaro.org")
         (pass (my-pass-password "ubuntu-one"))
         (auth (base64-encode-string (concat user ":" pass) t))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Referer" . "https://pastebin.ubuntu.com/")
            ("Authorization" . ,(concat "Basic " auth))))
        (url-request-data (format "poster=%s&syntax=text&content=%s"
                                  (url-hexify-string user-full-name)
                                  (url-hexify-string text))))
    (let ((buf (url-retrieve-synchronously
                "https://pastebin.ubuntu.com/")))
      (unwind-protect
          (with-current-buffer buf
            (setq my-global-debug-var buf)
            (goto-char (point-min))
            (if (re-search-forward "^Location: \\(.*?\\)\r?$" nil t)
                (let ((loc (match-string 1)))
                  (if (string-match-p "^http" loc)
                      loc
                    (concat "https://pastebin.ubuntu.com" loc)))
              (error "Error during pasting to pastebin.ubuntu.com")))
        (kill-buffer buf)))))

(defun lui-autopaste-service-centos (text)
  "Paste TEXT to paste.centos.org and return the paste url."
  (let* ((paste-host "paste.centos.org")
         (paste-url (format "https://%s/" paste-host))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/x-www-form-urlencoded")
            ("Referer" . ,paste-url)))
         (url-request-data (format "title=%s&lang=text&code=%s&expire=1440&submit=submit"
                                   (url-hexify-string "Circe Paste")
                                   (url-hexify-string text)))
         (url-http-attempt-keepalives nil)
         (buf (url-retrieve-synchronously paste-url)))
    (unwind-protect
        (with-current-buffer buf
          (setq my-global-debug-var buf)
          (goto-char (point-min))
          (if (re-search-forward
               (my-create-pastebin-regex paste-host "view" t)
               nil t)
              (concat (match-string 1))
            (error "Error during pasting to paste.centos.org")))
      (kill-buffer buf))))

;; Auto pasting
(use-package lui-autopaste
  :commands enable-lui-autopaste
  :config (setq lui-autopaste-function #'lui-autopaste-service-centos))

;; Auto join everything
(defvar my-irc-login-timer
  nil
  "Timer for login.")

(defun my-irc-login ()
  "Login into my usual IRCs."
  (interactive)
  ;; is lagmon causing the constant reconnects?
  ;; (circe-lagmon-mode)
  (when I-am-at-work
    (circe "irccloud (libera)")
    (circe "irccloud (oftc)")
    (circe "irccloud (Linaro Slack)"))
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
  :init (when (and I-am-at-work (daemonp)
                   (not (or I-am-root my-irc-login-timer)))
          (setq my-irc-login-timer (run-with-idle-timer 120 nil 'my-irc-login)))
  :config (setq circe-reduce-lurker-spam t))

(use-package circe-chanop
  :after circe)

(use-package circe-color-nicks
  :after circe
  :commands enable-circe-color-nicks
  :init (enable-circe-color-nicks))

(provide 'my-circe)
;;; my-circe.el ends here
