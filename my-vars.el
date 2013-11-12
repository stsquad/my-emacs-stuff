;;
;; Common variables used by all config files
;;

(require 'my-utils)

(defvar I-am-emacs-21+ (>= emacs-major-version 21))
(defvar I-am-emacs-22+ (>= emacs-major-version 22))
(defvar I-am-emacs-23+ (>= emacs-major-version 23))
(defvar I-am-emacs-24+ (>= emacs-major-version 24))

(defvar I-am-gnuemacs (string-match "GNU Emacs" (emacs-version)))
(defvar I-am-xemacs (string-match "XEmacs" (emacs-version)))

;; Lets define which machine I'm on, therefor if I am at work
;; (this of course falls down when logging on remotely via tramp)

(defvar I-am-at-work (string-match "sloy" (system-name)))
(defvar I-am-at-home (string-match "danny" (system-name)))
(defvar I-am-on-netbook (string-match "trent" (system-name)))
(defvar I-am-on-pixel (and (string-match "localhost" (system-name))
                           (which-lookup "host-x11")))

;; Lets set some parameters if we are running as a console or under X
;
; Note these are not useful for --daemon invocations and should now be
; deprecated in favour of "live" tests on window-system
;
(defvar I-am-in-X (eval 'window-system));
(defvar I-am-in-console (not (eval 'window-system)))
(defvar I-am-on-MacOSX (or (string-match "Carbon" (emacs-version))
                           (string-match "apple-darwin" (emacs-version))))
(defvar I-am-remote (getenv "SSH_TTY"))

(provide 'my-vars)
