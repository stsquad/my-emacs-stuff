;;; my-vars.el --- Common variables used used by all config files
;;
;;; Commentary:
;;
;; These are just a bunch of simple variables I use throughout the
;; rest of the code for controlling optional features.
;;
;;; Code:

(require 'my-utils)

(defvar I-am-emacs-21+ (>= emacs-major-version 21))
(defvar I-am-emacs-22+ (>= emacs-major-version 22))
(defvar I-am-emacs-23+ (>= emacs-major-version 23))
(defvar I-am-emacs-24+ (>= emacs-major-version 24))

(defvar I-am-gnuemacs (string-match "GNU Emacs" (emacs-version)))
(defvar I-am-xemacs (string-match "XEmacs" (emacs-version)))

;; Lets define which machine I'm on, therefor if I am at work
;; (this of course falls down when logging on remotely via tramp)

(defvar I-am-at-work (string-match "zen" (system-name)))
(defvar I-am-at-home (string-match "danny" (system-name)))
(defvar I-am-on-netbook (string-match "trent" (system-name)))
(defvar I-am-on-pixel (and (string-match "localhost" (system-name))
                           (which-lookup "host-x11")))
(defvar I-am-on-server (string-match "socrates" (system-name)))

;; I can probably disable a bunch of stuff for test machines
(defun my-primary-machine-p ()
  "Return t if I'm on a primary machine."
  (or I-am-at-home I-am-at-work I-am-on-pixel I-am-on-server))

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

;; Environment variables the shell may not have set
(when I-am-at-work
  (setenv "DEBEMAIL" "alex.bennee@linaro.org")
  (setenv "DEBFULLNAME" "Alex Bennée"))

(provide 'my-vars)
;;; my-vars.el ends here
