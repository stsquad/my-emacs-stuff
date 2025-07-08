;;; my-vars.el --- Common variables used used by all config files
;;
;;; Commentary:
;;
;; These are just a bunch of simple variables I use throughout the
;; rest of the code for controlling optional features.
;;
;;; Code:

(require 'my-utils)

;; Lets define which machine I'm on, therefor if I am at work
;; (this of course falls down when logging on remotely via tramp)

(defvar I-am-at-work (string-match "draig" (system-name)))
(defvar I-am-at-home (string-match "danny" (system-name)))
(defvar I-am-on-server (string-match "socrates" (system-name)))
(defvar I-am-on-pixelbook (string-match "penguin" (system-name)))

;; I can probably disable a bunch of stuff for test machines
(defun my-primary-machine-p ()
  "Return t if I'm on a primary machine."
  (or I-am-at-home I-am-at-work I-am-on-server))

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
(defvar I-am-root (= (user-uid) 0))

;; tweaks some low level vars
(setq read-process-output-max (* 1024 1024))

;; Environment variables the shell may not have set
(when I-am-at-work
  (setenv "DEBEMAIL" "alex.bennee@linaro.org")
  (setenv "DEBFULLNAME" "Alex Bennée")
  (setenv "LSP_USE_PLISTS" "1"))

;; Shared regexps

;; DCO Tag snarfing
;;
;; This is used for grabbing Reviewed-by and other such tags from a
;; mailing list. This will fail on older Emacsen (before ~27.1) but
;; that is over 4 years old by now so I've dropped the manual
;; fallback.
;;

;; capture group for contents of <data>
(rx-define my-capture-brackets-rx
  (: (zero-or-one "<")
     (group (one-or-more (not (in ">" "/" blank cntrl))))))

(rx-define my-bare-dco-tag-rx
  (: (any "RSTA") (one-or-more (in alpha "-")) "-by: "    ;; tag
     (one-or-more (in alpha blank "-."))                  ;;name
     blank
     "<" (one-or-more (not (in ">"))) ">"))               ;; email

(rx-define my-dco-tag-rx
  (: bol (zero-or-more (in blank)) my-bare-dco-tag-rx))

(rx-define my-msgid-tag-rx
  (: "Message-" (or "I" "i") (or "d" "D")))

(rx-define my-based-on-tag-rx
  (: "Based-" (or "O" "o") "n"))

(rx-define my-msgid-rx
  (: my-msgid-tag-rx ": "
     "<"
     (one-or-more (not (in ">")))
     ">"))

(defvar my-bare-dco-tag-re
  (rx my-bare-dco-tag-rx)
  "Regexp to match plain DCO tag")

(defvar my-dco-tag-re
  (rx my-dco-tag-rx)
  "Regexp to match DCO style tag.")

(defvar my-msgid-re
  (rx my-msgid-rx)
  "Regexp to match Message-Id")

;; Capturing msgids
(defvar my-capture-msgid-re
  (rx (: (or my-based-on-tag-rx my-msgid-tag-rx "patchew.org/qemu")
         (or "/" ": ")
         my-capture-brackets-rx))
  "Regexp to extract Message-Id from git tags. Expects a lowered string.")


;; Common helper functions

(defun my-capture-review-tags ()
  "Return a list of DCO style tags for current buffer."
  (let ((tags))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward my-dco-tag-re (point-max) t)
        (add-to-list 'tags (match-string-no-properties 0))))
    tags))

(defun my-capture-msgids()
  "Returns a list of unique msg-id strings for current buffer."
  (let ((ids) (case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward my-capture-msgid-re nil t)
        (push (match-string-no-properties 1) ids)))
    (delete-dups ids)))


;; This is used for grabbing logins
(defvar my-ssh-login-re
  (rx (: (one-or-more alnum)
         "@"
         (one-or-more (one-or-more alnum)
                      (zero-or-one "."))))
  "Regexp to match host.")

(defun my-capture-login ()
  "Return a login string if one exists in the buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward my-ssh-login-re (point-max) t)
      (match-string-no-properties 0))))

(defvar my-debug-var nil
  "A general purpose buffer local debug variable, for debugging.")
(make-variable-buffer-local 'my-debug-var)
(put 'my-debug-var 'permanent-local t)

(defvar my-global-debug-var nil
  "A general purpose global debug variable, for debugging.")

(provide 'my-vars)
;;; my-vars.el ends here
