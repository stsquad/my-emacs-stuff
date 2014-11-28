;;; my-utils.el --- a bunch of simple utility functions
;;
;;; Commentary:
;;
;; Some of this stuff is no doubt replaced by better libraries in ELPA/MELPA.
;;
;;; Code:

(eval-when-compile (require 'cl))

;; String munging functions
;
; Extract the first group in a regex

(defun extract-string(regex string)
  "Extract a string in a regex (the bit in ()'s)."
  (interactive)
  (let ((s string)) (if (string-match regex s) (match-string 1 s) s)))

; And building on that
(defun extract-value-from-pair(key string)
  "Extract the value from AAAA=value pairs"
  (let ((regex (concat key "=\\(.*\\)$")))
    (extract-string regex string)))

; examples:
;  (extract-string "name is \\(\.*\\) " "my name is Alex ok") = Alex
;  (extract-value-from-pair "AAA" "AAA=xxx") = xxx

; chomp
(defun chomp(string)
  "Perform a perl-like chomp"
  (let ((s string)) (if (string-match "\\(.*\\)\n" s) (match-string 1 s) s)))

;; which-lookup
;
; Like the shell command of the same name except it trims to 'nil if
; it can't find anything

(defun which-lookup(name-or-list)
  "Perform a `which` like file look-up, returning the first hit or
'nil if no match found"
  (loop for x in (if (listp name-or-list) name-or-list (list name-or-list))
        do (let ((path (chomp (shell-command-to-string (concat "which " x)))))
             (if (and (file-exists-p path) (> (length path) 0))
                 (return path)))))

; uses common lisp
(defun find-valid-file (list-of-files)
  "Go though LIST-OF-FILES and return the first one that is present."
  (loop for path in list-of-files
        until (file-exists-p path)
        finally return path))

; the 'elisp' way
(defun find-valid-file-elisp-way (list-of-files)
  "Go though LIST-OF-FILES and return the first one that is present."
  (let (r '())
    (mapc #'(lambda (f)
              (if (file-exists-p f) (add-to-list 'r f)))
          list-of-files)
    (car r)))

; using 'cl-macs
(defun find-valid-file-dolist-way (list-of-files)
  "Go though LIST-OF-FILES and return the first one that is present."
  (dolist (f list-of-files)
    (if (file-exists-p f)
        (return f))))

; via: http://stackoverflow.com/questions/3815467/stripping-duplicate-elements-in-a-list-of-strings-in-elisp
(defun strip-duplicate-strings (list)
  "Remove any duplicate strings in LIST."
  (let ((new-list nil))
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

;; Useful for making numbers readable for debugging
;; or you could just use num3-mode
(require 'thingatpt)
(defun my-split-hexnum ()
  "If `thing-at-point' is a hex string split it into easily readable chunks."
  (interactive)
  (when (thing-at-point-looking-at "[[:xdigit:]]\\{4,\\}")
    (save-excursion
      (kill-region (match-beginning 0) (match-end 0))
      (let ((hex-string (current-kill 0))
            (word))
        (while (> (length hex-string) 4)
          (setq word (substring hex-string -4)
                hex-string (substring hex-string 0 -4))
          (save-excursion
            (insert ":" word)))
        (insert hex-string)))))


;; Fix up the frame so we don't send pinentry to the wrong place
(defun my-fixup-gpg-agent (frame)
  "Tweak DISPLAY and GPG_TTY environment variables as appropriate to `FRAME'."
  (when (fboundp 'keychain-refresh-environment)
    (keychain-refresh-environment))
  (if (display-graphic-p frame)
      (setenv "DISPLAY" (terminal-name frame))
    (setenv "GPG_TTY" (terminal-name frame))
    (setenv "DISPLAY" nil)))

(add-hook 'after-make-frame-functions 'my-fixup-gpg-agent)
(add-hook 'focus-in-hook #'(lambda() ((my-fixup-gpg-agent (selected-frame)))))

(defun my-switch-browser (frame)
  "Tweak default browser depending on frame visibility"
  (setq browse-url-browser-function
        (cond
         (I-am-on-pixel 'eww-browse-url)
         ((not (display-graphic-p frame)) 'eww-browse-url)
          (t 'browse-url-xdg-open))))

(add-hook 'after-make-frame-functions 'my-switch-browser)
(add-hook 'focus-in-hook #'(lambda() (my-switch-browser (selected-frame))))

;; Simple caching
(defvar my-cached-passwords
  nil
  "Cache of passwords. Stored in plain text so you only want to cache
  them if of low value.")

(defun my-pass-password (pass-name &optional cache)
  "Return the password for the `PASS-NAME'."
  (let ((cached-pass (assoc-default pass-name my-cached-passwords)))
    (if cached-pass
        cached-pass
      (when (selected-frame)
        (my-fixup-gpg-agent (selected-frame))
        (let ((new-pass (chomp
                         (shell-command-to-string
                          (format "pass %s" pass-name)))))
          (when (and new-pass cache)
            (add-to-list 'my-cached-passwords (cons pass-name new-pass)))
          new-pass)))))

;;
;; Bit extraction utils
;;
(defun extract-bits (value pos length)
  "Extract from `VALUE' at `POS' `LENGTH' bits."
  (let ((rsh-amount (- 0 pos))
        (mask (- (lsh 1 length) 1)))
    (logand mask (lsh value rsh-amount))))

;;
;; Set up a pair-programming copy of the current buffer in Evil mode
;;
(defun my-make-evil-twin ()
  "Create an indirect buffer in evil mode."
  (interactive)
  (when (require 'evil nil t)
    (clone-indirect-buffer (concat (buffer-name) "<vi>") t)
    (evil-local-mode)
    (delete-other-windows)
    (message "Evil twin created")))

;; maybe-load-library (DEFUNCT)
;
; A little less than using (require 'lib) - but require has optional args

(defun maybe-load-library (libname)
  "Try and load library LIBNAME if it is in the path."
  (when (locate-library libname)
    (load-library libname)))

;; insert-sequence-key
;
; Handy little key sequence utility so I don't have to guess at the
; correct kbd incantation

(defun insert-sequence-key (key)
  "Insert a string formating KEY suitable for use in fcns like `global-set-key'."
  (interactive "kInsert key chord: ")
  (insert (format "(kbd \"%s\")" (key-description key))))

(global-set-key (kbd "<C-f8>") 'insert-sequence-key)

;; Choose a directory
(defun my-choose-directory (&optional prompt)
  "Return a history aware directory. `PROMPT' is optional."
  (interactive)
  (let* ((query (or prompt "Target directory: "))
         (path
          (ido-read-directory-name query
                                   (car ido-work-directory-list)
                                   "~/" t)))
    (setf ido-work-directory-list
          (cons path (delete path ido-work-directory-list)))
  path))

(provide 'my-utils)
;;; my-utils.el ends here
