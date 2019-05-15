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

(defun my-switch-browser (&optional frame)
  "Tweak default browser depending on `FRAME' visibility.
If frame is not set use (current-frame)."
  (setq browse-url-browser-function
        (cond
         ((not (display-graphic-p (or frame (selected-frame)))) 'eww-browse-url)
         (t 'browse-url-xdg-open))))

(add-hook 'after-make-frame-functions 'my-switch-browser)
(add-hook 'focus-in-hook #'(lambda() (my-switch-browser (selected-frame))))

;; Simple caching
(defvar my-cached-passwords
  nil
  "Cache of passwords.
Stored in plain text so you only want to cache them if of low value.")

(defun my-pass-password (pass-name &optional cache)
  "Return the password for the `PASS-NAME'.
Save password if `CACHE' is non nil."

  (let ((cached-pass (assoc-default pass-name my-cached-passwords)))
    (if (and cached-pass cache)
        cached-pass
      (when (selected-frame)
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
;; MACRO CANDIDATE
(defun my-choose-directory (&optional prompt)
  "Return a history aware directory.
`PROMPT' is optional."
  (interactive)
  (let* ((query (or prompt "Target directory: "))
         (path
          (ido-read-directory-name query
                                   (car ido-work-directory-list)
                                   "~/" t)))
    (setf ido-work-directory-list
          (cons path (delete path ido-work-directory-list)))
  path))


(defvar my-last-set-directory
  nil
  "Last value I set the directory to")

(defun my-set-default-directory (&optional dir)
  "Set new `default-directory' to DIR."
  (interactive)
  (let ((dir (or dir (read-directory-name "New Default Directory: "))))
    (setq default-directory dir
          my-last-set-directory dir)))

;; arm decoder
(defun my-decode-arm64-cpreg (input)
  (let ((op0 (lsh (logand #xc000 input) -14))
        (op1 (lsh (logand #x3800 input) -11))
        (crn (lsh (logand #x780 input) -7))
        (crm (lsh (logand #x78 input) -3))
        (op2 (lsh (logand #x7 input) 0)))
    (format "op0:%x op1:%x crn:%x crm:%x op2:%x"
            op0 op1 crn crm op2)))

(defun my-encode-arm64-cpreg (name op0 op1 crn crm op2)
  (format "%s:0x%x"
          name
          (logior (lsh op0 14)
                  (lsh op1 11)
                  (lsh crn 7)
                  (lsh crm 3)
                  (lsh op2 0))))

;; Adding paths to environment
(defun my-add-world-to-env (root)
  "Add `root'/bin and `root'/lib to run env if they exist."
  (interactive
   (list (ido-read-directory-name "Add world root:")))
  (let ((bin-path (format "%s/bin" root))
        (lib-path (format "%s/lib" root)))
    (when (file-exists-p bin-path)
      (let ((new-path (getenv "PATH")))
        (setenv "PATH" (format "%s:%s" bin-path new-path))))
    (when (file-exists-p lib-path)
      (let ((new-lib-path (getenv "LD_LIBRARY_PATH")))
        (setenv "LD_LIBRARY_PATH" (format "%s:%s" lib-path new-lib-path))))))


(provide 'my-utils)
;;; my-utils.el ends here
