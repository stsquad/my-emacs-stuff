;;
;; My utility functions
;;

(eval-when-compile (require 'cl))

;; String munging functions
;
; Extract the first group in a regex

(defun extract-string(regex string)
  "Extract a string in a regex (the bit in ()'s)"
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

; Am I on the pixel?
(defvar I-am-on-pixel (and (string-match "localhost" (system-name))
                           (which-lookup "host-x11")))

; uses common lisp
(defun find-valid-file (list-of-files)
  "Go though a list of files and return the first one that is present"
  (loop for path in list-of-files
        until (file-exists-p path)
        finally return path))

; the 'elisp' way
(defun find-valid-file-elisp-way (list-of-files)
  "Go though a list of files and return the first one that is present"
  (let (r '())
    (mapc #'(lambda (f)
              (if (file-exists-p f) (add-to-list 'r f)))
          list-of-files)
    (car r)))

; using 'cl-macs
(defun find-valid-file-dolist-way (list-of-files)
  "Go though a list of files and return the first one that is present"
  (dolist (f list-of-files)
    (if (file-exists-p f)
        (return f))))


(provide 'my-utils)
