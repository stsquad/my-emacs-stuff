;; my-find-binary
;
; This adds the command find-binary-file which will attempt to load a
; binary file into emacs via a helper.
;

(defun objdump-file (filename)
  "Objdump a file"
  (interactive (find-file-read-args "Find file: " nil))
  (if (file-exists-p filename)
      (progn
        (let ((bufnam (concat "*Objdump of " filename "*")))
          (shell-command (concat "objdump -D " filename) bufnam)
          (switch-to-buffer bufnam)
          (delete-other-windows)))
    (message "File doesn't exist")))

; (objdump-file "/bin/ls")
; (objdump-file "/usr/export/alexjb-desktop/dynamite/cr6436.git/non-products/sierra-nevada/build/BLACKHAWK/dynamite")

(defvar binary-helper-alist
  (mapc
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   `(
     ("ELF" . objdump-file)))
  "Alist of filetype patterns (as returned by 'file -b' and the helper
  to execute to display it")


;(shell-command-to-string "file -b /bin/ls")

;(shell-command "objdump -D /bin/ls" "*Objdump of /bin/ls*")


(defun find-binary-file (filename)
  "Load a file through a binary helper (like objdump)"
  (interactive "f")
  (let* ((type (shell-command-to-string
                (concat "file -b " filename)))
         (helper (assoc-default type binary-helper-alist 'string-match)))
    (when helper
      (funcall helper filename))))
          
; (assoc-default "ELF" binary-helper-alist 'string-match)

;(find-binary-file "/bin/ls")
;(find-binary-file "~/src/rockbox/ipod.build/apps/codecs/aac.elf")

