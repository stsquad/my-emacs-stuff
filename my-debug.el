;;; my-debug --- debug helper functions
;;
;;; Commentary:
;;
;; This is not included normally and has to be loaded manually
;;
;;; Code:

(defun my-check-for-subdir (filename newdir)
  "Check to see if newdir is bellow where filename is in the heirachy"
  (let ((dir (file-name-directory filename)))
    (> (length newdir) (length dir))))

;; (my-check-for-subdir "/this/is/a.file" "/this/is/in/a/subdir")
;; (my-check-for-subdir "/this/is/a.file" "/this/is/")

(defun my-default-dir-watcher (symbol newval operation where)
  (when (and where (eq operation 'set))
    (let ((name (buffer-name where))
          (path (buffer-file-name where)))
      (message "default-directory set to: %s in %s" newval name)
      (when (and path
                 (my-check-for-subdir (buffer-file-name) newval))
        (message "%s not in %s\n%s" newval path
                 (catch 'catcher
                   (throw 'catcher
                          (with-temp-buffer
                            (backtrace)
                            (buffer-string)))))))))

;; (add-variable-watcher 'default-directory 'my-default-dir-watcher)
;; (remove-variable-watcher 'default-directory 'my-default-dir-watcher)

(defun my-add-dir-watcher-for-this-buffer ()
  "Add a watcher to current-buffer for when default-directory changes
from it's current value."
  (interactive)
  (let ((orig-dir default-directory)
        (buf (current-buffer)))
    (add-variable-watcher
     'default-directory
     (lambda (symbol newval operation where)
       (when (and (not (eq operation 'let))
                  (eq buf (current-buffer))
                  (not (string-equal orig-dir newval)))
         (message "%s default-directory to %s (from %s) in %s"
                  operation newval orig-dir (current-buffer))
         (backtrace))))))

(defun my-reset-dir-watchers ()
  "Remove all default-directory watchers."
  (interactive)
  (--each
      (get-variable-watchers 'default-directory)
      (remove-variable-watcher 'default-directory it)))


;; Total buffer size
(defun my-debug-total-buffer-size ()
  (let ((stext 0))
    (dolist (b (buffer-list))
      (setq stext (+ stext (buffer-size))))
    stext))



(provide 'my-debug)
;;; my-debug.el ends here
