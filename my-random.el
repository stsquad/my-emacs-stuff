;;; my-random --- Random snippets of elisp
;;
;; Copyright (C) 2014 Alex Bennée
;;
;; Author: Alex Bennée <alex@bennee.com>
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;;; Commentary:
;;
;; This is not currently loaded into Emacs on normal startup. However
;;it does provide a handy dumping ground for things.
;;
;;; Code:

;; Require prerequisites
(require 'cl)

;; Variables

;; Code

;; hmm doesn't work like it should
(defun my-periodical-process-pause ()
  "Periodically pause the current process."
  (interactive)
  (lexical-let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (lexical-let ((old-filter (process-filter proc)))
	(set-process-filter proc t)
	(run-with-idle-timer
	 1 nil
	 #'(lambda()
	     (set-process-filter proc old-filter)))))))


;; Lets try a macro
(require 'async)

(defmacro my-async-org-function (async-form)
  "Expands `ASYNC-FORM' as an asynchronus org-bable function.
If executed inside an org file will insert the results into the src
  blocks results.  Otherwise the result will be echoed to the Message
  buffer."

  ;; All the my-async-* values we need to make available to the
  ;; asynchronus call. They need to be pure strings as the async
  ;; process will have no information about the parents internal
  ;; structures.

  (let ((my-async-buffer (buffer-name))
        (my-async-org-name (nth 4 (org-babel-get-src-block-info)))
        (my-async-sexp async-form))

    `(async-start

      ;; The async code runs in the inferior process so will not have
      ;; any of the current emacs environment. The results will be
      ;; echoed back to the handler function when done.
      (lambda ()
        ,(async-inject-variables "my-async-")
        (list
         (cons 'buffer my-async-buffer)
         (cons 'name my-async-org-name)
         (cons 'result (eval my-async-sexp))))

      ;; This code runs in the current emacs process.
      (lambda (result)
        (let ((buf (cdr (assoc 'buffer result)))
              (name (cdr (assoc 'name result)))
              (res (cdr (assoc 'result result))))
          (if name
            (save-excursion
              (with-current-buffer buf
                (org-babel-goto-named-result name)
                (next-line)
                (goto-char (org-babel-result-end))
                (org-babel-insert-result (format "we got: %s" res))))
            (message "async-result: %s" res)))))))

(my-async-org-function (format "hello there:%s" "alex"))

(provide 'my-random)
;;; my-random.el ends here

