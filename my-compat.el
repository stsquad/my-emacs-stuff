;;; my-compat --- compatibility hacks
;;
;;; Commentary:
;;
;; While I use emacs24 across the board there is some stuff that was
;; introduced a little later.
;;
;;; Code:

(when (not (fboundp 'with-eval-after-load))
    (defmacro with-eval-after-load (file &rest body)
      "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
      (declare (indent 1) (debug t))
      `(eval-after-load ,file (lambda () ,@body))))

(provide 'my-compat)
;;; my-compat.el ends here
