;;
;; My python mode mods
;;

; There are two python modes - this refers to the python supplied one
(require 'python-mode) ; hmm, broken

(if (maybe-load-library "ipython")
    (setq py-python-command-args '( "-colors" "Linux")))

(message "Done loading my-python-mode")
(provide 'my-python-mode)
