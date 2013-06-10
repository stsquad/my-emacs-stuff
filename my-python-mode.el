;;
;; My python mode mods
;;

; There are two python modes - this refers to the python supplied one
;(require 'python-mode) ; hmm, broken

;(when (maybe-load-library "ipython")
;    (setq py-python-command-args '( "-colors" "Linux")))

(when (require 'elpy 'nil 't)
  (elpy-enable)
  (elpy-clean-modeline))

(message "Done loading my-python-mode")
