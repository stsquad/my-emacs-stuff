;;
;; My python mode mods
;;
; There are two python modes, one supplied with Emacs (python.el) and
; a Python community one called python-mode.el. elpy works with the
; Emacs version. Annoyingly both these modes clash so hi-jinks can
; ensue when your not sure what you are running.

(when (require 'elpy 'nil 't)
  (setq elpy-rpc-backend "jedi"
	elpy-rpc-project-specific 't)
  (elpy-enable)
  (elpy-clean-modeline))

;  (define-key elpy-mode-map (kbd "C-c C-f") 'elpy-eldoc-documentation))

(message "Done loading my-python-mode")
