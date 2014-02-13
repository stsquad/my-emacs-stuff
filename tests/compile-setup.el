;;
;; Set up for byte-compiling
;;

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/my-elisp")
(setq byte-compile-error-on-warn t)
(package-initialize)
