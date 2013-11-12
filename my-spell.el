;;
;; Configuration for the spelling
;;

(require 'my-utils)
(require 'ispell)
(require 'flyspell)


;; ispell
;
; There should be an easier way to set the default
; however I'm currently setting each time a file
; is opened using the find-file-hooks
;
; Also if I'm on a odd machine I'll skip it as they have the ispell.el
; library but not the actual ispell program
;
; And I want to use aspell by default as more people have that (and it
; should be better right?)

(let ((spell-path (which-lookup '("aspell" "ispell")))) ; aspell is preferred
  (when spell-path
    (setq ispell-program-name spell-path
          ispell-dictionary "british")
    
    (add-hook 'text-mode-hook 'turn-on-flyspell)
    (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
    (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
    (message "Skipping ispell - no programs")))

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))
