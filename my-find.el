;;
;; Find methods
;;
;
; I want two find methods:
;   f5 - from the "project" root
;   f6 - from where I am (or somewhere else)
;

(defvar my-find-history
  '()
  "History of searches I've done")

(defun my-find-search-string()
  "Return a historical based search string via ido"
  (ido-completing-read "Search string:"
		       my-find-history ; sources
		       'nil ; predicate
		       'nil ; require-match
		       (thing-at-point 'symbol) ; initial input
		       'my-find-history)) ;history

(defun my-find-paramters()
  "Return a root directory and search string combo"
  (list
   (read-directory-name "Root dir:")
   (my-find-search-string)))
  
(defun my-grep-find (&optional root search)
  (interactive (my-find-paramters))
  (grep-find (concat
	      "find "
	      root " -print0 |  xargs -0 -e grep -n -e " search)))

(defvar my-project-find-fallback-func
  'my-grep-find
  "The function I call to do the actual search")

(defun my-project-find (&optional search)
  "Search within the project"
  (interactive (list (my-find-search-string)))
  (funcall my-project-find-fallback-func default-directory search))

(global-set-key (kbd "<f5>") 'my-project-find)
(global-set-key (kbd "<f6>") '(lambda (root search)
				(interactive (my-find-paramters))
				(funcall my-project-find-fallback-func
					 root search)))

;
; Now we have some basic defaults we might want to enhance if we have
; certain packages available to us.
;

(when (require 'ack-and-a-half nil t)
  (defun my-ack-and-a-half-wrapper (root search)
    (ack-and-a-half search 'nil root))
  (setq my-project-find-fallback-func 'my-ack-and-a-half-wrapper))

(provide 'my-find)
