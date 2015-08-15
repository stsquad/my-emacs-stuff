;;
;; Find methods
;;
;
; I want two find methods:
;   f5 - from the "project" root
;   f6 - from where I am (or somewhere else)
;

(require 'use-package)

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

;;   (interactive (list
;;                 (eproject--do-completing-read
;;                  "Project name: " (eproject-project-names))))

(defun my-project-find (prefix &optional search)
  "Search within the project. If prefix is set prompt for a directory."
  (interactive (list (my-find-search-string)))
  (funcall my-project-find-fallback-func default-directory search))

(global-set-key (kbd "<f5>") 'my-project-find)

;;
;; <f6> for project search, last successful one wins
;;

(global-set-key (kbd "<f6>") '(lambda (root search)
                                (interactive (my-find-paramters))
                                (funcall my-project-find-fallback-func
                                         root search)))
(use-package ack-and-a-half
  :commands ack-and-a-half
  :init (global-set-key (kbd "<f6>") 'ack-and-a-half))

(use-package helm-ag
  :commands helm-ag
  :init (global-set-key (kbd "<f6>") 'helm-do-ag))

(use-package swiper
  :bind ("C-s" . swiper))

(provide 'my-find)
