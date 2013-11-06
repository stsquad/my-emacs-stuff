;; Development Hooks
;;
;; All other development modes should be triggered from here.
;;

(cond
 ((maybe-load-library "eproject") (load-library "my-eproject"))
 ((maybe-load-library "projectile") (load-library "my-projectile"))
 (t (load-library "my-project")))

;; Compilation mode tweaks
(when I-am-emacs-23+
    (setq compilation-auto-jump-to-first-error 'nil))

(setq compilation-scroll-output t
      compilation-window-height 10)

; lets not overtax the regex matcher on our huge compilation buffers
(when I-am-at-work
  (setq compilation-error-regexp-alist '(gcc-include gnu)))

;; currently errors in CI, must figure out why
;; (define-key compilation-mode-map (kbd "n") 'compilation-next-error)
;; (define-key compilation-mode-map (kbd "p") 'compilation-previous-error)

; Global keybindings for compiling
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

;; If we have the tracking library add compilation buffer when complete
(when (require 'tracking nil 'noerror)
  (defun my-hide-compilation-buffer (proc)
    "Hide the compile buffer"
    (delete-window (get-buffer-window "*compilation*")))

  (defun my-report-compilation-finished (buf exit-string)
    "Report the compilation buffer to tracker"
    (tracking-add-buffer buf))

  (add-hook 'compilation-start-hook 'my-hide-compilation-buffer)
  (add-hook 'compilation-finish-functions 'my-report-compilation-finished))

;; Makefiles
;
; The auto-mode-alist isn't quite set-up to handle Makefile.something
(setq auto-mode-alist
      (append (list (cons "Makefile\.*" 'makefile-gmake-mode))
	      auto-mode-alist))

;; PHP
;

(defun my-wordpress-hook ()
  "Hook function for editing Wordpress code"
  (interactive)
  (setq tab-width 4
	c-basic-offset 4
	indent-tabs-mode 't))

(defvar my-php-hooks-alist
  (mapc
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   '(
     (".*/wp-content.*php$" . my-wordpress-hook )
     (".*/wordpress.*php$" . my-wordpress-hook )))
  "A list of reg-ex to php-mode hooks")

; You can add to the alist with something like:
; (setq my-php-hooks-alist (cons '(".*/wp-content.*php$" . 'my-wordpress-hook ) my-php-hooks-alist))
; (assoc-default "/path/to/wp-content/test.php" my-php-hooks-alist 'string-match) => my-wordpress-hook

;;
;; Web stuff, use web-mode
;;

;(when (featurep web-mode)

(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))

(defun my-php-mode-hook()
  "Run hooks for PHP mode"
  (interactive)
  (if (buffer-file-name)
      (let ((f (assoc-default 
		(buffer-file-name) my-php-hooks-alist
		'string-match)))
	(message (format "my-php-mode-hook: got %s" f))
	(if (functionp f)
	    (funcall f)))))

(add-hook 'php-mode-hook 'my-php-mode-hook)

;;
;; Find methods
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

(when (require 'ack-and-a-half nil 'noerror)
  (defun my-ack-and-a-half-wrapper (root search)
    (ack-and-a-half search 'nil root))
  (setq my-project-find-fallback-func 'my-ack-and-a-half-wrapper))

