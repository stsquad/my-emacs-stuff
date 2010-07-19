;; Development Hooks
;;
;; All other development modes should be triggered from here.
;;


(if (maybe-load-library "eproject")
    (load-library "my-eproject")
  (load-library "my-project"))

;; Compilation mode tweaks
(if I-am-emacs-23+
    (setq compilation-auto-jump-to-first-error t))
(setq compilation-scroll-output t
      compilation-window-height 10)

(add-hook 'compilation-mode-hook '(lambda()
				    (define-key compilation-mode-map (kbd "n") 'compilation-next-error)
				    (define-key compilation-mode-map (kbd "p") 'compilation-previous-error)))

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
