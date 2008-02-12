;; My C Mode Customisations
;
; Having all the C Mode stuff loaded up every time is very time
; consuming so I keep it all in here. This also means the compile mode
; and other such tweaks can live in this file.


; I'll define all my basic prefs in my-c-style from which the others
; can inherit depending on what I'm working on.
;

(message "Starting my-c-mode.el customisation")

; Need cc-style
(require 'cc-styles)

(defconst my-c-style
  '((c-tab-always-indent . nil)
    (c-indent-level 3)
    (c-comment-only-line-offset 0)
    (c-basic-offset . 4)
;    (c-echo-syntactic-information-p . t) - this is only useful for debugging
    (c-electric-pound-behavior . (alignleft))
    (c-hanging-comment-ender-p . nil)
    (c-comment-continuation-stars . "* ")
    (c-recognize-knr-p . nil)
    (c-cleanup-list . (empty-defun-braces
		       defun-close-semi
		       list-close-comma
		       scope-operator))
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-list-close)
			       (block-close . c-snug-do-while)
			       (substatement-open before after)))
    (c-hanging-colons-alist . ((member-init-intro after)
			       (access-label after)
			       (inher-intro after)
			       (case-label after)
			       (label after)))
    (c-offsets-alist . ((arglist-close . c-lineup-arglist)
			(arglist-cont-nonempty . c-lineup-arglist)
			(substatement-open . 0)
			(statement-cont . ++)
;;			(arglist-cont-nonempty . ++)
;		     (ansi-funcdecl-cont . 0)
			(case-label . +)
			(block-open . 0)))
    (indent-tabs-mode . nil)
    )
  "Alex's C style")

(c-add-style "my-c-style" my-c-style)
(setq c-default-style "my-c-style")

;;
;; my-c-mode-hook is called every time
;; I enter c-mode where I can enforce my will
;;
(defun my-c-mode-hook()
  "My c-mode hook"
  (interactive)
  (message "In my-c-mode-hook")
  (turn-on-auto-fill)

  ; If I'm working with git-grep this won't be set
  (if (bound-and-true-p find-c-files)
      (set-my-find-files find-c-files))

  ; ensure tab width matches c-basic-offset
  (setq tab-width c-basic-offset)
  (if I-am-emacs-21+
      (cwarn-mode)))
  
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;;
;; Linux Kernel C-mode
;;
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (message "Setting Linux C Style")
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

;; if its got linux in the path then its a kernel file
(setq auto-mode-alist (cons '(".*/linux.*/.*\\.[ch]$" . linux-c-mode)
                       auto-mode-alist))

;;
;; GNU Code
;;
(defun gnu-c-mode ()
  "C mode for GNU stuff"
  (interactive)
  (c-mode)
  (message "Setting Linux C Style")
  (c-set-style "gnu")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 2))
  
(setq auto-mode-alist (cons '(".*binutils.*\\.[ch]$" . gnu-c-mode)
                       auto-mode-alist))

(message "Done with cc-mode customisation")

;;
;; End of c-mode customisations
;;

;; load the ctags library and bind C-f to something worth using :-)
(if (locate-library "etags")
    (progn
      (autoload 'find-tag "etags" "Emacs Tags library" t)
      (global-unset-key "\C-f")
      (global-set-key "\C-f" 'find-tag)))


;; CPerl-mode
(setq interpreter-mode-alist (append (list (cons "perl" 'cperl-mode)
					   (cons "perl5" 'cperl-mode)
					   (cons "miniperl" 'cperl-mode))
				     interpreter-mode-alist))

(setq auto-mode-alist (append (list 
			       (cons "\\.\\([pP][Llm]\\|al\\)\\'" 'cperl-mode)
			       (cons "\\.plx\\'" 'cperl-mode)
			       (cons "\\.cgi\\'" 'cperl-mode)
			       (cons "\\.pod\\'" 'cperl-mode)
			       (cons ".*/perl/.*" 'cperl-mode))
			      auto-mode-alist))

(setq cperl-info-on-command-no-prompt nil
      cperl-clobber-lisp-bindings     nil
      cperl-electric-parens           nil
      cperl-electric-keywords         nil)
					 ; cperl-mode tries to load
					 ; abbrev before running the hook

(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (cperl-set-style "BSD")
	     (setq cperl-hairy                                 t
	           cperl-merge-trailing-else                   nil
	           cperl-tab-always-indent                     nil
	           cperl-auto-newline                          nil
	           cperl-electric-lbrace-space                 nil
	           cperl-electric-linefeed                     t
	           cperl-electric-parens                       nil
	           cperl-electric-keywords                     nil
	           cperl-lazy-help-time                        1
	           cperl-extra-newline-before-brace            t
	           cperl-extra-newline-before-brace-multiline  t
	           cperl-max-help-size                         50)
	     (turn-on-auto-fill)
	     (imenu-add-to-menubar "Imenu")
	     (if (not cperl-lazy-installed)	; Only toggle if it's
		 (cperl-toggle-autohelp))	; not already set
	     (if (locate-library "mode-compile")
		 (define-key cperl-mode-map "\C-cr" 'mode-compile))
	     (define-key cperl-mode-map "\C-cc" 'cperl-check-syntax)
	     (define-key cperl-mode-map "\C-j"  'cperl-linefeed)
	     (message "Ran cperl-mode hook")))


;; Compile Mode Stuff
;
; I want to be able to search straight from the compile mode window
;
; I've tried doing stuff with compile-/compile+ but never got it to work.

(if (locate-library "compile")
    (progn
      (autoload 'compile "compile")
      (autoload 'recompile "compile")
      
      ; ensure the default compile command is sensible
      (setq compile-command (format "cd %s && make -k" current-project-root))
      ; Some keybindsings for compile-command and recompile
      (global-set-key (kbd "C-c c") 'compile)
      (global-set-key (kbd "<f3>")  'compile)
      (global-set-key (kbd "C-c r") 'recompile)
      ; Tweak some variables
      (setq compilation-scroll-output t
      ; compilation-buffer-name-function nil
	    compile-auto-highlight    t
	    compilation-window-height 10)
      (message "Setup compile-mode"))
    (message "Unable to find compile libs..."))


(message "Finished my-c-mode.el customisation")
