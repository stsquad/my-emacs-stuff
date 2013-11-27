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
(require 'cc-mode)
(require 'cc-styles)
(require 'cperl-mode nil t)

(defconst my-c-style
  '((indent-tabs-mode . nil)
    (c-tab-always-indent . nil)
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
			(block-open . 0))))
  "Alex's C style")

; We don't set this as the default style as it gets in the way when
; editing Random C code.

(c-add-style "my-c-style" my-c-style)

; GTKG
(defconst gtkg-style
  '(
    "my-c-style"
    (c-basic-offset . 4)
    (c-tab-always-indent . t))
  "Style for GTK Gnutella Development")

(c-add-style "gtkg-style" gtkg-style)

; Rockbox
(defconst rockbox-c-style
  '(
    "my-c-style" ; derive from my-c-style
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open before after)))
    (c-offsets-alist . ((topmost-intro        . 0)
			(topmost-intro-cont   . 0)
			(substatement         . +)
			(substatement-open    . 0)
			(statement-case-intro . +)
			(statement-case-open  . 0)
			(case-label           . +)
			))
    )
  "Rockbox C Programming Style")

(c-add-style "rockbox-c-style" rockbox-c-style)

; GIT
(defconst git-c-style
  '(
    "my-c-style"
    (c-basic-offset . 8)
    (indent-tabs-mode . t))
  "GIT C programming style")

(c-add-style "git-c-style" git-c-style)

; EasyTag
(defconst easytag-c-style
  '(
    "my-c-style"
    (indent-tabs-mode . nil)
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0))
  "EasyTag Programming Style")

(c-add-style "easytag-c-style" easytag-c-style)

; Qemu C Style
(defconst qemu-c-style
  '(
    "my-c-style"
    (indent-tabs-mode . nil)
    (c-tab-always-indent . nil)
    (c-comment-only-line-offset 0)
    (c-basic-offset . 4)
    (c-offsets-alist . ((case-label . 0)))))

(c-add-style "qemu-c-style" qemu-c-style)

;; my-c-style-guesser
;
; Go through the list of patterns and see if we know what style
; we want to run. Otherwise we shall guess on the state of the file.
;

(defvar my-c-styles-alist
  (mapc
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   '(
     (".*/linux.*/.*\\.[ch]$" . "linux")  
     (".*/.*kernel.*/.*\\.[ch]$" . "linux")
     (".*/.*linux/.*\\.[ch]$" . "linux")
     (".*binutils.*\\.[ch]$"  . "gnu")
     (".*gtk-gnutella.*"      . "gtkg-style")
     (".*rockbox.*\\.[ch]$"   . "rockbox-c-style")
     (".*mysrc.*\\.[ch]$"     . "my-c-style")
     (".*easytag.*/src/.*\\.[ch]$". "easytag-c-style")))
  "A list of reg-ex to styles for my-c-style-guesser")

; You can add to the alist with something like:
; (setq my-c-styles-alist (cons '(".*mysrc.*$" . my-c-style) my-c-styles-alist))
; (setq my-c-styles-alist (cons '(".*easytag.*/src/.*\\.[ch]$". "easytag-c-style") my-c-styles-alist))

(defun my-c-style-guesser(filename)
  "Guess the C style we should use based on the path of the buffer"
  (message (concat "my-c-style-guesser " filename))
  (assoc-default filename my-c-styles-alist 'string-match))

; examples
; (my-c-style-guesser "/home/alex/src/kernel/linux-2.6/drivers/ide/ide-cd.c")
; (my-c-style-guesser "/home/alex/mysrc/mysrc.old/c/binmerge/binmerge.c")
; (my-c-style-guesser "/home/alex/src/gtk-gnutella/gtk2-gnutella/src/core/bitzi.c")
; (my-c-style-guesser "/home/alex/src/cvsps.git/cvsps.c")
; (my-c-style-guesser "nms-manager-apps/vsalarmd/snmp_interface.c")
; (my-c-style-guesser "/export/csrc/work.git/e1mon/ifTable.c")
; (my-c-style-guesser "/export/src/parsecvs.git/tree.c")
; (my-c-style-guesser "/eng/ajb/ft-kernel.git/net/atm/common.c")
; (my-c-style-guesser "/export/csrc/ppc-linux/common.c")
; (my-c-style-guesser "/export/csrc/intel-linux/common.c")
; (my-c-style-guesser "/home/alex/src/rockbox/rockbox.git-svn/apps/playlist.c")
; (my-c-style-guesser "/home/alex/src/easytag.git/src/et_core.c")

;;
;; my-c-mode-hook is called every time
;; I enter c-mode where I can enforce my will
;;
(defun my-c-mode-hook()
  "My c-mode hook"
  (interactive)
  (message "In my-c-mode-hook")
  (turn-on-auto-fill)

  ; Set the c-style if we can. I think mmm-mode gets in the way of
  ; buffer-file-name for setting sub-modes, so check we have one first
  (if (eval buffer-file-name)
      (progn
	(message (format "looking for style for buffer %s" (buffer-file-name)))
  
	(let ((style (my-c-style-guesser (buffer-file-name))))
	  (message (format "Found style:%s" style))
	  (if style
	      (c-set-style style)
					; fallback
	    (message "Falling back to defaults")
	    (c-set-style "my-c-style")))))

  (if I-am-emacs-21+
      (cwarn-mode)))

(add-hook 'c-mode-hook 'my-c-mode-hook)

(message "Done with cc-mode customisation")

;;
;; End of c-mode customisations
;;

;; load the etags library and bind C-f to something worth using :-)
(if (require 'etags-select nil t)
    (define-key c-mode-map (kbd "C-c f") 'etags-select-find-tag)
  (define-key c-mode-map (kbd "C-c f") 'find-tag))

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
	     (setq cperl-hairy                                 nil
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


(message "Finished my-c-mode.el customisation")

(provide 'my-c-mode)
