;;; my-c-mode.el ---  My C Mode Customisations
;;
;;; Commentary:
;;
;; Having all the C Mode stuff loaded up every time is very time
;; consuming so I keep it all in here.  This also means the compile mode
;; and other such tweaks can live in this file.
;;
;; I'll define all my basic prefs in my-c-style from which the others
;; can inherit depending on what I'm working on.
;;
;;; Code:

(require 'use-package)
(require 'my-vars)
  

(defconst my-c-style
  '((indent-tabs-mode . nil)
    (c-tab-always-indent . nil)
    (c-indent-level 3)
    (c-comment-only-line-offset 0)
    (c-basic-offset . 4)
;    (c-echo-syntactic-information-p . t) - this is only useful for debugging
    (c-electric-pound-behavior . (alignleft))
    (c-hanging-comment-ender-p . nil)
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



; GTKG
(defconst gtkg-style
  '(
    "my-c-style"
    (c-basic-offset . 4)
    (c-tab-always-indent . t))
  "Style for GTK Gnutella Development")



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



; GIT
(defconst git-c-style
  '(
    "my-c-style"
    (c-basic-offset . 8)
    (indent-tabs-mode . t))
  "GIT C programming style")



; EasyTag
(defconst easytag-c-style
  '(
    "my-c-style"
    (indent-tabs-mode . nil)
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0))
  "EasyTag Programming Style")



; Qemu C Style
(defconst qemu-c-style
  '(
    "my-c-style"       ; derived from style
    (tab-width . 8)
    (indent-tabs-mode . nil)
    (comment-style 'extra-line)
    (c-doc-comment-style . javadoc)
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((substatement-open before after)))
    (c-offsets-alist . ((case-label . 0)
                        (statement-block-intro . +)
                        (substatement-open . 0)
                        (label . 0)
                        (statement-cont . +)
                        (innamespace . 0)
                        (inline-open . 0)
                        ))
    (c-hanging-braces-alist .
                            ((brace-list-open)
                             (brace-list-intro)
                             (brace-list-entry)
                             (brace-list-close)
                             (brace-entry-open)
                             (block-close . c-snug-do-while)
                             ;; structs have hanging braces on open
                             (class-open . (after))
                             ;; ditto if statements
                             (substatement-open . (after))
                             ;; and no auto newline at the end
                             (class-close)
                             ))
    )
  "QEMU C Programming Style")



(defconst risu-style
  '("ellemtel"
    ;; fix indent in case statements
    (statement-case-intro . +)
    ;; and structure definitions
    (inclass . +)
    ;; Java methods
    (access-label . 0)
    ;; Java try..catch
    (statement-cont . 0)
    ;; and in arrays, enums and do-while loops
    (c-hanging-braces-alist .
                            ((brace-list-open)
                             (brace-list-intro)
                             (brace-list-entry)
                             (brace-list-close)
                             (block-close
                              . c-snug-do-while)))
    (comment-column . 70)
    )
    "RISU Style (PMM's C Programming Style)")



;; Linux style
;
; Who knew the defaults didn't ensure tabs were set correctly.
; From Documentation/CodingStyle Chapter 9

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defconst linux-tabs-style
  '("linux"
    (indent-tabs-mode . t)
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only))))


;;
;; cc-styles
;;
;; Add the style definitions on demand when we load it
(use-package cc-styles
  :commands (c-add-style c-set-style)
  :config
  (progn
    (c-add-style "my-c-style" my-c-style)
    (c-add-style "gtkg-style" gtkg-style)
    (c-add-style "rockbox-c-style" rockbox-c-style)
    (c-add-style "git-c-style" git-c-style)
    (c-add-style "easytag-c-style" easytag-c-style)
    (c-add-style "qemu-c-style" qemu-c-style)
    (c-add-style "risu-c-style" risu-style)
    (c-add-style "linux-tabs-style" linux-tabs-style)))


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
  (when buffer-file-name
    (message (format "looking for style for buffer %s" (buffer-file-name)))
    (let ((style (my-c-style-guesser (buffer-file-name))))
      (when style
        (message (format "my-c-mode-hook: found style %s" style))
        (c-set-style style))))

  (if I-am-emacs-21+
      (cwarn-mode)))

(use-package cc-mode
  :commands c-mode
  :config
  (progn
    (add-hook 'c-mode-hook 'my-c-mode-hook)
    (define-key c-mode-map (kbd "C-c f") 'find-tag)
    (use-package etags-select
      :commands etags-select-find-tag
      :init (define-key c-mode-map (kbd "C-c f") 'etags-select-find-tag))))

;; Enhance C/C++ Development

(defun my-irony-cdb-setup ()
  "Wrapper around `irony-cdb-autosetup-compile-options'.

This is simply to avoid trying to load when dealing with header files
  as they are generally not in compilation databases."
  (unless (s-suffix? ".h" (buffer-file-name))
    (irony-cdb-autosetup-compile-options)))

(defun sarcasm-irony-cdb-not-found (command &rest args)
  (when (eq command 'get-compile-options)
    (message "Irony: compile options not found!")
    nil))

(setq-default irony-cdb-compilation-databases '(irony-cdb-clang-complete
                                                irony-cdb-libclang
                                                sarcasm-irony-cdb-not-found))

(use-package irony
  :ensure t
  :config (progn
            (add-hook 'c-mode-hook 'irony-mode)
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'irony-mode-hook 'my-irony-cdb-setup)))

(when I-am-at-work
  (use-package irony-eldoc
    :ensure
    :config (add-hook 'irony-mode-hook #'irony-eldoc)))

(message "Done with cc-mode customisation")

;;
;; End of c-mode customisations
;;

(provide 'my-c-mode)
