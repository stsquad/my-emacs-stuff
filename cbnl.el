; -*- emacs-lisp -*-
;
; Cambridge Broadband Emacs Setup
;
; A bunch of handy customisations for hacking around with CBNL
; code.
;
; This assumes certain functions exist from my dotemacs

(message "Doing local CNBL Customisations")

(require 'my-c-mode)

;; Sanity

(if (or (not (functionp 'chomp))
	(not (functionp 'extract-string)))
    (error "Need some string munging functions defined"))

;; Sanity
;
; I only expect to start in a working directory of checked out src
; tree. So there should be a `pwd`/build-system. This assumption is
; slightly broken for eproject

(defun project-is-cbnl-project (path)
  "Return true if the current project path is a CBNL build tree"
  (file-exists-p (concat path "/build-system")))

;; Project Root variables
;
; These are defined in my .emacs and are re-implemented here for
; portability reasons.
;
; The current project root, used to build stuff later
(if (and (not (bound-and-true-p current-project-root))
	 (not (bound-and-true-p eproject-root)))
    (require 'cbnl-project))

(message "Keyboard Hacks")

;;
; On BOB under screen I get stange backspace behaviour
(if (string-match "bob" (system-name))
    (normal-erase-is-backspace-mode 1))

;;
;
; We have a number of c styles, my-c-mode.el defines a guesser
; variable which we can use for this stuff.

(message "Additional coding styles")

; Need cc-style
(require 'cc-styles)


;
; Define the coding style for nms-manager-apps
;
(defconst cbnl-nms-style
  '(
    ; Display the cc-style syntax point, useful for debugging
    (c-echo-syntactic-information-p . t)

    ; No tabs, 2 space indent level
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)

    ; only indent if point left of line, otherwise insert whitespace
    (c-tab-always-indent . nil)
    
    (c-comment-only-line-offset 0)
    (c-electric-pound-behavior . (alignleft))
    (c-hanging-comment-ender-p . nil)
    (c-comment-continuation-stars . "* ")
    (c-recognize-knr-p . nil)
    (c-cleanup-list . (empty-defun-braces
		       defun-close-semi
		       list-close-comma
		       scope-operator))
    ; "To specify which kinds of braces you want auto-newlines put around"
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-list-close)
			       (block-close . c-snug-do-while)
			       (substatement-open before after)))

    ; 
    (c-hanging-colons-alist . ((member-init-intro after)
			       (access-label after)
			       (inher-intro after)
			       (case-label after)
			       (label after)))

    ; Define the offsets for various states
    (c-offsets-alist . ((arglist-close . c-lineup-arglist)
			(arglist-cont-nonempty . c-lineup-arglist)
			(substatement-open . 0)
			(statement-cont . ++)
			(case-label . +)
			; Block open/close braces should be on the same line
			; as the open/close statement, so de-indent
			(defun-block-intro . +)
			(block-open . 0)
			(block-close . 0))))
  "CBNL NMS Apps")

(c-add-style "cbnl-nms-style" cbnl-nms-style)

; Add it to my style guesser list if it exists, forcing the load of
; my-c-mode.el

(require 'my-c-mode)

(setq my-c-styles-alist (cons '(".*mibgroup/.*$" . "cbnl-nms-style") my-c-styles-alist))
(setq my-c-styles-alist (cons '(".*nms-manager-apps.*$" . "cbnl-nms-style") my-c-styles-alist))
(setq my-c-styles-alist (cons '(".*include/ems/.*$" . "cbnl-nms-style") my-c-styles-alist))
(setq my-c-styles-alist (cons '("/export/csrc/.*\.git/.*[ch]$" . "cbnl-nms-style") my-c-styles-alist))

; (my-c-style-guesser "nms-manager-apps/vsalarmd/snmp_interface.c")
; (my-c-style-guesser "/export/csrc/work.git/e1mon/ifTable.c")
; (my-c-style-guesser "/export/csrc/work.git/third-party/br2684/br2684ctrl.c")
; (my-c-style-guesser "/export/csrc/work.git/intel-linux/kernel/context.c")
; (my-c-style-guesser "/export/csrc/rc/rc-work.git/librc/output_enum.c")

;; TAGs support
;
; This used to just do a global TAGS file but there is a lot of
; duplicated code in the code base so really I need to do a TAGS file
; per app.
;

;(file-name-directory buffer-file-name) )
; (string-match "/lib" "nms-manager-apps/libgrok")
; (string-match "/lib" "/export/csrc/work.git/nms-manager-apps/vsbs/")

(defun generate-cbnl-tags (project-root app-dir extra-dirs tag-file))


(defun create-cbnl-tags (root current-filename)
  (interactive "DProject Root: 
GFilename: ")
  (message "create-cbnl-tags")
  (let* ((app-dir (file-name-directory current-filename))
	 (tag-file (concat  app-dir "TAGS")))
    (message "app-dir:%s tag-file:%s" app-dir tag-file)
    (unless (or	 (string-match "nms-manager-apps/lib" app-dir)
		 (string-match "/include" app-dir))
      (unless  (file-exists-p tag-file)
	(let* ((find-paths (concat "include/common/ include/ems/ nms-manager-apps/lib* librc/*" app-dir))
	       (command (concat "cd " root "; find "  find-paths  " -iname \"*.[ch]\" | etags -o " tag-file " -")))
	  (message (concat "Creating tags with:" command))
	  (shell-command command)))
      (message "Visiting: %s" tag-file)
      (visit-tags-table tag-file))))


; Only do this if current file is in project, and the project is a
; CBNL one. Handle both eproject and old style stuff

(defadvice find-tag (before c-tag-file activate)
  "Automatically create tags file for an app."
  (if (boundp 'eproject-root)
      (visit-tags-table (concat eproject-root "/TAGS"))))

(message "Done with cbnl customisations")
(provide 'cbnl)
