;;
;; transitive.el
;;
;; Alex Bennee
;;
;; This contains a bunch of handy things for hacking dynamite from
;; within emacs. I include it at the bottom of my main .emacs but it
;; should stand alone.
;;
;; There are some handy functions at the top which are conditionally
;; defined as I have then in my .emacs. If you already have something
;; named the same you may need to do something else
;;
;; I also base transitive-c-style on my own. This may need tweaking
;;

(message "Doing local Transitive Customisations")

;; Define Handy Functions
;
; I have these in my .emacs hence the conditionals but it should work
; out of the box.

(defun chomp(string)
  "Perform a perl-like chomp"
  (let ((s string)) (if (string-match "\\(.*\\)\n" s)
			(match-string 1 s) s)))

;; Variables
;
; Define a bunch of variables used in the rest of the code

; *CHANGE* this is a hardcoded hack for my laptop to avoid using
; certain forms if I happen to be detached from the network
(defvar I-am-on-laptop (string-match "ajb-laptop" (system-name)))

; This variable picks up if we are on a work machine
(defvar I-am-at-work (string-match "transitives" (system-name)))

; The current project root, used to build stuff later
(if (not (bound-and-true-p current-project-root))
    (progn

      (defvar current-project-root
	(concat (chomp (shell-command-to-string "pwd")))
	"Describes the current project root dir (top of src tree)")

      (defvar project-root-history 'current-project-root
	"A history of projects I have been to")

      ; this is redefined later to someting smarter
      (defun set-project-root ()
	"Set a primary project root"
	(interactive)
	(setq current-project-root
	      (read-file-name "Project Root:")))))

;; Project Variables
;
; These two variables are used to indicate information about the
; current project environment. They are used when building the compile
; command

; This is used as a history variable later, you can fill it with as
; many or few variables as you like.
(defvar transitive-projects
  '("non-products/sierra-nevada/build/THUNDERBOLT-64"
    "non-products/sierra-nevada/build/MATTERHORN"
    "non-products/sierra-nevada/build/CLOUDRIPPER"
    "non-products/sierra-nevada/build/BLACKHAWK"
    "products/rocky/build/X86-PPC"
    "products/drum/build/PPC-X86"
    "products/hamilton/build/PPC-X86"
    "products/redcloud/build/MIPS4-IA64"
    "products/shasta/build/ARM-X86")
  "A List of Dynamite Projects")

;; current-project
;
; *CHANGE* You can set a default here to your favorite.
(defvar current-project
  (concat "non-products/sierra-nevada/build/BLACKHAWK")
  "Describes the current project path (off root of src tree)")

;; current-build-flags
;
; *CHANGE* You can set your default build flags here.

(defvar current-build-flags
  (let ((bf (getenv "DEFAULT_DYNAMITE_BUILD_FLAGS")))
    (if (eval bf)
	(progn
	  (message "Overidding default build flags from env")
	  (eval bf))
      (concat "production-full BOB=1 LOUD=2")))
  "Describes the current default build flags")


; This looks to your environment to see if you already have a
; different project set up
(let ((bp (getenv "DEFAULT_DYNAMITE_BUILD_PATH"))
      (bf (getenv "DEFAULT_DYNAMITE_BUILD_FLAGS")))
  (if (eval bp)
      (progn
	(message "Overring default build from env")
	(setq current-project bp))))

(message "Defined project variables")


(defun extract-string(regex string)
  "Extract a string in a regex (the bit in ()'s)"
  (interactive)
  (let ((s string)) (if (string-match regex s) (match-string 1 s) s)))

; example:
;(extract-string "DEFAULT_DYNAMITE_BUILD_PATH=\\(.*\\)$" "DEFAULT_DYNAMITE_BUILD_PATH=non-products/A-A/build/ARMle-ARMle-Linux")

(defun extract-value-from-pair(key string)
  "Extract the value from AAAA=value pairs"
  (let ((regex (concat key "=\\(.*\\)$")))
    (extract-string regex string)))
  

; example (extract-value-from-pair "DEFAULT_DYNAMITE_BUILD_PATH" "DEFAULT_DYNAMITE_BUILD_PATH=somepath")

;; Transitive Coding Style
;
; Define the special coding style for Transitive that matchs the
; Coding Style guidelines.
;

; This requires cc-styles (which is part of emacs so shouldn't be a
; problem)

(require 'cc-styles)

; If there is no my-c-style defined we'll create one from which
; transitive-c-style can hang from
(if (not (bound-and-true-p my-c-style))
    (progn
      (message "Using default stroustrup style as base")
      (defconst my-c-style
	'("stroustrup"
	  (c-basic-offset . 2))
	"my-c-style")

      (c-add-style "my-c-style" my-c-style)))


(defconst transitive-c-style
  '("my-c-style"  ; based on my style
    (c-basic-offset . 2)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open before after)))
    (c-offsets-alist . ((topmost-intro      . 0)
			(topmost-intro-cont . 0)
			(substatement       . +)
			(substatement-open  . 0)
			(case-label         . +)
			(access-label       . -)
			(inclass            . +)
			(inline-open        . 0)
			(block-open         . 0)
			))
    (indent-tabs-mode . nil)
    )
  "Transitive's C Style")


(c-add-style "transitive-c-style" transitive-c-style)

(message "defined transitive c style")

;; Clever auto compile-command stuff
;
; This stuf uses the existing prject variables combnined with a
; special transitive mode to frob the compile command when required.
;
; If you are on the network  it will ensure the cd is into the
; /usr/export to ensure GDB paths are correct. This could be cleverer
; if your building and testing on the same machine.

;; Set current project root
;
; Iterate up the path to the root of the current src tree. Uses Will's
; bash scripting hack
;

(defun extract-project-root(string)
  "Extract the project root from a given path"
  (interactive)
  (let ((s string))
    (if (string-match "\\(.*\\)/src.*" s) (match-string 1 s) s)))

;(extract-project-root "/export/dynamite/cr2787/src/fuse/common")
;(extract-project-root "/home/alexjb/.emacs")

;; set-project-root
;
; This function is interactive so I can call it from M-x if things
; haven't happend automatically (although that's a bug I haven't seen
; for a while).

(defun get-project-root ()
  "Calculate a project root from the current working dir"
  (interactive)
  (extract-project-root
   (if (interactive-p)
       (chomp (shell-command-to-string "pwd"))
     (concat buffer-file-name))))

; redefines my .emacs set-project-root
(defun set-project-root ()
  "Set the current-project-root from current working dir"
  (interactive)
  (setq current-project-root (get-project-root))
  (message (concat "project root set to:" current-project-root)))

;; checks to see if the current buffer is in a new project root,
;; otherwise returns nil
(defun check-buffer-for-new-project-root ()
  "Checks to see if project root needs updating"
  (if (buffer-file-name)
    (if (not
	 (string-match current-project-root buffer-file-name))
	(set-project-root))))

;; set-transitive-compile-command
;
; (re-)sets the compile command using project root and other such
; magic. If I'm not at work then I send the compile command through
; ssh to my work machine (who wants to compile at home anyway ;-)
;
; the function frob-project-root

;; Frob the project root
;
; If I'm on a work export directory in /export I really mean
; /usr/export/'hostname' unless I'm the laptop
;
(defun frob-project-root(root)
  "Frob the project root to the /usr/export/machine/path"
  (if (and I-am-at-work (string-match "^/export" root))
      (replace-regexp-in-string
       "/export"
       (format "/usr/export/%s" (chomp (shell-command-to-string
					"hostname")))
       root)
    (concat root)))

;(frob-project-root "/export/dynamite/cr2787/")
;(frob-project-root "/usr/export/dynamite/cr2787/")
;(frob-project-root "/home/alexjb/src/")
;(frob-project-root "/export/dynamite/SICRectifyFix/dynamite07/")

(defvar have-set-transitive-compile-command-once nil
  "Have we ever defined a transitive compile command")

(defun set-transitive-compile-command ()
  "Set the compile command for dynamite"
  (interactive)
  (setq compile-command
	(format "cd %s/%s && make %s"
		(frob-project-root current-project-root)
		current-project current-build-flags))
  (setq have-set-transitive-compile-command-once "yes"))


;; If we can talk to the build system we can do extra magic
(when (file-exists-p "~/bin/ajb_build_info.pl")

  (defun set-build ()
    "Set the build path and flags based on the info stored in the GK
system"
    (interactive)
    (let ( (answer (shell-command-to-string
		    (concat "~/bin/ajb_build_info.pl -v " (read-string "Build ID:"))) ) )
      (setq
       current-project
       (extract-value-from-pair "DEFAULT_DYNAMITE_BUILD_PATH" answer))
      (setq
       current-build-flags
       (extract-value-from-pair "DEFAULT_DYNAMITE_BUILD_FLAGS" answer))

      (set-transitive-compile-command))
    (message (concat "New compile command:" compile-command))))

;; transitive-compile
;
; This is our default compile command. We will want to set the compile
; command if:
;
; 1. This is the first time we have tun this command
; 2. We have changed into a new project root.

(defun transitive-compile ()
  "A special transitive version of compile which attempts to set
the correct compile command"
  (interactive)
  (if (or (check-buffer-for-new-project-root) (not have-set-transitive-compile-command-once))
      (set-transitive-compile-command))
  (call-interactively 'compile))

(message "defined compile hacks")


;;
;; Give my compilation buffer a unique name
;;

(defun transitive-compile-window-name (curr-major-mode)
  (downcase
   (concat "*"
	   curr-major-mode
	   " of "
	   (shell-command-to-string "pwd | perl -ne \"chomp; s#.*dynamite/##; s#/.*##; print;\"")
	   " *")))


;;
;; Auto-mode hacks
;;
;
; This is the magic mode for transitive. It ensures we use the special
; transitive style and auto-magiced compile-command

;; do-transitive-font-lock-mods
;
; Tweak the font locks for transitives CodingStyle. We do this by
; frobing c++-font-lock-extra-types and making it buffer local
;
; uNint and friends
; CamalCase types

(defface extra-whitespace-face
  '((t (:background "pale green")))
  "Used for tabs and such.")

(defvar my-extra-keywords
  '(("\t" . 'extra-whitespace-face)))

(defun do-transitive-font-lock-mods ()
  "Tweak font lock for our coding style"
  (interactive)
  (message "do-transitive-font-lock-mods")
  (make-local-variable 'c++-font-lock-extra-types)
  (add-to-list 'c++-font-lock-extra-types "u8int")
  (add-to-list 'c++-font-lock-extra-types "u16int")
  (add-to-list 'c++-font-lock-extra-types "u32int")
  (add-to-list 'c++-font-lock-extra-types "u64int")
  (add-to-list 'c++-font-lock-extra-types "s8int")
  (add-to-list 'c++-font-lock-extra-types "s16int")
  (add-to-list 'c++-font-lock-extra-types "s32int")
  (add-to-list 'c++-font-lock-extra-types "s64int")
  (add-to-list 'c++-font-lock-extra-types
	       "[A-Z]\\{1\\}[a-z]+[A-Z]\\{1\\}\\w*")
  (font-lock-add-keywords nil my-extra-keywords))

;; Return the pointer to the tags table for dynamite trees
;
; This function is set to default-tags-table-function and if in
; dynamite should point at the default tags table for the current
; build

(defun transitive-default-tags-table-function ()
  "Return the the current projects TAG's table"
  (interactive)
  (if (buffer-file-name)
    (if (not (string-match buffer-file-name (get-project-root)))
        (setq tags-file-name (format "%s/%s/TAGS" (frob-project-root
                                                   (get-project-root))
                                     current-project))))
  (concat tags-file-name))

;; Set the proper values for works coding standards
;
; this could be better with the global mods
(defun transitive-c-mode()
  "C mode with adjusted defaults for Transitive"
  (interactive)
  (message "transitive-c-mode")
  (c++-mode)
  ; c-mode hook will of set stuff, now we can overide
  (c-set-style "transitive-c-style")
  (do-transitive-font-lock-mods)
  ; fix keys
  (global-set-key (kbd "C-c c") 'transitive-compile)
  (global-set-key (kbd "<f3>") 'transitive-compile)
  ; fancy compile mode stuff
  (setq compilation-buffer-name-function 'transitive-compile-window-name)
  (setq-default indent-tabs-mode nil)
  (setq tags-file-name (concat current-project-root "/"
			       current-project "/TAGS"))
  (setq default-tags-table-function 'transitive-default-tags-table-function))

;; auto-mode
;
; Make sure the auto-modes pick up transitive stuff. Basically
; it does this by looking for dynamite if the file string of any
; C/C++ files.
;
; I put all my dynamites in /export/dynamite but it should pick up
; checkouts with the BLAH/dynamite07/.. syntax as well
;

(setq auto-mode-alist (cons '(".*dynamite.*\\.[ch]$" . transitive-c-mode)
			    auto-mode-alist))
(setq auto-mode-alist (cons '(".*dynamite.*\\.cc$" . transitive-c-mode)
			    auto-mode-alist))

;; We want a tweaked XML mode for debug streams 'cause they are so big
;
(defun transitive-xml-mode()
  "XML Mode for Debug Streams crap"
  (interactive)
  (setq buffer-read-only t)
  (view-mode)
;  (local-set-key (kbd "f")  '(progn
;                               View-revert-buffer-scroll-page-forward 0))
  (flyspell-mode -1))

(setq auto-mode-alist (cons '(".*/dynamite_.*.xml$" . transitive-xml-mode)
                            auto-mode-alist))

;; Transitive Makefile mode
;
; Doesn't do anything except ensure we set the compile command.
; If I was being cleverer I could auto switch project

(defun transitive-makefile-mode()
  "Makefile mode for Transitive stuff"
  (interactive)
  (message "Setting Transitive Makefile Mode"
  (makefile-mode)
  (global-set-key (kbd "C-c c") 'transitive-compile)
  (setq compilation-buffer-name-function 'transitive-compile-window-name)))

; hey if its a Makefile still frig the compile command
(setq auto-mode-alist (cons '(".*dynamite.*Makefile*" .
                              transitive-makefile-mode)
                            auto-mode-alist))

;
; switch-project
;
(defun switch-project ()
  "Switch to a new project"
  (interactive)
  (setq current-project (read-string "New project:" nil
                                     'transitive-projects 0))
  (set-transitive-compile-command))


;; my-cvs-conflict
;
; Defines my-dired-script which is a hacked version of find-dired
; which takes a script that gives dired style output. This is then
; used by my-cvs-conflict which uses a hacked version of the
; worldStatus.pl scipt to feed the mode
;
; This isn't massivley well tested since I moved to GIT (AJB 16/4/07)
;

(load-library "find-dired")

(defun my-dired-script (dir script)
  "Run `script' and go into Dired mode on a buffer of the output."

  (interactive (list (read-file-name "Run in directory: " nil "" t)
                     (read-string "Run command: " script)))

  (message "my-dired-script start")

  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (abbreviate-file-name
               (file-name-as-directory (expand-file-name dir))))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
        (error "my-dired-script needs a directory: %s" dir))
    (switch-to-buffer (get-buffer-create "*my-dired-script*"))

    ;; See if there's still a `script' running, and offer to kill
    ;; it first, if it is.
    (let ((find (get-buffer-process (current-buffer))))
      (when find
        (if (or (not (eq (process-status find) 'run))
                (yes-or-no-p "A `my-dired-script' process is running; kill it? "))
            (condition-case nil
                (progn
                  (interrupt-process find)
                  (sit-for 1)
                  (delete-process find))
              (error nil))
          (error "Cannot have two processes in `%s' at once" (buffer-name)))))

    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)

    ; hmm maybe some of these should be locals
    (setq default-directory dir
          my-dired-script-script script		; save for next interactive call
          my-dired-script-script-call (concat "cd " dir " && " script))

    (message (concat "set script-call to "
                     my-dired-script-script-call))
    
    ;; go into dired mode
    (dired-mode dir)
    
    ;; This really should rerun the find command, but I don't
    ;; have time for that.

    ; Whats this for????
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "g" 'undefined)

    ;;; I assume this is a vraible that dired needs?
    
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
	;; will work even with nested dired format (dired-nstd.el,v 1.15
	;; and later)
	(dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm) 
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker)))))

    
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " dir ":\n")
    ;; Make second line a ``find'' line in analogy to the ``total'' or
    ;; ``wildcard'' line. 
    (insert "  " my-dired-script-script-call "\n")

    ; this is for the benefit if ls
    (setenv "COLUMNS" (number-to-string (window-width)))
    
    ;; Start the find process.
    (let ((proc (start-process-shell-command "running a script" (current-buffer) my-dired-script-script-call)))
      (set-process-filter proc (function find-dired-filter))
      (set-process-sentinel proc (function find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) 1 (current-buffer)))
    (setq mode-line-process '(":%s"))))


(defun my-cvs-conflict ()
  (interactive)
  (let
      ((command "/home/alexjb/bin/ajb_ws.pl -C -f"))
    (message (concat "command is " command))
    (my-dired-script current-project-root command)))



(message "Done with Transitive customisations")
