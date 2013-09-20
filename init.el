; -*- emacs-lisp -*-
;
; Alex Bennée's .emacs
;
; This is my Emacs, there are many like it but this is my own.
;.It is mainly an amalgem of different hacks acquired over time
; which I use on many of the machines I work with.
;
; It has a cobbled heritage from various sites and wiki's and its
; probably safest to assume the code is all either GPL or Public
; Domain. Feel free to use whatever bits may be of use to you in this
; file that it is my right to license ;-)
;

;; Compiled .emacs work around
;
; If I've updated via an SCM there is a chance the compiled
; code is out of date. In which case we really should unlink
; the .elc file and recompile the .emacs
;
; I also want to auto compile any .el. As we work with a vc-repo (to
; which .emacs links) we  straight away want to resolve where ~/.emacs
; resolves to first


; debugging weird stat-up issues.
(setq debug-ignored-errors (remq 'user-error debug-ignored-errors))
(setq debug-on-error 't)

(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
      (byte-compile-file dotemacs))))

;not the speed saving you might think...
;(add-hook 'after-save-hook 'autocompile)

; check the compiled version not out of date
(when (and user-init-file
	   (string-match ".elc" user-init-file))
  (let* ((src-file (file-name-sans-extension user-init-file)))
    (when (and (file-exists-p src-file)
	       (file-newer-than-file-p src-file user-init-file))
      (message "working around newer source file")
      (byte-compile-file src-file)
      (load src-file))))
	
(message (concat user-init-file " start"))


;;;; Start of real code.

;;; Basic sanity
;; Find out about my environment

; Define some variable about what sort of emacs I'm running in

(defvar I-am-emacs-21+ (>= emacs-major-version 21))
(defvar I-am-emacs-22+ (>= emacs-major-version 22))
(defvar I-am-emacs-23+ (>= emacs-major-version 23))
(defvar I-am-emacs-24+ (>= emacs-major-version 24))

(defvar I-am-gnuemacs (string-match "GNU Emacs" (emacs-version)))
(defvar I-am-xemacs (string-match "XEmacs" (emacs-version)))

;; Lets define which machine I'm on, therefor if I am at work
;; (this of course falls down when logging on remotely via tramp)

(defvar I-am-at-work (string-match "sloy" (system-name)))
(defvar I-am-at-home (string-match "danny" (system-name)))
(defvar I-am-on-netbook (string-match "trent" (system-name)))
 
;; Lets set some parameters if we are running as a console or under X
;
; Note these are not useful for --daemon invocations and should now be
; deprecated in favour of "live" tests on window-system
;
(defvar I-am-in-X (eval 'window-system));
(defvar I-am-in-console (not (eval 'window-system)))
(defvar I-am-on-MacOSX (or (string-match "Carbon" (emacs-version))
			   (string-match "apple-darwin" (emacs-version))))
(defvar I-am-remote (getenv "SSH_TTY"))

;; Server stuff
(defvar will-start-server nil)

;;
;; Basic config variables
;;

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Default mode is text-mode,
(setq default-major-mode 'text-mode)

;; Don't truncate message buffer. For debugging reasons.
(setq message-log-max t)

;; Less verbosity
(fset 'yes-or-no-p 'y-or-n-p)

;; Obey local variables set in -*- type things
(setq enable-local-variables t)

;; Silently add trailing newline to file
(setq require-final-newline t)

;; ^K deletes line, not delete to EOL
(setq-default kill-whole-line t)

;; Searches are case sensitive
(setq-default case-fold-search nil)

;; Automagically decompress files
(unless I-am-xemacs
  (auto-compression-mode t))

;; Seriously the kernel TAGS is >10Mb 
(setq large-file-warning-threshold 40000000)

;; Stop popping up the file dialog, very annoying when compile-mode
;; want to find an error in a non-existent file
(setq use-file-dialog 'nil)

;; Change menu entry so it doesn't use faces.
(define-key global-map [menu-bar tools print ps-print-buffer]
  '("Postscript Print Buffer" . ps-print-buffer))

;;
;; TRAMP customisations.
;;

; You can pretty much guarantee tramp implies over ssh
(setq tramp-default-method "scp")
; Auto-saves tramp files on our local file system
(add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp "~/.emacs.d/tramp-saves"))
; If I'm travelling assume no one is messing with files on my work
; machine
(when (string-match "symbiot" (system-name))
  (setq remote-file-name-inhibit-cache 'nil
	tramp-completion-reread-directory-timeout 'nil))
; I tend to use magit anyway, but disable VC mode over TRAMP
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))

;; Move the custom file out of init.el
(setq custom-file "~/.emacs.d/my-custom.el")

;; Let's try CEDET one more time
; early in config to avoid clashing with built-in...
(let* ((cedet-devel (concat (getenv "HOME")
			    "/.emacs.d/cedet.git/cedet-devel-load.el")))
  (when (file-exists-p cedet-devel)
    (load-file cedet-devel)

    ;; Add further minor-modes to be enabled by semantic-mode.
    ;; See doc-string of `semantic-default-submodes' for other things
    ;; you can use here.
    (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
    (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
    ;; Enable Semantic
    (semantic-mode 1)))

;; Packaging, if we have it

(when (and I-am-emacs-24+ (require 'package "package" 'nil))
  (package-initialize)
  (add-to-list 'package-archives
            '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
	       '("org" . "http://orgmode.org/elpa/") t)
  
  ; filter some packages out
  (setq package-filter-function
      (lambda (package version archive)
        (or (not (string-equal archive "melpa"))
            (not (memq package '(magit org))))))

  ; list of packages I care about
  (defvar ajb-packages
    '(ack-and-a-half expand-region magit magithub python
		     solarized-theme zenburn-theme dynamic-fonts))

  ; check what's installed
  (defun ajb-packages-installed-p ()
    (loop for p in ajb-packages
	  when (not (package-installed-p p)) do (return nil)
	  finally (return t))))

;; Add local search path
;
; This is recursive so adding test libraries should just
; be a case of throwing the directory into .emacs.d
;
(when (and (file-exists-p "~/.emacs.d/")
	   (fboundp 'normal-top-level-add-subdirs-to-load-path))
      (let* ((my-lisp-dir "~/.emacs.d/")
	     (default-directory my-lisp-dir))
	(setq load-path (cons my-lisp-dir load-path))
	(normal-top-level-add-subdirs-to-load-path)))

;; maybe-load-library
;
; A little less than using (require 'lib) - but require has optional args

(defun maybe-load-library (libname)
  "Try and load library 'libname' if it is in the path"
  (when (locate-library libname)
    (load-library libname)))

;; Do we want an edit-server?
(when (and (daemonp) (maybe-load-library "edit-server"))
  (when (maybe-load-library "edit-server-htmlize")
    (add-hook 'edit-server-start-hook
	      'edit-server-maybe-dehtmlize-buffer)
    (add-hook 'edit-server-done-hook
	      'edit-server-maybe-htmlize-buffer))
  (when (maybe-load-library "mediawiki")
    (add-to-list 'edit-server-url-major-mode-alist '("mediawiki" .
						     mediawiki-mode)))
  (add-to-list 'edit-server-url-major-mode-alist
	       '("mail.google" . mail-mode))
  (add-hook 'emacs-startup-hook '(lambda ()
				   (message "starting up edit-server")
				   (edit-server-start))))

;;
;; Load any global modes/extensions that are used throughout emacs.
;; This includes snippets and auto-completion libraries.
;;

;; Do we have snippets?
(when (and (maybe-load-library "yasnippet")
	   (file-exists-p "~/.emacs.d/my-snippets"))
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/my-snippets")
  (yas-global-mode))

(when (maybe-load-library "auto-complete")
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-menu-map 't)
  (ac-set-trigger-key "M-/") ; override dabrev-expand
  (setq-default ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)))

;; Expansion
; This gets over-ridden when auto-complete is in effect
(global-set-key (kbd "M-/") 'hippie-expand)

; Nice for jumping about windows.
(when (maybe-load-library "ace-jump-mode")
  (global-set-key (kbd "C-x j") 'ace-jump-mode))

; Better M-x
(when (maybe-load-library "smex")
  (global-set-key (kbd "M-x") 'smex))

(when (require 'keyfreq nil 'noerror)
  (keyfreq-mode)
  (keyfreq-autosave-mode))

; On Mac we we want to add /sw/bin for fink (where things like
; aspell live)
(when (and I-am-on-MacOSX (file-exists-p "/sw/bin"))
      (setenv "PATH" (concat (getenv "PATH") ":/sw/bin")))

(message "Done Basic Sanity")


;;; Miscellaneous functions

; I like my name
(defun insert-myname ()
  (interactive)
  (insert (format "Alex Bennée")))

(global-set-key (kbd "C-M-e") 'insert-myname)


(defun praise-emacs ()
  (interactive)
  (message "Basking in the glory of your praise...")
  (sleep-for 1)
  (message "Basking in the glory of your praise...Done."))

;;
;; Need a better way to do this as fortune is on different places
;; on different machines.
;;
(defun fortune ()
  "Famous unix fortune teller."
  (shell-command-to-string "/usr/bin/fortune"))

;; String munging functions
;
; Extract the first group in a regex

(defun extract-string(regex string)
  "Extract a string in a regex (the bit in ()'s)"
  (interactive)
  (let ((s string)) (if (string-match regex s) (match-string 1 s) s)))

; And building on that
(defun extract-value-from-pair(key string)
  "Extract the value from AAAA=value pairs"
  (let ((regex (concat key "=\\(.*\\)$")))
    (extract-string regex string)))

; examples:
;  (extract-string "name is \\(\.*\\) " "my name is Alex ok") = Alex
;  (extract-value-from-pair "AAA" "AAA=xxx") = xxx

; chomp
(defun chomp(string)
  "Perform a perl-like chomp"
  (let ((s string)) (if (string-match "\\(.*\\)\n" s) (match-string 1 s) s)))

;; which-lookup
;
; Like the shell command of the same name except it trims to 'nil if
; it can't find anything

(eval-when-compile (require 'cl))

(defun which-lookup(name-or-list)
  "Perform a `which` like file look-up, returning the first hit or
'nil if no match found"
    (loop for x in (if (listp name-or-list) name-or-list (list name-or-list))
      do (let ((path (chomp (shell-command-to-string (concat "which " x)))))
           (if (and (file-exists-p path) (> (length path) 0))
		 (return path)))))

; (which-lookup "foo") => nil
; (which-lookup "emacs") = "/usr/bin/emacs"
; (which-lookup '("aspell" "ispell")) => "/usr/bin/aspell"
; (which-lookup '("ack-grep" "ack" "grep"))

; Am I on the pixel?
(defvar I-am-on-pixel (and (string-match "localhost" (system-name))
			   (which-lookup "host-x11")))

; uses common lisp
(defun find-valid-file (list-of-files)
  "Go though a list of files and return the first one that is present"
  (loop for path in list-of-files
	until (file-exists-p path)
	finally return path))

; the 'elisp' way
(defun find-valid-file-elisp-way (list-of-files)
  "Go though a list of files and return the first one that is present"
  (let (r '())
    (mapc '(lambda (f)
	     (if (file-exists-p f) (add-to-list 'r f)))
	  list-of-files)
    (car r)))

; using 'cl-macs 
(defun find-valid-file-dolist-way (list-of-files)
  "Go though a list of files and return the first one that is present"
  (dolist (f list-of-files)
    (if (file-exists-p f)
	(return f))))

;; Load sub-modules
;
; Development related stuff, including project root
(load-library "my-devel")
; Org configuration
(maybe-load-library "my-org")

(message "Done defuns")

;;; keymapping
;;
;; I'm allowed to bind: C-c [letter], [f5]-[f9]
;;
;; NB I've also re-mapped Caps-Lock to control

;; Goto-line should be easy
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\M-g" 'goto-line)

;; Return => newline-and-indent
(global-unset-key "\C-m")
(global-set-key "\C-m" 'newline-and-indent)

;; Make delete do what I expect
;? Do I still need this?
(global-unset-key [delete])
(global-set-key [delete] 'delete-char)

;; C-k deletes whole line
(global-set-key "\C-k" 'kill-whole-line)

;; Let's also do Alt-X and easier way
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Window navigation and size control
(when (maybe-load-library "windmove")
  (windmove-default-keybindings))

(unless (and (fboundp 'crmbk-running-in-host-x11-p)
             (crmbk-running-in-host-x11-p))
  (global-set-key (kbd "<M-down>") 'enlarge-window)
  (global-set-key (kbd "<M-right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "<M-up>") 'shrink-window)
  (global-set-key (kbd "<M-left>") 'shrink-window-horizontally))

;(global-set-key (kbd "<C-tab>") 'bury-buffer)
(global-set-key (kbd "<C-tab>") 'pop-global-mark)

;; Allow windows to be dedicated to one thing interactively
;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key [pause] 'toggle-window-dedicated)

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame
;; I don't use the default binding of 'C-x 5', so use toggle-frame-split instead
(global-set-key (kbd "C-x %") 'toggle-frame-split)

;; Handle next/prev error on keymap / and * (with numlock off)
(global-set-key (kbd "M-O o") 'previous-error)
(global-set-key [kp-divide] 'previous-error)
(global-set-key (kbd "M-O j") 'next-error)
(global-set-key [kp-multiply] 'next-error)

;; Helm customisations
(setq helm-yank-symbol-first 't)

;; iMenu find
(if (fboundp 'helm-imenu)
    (global-set-key (kbd "C-f") 'helm-imenu)
  (global-set-key (kbd "C-f") 'imenu))

; Occur stuff
(if (fboundp 'helm-occur)
    (global-set-key (kbd "C-c o") 'helm-occur)
  (global-set-key (kbd "C-c o") 'occur))

(global-set-key (kbd "C-c e") 'eshell)

;; Handle special Mac'isms
;
; If we are running over ssh lets map some more stuff to move around,
; mainly what MacOS sends over the terminal when I'm working on
; Fliss' laptop. I wish I could detect that case....
;
; I also have the same issues if I run in console mode under MacOS
;
; C-v and M-v scroll up and down, but I rarely remember that
;
; FIXME: This breaks if I ssh and resume an existing screen session,
;tweaked by adding an interactive function to do the keys

(defun my-tweak-macos-keys ()
  (interactive)
					; We set the keyboard coding system to utf-8 which makes some
					; things behave (I'm not totally sure why, must read up more)
  (set-keyboard-coding-system 'utf-8)

  
					; this is what I get when I hit Alt-ArrowKeys on the Mac
  (global-set-key (kbd "ESC <up>") 'scroll-down)
  (global-set-key (kbd "ESC <down>") 'scroll-up)
  (global-set-key (kbd "ESC <left>") 'beginning-of-buffer)
  (global-set-key (kbd "ESC <right>") 'end-of-buffer)

					; done
  (message "tweaked keys for remote/macos console mode"))

(if (or I-am-remote (and I-am-on-MacOSX I-am-in-console))
    (my-tweak-macos-keys))

;; Make Bookmark handling a little more sane
;
; This overides existing backward-sentence and backward-word default
; binding
;(global-set-key [(meta b)] 'list-bookmarks)
;(global-set-key [(meta a)] 'bookmark-set)

;; Function Keys, I'm currently going over what my Microsoft ergonomic
;; keyboard has sentsiled on the function keys

; Find help
(global-set-key (kbd "<C-f1>") 'apropos)

; Make Undo a little less octopedal
(global-set-key (kbd "<C-f2>") 'undo)

;; In Emacs 21+, home and end go to beginning and end of line. This is
;; clearly the Wrong Thing.
(if I-am-emacs-21+
    (progn
      (global-unset-key [home])
      (global-set-key [home] 'beginning-of-buffer)
      (global-unset-key [end])
      (global-set-key [end] 'end-of-buffer)))

;; Macro keys
; If I define a single press macro keys I may use them more often

(global-set-key [(control meta m)] 'call-last-kbd-macro)
(global-set-key (kbd "<f10>") 'call-last-kbd-macro)
(global-set-key (kbd "<C-f11>") 'start-kbd-macro)
(global-set-key (kbd "<C-f12>") 'end-kbd-macro)

;; insert-sequence-key
;
; Handy little key sequence utility so I don't have to guess at the
; correct kbd incantation

(defun insert-sequence-key (key)
  "Inserts a keystroke suitable for use in fcns like global-set-key"
  (interactive "kInsert key chord: ")
  (insert (format "(kbd \"%s\")" (key-description key))))

(global-set-key (kbd "<C-f8>") 'insert-sequence-key)

;; Multiple cursors
;
; This is ace and I should use it more
(when (require 'multiple-cursors nil 't)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-x ;") 'mc/mark-all-like-this-dwim)
  (global-set-key (kbd "C-+") 'mc/mark-all-like-this-dwim)
  (global-set-key (kbd "M-+") 'mc/edit-lines))

;; Expand region
(when (require 'expand-region nil 't)
  (global-set-key (kbd "C-=") 'er/expand-region))

(message "Done keymapping")

;;; Backup settings
;;
;; I like to use symbolic links for a lot of my ~/bin scripts to my
;; working dir. By default these get broken when emacs writes a new
;; file.

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))   ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 3
   version-control t)       ; use versioned backups


(message "Setting up display")

;;; Display hacks
;
; With --daemon mode using start-up evaluation is no longer useful as
; the .emacs will almost certainly run without window-system meaning
; anything. Instead we should do things on the fly as new frames are
; created.

; winner mode to remember window layouts
(winner-mode 't)

;; Disable the menu and tool bars, they just take up space.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use imagemagick if we have it to view images
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(when (and (require 'chromebook "chromebook" 't)
           (crmbk-running-in-host-x11-p))
  (set-face-attribute 'default nil :height 250)
  (add-hook 'crmbk-frame-mode-hook 'crmbk-remap-search)
  (add-hook 'crmbk-frame-mode-hook 'crmbk-disable-touchpad)
  (define-key crmbk-frame-mode-map (kbd "<M-up>") 'scroll-down)
  (define-key crmbk-frame-mode-map (kbd "<M-down>") 'scroll-up)
  (when (boundp 'edit-server-new-frame-alist)
    (setq edit-server-new-frame-alist '((name . "Edit Server Frame")
					(fullscreen . 'fullboth)))))

; Re-use existing frames if buffer already exists in one
(unless I-am-on-pixel
  (setq-default display-buffer-reuse-frames t))


(message "Display Done")

;; Prettier unique buffer names.
(unless I-am-xemacs
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Mouse set-up
;
; I don't use the mouse for a lot of things and on my netbook it
; positively gets in the way. Really it's only used for links and
; the occasional scroll of the buffer.

;; Stop the mouse cursor getting in the way. This is great.
(unless I-am-xemacs
  (when 'window-system
    (mouse-avoidance-mode 'exile)))

;; enable the mouse wheel
(autoload 'mwheel-install "mwheel" "Enable wheely mouse")
(mwheel-install)

; X11 paste to point
(setq mouse-yank-at-point t)

;; Change the cursor colour in Ovwrt mode
(defun ins-cursor-set ()
  "Set cursor colour according to insert mode"
  (set-cursor-color
   (if overwrite-mode
       "red"
     "black")))

(if I-am-emacs-21+
    (blink-cursor-mode -1))

(add-hook 'post-command-hook 'ins-cursor-set)

(setq frame-title-format "%b")
(setq  icon-title-format "%b")

;(if I-am-emacs-21
;    (progn 
;      (autoload 'zone-when-idle "zone")
;      (zone-when-idle 60)))

;; want to reduce the amount of white space in the mode-line
(setq global-mode-string
      '("" org-mode-line-string))

(setq-default mode-line-format
      '("-"
	mode-line-mule-info
	mode-line-modified
	" "
	mode-line-buffer-identification
	" "
	"%l/%c "
	"%[("
	mode-name
	mode-line-process
	minor-mode-alist
	"%n"
	")%]"
	"--"
	global-mode-string
	"--"
	))



;; Let's shrink the minor-mode-alist down to size.
(setcdr (assq 'abbrev-mode minor-mode-alist) '(" Ab"))
(setcdr (assq 'auto-fill-function minor-mode-alist) '(" Fl"))

;; Not added until the relevant mode is loaded.
(setq minor-mode-alist (cons '(compilation-in-progress nil)
			     minor-mode-alist))

;; Uses a separate variable. Isn't that nice?
(setq eldoc-minor-mode-string nil
      highlight-changes-passive-string nil)

;; (display-time) is needed for appt to display in the mode-line, but
;; we don't want the time taking up precious space.
(unless I-am-xemacs
  (setq display-time-interval 20
	display-time-format 'nil
	display-time-string-forms '( 24-hours ":" minutes ))
  (display-time-mode))

;; Displays current function() in programming modes. 
(setq which-func-modes 'nil)
;;      which-func-format '("[" which-func-current "]-"))
;(which-function-mode 'nil)

;; Reduce white space
(setq-default mode-line-buffer-identification '("%b"))

;; Make fill do the Right Thing with full-stops.
(setq sentence-end-double-space nil)

;; Highlights region _all_ the time. Slightly buggy...
(transient-mark-mode t)
(delete-selection-mode 1)

;; Groovy things with matching parentheses
(show-paren-mode 1)

;; Tweaks to scrolling behaviour. Still a bit odd.
(setq scroll-preserve-screen-position t
      scroll-conservatively 5
      scroll-step 1
      next-line-add-newlines nil)

;; Make pound signs work
(set-language-environment "UTF-8")
(setq unibyte-display-via-language-environment t)

;; Allow narrowing.
(put 'narrow-to-region 'disabled nil)
(global-set-key (kbd "C-x n r") 'narrow-to-region)

;; Use xdg-open
(setq browse-url-browser-function 'browse-url-xdg-open)

(message "Done Display Hacks")

;; Don't prompt me to revert something
(global-auto-revert-mode 1)

;; Expands a time-stamp line
(setq time-stamp-format "%02H:%02M on %:a, %:d %:b %:y by %u")
(add-hook 'write-file-hooks 'time-stamp)

;; Auto-Insert
(auto-insert-mode 1)
(setq auto-insert-alist ())		;? html-helper

;; I hate tabs - they are set in cc-mode but not everything respects that
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

; TODO: clean-up my defaults for this
(when I-am-emacs-23+
  (setq whitespace-chars '(trailing tabs space-before-tab
                           empty space-after-tab))
  (setq whitespace-style '(faces tabs trailing lines-tail empty space-after-tab tab-mark)))

;; Bow down before font-lock
(add-hook 'font-lock-mode-hook
          '(lambda ()
             (setq font-lock-maximum-decoration  t
                   font-lock-verbose             t
                   font-lock-support-mode        'jit-lock-mode
                   lazy-lock-defer-on-scrolling  nil
                   lazy-lock-defer-contextually  t
                   lazy-lock-stealth-verbose     t
                   lazy-lock-stealth-lines       50
                   lazy-lock-stealth-time        3)))
(global-font-lock-mode t)


;; Font locking info mode (from Andy.Ling@quantel.com)
(defvar info-font-lock-keywords
  (list
   '("^\\* [^:]+:+" . font-lock-function-name-face)
   '("\\*[Nn]ote\\b[^:]+:+" . font-lock-reference-face)
   '("  \\(Next\\|Prev\\|Up\\):" . font-lock-reference-face))
  "Additional expressions to highlight in Info mode")

(add-hook 'Info-mode-hook
	  (lambda ()
	    (make-local-variable 'font-lock-defaults)
	    (setq
	          font-lock-defaults '(info-font-lock-keywords nil t)
		  case-fold-search nil)))

;; ediff
;
; Need to setup properly
;
;? Also need to find a way to restore it all on
;  resume. This stuff is all far from bullet-proof.

(eval-after-load "ediff"
  '(progn
     (message "doing ediff customisation")
     (setq diff-switches               "-u"
	   ediff-custom-diff-options   "-U3"
	   ediff-split-window-function 'split-window-horizontally
	   ediff-window-setup-function 'ediff-setup-windows-plain)

     (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
     (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
     (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display)))

;; diff-mode and its derivitives
;
; Here we have some tweaks to the diff-mode for testing a series of
; patchs and/or applying a whole patch
;

(when (locate-library "diff-mode")
  (if (locate-library "my-diff-mode")
      (progn
	(message "Hooking in my-diff-mode")
	(autoload 'my-diff-mode "my-diff-mode")
	(defalias 'dmode-alias 'my-diff-mode))
    (autoload 'diff-mode "diff-mode")
    (defalias 'dmode-alias 'diff-mode))

					; Which ever version we have we need to set the
      ; automode up so it loads when we need it
      (setq auto-mode-alist (append (list
				     (cons "\.diff$"  'dmode-alias)
				     (cons "\.patch$" 'dmode-alias)
				     (cons "\.rej$" 'dmode-alias)
				     (cons "\.dotest/0.*"
					   'dmode-alias))
				    auto-mode-alist)))
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
;

; I'm British, not Amercian damit!
;(defun set-british-dict ()
;  "Set British Dictionary"
;  (ispell-change-dictionary "british")
;  (message "Set ispell to British Dictionary"))

;(defun my-text-mode-hook ()
;  (set-british-dict)
;  (define-key text-mode-map (kbd "<f1>") ispell-word))

(if (locate-library "ispell")
    (let ((spell-path (which-lookup '("aspell" "ispell")))) ; aspell is preferred
      (if spell-path
	  (progn
	    (setq ispell-program-name spell-path
		  ispell-dictionary "british")

	    ;; flyspell mode
	    ; I think this has been in emacs a while, but best practice to check
	    ; (from http://trey-jackson.blogspot.com/2008/04/emacs-tip-16-flyspell-and-flyspell-prog.html)
	    (when (locate-library "flyspell")
	      (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
	      (add-hook 'message-mode-hook 'turn-on-flyspell)
	      (add-hook 'text-mode-hook 'turn-on-flyspell)
	      (add-hook 'mail-mode-hook 'turn-on-flyspell)
	      (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
	      (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
	      (defun turn-on-flyspell ()
		"Force flyspell-mode on using a positive arg.  For use in hooks."
		(interactive)
		(flyspell-mode 1))))
	(message "Skipping ispell - no programs")))
  (message "Skipping ispell - no ispell library"))

;; calculator
;
; If we have the calculator library available lets load it in
;

(when (locate-library "calculator")
  (autoload 'calculator "calculator"
    "Run the Emacs calculator." t)
  (global-set-key [(control return)] 'calculator))

;; GPG Support
;
(when (maybe-load-library "epa-file")
  (setenv "GPG_AGENT_INFO" nil) ; gpg-agent confuses epa when getting passphrase
  (epa-file-enable))

;; my-find-binary
;
; Handy for dumping objdump into a buffer
(when (locate-library "my-find-binary")
    (autoload 'find-binary-file "my-find-binary"))

;; Version control library
;
;

(setq vc-command-messages t
      vc-initial-comment t)

; I like to use .git/.bzr etc in my directory names
(setq completion-ignored-extensions
      (remove ".git/" (remove ".bzr/" (remove ".svn/" completion-ignored-extensions))))

; Git Hooks, prefer magit over vc enabled git
(if (and (locate-library "vc-git.el")
	 (not (locate-library "magit")))
    (add-to-list 'vc-handled-backends 'Git)
  (setq vc-handled-backends (remq 'Git vc-handled-backends))
  (autoload 'magit-status "magit" "magit front end" t)
  (global-set-key (kbd "C-x g") '(lambda ()
				   (interactive)
				   (if buffer-file-name
				       (magit-status (file-name-directory (file-chase-links buffer-file-name)))
				     (magit-status
				      default-directory))))
  (eval-after-load "magit"
    '(progn
       (add-hook 'magit-mode-hook #'(lambda() (yas-minor-mode -1)))
       (add-hook 'magit-commit-mode-hook #'(lambda() (auto-fill-mode 1)))
       (setq magit-status-buffer-switch-function 'switch-to-buffer
	     magit-rewrite-inclusive 'nil))))

; Also the git-blame and git-status stuff
(when (locate-library "git")
  (autoload 'git-status "git"
    "Git Status" t))

(when (locate-library "git-blame")
  (autoload 'git-blame-mode "git-blame"
    "Minor mode for incremental blame for Git." t))

(eval-after-load "git-messenger"
  '(progn
     (setq git-messenger:show-detail 't)
     (global-set-key (kbd "C-h g") 'git-messenger:popup-message)))

(message "Done GIT hooks")

;; WoMan - WithOut Man
;
; This is mostly handy if you are running on window and you don't have
; man installed.
;

(when (and (not (which-lookup "man"))
	   (locate-library "woman"))
  (autoload 'woman "woman" "Decode and browse a UN*X man page." t)
  (autoload 'woman-find-file "woman" "Decode UN*X man-page file." t)
  (autoload 'woman-dired-find-file "woman" "Browse man page from dired" t)
  (message "Enabling woman for man pages"))

;; Dired stuff
(add-hook 'dired-mode-hook
		(lambda ()
		  (setq truncate-lines t)))

;; Web Development Modes
;
; I have an enormous hack (my-web-mode) which didn't work very well.
; Use nxhtml-mode instead. However it only works with emacs23
; (the emacs22 autoload fails due to missing files)

(when (and I-am-emacs-23+ (maybe-load-library "~/.emacs.d/nxhtml/autostart.el"))
  (setq nxhtml-skip-welcome t)
  (when (maybe-load-library "js2-mode")
    (defalias 'javascript-mode 'js2-mode "js2-mode is aliased to javascript mode")))

(when (maybe-load-library "htmlize")
  (setq htmlize-output-type 'inline-css)

; From http://ruslanspivak.com/2007/08/18/htmlize-your-erlang-code-buffer/
  (defun my-htmlize-region (beg end)
    "Htmlize region and put into <pre> tag style that is left in <body> tag
plus add font-size: 8pt"
    (interactive "r")
    (let* ((buffer-faces (htmlize-faces-in-buffer))
	   (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
	   (pre-tag (format
		     "<pre style=\"%s font-size: 8pt\">"
		     (mapconcat #'identity (htmlize-css-specs
					    (gethash 'default face-map)) " ")))
	   (htmlized-reg (htmlize-region-for-paste beg end)))
      (switch-to-buffer-other-window "*htmlized output*")
					; clear buffer
      (kill-region (point-min) (point-max))
					; set mode to have syntax highlighting
      (nxml-mode)
      (save-excursion
	(insert htmlized-reg))
      (while (re-search-forward "<pre>" nil t)
	(replace-match pre-tag nil nil))
      (goto-char (point-min)))))

;; Elisp mode
;
; I keep this in the main .emacs as I edit .emacs quite a bit

; auto-mode lists
(setq auto-mode-alist
      (append (list (cons "\\.emacs\\'" 'emacs-lisp-mode))
	      auto-mode-alist))
; I may also edit as dotemacs
(setq auto-mode-alist
      (append (list (cons "dotemacs" 'emacs-lisp-mode))
	      auto-mode-alist))

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (eldoc-mode t)
	     (turn-on-auto-fill)))

; For most web-forms I want longlines-mode by default
;
; It's All Text: /home/ajb/.mozilla/firefox/hgd0onxt.default/itsalltext/.2e2i2y3b2c.txt
; Emacs Chrome: /tmp/tmpcUbYA_.txt

(add-hook 'text-mode-hook
	  '(lambda ()
             ; Allow toggling
	     (local-set-key (kbd "C-l") 'visual-line-mode)
	     (when (or (not buffer-file-name)
		       (and (buffer-file-name)
			    (or (string-match "itsalltext" (buffer-file-name))
				(string-match "/tmp/tmp" (buffer-file-name)))))
	       (message "enabling visual-line-mode for web stuff")
	       (visual-line-mode 'nil))))

;; Enable mail-mode for mutt spawned files
(add-to-list 'auto-mode-alist '("/tmp/mutt-*" . mail-mode))
(add-to-list 'auto-mode-alist '(".*/\.git/\.gitsendemail.msg.*" . mail-mode))

(defun my-mail-mode-tweaks()
  "Customise mail-mode stuff"
  (interactive)
  (turn-on-auto-fill)
  (when (and buffer-file-name
	     (or 
	      (string-match "/tmp/mutt" buffer-file-name)
	      (string-match "gitsend" buffer-file-name)))
    (local-set-key (kbd "C-c C-c") 'server-edit)
    (local-set-key (kbd "C-c C-s") 'server-edit)))

(add-hook 'mail-mode-hook 'my-mail-mode-tweaks)

;; Python Mode
;
; TODO - automode alist
(autoload 'python-mode "python-mode")
(eval-after-load "python-mode"
  (load-library "my-python-mode"))

(message "Done various programming modes")


;; Buffer Selection
;
; Use lusty-explorer if I can, otherwise leave it to ido-mode which
; has been in emacs since version 22.
;
; Still have a bs-show "all" bound to C-x C-b for when I want to see
; everything

(require 'midnight)
(setq midnight-mode 't)

;; ido-mode - better buffer selection
(ido-mode t)
(setq ido-enable-flex-matching t)
(when (require 'ido-better-flex nil 'noerror)
  (ido-better-flex/enable))
(when (require 'ido-ubiquitous nil 'noerror)
  (ido-ubiquitous-mode))

;; but if we have lusty still use that...
(when (require 'lusty-explorer nil 'noerror)
  ;; overrride the normal file-opening, buffer switching
  (global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
  (global-set-key (kbd "C-x b")   'lusty-buffer-explorer))

;; ibuffer has been around for some time
(defun my-ibuffer-bs-show ()
  "Emulate `bs-show' from the bs.el package."
  (interactive)
  (ibuffer nil "*Ibuffer-my-bs*" '((filename . ".*")) nil t)
  (define-key (current-local-map) "a" 'ibuffer-bs-toggle-all))

(global-set-key (kbd "C-x C-b") 'my-ibuffer-bs-show)

(setq ibuffer-saved-filters
      (quote (("csrc" ((filename . "/export/csrc/*")))
	      ("tramp" ((filename . "\\/ssh:")))
	      ("irc" ((mode . erc-mode)))
	      ("magit" ((mode . magit-status-mode))) 
	      ("programming" ((or (mode . emacs-lisp-mode)
				  (mode . cperl-mode)
				  (mode . c-mode)
				  (mode . java-mode)
				  (mode . idl-mode)
				  (mode . lisp-mode)))))))

(message "Done Buffer Handling Tweaks")

(when I-am-at-work
  (setenv "DEBEMAIL" "ajb@cbnl.com")
  (setenv "DEBFULLNAME" "Alex Bennée"))
  
;; Lets use mark-tools if we can
(when (maybe-load-library "mark-tools")
  (global-set-key (kbd "C-x m") 'list-marks))

;;
;; IRC Stuff
;;

(if (locate-library "circe")
    (progn
      (autoload 'circe "circe" "Start CIRCE" t)
      (eval-after-load
	  "circe" (maybe-load-library "my-circe")))
  (when (locate-library "erc")
    (autoload 'erc-select "erc" "Start ERC" t)
    (eval-after-load
	"erc" (maybe-load-library "my-erc"))))


;;
;; Eshell tweaks
;;
(eval-after-load "eshell"
  '(progn
     (defun my-eshell-kill-output ()
       "Really kill (not delete) all output from interpreter since last input.
Does not delete the prompt."
       (interactive)
       (save-excursion
         (goto-char (eshell-beginning-of-output))
         (insert "*** output flushed ***\n")
         (kill-region (point) (eshell-end-of-output))))

     (add-hook 'eshell-mode-hook #'(lambda ()
                                     (define-key eshell-mode-map (kbd "C-c C-o") 'my-eshell-kill-output)))))

;; Finally enable desktop mode
; Stuff will be saved in current-project-root (i.e. cwd when emacs was invoked)

(unless (or will-start-server (daemonp))
  (when I-am-emacs-22+
    (setq desktop-dirname (concat (chomp (shell-command-to-string "pwd")))
	  desktop-save 'ask-if-new)
    (desktop-save-mode 1)))

;; Load any hand-made customisations
(when (file-exists-p custom-file)
  (load custom-file))

(message "Done .emacs")
