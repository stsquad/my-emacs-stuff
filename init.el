;;; init.el --- Alex Bennée's .emacs
;;
;;; Commentary:
;;
;; This is my Emacs, there are many like it but this is my own.
;; It is mainly an amalgem of different hacks acquired over time
;; which I use on many of the machines I work with.
;;
;; It has a cobbled heritage from various sites and wikis and it is
;; probably safest to assume the code is all either GPL or Public
;; Domain.  Feel free to use whatever bits may be of use to you in these
;; files that it is my right to license ;-)
;;
;;; Code:

; debugging weird start-up issues.
;(setq debug-ignored-errors (remq 'user-error debug-ignored-errors))
;(setq debug-on-error 't)

(defvar my-config-root
  "~/.emacs.d"
  "Where all my config files are kept.")

(when (and (file-exists-p my-config-root)
           (file-directory-p my-config-root))
  (add-to-list 'load-path my-config-root))

;;;; Start of real code.

;; Find out about my environment
(require 'my-vars)

;; For auto-testing
(defvar I-completed-loading-dotinit 'nil
  "Flag indicating succesful start-up.")

;; Packaging, if we have it
(when (or I-am-emacs-24+
          (require 'package "package" t))
  (load-library "my-package.el"))

;; Add local search path
;
; This adds everything ~/.emacs.d/*.git that's not elpa related
; to the start of the load-path
;
(mapc #'(lambda (f)
          (let ((default-directory f))
            (setq load-path
                  (append
                   (let ((load-path (copy-sequence load-path))) ;; Shadow
                     (append
                      (copy-sequence (normal-top-level-add-to-load-path '(".")))
                      (normal-top-level-add-subdirs-to-load-path)))
                   load-path))))
      (directory-files my-config-root 't "\.git$"))

;;
;; Basic config variables
;;

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Default mode is text-mode,
(setq-default major-mode 'text-mode)

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

(eval-after-load 'tramp
  (load-library "my-tramp.el"))

;; Move the custom file out of init.el
(setq custom-file "~/.emacs.d/my-custom.el")



;; maybe-load-library
;
; A little less than using (require 'lib) - but require has optional args

(defun maybe-load-library (libname)
  "Try and load library LIBNAME if it is in the path."
  (when (locate-library libname)
    (load-library libname)))

;; Do we want an edit-server?
(when (and (daemonp)
           (require 'edit-server nil t))
  (load-library "my-edit-server.el"))

;;
;; Load any global modes/extensions that are used throughout emacs.
;; This includes snippets and auto-completion libraries.
;;

;; Expansion
; This gets over-ridden when auto-complete is in effect
(global-set-key (kbd "M-/") 'hippie-expand)

;; Do we have snippets?
(when (require 'yasnippet nil t)
  (load-library "my-yasnippet.el"))

(when (require 'auto-complete nil t)
  (load-library "my-autocomplete"))

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
; although C-x 8 ' e inserts é
(defun insert-myname ()
  "Insert my name, accents and all."
  (interactive)
  (insert (format "Alex Bennée")))

(global-set-key (kbd "C-x 8 e") 'insert-myname)
;perhaps this is better solved with a snippet?
;(global-set-key (kbd "C-M-e") 'insert-myname)

;;
;; Need a better way to do this as fortune is on different places
;; on different machines.
;;
(defun fortune ()
  "Famous unix fortune teller."
  (shell-command-to-string "/usr/bin/fortune"))

(require 'my-utils)

;; Load sub-modules
;
; email
(load-library "my-email")
; Development related stuff, including project root
(load-library "my-devel")
; Org configuration
(when (fboundp 'org-mode)
  (maybe-load-library "my-org"))

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
  "Toggle whether the current active window is dedicated or not."
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
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
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

(when (require 'helm nil t)
  (load-library "my-helm.el"))

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
; tweaked by adding an interactive function to do the keys

(defun my-tweak-macos-keys ()
  "Tweak keys when on MacOS machines."
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
(global-set-key (kbd "<f2>") 'undo)

;; In Emacs 21+, home and end go to beginning and end of line. This is
;; clearly the Wrong Thing.
(when I-am-emacs-21+
  (global-unset-key [home])
  (global-set-key [home] 'beginning-of-buffer)
  (global-unset-key [end])
  (global-set-key [end] 'end-of-buffer))

;; Macro keys
; If I define a single press macro keys I may use them more often

(global-set-key [(control meta m)] 'call-last-kbd-macro)
(global-set-key (kbd "<f10>") 'call-last-kbd-macro)
(global-set-key (kbd "<C-f11>") 'start-kbd-macro)
(global-set-key (kbd "<C-f12>") 'end-kbd-macro)

; This stops warnings with re-mapped
; return (when using xcape)
(global-set-key (kbd "<key-4660>") 'ignore)

;; insert-sequence-key
;
; Handy little key sequence utility so I don't have to guess at the
; correct kbd incantation

(defun insert-sequence-key (key)
  "Insert a string formating KEY suitable for use in fcns like `global-set-key'."
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

;; Learn key strokes
(defvar guide-key/guide-key-sequence)
(when (require 'guide-key nil 't)
  (setq guide-key/guide-key-sequence
        '("C-x c" "C-x n" "ESC" "C-x r" "C-x 4" "C-x 8"))
  (guide-key-mode 1))

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
(winner-mode t)

;; Disable the menu and tool bars, they just take up space.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use imagemagick if we have it to view images
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))


(declare-function crmbk-running-in-host-x11-p "chromebook")
(defvar crmbk-frame-mode-map)

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
  (setq-default display-buffer-reuse-frames t)

  ; messing about - what about dynamic-font stuff?
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 140
                      :weight 'normal
                      :width 'normal)
  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      'unicode
                      (font-spec :family "DejaVu Sans Mono"
                                 :width 'normal
                                 :size 12.4
                                 :weight 'normal))))

(ignore-errors
  (load-theme 'zenburn t))

(message "Display Done")

;; Prettier unique buffer names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

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
(setq eldoc-minor-mode-string nil)

;; (display-time) is needed for appt to display in the mode-line, but
;; we don't want the time taking up precious space.
(require 'time)
(setq display-time-interval 20
      display-time-format 'nil
      display-time-string-forms '( 24-hours ":" minutes ))
(display-time-mode)

;; Displays current function() in programming modes.
(when (require 'which-func nil t)
  (which-function-mode))

;; Reduce white space
(setq-default mode-line-buffer-identification '("%b"))

(when (require 'smart-mode-line nil t)
  (sml/setup))

(when (require 'tracking nil t)
  (tracking-mode))

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

;; I hate tabs - they are set in cc-mode but not everything respects that
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

; TODO: clean-up my defaults for this
(defvar whitespace-style)
(when (require 'whitespace nil t)
  (setq whitespace-style '(face
                           tabs trailing lines-tail empty
                           space-after-tab tab-mark))
  (global-set-key (kbd "C-c w") 'whitespace-mode))

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

(when (require 'ispell 'nil t)
  (load-library "my-spell.el"))

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
(when (require 'keychain-environment nil t)
  (keychain-refresh-environment))

;; (when (maybe-load-library "epa-file")
;;   (setenv "GPG_AGENT_INFO" nil) ; gpg-agent confuses epa when getting passphrase
;;   (epa-file-enable))

;; my-find-binary
;
; Handy for dumping objdump into a buffer
(when (locate-library "my-find-binary")
    (autoload 'find-binary-file "my-find-binary"))

; I like to use .git/.bzr etc in my directory names
(setq completion-ignored-extensions
      (remove ".git/"
              (remove ".bzr/"
                      (remove ".svn/" completion-ignored-extensions))))

(when (require 'magit nil t)
  (load-library "my-git.el"))

;; Dired stuff
(add-hook 'dired-mode-hook
          (lambda ()
            (setq truncate-lines t)))

;; Web Development Modes
;
; I used to use nxhtml, now I use web-mode

(when (maybe-load-library "js2-mode")
  (defalias 'javascript-mode 'js2-mode "js2-mode is aliased to javascript mode"))

(when (require 'htmlize nil t)
  (load-library "my-htmlize.el"))

;; Elisp mode
;
; I keep this in the main .emacs as I edit .emacs quite a bit

; auto-mode lists
(setq auto-mode-alist
      (append (list
               '("\\.emacs\\'" . emacs-lisp-mode)
               '("dotemacs" . emacs-lisp-mode))
              auto-mode-alist))

(defun my-elisp-compile-buffer ()
  "Compile the current buffer"
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun my-elisp-hook-functions ()
  "A few quick elisp hook customisations"
  (setq mode-name "elisp")
  (eldoc-mode t)
  (local-set-key (kbd "C-c C-c") 'my-elisp-compile-buffer)
  (turn-on-auto-fill))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-hook-functions)

; For most web-forms I want longlines-mode by default
;
; It's All Text: /home/ajb/.mozilla/firefox/hgd0onxt.default/itsalltext/.2e2i2y3b2c.txt
; Emacs Chrome: /tmp/tmpcUbYA_.txt

(defun my-text-mode-hook ()
  "My local setting"
  (local-set-key (kbd "C-l") 'visual-line-mode)
  (local-set-key (kbd "C-f") 'auto-fill-mode))

(add-hook 'text-mode-hook 'my-text-mode-hook)

;;
;; Simple mail-mode and message-mode hooks.
;;
;; Ostensibly they both do the same thing however message-mode (and
;; the derived mu4e-compose-mode) assume they are sending from within
;; emacs. So I'll use the convention that I'll use mail-mode for
;; edit-server spawned mails and message-mode for the rest
;;

;; Enable mail-mode for mutt spawned files
(add-to-list 'auto-mode-alist '("/tmp/mutt-*" . mail-mode))
(add-to-list 'auto-mode-alist '("0000-cover-letter.patch" . mail-mode))
(add-to-list 'auto-mode-alist '(".*/\.git/\.gitsendemail.MSG.*" . mail-mode))

(defun my-common-mail-tweaks ()
  "Enable common mail tweaks for sending messages"
  (interactive)
  (turn-on-flyspell)
  (turn-on-auto-fill))

(defun my-mail-mode-tweaks()
  "Customise mail-mode stuff"
  (interactive)
  (my-common-mail-tweaks)
  (when (and
         buffer-file-name;
         (or
          (string-match "/tmp/mutt" buffer-file-name)
          (string-match "gitsend" buffer-file-name)))
    (define-key (current-local-map) (kbd "C-c C-c") 'server-edit)
    (define-key (current-local-map) (kbd "C-c C-s") 'server-edit)))

(add-hook 'mail-mode-hook 'my-mail-mode-tweaks)
(add-hook 'message-mode-hook 'my-common-mail-tweaks)

;; Python Mode
;
; TODO - automode alist
(add-hook 'python-mode-hook #'(lambda () (require 'my-python-mode)))

(message "Done various programming modes")

(require 'my-buffer)

(when I-am-at-work
  (setenv "DEBEMAIL" "alex.bennee@linaro.org")
  (setenv "DEBFULLNAME" "Alex Bennée"))

;; Lets use mark-tools if we can
(when (maybe-load-library "mark-tools")
  (global-set-key (kbd "C-x m") 'list-marks))

;;
;; IRC Stuff
;;

(when (locate-library "circe")
  (autoload 'circe "circe" "Start CIRCE" t)
  (eval-after-load
      "circe" (maybe-load-library "my-circe")))

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

;; Load any hand-made customisations
(when (file-exists-p custom-file)
  (load custom-file))

(message "Done .emacs")
(setq I-completed-loading-dotinit 't)

(provide 'init)
;;; init.el ends here
