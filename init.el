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

;; Use .el if it is newer
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

;; Manually load this (as paths not yet set)
(declare-function my-add-config-paths "~/.emacs.d/my-elisp/my-paths" t t)
(when (load-library "~/.emacs.d/my-elisp/my-paths")
  (my-add-config-paths))

;;;; Start of real code.

;; Find out about my environment
(require 'my-vars)

;; For auto-testing
(defvar I-completed-loading-dotinit 'nil
  "Flag indicating succesful start-up.")

;; Packaging
;; (package-initialize) is called in here.
(load-library "my-package.el")

;; Add ~/.emacs.d/*.git project into search path
;; They are added in-front of all other paths so will override
;; ELPA/MELPA versions
(when (fboundp 'my-add-git-project-paths)
  (my-add-git-project-paths))

(require 'my-config)

(require 'my-keybinds)

(eval-when-compile
  (require 'use-package))

(use-package my-libs)
(use-package my-basic-modes)

;; Load any hand-made customisation
;; we do this early to prevent problems with theme safety and the like
(when (file-exists-p custom-file)
  (load custom-file))

(use-package my-display)
(use-package my-modeline)

;; We only need one edit-server at a time really
(use-package my-web)

(when (or (and (getenv "DISPLAY") (daemonp) (not I-am-root))
          I-am-on-pixelbook)
  (when (locate-library "atomic-chrome")
    (use-package my-atomic-chrome))
  (use-package my-edit-server))

;; Stuff I always want
;; general editing
(use-package my-editing)
(use-package my-hydra)

;; email
(use-package my-email
  :if (and (file-accessible-directory-p "~/Maildir")
           (not I-am-root)))
;; gnus for backup
(use-package my-gnus
  :if (not I-am-root))

;; Development related stuff, including project root
(use-package my-devel)

(use-package my-flycheck
  :if (version<= "24.4" emacs-version))

(use-package my-elisp)
(use-package my-python)

;; Org configuration
(use-package my-org
  :if (not I-am-root))
;; Helm
(use-package my-helm)
;; More keybindings
(use-package my-toggles)
;; Window and buffer navigation
(use-package my-windows)
(use-package my-buffer)
(use-package my-dired)
(use-package my-keyhelp)

;; Locally installed pkgs
(use-package my-local-pkgs)

;; Useful modes
(use-package my-company)
(use-package my-yasnippet)

;; other customisations
(use-package my-tramp)
(use-package my-spell)
(use-package my-gpg)

(use-package my-git
  :if (version<= "25.1" emacs-version))

(use-package my-htmlize)
(use-package my-eshell)

(use-package my-circe
  :if (not I-am-root))

(use-package my-diff)

(use-package my-transmission
  :if (string-match "seed" (system-name)))

;; Finished loading

(setq I-completed-loading-dotinit 't)
(message "Done .emacs")

(provide 'init)
;;; init.el ends here
