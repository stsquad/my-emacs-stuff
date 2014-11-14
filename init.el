
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

;; Manually load this (as paths not yet set)
(when (load-library "~/.emacs.d/my-elisp/my-paths")
  (my-add-config-paths))

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

;; Add ~/.emacs.d/*.git project into search path
;; They are added in-front of all other paths so will override
;; ELPA/MELPA versions
(when (fboundp 'my-add-git-project-paths)
  (my-add-git-project-paths))

;;;; Benchmarking - only occurs if setup
(when (require 'benchmark-init nil t)
  (benchmark-init/activate))

(require 'my-config)

;; Load any hand-made customisation
;; we do this early to prevent problems with theme safety and the like
(when (file-exists-p custom-file)
  (load custom-file))

(require 'my-basic-modes)

(when (require 'use-package nil t)

  (use-package tramp
    :init
    (load-library "my-tramp.el"))

  (use-package edit-server
    :if (and window-system (daemonp) (not (= 0 (user-uid))))
    :init
    (progn
      (add-hook 'after-init-hook 'edit-server-start t)
      (load-library "my-edit-server.el")))

  (use-package async
    :commands ido-dired dired
    :init
    (when (require 'dired-aux)
      (require 'dired-async)))

  (require 'my-local-pkgs)
  

  (load "the-rest.el"))

;; Finished loading

(message "Done .emacs")

(setq I-completed-loading-dotinit 't)
(when (fboundp 'benchmark-init/deactivate)
  (benchmark-init/deactivate)
  (require 'benchmark-init-modes))

(provide 'init)
;;; init.el ends here
