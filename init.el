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
;(eval-when-compile (defvar god-local-mode))
(declare-function my-add-config-paths "~/.emacs.d/my-elisp/my-paths" t t)
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

(require 'my-keybinds)

(require 'use-package nil t)

(when (fboundp 'use-package)

  (require 'my-basic-modes)
  (require 'my-display)

  (use-package edit-server
    :if (and window-system (daemonp) (not (= 0 (user-uid))))
    :commands edit-server-start
    :idle (edit-server-start)
    :config (load-library "my-edit-server.el"))

  (use-package async
    :commands ido-dired dired
    :config
    (when (require 'dired-aux)
      (require 'dired-async)))

  ;; Stuff I always want
  ;; email
  (load-library "my-email")
  ;; Development related stuff, including project root
  (load-library "my-devel")
  (load-library "my-flycheck")
  (load-library "my-web")
  (load-library "my-elisp")
  (load-library "my-python")
  ;; Org configuration
  (load-library "my-org")
  ;; Helm
  (load-library "my-helm")
  ;; More keybindings
  ;; (load-library "my-toggles")
  (require 'my-toggles)
  ;; Window and buffer navigation
  (load-library "my-windows")
  (load-library "my-buffer")

  ;; Locally installed pkgs
  (load-library "my-local-pkgs")

  ;; Useful modes
  (load-library "my-company")
  (load-library "my-yasnippet")

  ;; other customisations
  (load-library "my-tramp")
  (load-library "my-spell")
  (load-library "my-gpg")
  (load-library "my-git")
  (load-library "my-htmlize")
  (load-library "my-eshell")
  (load-library "my-circe")
  (load-library "my-diff")
  
  ;; Nice for jumping about windows.
  (use-package ace-jump-mode
    :bind ("C-x j" . ace-jump-mode))

  ;; Multiple cursors
  (use-package multiple-cursors
    :bind (( "C->" . mc/mark-next-like-this)
           ( "C-<" . mc/mark-previous-like-this)
           ( "C-x ;" . mc/mark-all-like-this-dwim)
           ( "C-+" . mc/mark-all-like-this-dwim)
           ( "M-+" . mc/edit-lines)))

  ;; Expand region
  (use-package expand-region
    :bind ("C-=" . er/expand-region))

  ;; Learn key strokes
  (use-package guide-key
    :commands guide-key-mode
    :idle (guide-key-mode 1)
    :diminish ""
    :config
    (setq guide-key/guide-key-sequence
          '("C-x C-k" "C-x c" "C-x t" "C-x n" "ESC" "C-x r" "C-x 4" "C-x 8")))

  ;; God-mode
  (use-package god-mode
    :commands god-mode-all
    :requires my-toggles
    :init (define-key my-toggle-map "g" 'god-mode-all)
    :config
    (progn
      (defun my-update-god-cursor ()
        "Update the cursor style depending on status of god-mode."
        (setq cursor-type (if (or god-local-mode buffer-read-only)
                              'hollow
                            'box)))
      (add-hook 'god-mode-disabled-hook 'my-update-god-cursor)
      (add-hook 'god-mode-enabled-hook 'my-update-god-cursor)))

  ;; Lets use mark-tools if we can
  (use-package mark-tools
    :bind ("C-x m" . list-marks))

  (use-package paradox
    :ensure paradox
    :commands paradox-list-packages
    :config
    (setq paradox-github-token (my-pass-password "paradox" t)))
  
  (load "the-rest.el"))

;; Finished loading

(message "Done .emacs")

(setq I-completed-loading-dotinit 't)
(when (fboundp 'benchmark-init/deactivate)
  (benchmark-init/deactivate)
  (require 'benchmark-init-modes))

(provide 'init)
;;; init.el ends here
