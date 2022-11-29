;;; my-package.el --- my package customisation
;;
;;; Commentary:
;;
;; Basic package configuration stuff including a list of used packages
;;
;;; Code:

(require 'package)
(require 'my-vars)
(eval-when-compile (require 'cl-lib))

(defun my-package-recompile(&optional dir)
  "Recompile all packages in elpa or optionally override with DIR."
  (interactive "D")
  (byte-recompile-directory (or dir "~/.emacs.d/elpa") 0 t))

;;
;; Always run on loading
;;

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Setup packages
(when I-am-at-work
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/") t))

(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar have-melpa
  (assoc "melpa" package-archives)
  "Do we have melpa on this machine?")

;; Pin use-package
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((use-package . "melpa-stable"))))

(package-initialize)

;; Workaround https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version<= "26.0" emacs-version) (version<= emacs-version "26.3"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(when (version<= "24.4" emacs-version)
  ;; Advise installs to refresh
  ;; see: https://github.com/jwiegley/use-package/issues/256
  (defun my-package-install-refresh-contents (&rest args)
    "Advice wrapper for `package-install' to ensure list upto date.
This ensures we update the package list at least once when a new
package is installed programatically."
    (package-refresh-contents)
    (advice-remove 'package-install 'my-package-install-refresh-contents))

  (advice-add 'package-install :before 'my-package-install-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package async
  :ensure t
  :init (async-bytecomp-package-mode))

;; native-compile support
(when (version<= "28.0.50" emacs-version)
  (use-package comp
    :init (setq comp-deferred-compilation (daemonp)
                ;; fixme, very much zen specific
                native-comp-compiler-options '("-O2" "-march=haswell" "-mtune=native"))))

(when (version<= "24.4" emacs-version)
  (use-package paradox
    :ensure t
    :commands paradox-list-packages
    :config
    (when (my-primary-machine-p)
      (setq paradox-github-token (my-pass-password "paradox" t)
            paradox-execute-asynchronously nil))))

(provide 'my-package)
;;; my-package.el ends here

