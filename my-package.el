;;; my-package.el --- my package customisation
;;
;;; Commentary:
;;
;; Basic package configuration stuff including a list of used packages
;;
;;; Code:

(require 'package)
(require 'my-vars)
(require 'cl)

(defun my-package-recompile()
  "Recompile all packages"
  (interactive)
  (byte-recompile-directory "~/.emacs.d/elpa" 0 t))

;;
;; Always run on loading
;;

;; Setup packages
(when I-am-at-work
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/") t))

(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list
 'package-archives
 '("org" . "https://orgmode.org/elpa/") t)

;; Pin use-package
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((use-package . "melpa-stable"))))

(package-initialize)

;; Remove Org-mode that was shipped with Emacs
;
; This seems to be a case of something triggering a load from the
; original when we should be getting everything from the ELPA version.
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

; and force installation, as :ensure t will not
(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  (message "No org-mode installed, things will likey fail"))
;  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'org))

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

(require 'use-package)

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

