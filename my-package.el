;;; my-package.el --- my package customisation
;;
;;; Commentary:
;;
;; Basic package configuration stuff including a list of used packages
;;
;;; Code:

(require 'package)
(require 'my-vars)

(defvar my-essential-packages
  '(use-package)
  "List of essential packages.")

(defvar my-useful-packages
  '(ace-jump-mode ace-jump-buffer
    circe
    elpy
    expand-region
    flycheck
    git-messenger
    gitconfig-mode
    gitignore-mode
    golden-ratio
    helm
    helm-ag helm-descbinds
    helm-swoop helm-themes
    htmlize
    hydra
    js2-mode
    json-mode
    keychain-environment
    keyfreq
    litable
    lusty-explorer
    magit
    markdown-mode markdown-mode+
    mediawiki
    mc-extras multiple-cursors
    org ox-reveal
    use-package
    smart-mode-line
    smartparens
    solarized-theme
    swiper
    tangotango-theme
    tracking
    web-mode
    yasnippet
    zenburn-theme)
  "List of packages I use a lot.")

(defun my-have-package-p (pkg)
  "Check if `PKG' is installed manually or via package."
  (or (package-installed-p pkg)
      (require pkg nil t)))

(defun my-check-and-maybe-install (pkg)
  "Check and potentially install `PKG'."
  (when (not (package-installed-p pkg))
    (when (not (require pkg nil t))
      (ignore-errors
	(package-install pkg)))))

(defun my-packages-reset()
  "Reset package manifest to the defined set."
  (interactive)
  (package-refresh-contents)
  (mapc 'my-check-and-maybe-install my-useful-packages))

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
   '("melpa" . "http://melpa.org/packages/") t))

(add-to-list
 'package-archives
 '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list
 'package-archives
 '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Ensure we have the minimum set required
(when (not (cl-reduce
            (lambda (a b) (and a b))
            (mapcar 'my-have-package-p my-essential-packages)))
  (package-refresh-contents)
  (ignore-errors
    (mapc 'package-install my-essential-packages)))

(provide 'my-package)
;;; my-package.el ends here

