;;; my-package.el --- my package customisation
;;
;;; Commentary:
;;
;; Basic package configuration stuff including a list of used packages
;;
;;; Code:

(require 'package)

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
    helm-ack helm-descbinds
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


;; (add-to-list 'package-archives
;;              '("marmalade" .
;;              "https://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             (if I-am-at-work
                 '("melpa" . "http://melpa.org/packages/")
               '("melpa-stable" . "http://stable.melpa.org/packages/"))
             t)
             
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

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

(provide 'my-package)
;;; my-package.el ends here

