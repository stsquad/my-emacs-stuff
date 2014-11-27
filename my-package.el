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
    edit-server edit-server-htmlize
    elpy
    eproject
    expand-region
    flycheck
    flycheck-tip
    git-blame
    git-commit-mode
    git-messenger
    gitconfig-mode
    gitignore-mode
    guide-key
    helm
    helm-ack helm-git-grep helm-descbinds helm-c-yasnippet
    helm-swoop helm-themes
    htmlize
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
    solarized-theme
    tangotango-theme
    tracking
    web-mode
    yasnippet
    zenburn-theme)
  "List of packages I use a lot.")


(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(defun my-check-and-maybe-install (pkg)
  "Check and potentially install `PKG'."
  (when (not (package-installed-p pkg))
    (when (not (require pkg nil t))
      (message "Installing: %s" pkg)
      (package-install pkg)
      (message "Installed: %s" pkg))))

(defun my-packages-reset()
  "Reset package manifest to the defined set."
  (interactive)
  (package-refresh-contents)
  (mapc 'my-check-and-maybe-install my-useful-packages))

(provide 'my-package)
;;; my-package.el ends here
