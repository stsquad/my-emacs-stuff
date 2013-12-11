;;; my-package.el --- my package customisations
;;
;;; Commentary:
;;
;; Basic package configuration stuff including a list of used packages
;;
;;; Code:

(require 'package)
(require 'package+ nil t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'package+)
  (package-refresh-contents)
  (package-install 'package+))

;; This is dangerous to call in init.el as it will remove
;; all packages not explicitly in the manifest. This may be helpful
;; to keep packages clean but it breaks stuff you might be playing
;; with between boots.
(defun my-packages-reset()
  "Reset package manifest to the defined set"
  (interactive)
  (package-refresh-contents)
  (package-manifest 'ac-c-headers 'ac-dabbrev 'ac-helm 'ac-js2
                    'ace-jump-mode
                    'ack-and-a-half
                    'android-mode
                    'apache-mode
                    'auto-complete
                    'backtrace-mode
                    'circe
                    'dynamic-fonts
                    'edit-server
                    'edit-server-htlmize
                    'elpy
                    'edebug-x
                    'eproject
                    'emms
                    'expand-region
                    'flycheck
                    'flycheck-tip
                    'git-blame
                    'git-commit-mode
                    'gitconfig-mode
                    'gitignore-mode
                    'gplusify
                    'guide-key
                    'helm
                    'helm-ack
                    'helm-git-grep
                    'helm-c-yasnippet
                    'helm-themes
                    'htmlize
                    'ido-ubiquitous
                    'ido-vertical-mode
                    'js2-mode
                    'json-mode
                    'keychain-environment
                    'keyfreq
                    'litable
                    'lusty-explorer
                    'magit
                    'markdown-mode
                    'markdown-mode+
                    'mediawiki
                    'mc-extras
                    'multiple-cursors
                    'org
                    'org-trello
                    'ox-reveal
                    'package+
                    'paredit
                    'pastebin
                    'projectile
                    'protobuf-mode
                    'rainbow-delimiters
                    'smart-mode-line
                    'smex
                    'ssh-config-mode
                    'solarized-theme
                    'syslog-mode
                    'tracking
                    'web-mode
                    'yasnippet
                    'yaml-mode
                    'zenburn-theme))

(defun my-install-additional-pkgs ()
  "Install non-core packages that are needed for testing."
  (unless (require 'edit-server nil t)
    (package-refresh-contents)
    (package-install 'edit-server)))

(provide 'my-package)
;;; my-package.el ends here
