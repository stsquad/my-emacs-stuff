;;; my-helm --- Customisation of helm
;;
;;; Commentary:
;;
;; Helm provides a fairly comprehensive narrowing framework for
;; selecting from a list of options.  The actions are quite
;; comprehensive and reasonably well documented.  But it is a lot to get
;; your head around.
;;
;;; Code:

(require 'use-package)

(use-package helm
  :ensure t
  :init (progn
          (require 'helm-config)
          (setq helm-yank-symbol-first t
                helm-idle-delay 0.0
                helm-input-idle-delay 0.01
                helm-quick-update t
                helm-M-x-requires-pattern nil
                helm-ff-skip-boring-files t)))

(use-package helm-buffers
  :ensure helm
  :commands (helm-buffers-list helm-mini)
  :config (setq helm-buffers-fuzzy-matching t
                helm-mini-default-sources '(helm-source-buffers-list
                                            helm-source-recentf
                                            helm-source-bookmarks)))

(use-package helm-elisp
  :bind ("C-h a" . helm-apropos))

(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)
         ("C-h h" . helm-descbinds)))

(use-package helm-themes
  :ensure t)

(provide 'my-helm)
;;; my-helm.el ends here
