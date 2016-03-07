;;; my-helm --- Customisation of helm
;;
;;; Commentary:
;;
;; Helm provides a fairly comprehensive narrowing framework for
;; selecting from a list of options. The actions are quite
;; comprehensive and reasonably well documented. But it's a lot to get
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
                helm-ff-skip-boring-files t))
  :bind (("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-f" . helm-semantic-or-imenu)
         ("C-x 8 <RET>" . helm-ucs)
         ("C-<f1>" . helm-apropos)))

(use-package helm-buffers
  :ensure helm
  :commands helm-buffers-list
  :config (setq helm-buffers-fuzzy-matching t))

(use-package helm-elisp
  :bind ("C-h a" . helm-apropos))

(use-package helm-git-grep
  :ensure t
  :if (locate-library "helm-git-grep")
  :commands helm-git-grep
  :config (setq helm-git-grep-candidate-number-limit nil))

(use-package helm-gtags
  :ensure t
  :commands (helm-gtags-mode helm-gtags-dwim)
  :diminish "HGt"
  :config
  (progn
    ;; keys
    (define-key helm-gtags-mode-map (kbd "C-c f") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;; Enable helm-gtags-mode in code
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(use-package helm-swoop
  :ensure t
  :bind (("C-c o" . helm-swoop)
         ("C-c O" . helm-multi-swoop)))

(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)
         ("C-h h" . helm-descbinds)))

;; Helm git grep

(provide 'my-helm)
;;; my-helm.el ends here
