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

(use-package helm-config
  :bind (("M-x" . helm-M-x)
         ("C-f" . helm-semantic-or-imenu)
         ("C-<f1>" . helm-apropos))
  :config
  (progn
    ;; Vars
    (setq helm-yank-symbol-first 't
          helm-git-grep-candidate-number-limit nil)))

;; ;; helm buffers?
;; ;; Currently undecided - I like lusty but with lots of identical
;; ;; buffer names helm-buffers-list works better.
;; (when (fboundp 'helm-buffers-list)
;;   (global-set-key (kbd "C-x b")   'helm-buffers-list))

(use-package helm-gtags
  :commands helm-gtags-mode
  :config
  (progn
    ;; keys
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
  :bind ("C-c o" . helm-swoop))

(use-package helm-descbinds
  :bind ("C-h h" . helm-descbinds))

;; Helm git grep

(provide 'my-helm)
;;; my-helm.el ends here
