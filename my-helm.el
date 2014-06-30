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

(require 'helm-config)
(require 'helm-utils)

;; Helm customisations
(setq helm-yank-symbol-first 't)

;; Better M-x?
; one thing it does do is show key-bindings as it narrows
(when (fboundp 'helm-M-x)
  (global-set-key (kbd "M-x") 'helm-M-x))

;; iMenu find
(if (fboundp 'helm-imenu)
    (global-set-key (kbd "C-f") 'helm-imenu)
  (global-set-key (kbd "C-f") 'imenu))

;; Search key bindings for current mode
(when (fboundp 'helm-descbinds)
  (global-set-key (kbd "C-h h") 'helm-descbinds))

(when (locate-library "helm-gtags")
    ;; Enable helm-gtags-mode
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; Set key bindings
    (eval-after-load "helm-gtags"
      '(progn
         (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
         (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
         (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;         (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
         (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
         (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
         (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))))

;; Occur stuff
(cond
 ((fboundp 'helm-swoop)
  (global-set-key (kbd "C-c o") 'helm-swoop))
 ((fboundp 'helm-occur)
  (global-set-key (kbd "C-c o") 'helm-occur))
 (t (global-set-key (kbd "C-c o") 'occur)))

;; Helm git grep
(setq helm-git-grep-candidate-number-limit nil)

(provide 'my-helm)
;;; my-helm.el ends here
