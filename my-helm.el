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
