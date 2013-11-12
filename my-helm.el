;;
;; Helm and it's related packages
;;
(require helm)

;; Helm customisations
(setq helm-yank-symbol-first 't)

;; iMenu find
(if (fboundp 'helm-imenu)
    (global-set-key (kbd "C-f") 'helm-imenu)
  (global-set-key (kbd "C-f") 'imenu))

; Occur stuff
(if (fboundp 'helm-occur)
    (global-set-key (kbd "C-c o") 'helm-occur)
  (global-set-key (kbd "C-c o") 'occur))
