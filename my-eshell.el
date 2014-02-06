;;; my-eshell.el --- eshell configuration
;;
;;; Commentary:
;;
;; Just basic stuff for now
;;
;;; Code:

(require 'eshell)
(require 'em-hist)

(defun my-eshell-search-history ()
  "Prompt for shell history."
  (interactive)
  (insert
   (ido-completing-read "Eshell history: "
                        (delete-dups
                         (ring-elements eshell-history-ring)))))


(defun my-eshell-kill-output ()
       "Really kill (not delete) all output from interpreter since last input.
Does not delete the prompt."
       (interactive)
       (save-excursion
         (goto-char (eshell-beginning-of-output))
         (insert "*** output flushed ***\n")
         (kill-region (point) (eshell-end-of-output))))


;; eshell-mode-map only exists in eshell buffers so we
;; add our bindings there.
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-o") 'my-eshell-kill-output)
              (define-key eshell-mode-map (kbd "C-r") 'my-eshell-search-history)))

(provide 'my-eshell)
;;; my-eshell.el ends here
