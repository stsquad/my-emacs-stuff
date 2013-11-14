;;
;; YASnippet setup
;;

(require 'yasnippet)

(when (file-exists-p "~/.emacs.d/my-snippets")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/my-snippets"))

(setq yas-prompt-functions
      '(yas-ido-prompt yas-completing-prompt yas-no-prompt))

;; TODO add helper functions

(yas-global-mode)
