;;
;; YASnippet setup
;;

(require 'yasnippet)

(when (file-exists-p "~/.emacs.d/my-snippets")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/my-snippets"))

;; TODO add helper functions

(yas-global-mode)
