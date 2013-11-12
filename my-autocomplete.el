;;
;; Auto Complete Settings
;;

(require 'auto-complete)
(require 'auto-complete-config)

(setq ac-use-menu-map 't)
(ac-set-trigger-key "M-/") ; override dabrev-expand
(add-to-list 'ac-modes 'org-mode)
(ac-config-default)
(setq-default ac-sources
              '(ac-source-yasnippet
                ac-source-abbrev
                ac-source-dictionary
                ac-source-words-in-same-mode-buffers))
