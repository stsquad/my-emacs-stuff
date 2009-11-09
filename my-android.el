;;
;; My Android customisations
;;

(require 'android-mode)

(setq android-mode-sdk-dir "/home/alex/android-sdk-linux_x86-1.6_r1")
(setq android-mode-avd "test")

(setq compile-command (format "cd %s && ant compile"
				  current-project-root))
      
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "<f3>")  'compile)

