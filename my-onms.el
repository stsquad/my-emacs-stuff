;; OpenNMS build tweaks
;

(message "Loading OpenNMS customisations")

(setq compile-command (format
		       "cd %s && ./build.sh install assembly:directory-inline"
		       current-project-root))
		   
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "<f3>")  'compile)
