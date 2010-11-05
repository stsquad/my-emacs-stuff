;; OpenNMS build tweaks
;

(require 'my-c-mode)

(message "Loading OpenNMS customisations")

;; JDEE requires CEDET, however bleeding edge emacs packages
; its own version.
(if (> emacs-major-version 23)
    (require 'cedet)
  (unless (featurep 'cedet) 
	(if (file-exists-p "/export/src/cedet.cvs/common/cedet.el")
	    (load-file "/export/src/cedet.cvs/common/cedet.el"))))

;;(add-to-list 'load-path (expand-file-name
;;			 "/export/src/jdee/jdee-2.4.0.1/lisp"))
;;
;;(require 'jde)
;;
;; (jde-set-variables
;;  '(jde-jdk-registry '(("1.6.1" . "/usr/lib/jvm/java-6-openjdk/")))
;;  '(jde-jdk (quote ("1.6.1"))))

; Java stuff
; (OpenNMS standards http://www.opennms.org/index.php/Code_conventions)

(defconst my-onms-style
  '((indent-tabs-mode . nil)
    (tab-width . 4)
    (c-basic-offset . 4))
  "Java style for OpenNMS")

(c-add-style "my-onms-style" my-onms-style)

(setq my-c-styles-alist (cons '(".*opennms.*java$" . my-onms-style) my-c-styles-alist))

(add-hook 'java-mode-hook (lambda () 
			    (c-set-style "my-onms-style")))

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "<f3>")  'compile)

; only for old style...
(if (boundp 'current-project-root)
    (setq compile-command (format
			   "cd %s && ./build.sh install assembly:directory-inline"
			   current-project-root)))
