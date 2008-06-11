;; My Web Mode Customisations
;
; This contains mode customisations for web related work (which
; includes PHP, JSP and plain old HTML mode). It is loaded dynamically
; when I need to edit such files.
;
; It uses mmm-mode to deal with complexities of things like code
; within HTML syntax
;

(message "Starting my-web-mode.el customisation")

; Ensure we have the programming modes we want

(require 'html-helper-mode)
(require 'my-c-mode)  ; brings in cc-mode for java-mode
(require 'css-mode)   ; for css
(require 'php-mode)   ; for php

; We need mmm-mode (Multiple Major Modes)

(require 'mmm-mode)
(require 'mmm-sample)

(setq mmm-global-mode 'maybe)

(set-face-background 'mmm-default-submode-face "DimGray")

; reset the alist
;(setq mmm-mode-ext-classes-alist nil)

;; set up an mmm group for editing JSP pages

(mmm-add-group
 'jsp-pages
 '((jsp-code
    :submode java-mode
    :match-face (("<%!" . mmm-declaration-submode-face)
   		 ("<%=" . mmm-output-submode-face)
   		 ("<%"  . mmm-code-submode-face))
    :front "<%[!]?"
    :back "%>"
    :insert ((?% jsp-code nil @ "<%" @ " " _ " " @ "%>" @)
    	     (?! jsp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
    	     (?= jsp-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )
   (jsp-directive
    :submode text-mode
    :face mmm-special-submode-face
    :front "<%@"
    :back "%>"
    :insert ((?@ jsp-directive nil @ "<%@" @ " " _ " " @ "%>" @))
    )
   ))


; hook mmm-mode to jsp-html-helper-mode
(add-to-list 'mmm-mode-ext-classes-alist '(jsp-html-helper-mode "\\.jsp$" jsp-pages))


;; html-helper
;;? See skeleton-insert & auto-insert-alist
;? if new file && /\.hitop$/
;	     (if (eq 0 (string-match "/home/iain/public_html"
;				     buffer-file-name))
;		 (insert "<SET NAME=\"TITLE\"  VALUE=\"Title\">\n"
;			 "<SET NAME=\"BANNER\" VALUE=\"Banner\">\n\n"
;			 "<DEF NAME=\"MAIN\">\n\n</DEF>\n\n"
;			 "<FILE SRC=\"${RELPATH}template.hitop\">\n"))

;; (if (locate-library "html-helper-mode")
;;     (progn
;;       (autoload 'html-helper-mode "html-helper-mode" "Enhanced HTML mode" t)

;;       (setq auto-mode-alist
;; 	    (append (list (cons "\\.s?html?\\'"  'html-helper-mode)
;; 			  (cons "\.jsp$"      'html-helper-mode))
;; 		    auto-mode-alist))

;;       (add-hook 'html-helper-load-hook
;; 		'(lambda ()
;; 		   (if (locate-library "html-helper-imenu")
;; 		       (progn
;; 			 (autoload 'html-helper-imenu-setup
;; 			   "html-helper-imenu")
;; 			 (setq html-helper-imenu-title "Imenu")
;; 			 (html-helper-imenu-setup)))

;; 		   (setq tempo-interactive t
;; 			 html-helper-build-new-buffer nil)

;; 		   (defun my-html-helper-timestamp ()
;; 		     (let ((time (current-time-string)))
;; 		       (insert (substring time 4 11)
;; 			       (substring time -4) " ")))
;; 		   (setq html-helper-timestamp-hook 'my-html-helper-timestamp)
;; 		   ))

;;       (add-hook 'html-helper-mode-hook
;; 		'(lambda ()
;; 		   (setq html-helper-basic-offset 4)
		   
;; 		   (set
;; 		    (make-local-variable 'time-stamp-format)
;; 		    "%:d-%:m-%:y")

;; 		   ;;? Add hitop tags to html-helper-types-to-install?
;; 		   ;;? This looks quite hairy
;; 		   (turn-on-auto-fill)))))


(message "Done with my-web-mode.el customisations")
(provide 'my-web-mode)
