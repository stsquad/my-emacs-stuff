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


;; Use JDE out of preference
;
; JDE does better than mmm-mode for JSP

(if (featurep 'jde)
    (progn
      (setq auto-mode-alist
	    (append (list (cons "\\.jsp$" 'jde-mode)
			  auto-mode-alist)))

      ; TODO:
      ; We still fail as we are being added on the html-helper-mode hook we had
      ; better check if we are a jsp buffer and switch mode
      )

  ;; set up an mmm group for editing JSP pages

  (mmm-add-group
   'jsp-pages
   '((jsp-code
      :submode 'java-mode
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
     )))


(message "Done with my-web-mode.el customisations")
(provide 'my-web-mode)
