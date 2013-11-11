;;
;; My ERT tests
;;

(require 'ert)

;; thanks to spacebat on #emacs for helping with this one
(defun my-define-compile-test (file)
  "Define a single compile test for one of my .emacs modules"
  (let* ((short-name
          (car
           (split-string (file-name-nondirectory file) "\\.")))
         (test-name (intern (format "compile-%s" short-name))))
    (eval `(ert-deftest ,test-name ()
             (let ((byte-compile-error-on-warn t))
               (should (byte-compile-file ,file)))))))

;; Define a test for each elisp file in the directory
(let*
   ((root-dir (file-name-directory
               (file-chase-links (locate-library "~/.emacs.d/init.el"))))
    (lisp-files (directory-files root-dir t "\.el$")))
  (mapc 'my-define-compile-test lisp-files))
  
;; Test my which-lookup function

(ert-deftest whick-lookup-tests ()
  (should (eql (which-lookup "foo") nil))
  (should (string-match "emacs" (which-lookup "emacs")))
  (should (string-equal (which-lookup '("aspell" "ispell")) "/usr/bin/aspell"))
  (should (string-equal (which-lookup '("ack-grep" "ack" "grep")) "/usr/bin/grep")))
