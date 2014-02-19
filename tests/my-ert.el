;;; my-ert.el -- ert tests for my stuff
;;
;;; Commentary:
;;
;; Hopefully the start of better testing
;;
;;; Code:

(require 'ert)

;; Setup path
(defun my-ert-setup-path ()
  "Setup path to packages for testing."
    (mapc #'(lambda (p)
              (when (and (file-exists-p p)
                         (file-directory-p p))
                (add-to-list 'load-path p)))
          '("~/.emacs.d/my-elisp" "~/.emacs.d/my-local-pkgs"))
  (message "load-path is: %s" load-path))

;; Test my which-lookup function

(ert-deftest whick-lookup-tests ()
  (my-ert-setup-path)
  (require 'my-utils)
  (should (eql (which-lookup "foo") nil))
  (should (string-match "emacs" (which-lookup "emacs")))
  (should (string-equal (which-lookup '("true" "false")) "/bin/true"))
  (should (string-equal (which-lookup '("not-there" "true")) "/bin/true")))

(provide 'my-ert)
;;; my-ert.el ends here
