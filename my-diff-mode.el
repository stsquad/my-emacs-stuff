;;; my-diff-mode --- A derived version of diff-mode for heavy patch use
;;
;; Copyright (C) 2014 Alex Bennée

;; Author: Alex Bennée <alex.bennee@linaro.org>
;; Maintainer: Alex Bennée <alex.bennee@linaro.org>
;; Version: 0.1
;; Homepage: 

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This provides a simple derived version of diff mode with a single
;; keypress map and a few helper functions.
;;
;;; Code:



;
; my-diff-mode
;
; An varient on diff-mode which is a little easier on my hands
;

; Needs diff-mode

(require 'diff-mode)

;;
;; Some utility functions the work ontop of diff-mode
;;
;; Not quite working as I want yet
;;

(defun diff-revert-hunk()
  "Revert an applied hunk"
  (interactive)
  (diff-apply-hunk t))


; test-whole-patch
;
; Go to the start and run through the whole patch testing to see if
; hunks are applied. As test-hunk may fail (with an error) at the top
; of a unlclean diff file I wrap it up my-test-hunk

(defun test-patch-until-fail()
  "Test the patch from point until we find something not cleanly applied"
  (interactive)
  (while (string-match "Hunk already applied" (diff-test-hunk))
    (diff-hunk-next))
  (message "oops"))

(defun continue-testing-patch()
  "Skip the current hunk and continue testing"
  (interactive)
  (diff-hunk-next)
  (test-patch-until-fail))

(defun my-test-hunk(&optional skip-applied)
  "Test a hunk to see if we should apply. Returns t if we should try,
otherwise returns nil. If skip-applied is set we recurse and skip
applied hunks"
  (interactive)
  (message "my-test-hunk: start")
  (condition-case nil
      (let ((test-result (diff-test-hunk)))
        (message (concat "my-test-hunk: test-result is " test-result))
        (cond ((string-match "Hunk already applied" test-result)
               (if skip-applied
                   (progn
                     (message "my-test-hunk: skipping")
                     (diff-hunk-next)
                     (my-test-hunk skip-applied))
                 nil))
               ((string-match "Hunk text not found" test-result)
                (message "my-test-hunk: text not found - should apply") t)
               ((string-match "Hunk not yet applied" test-result)
                (message "my-test-hunk: not yet applied") t)
               (t
                (message (concat "Unknown Response: " test-result))
                nil)))
    (error nil)))

(defun find-start-of-patch()
  "Find the start of a patch"
  (interactive)
  (goto-char (point-min))
  (if (not (my-test-hunk))
      (diff-hunk-next)))

(defun test-whole-patch()
  "Test if the whole patch has been cleanly"
  (interactive)
  (find-start-of-patch)
  (test-patch-until-fail))

(define-key diff-mode-map (kbd "C-c t") 'test-whole-patch)
(define-key diff-mode-map (kbd "C-c c") 'continue-testing-patch)

;; apply-whole-patch
;
; Apply a whole patch, only stopping when it finds something it can't
; deal with.
;

(defun apply-whole-patch()
  "Apply a whole patch, only pausing to ask the user when it has a
problem"
  (interactive)
  (find-start-of-patch)
  (while (my-test-hunk t)
    (condition-case err
        (let ((apply-result (diff-apply-hunk)))
          (message (concat "apply-whole-patch: apply-result is "
                           apply-result)))
      (error
       (when (y-or-n-p (format "%s: Skip hunk?" err))
         (diff-hunk-next))))))

;; diff-apply-and-kill
;
; Apply the patch and then kill the hunk in the diff file if it
; succeded

(defun diff-apply-and-kill()
  "Apply a patch to the code and then delete from the diff if it
succeded"
  (interactive)
  (save-excursion
    (diff-apply-hunk))
  (diff-hunk-kill))

;
; Define a new key-map
;
; It only defines a few of the keys I use as single key presses (to
; save my RSI).
;
(defvar my-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'diff-hunk-next)
    (define-key map (kbd "p") 'diff-hunk-prev)
    (define-key map (kbd "k") 'diff-hunk-kill)
    (define-key map (kbd "d") 'diff-hunk-kill)
    (define-key map (kbd "t") 'diff-test-hunk)
    (define-key map (kbd "a") 'diff-apply-hunk)
    (define-key map (kbd "A") 'diff-apply-and-kill)
    (define-key map (kbd "r") 'diff-revert-hunk)
    (define-key map (kbd "s") 'diff-split-hunk)
    (define-key map (kbd "RET") 'diff-goto-source)
    (define-key map (kbd "C-c t") 'test-whole-patch)
    (define-key map (kbd "C-c c") 'continue-testing-patch)
  map)
"Keymap for `my-diff-mode'.")

(define-derived-mode my-diff-mode diff-mode "My Diff Mode"
  "A derived version of diff-mode with my own simple key-bindings"
  (message "my-diff-mode entered")
  ; its ok to able to modify this buffer
  (setq buffer-read-only nil))

(provide 'my-diff-mode)
;;; my-diff-mode.el ends here
