;
; my-diff-mode
;
; An varient on diff-mode which is a little easier on my hands
;

; Needs diff-mode

(require diff-mode)

(message "Setting up my-diff-mode")

(defun diff-revert-hunk()
  "Revert an applied hunk"
  (diff-apply-hunk t))

(defvar my-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'diff-hunk-next)
    (define-key map (kbd "t") 'diff-test-hunk)
    (define-key map (kbd "a") 'diff-apply-hunk)
    (define-key map (kbd "r") 'diff-revert-hunk)
    (define-key map (kbd "s") 'diff-split-hunk)
    (define-key map (kbd "RET") 'diff-goto-source)
  map)
"Keymap for `my-diff-mode'.")
  
(define-derived-mode my-diff-mode diff-mode "My Diff Mode"
  "A derived version of diff-mode with my own simple keybindings"
  (message "my-diff-mode entered")
  ; its ok to able to modify this buffer
  (setq buffer-read-only nil))
      
(setq auto-mode-alist (cons '("\\.diff\\'" . my-diff-mode)
			    auto-mode-alist))

(message "my-diff-mode created")


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
  (interactive)
  (goto-line 0)
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
       (if (y-or-n-p (format "%s: Skip hunk?" err))
           (diff-hunk-next)
         (nil))))))

;         (message (format "Unhandled error %s" err))))))
;      (message "apply-whole-patch: next"))))

                   
;                 message (format "The error message was %s" err))))))


;      (if (string-match "Can't find the text to patch" apply-result)
;          ; prompt user to skip or abort
;          (if (y-or-n-p "Skip hunk?")
;              (diff-hunk-next))))))

