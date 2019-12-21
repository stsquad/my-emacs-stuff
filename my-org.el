;;; my-org --- org-mode related configuration bits
;;
;;; Commentary:
;;
;; I'm slowly using org-mode more and more although there seem to be
;; some poor interactions with use-package.  As a result I need to
;; split up the various sub-modes of org-mode.
;;
;;; Code:

;; (add-to-list 'org-export-options-alist '(:date "DATE" nil nil nil))
;; (add-to-list 'org-export-options-alist '(:title "TITLE" nil nil nil))


(eval-when-compile (require 'use-package))

(require 'my-libs)
(require 'my-vars)
(require 'my-email)
(require 'my-basic-modes)
(require 'my-hydra)
(require 'bookmark)

(defvar ajb-work-org-file
  (when I-am-at-work "/home/alex/org/index.org")
  "The location of my main work scratchpad.")

(use-package ob-shell
  :defer t
  :config (setq
           org-babel-default-header-args:sh
           '((:prologue . "exec 2>&1") (:epilogue . ":"))))

(use-package ob-async
  :ensure t)

(use-package ob-core
  :defer t)

(defvar my-org-babel-hashes nil
  "List of known babel code hashes.
This prevents org re-asking every time I restart.")
(add-to-list 'savehist-additional-variables 'my-org-babel-hashes)

(defvar my-org-default-action nil
  "Default action for this document to run on `org-ctrl-c-ctrl-c'.
\\<org-mode-map>
This will run via `org-ctrl-c-ctrl-c-hook' and should return a
non-nil result if it processed something. As such it can override
default `org-mode' behaviour for \\[org-ctrl-c-ctrl-c]. If you
want something to run at the end then you need to use
`my-org-default-code-block'")
(make-variable-buffer-local 'my-org-default-action)
(put 'my-org-default-action 'permanent-local t)

(defvar my-org-default-code-block nil
  "Default code block to run on `org-ctrl-c-ctrl-c'.

This is used by my-org-run-default-block which is added to
`org-ctrl-c-ctrl-c-final-hook'")
(make-variable-buffer-local 'my-org-default-code-block)

(defun my-org--do-action (func-or-string)
  "Evaluate a code block or call a function `FUNC-OR-STRING' from org-file."
  (let ((action-result))
    (cond
     ((stringp func-or-string)
      (save-excursion
        (org-babel-goto-named-src-block func-or-string)
        (when (memq (org-element-type (org-element-context))
	            '(inline-src-block src-block))
          (org-babel-eval-wipe-error-buffer)
          (setq action-result (org-babel-execute-src-block t)))))
     ((functionp func-or-string)
      (setq action-result (funcall func-or-string)))
     (t (error "What to do with: %s" func-or-string)))
    (if action-result
        (message "%s: %s" func-or-string action-result)
      nil)))

(defun my-org-run-default-action ()
  "Execute default action for this org file."
  (interactive)
  (when my-org-default-action
    (my-org--do-action my-org-default-action)))

(defun my-org-run-default-block ()
  "Evaluate the code block `my-org-default-code-block' if it exists."
  (interactive)
  (when my-org-default-code-block
    (cond
     ((stringp my-org-default-code-block)
      (save-excursion
        (org-babel-goto-named-src-block my-org-default-code-block)
        (org-babel-execute-src-block-maybe)))
     ((functionp my-org-default-code-block)
      (funcall my-org-default-code-block))
     (t (error "what to do with: %s" my-org-default-code-block)))))

;; (add-to-list 'org-ctrl-c-ctrl-c-final-hook 'my-org-run-default-block)

(use-package org-agenda
  :commands org-agenda
  :config
  (setq 
   ;; Agenda locations
   org-agenda-files '("~/org/")
   org-agenda-custom-commands
   '(
     ("n" "Agenda and top level tasks"
      ((agenda "")
       (tags "+LEVEL=2+tasks-TODO=\"DONE\"")))
     ("r" "Outstanding Reviews" tags-todo "reviews/TODO"))
   org-refile-targets '((nil :maxlevel . 2)
                        (org-agenda-files :maxlevel . 2))))

(use-package org-src
  :commands org-edit-src-code
  :config (progn
            (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)
            (setq org-src-window-setup 'reorganize-frame)))


(defun my-org-choose-target ()
  "Move cursor to insertion point for a given headline."
  (counsel-org-goto)
  (outline-show-entry)
  (outline-next-visible-heading 1)
  (previous-line)
  (move-end-of-line 1))

(use-package org-capture
  :commands org-capture org-capture-target-buffer
  :config
  (setq
   org-capture-templates
   '(
     ("g" "Save reference to review tag"
      entry
      (file+headline "review.org" "Review Tags")
      "** TODO %a
%c" :immediate-finish t)
     ("G" "Save reference to review tag (edit))"
      entry
      (file+headline "review.org" "Review Tags")
      "** %a
%c")
     ("r" "Save reference to review comment"
      checkitem
      (file+headline "review.org" "Review Comments")
      "  - [ ] %a")
     ("R" "Review Comment (region)"
      checkitem
      (file+headline "review.org" "Review Comments")
      "  - [ ] %i%?")
     ("t" "Add TODO task"
      entry
      (file+regexp "team.org" "\* Tasks ")
      "** TODO %i%?\n%T")
     ("m" "Add a maintainer TODO mail reference"
      checkitem
      (file+headline "qemu.org" "Maintainer Tasks")
      "  - [ ] %a")
     ("T" "Add TODO task with mail reference"
      entry
      (file+headline "team.org" "Tasks")
      "** TODO %i\nSee %a")
     ("p" "Posted email to list"
      item
      (file+function "team.org" my-org-choose-target)
      "  - posted %a")
     ("C" "Completed Review"
      entry
      (file+regexp "team.org" "Completed Reviews")
      "** DONE %a"
      :immediate-finish t)
     ("Q" "Queue Review (email)"
      entry
      (file+regexp "team.org" "Review Queue")
      "** TODO %a
Added: %t"
      :immediate-finish t :prepend t))))

;; ORG Based review automation

(defun my-org-maybe-capture-review-tag-or-comment ()
  "Check buffer for DCO tags and save, if not queue a review comment."
  (interactive)
  (let ((tags (my-capture-review-tags)))
    (if (not tags)
        (org-capture nil "r")
      (kill-new (mapconcat 'identity tags "\n"))
      (org-capture nil "g"))))

(with-eval-after-load 'mu4e-view
  (when (fboundp 'mu4e-view-mode-map)
    (define-key mu4e-view-mode-map (kbd "C-c C-c") 'my-org-maybe-capture-review-tag-or-comment)))

(defun my-org-get-elements (file heading)
  "Search FILE for HEADING and return the AST of that heading."
  (interactive)
  (let ((org-buf (org-capture-target-buffer file)))
    (with-current-buffer org-buf
      (org-element-map (org-element-parse-buffer) 'headline
         (lambda (hl)
           (when (string-match heading (org-element-property :raw-value hl))
             (identity hl)))))))

(defun my-org-find-review-tags (subject &optional new-status)
  "Return first review tag to match `SUBJECT'.
If `NEW-STATUS' is set then change TODO state."
  (interactive)
  (with-current-buffer (org-capture-target-buffer "review.org")
    (let ((done)
          (tags))
      (org-map-entries
       (fn
        (when (string-match-p (regexp-quote subject)
                              (nth 4 (org-heading-components)))
          ; extract tag
          (save-restriction
            (narrow-to-region (point) (org-entry-end-position))
            (setq tags (append tags (my-capture-review-tags)))
            ; maybe toggle the status
            (when new-status
              (org-todo new-status)))))
       "tags/-DONE" ; entries not yet marked
       'file
       (fn done))
      tags)))

(defun my-org-locate-review-comments (subject)
  "Locate review comments in review buffer pertaining to `SUBJECT'."
  (interactive)
  (let ((ast (my-org-get-elements "review.org" "Review Comments"))
        (buffer (org-capture-target-buffer "review.org")))
    (org-element-map ast 'item
      (lambda (item)
        (let ((check (org-element-property :checkbox item))
              (beg (org-element-property :contents-begin item))
              (end (org-element-property :contents-end item))
              (link))
          (setq link (with-current-buffer buffer
                       (buffer-substring-no-properties beg end)))
          (when (and (eq check 'off)
                     (stringp subject)
                     (string-match-p subject link))
            (cons buffer beg)))))))

(defun my-org-find-review-comments (subject)
  "Return links to comments pertaining to `SUBJECT'."
  (interactive)
  (let ((ast (my-org-get-elements "review.org" "Review Comments"))
        (buffer (org-capture-target-buffer "review.org")))
    (org-element-map ast 'item
      (lambda (item)
        (let ((check (org-element-property :checkbox item))
              (beg (org-element-property :contents-begin item))
              (end (org-element-property :contents-end item))
              (link))
          (setq link (with-current-buffer buffer
                       (buffer-substring-no-properties beg end)))
          (when (and (eq  check 'off)
                     (string-match-p subject link))
            (chomp link)))))))


;; Clocking behaviour
(use-package org-clock
  :disabled t
  :init (setq
         org-clock-persist 't
         org-clock-in-resume 't                 ; resume currently open clock
         org-clock-persist-query-resume 'nil    ; don't ask me about it
         org-log-into-drawer 't                 ; roll clocks up into drawers
         org-clock-idle-time 'nil
         ;; Mode line tweaks for clock
         org-clock-mode-line-total 'current
         org-clock-clocked-in-display 'frame-title))

(use-package ox-publish
  :commands org-publish
  :config
    (when I-am-at-work
      (setq
       org-publish-project-alist
       '(
         ("org-notes"
          :base-directory "~/org/"
          :base-extension "org"
          :publishing-directory "~/public_html/org/"
          :recursive nil
          :publishing-function org-html-publish-to-html
          :headline-levels 4             ; Just the default for this project.
          :auto-preamble t
          )
         ("org-presentations"
          :base-directory "~/org/presentations"
          :base-extension "html\\|css\\|js\\|png\\|jpg\\|gif\\|svg\\|pdf\\|mp3\\|ogg\\|eot\\|ttf\\|woff"
          :publishing-directory "~/public_html/org/presentations/"
          :recursive t
          :publishing-function org-publish-attachment
          )
         ("org-static"
          :base-directory "~/org/"
          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
          :publishing-directory "~/public_html/org/"
          :recursive t
          :publishing-function org-publish-attachment
          )
         ("org" :components ("org-notes" "org-presentations"
                             "org-static"))))))
;; Mail integration
(when I-am-at-work
  (use-package org-mu4e
    :if (locate-library "org-mu4e")
    :config
    (progn
      (setq mu4e-org-link-query-in-headers-mode t)
      (add-to-list 'org-modules 'org-mu4e t))))

(defun my-save-org-position-in-bookmark (&rest args)
  "Save position at jump."
  (and (org-mark-ring-push)
       (bookmark-set "org-pos-at-jump" nil)))

(defun my-return-to-org ()
  "Return to position at jump (if set)."
  (interactive)
  (ignore-errors
    (bookmark-jump "org-pos-at-jump")))

(defun my-return-to-org-file ()
  "Return current return jump file."
  (cdr (assoc
        'filename
        (assoc "org-pos-at-jump" bookmark-alist))))

(defun my-jump-to-org-file (&optional nojump)
  "Jump to the relevant section of the org-file unless NOJUMP is t.
Return the filespec of the jump."
  (interactive)
  (let* ((head (magit-git-str "log" "--pretty=%s" "HEAD^.."))
         (link (my-org-locate-review-comments head)))
    (when link
      (let ((buf (car (first link)))
            (pos (cdr (first link))))
      (if nojump
          (format "%s:%d" buf pos)
        (switch-to-buffer buf)
        (goto-char pos)
        (save-excursion
          (org-up-heading-safe)
          (org-show-entry)))))))

(defun my-org-mark-ring-info ())

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :commands (org-agenda org-capture)
  :bind (:map org-mode-map
              ("C-f" . counsel-org-agenda-headlines)
              ("C-c C-j" . counsel-org-goto))
  :init
  (progn
    (setq
     ;; General navigation
     org-return-follows-link t))
  :config
  (progn
    (setq
     ;; General navigation
     org-return-follows-link t
     ;; Agenda locations
     org-agenda-files '("~/org/")
     org-refile-targets '((nil :maxlevel . 2)
                          (org-agenda-files :maxlevel . 2))
     ;; Capture Templates
     org-directory "~/org"
     ;; TODO Hierarchy
     org-provide-todo-statistics t
     org-checkbox-hierarchical-statistics nil
     org-hierarchical-todo-statistics t
     org-log-done 'note
     org-enforce-todo-dependencies t
     org-todo-keywords '((sequence "TODO" "ACTIVE" "BLOCKED" "DONE"))
     org-todo-keyword-faces '(("TODO" . org-todo )
                              ("ACTIVE" . "blue")
                              ("BLOCKED" . org-warning)
                              ("DONE" . org-done))
     ;; Export settings
     org-export-allow-bind-keywords t)

    ;; Add my special handler.
    (add-to-list 'org-ctrl-c-ctrl-c-hook
                 'my-org-run-default-action)
    (add-to-list 'org-ctrl-c-ctrl-c-final-hook
                 'my-org-run-default-block)

    ;; Save jump position
    (add-to-list 'org-open-at-point-functions
                 'my-save-org-position-in-bookmark)

    ;; Ditta
    (let ((ditta-path "/usr/share/ditaa/ditaa.jar"))
      (when (file-exists-p ditta-path)
        (setq org-ditaa-jar-path ditta-path)))
    
    (with-eval-after-load 'hydra
      (global-set-key
       (kbd "C-x O")
       (defhydra my-hydra-org (:color blue)
         "
Org: _c_apture  _h_eadlines _j_ump to: %(my-jump-to-org-file t) _g_oto: %(my-return-to-org-file) save _p_osted work
Reviews: save _C_ompleted, _q_ueue or capture _r_eview comment"
         ("c" org-capture nil)
         ("h" counsel-org-agenda-headlines nil)
         ("j" my-jump-to-org-file nil)
         ("g" my-return-to-org nil)
         ("h" counsel-org-agenda-headlines nil)
         ("p" (org-capture nil "p") nil)
         ("C" (org-capture nil "C") nil)
         ("q" (org-capture nil "Q") nil)
         ("r" (org-capture nil "r") nil))))
    (org-clock-persistence-insinuate)))

;; Exporting
(defvar my-org-mu4e-index-links
  (rx
   (: (or "query:i:" "msgid:") (group-n 1 (one-or-more any))))
  "A regex to match mu4e links of the form:

    query:i:20170228171921.21602-1-ale+qemu@clearmind.me
")

(defun my-org-mu4e-export (path desc format)
  "Format mu4e links for export."
  (when (string-match my-org-mu4e-index-links path)
    (cond
     ((eq format 'html)
      (format "<a href=\"%s%s\">%s</a>"
              "https://www.google.com/search?q="
              (match-string 1 path)
              desc))
     ((eq format 'jira)
      (format "[%s|%s%s]"
              desc
              "https://www.google.com/search?q="
              (match-string 1 path)))
     ((eq format 'ascii)
      (format "%s\nMessage-Id: <%s>" desc (match-string 1 path))))))


(use-package ox
  :config
  (org-link-set-parameters "mu4e" :export 'my-org-mu4e-export))

;; Org reveal
(use-package org-re-reveal
  :ensure t
  :after org)

;; Load optional MELPA packages
;;
;; not really optional at work, but I don't want getting in the way
;; elsewhere.

(when (assoc "melpa" package-archives)
  ;; for JIRA export
  (use-package ox-jira
    :ensure t)
  (use-package ob-restclient
    :ensure t))

;; org-babel packages in stable
(use-package ob-async
  :ensure t)

;; Org Babel configurations
(let ((lob "~/org/library.org"))
  (when (file-exists-p lob)
    (org-babel-lob-ingest lob)))

(defun org-src-debug ()
  "Put a call to this function at the beginning of the org source block to debug it."
  (save-excursion
    (let ((pt (let ((case-fold-search t)) (org-babel-where-is-src-block-head))))
      (unless pt (error "Not at source block"))
      (goto-char pt)
      (org-edit-src-code)
      (let ((parse-sexp-ignore-comments t))
        (goto-char (point-min))
        (forward-sexp 2)
        (edebug-defun)))))

(use-package org-src
  :config
  (progn
    (setq org-src-fontify-natively t)))

;; Build list of available languages
(let ((langs '((emacs-lisp . t)
               (C . t)
               (dot . t)
               (ditaa . t)
               (makefile . t)
               (python . t))))
  (when (locate-library "ob-perl")
    (add-to-list 'langs '(perl . t)))
  (when (locate-library "ob-r")
    (add-to-list 'langs '(R . t)))
  (when (locate-library "ob-restclient")
    (add-to-list 'langs '(restclient . t)))
  (when (locate-library "ob-gnuplot")
    (add-to-list 'langs '(gnuplot . t)))
  (when (locate-library "sparql-mode")
    (add-to-list 'langs '(sparql . t)))
  (if (locate-library "ob-sh")
      (add-to-list 'langs '(sh . t))
    (add-to-list 'langs '(shell . t)))
  (org-babel-do-load-languages
   'org-babel-load-languages langs))

(use-package graphiz-dot-mode
  :if (locate-library "graphiz-dot-mode")
  :config (progn
            (let ((cust-install
                   (format "%s/src/graphviz/install"
                           (getenv "HOME"))))
              (when (file-exists-p cust-install)
                (my-add-world-to-env cust-install)))
            (add-to-list 'org-src-lang-modes
                         '("dot" . graphviz-dot))))

;;
;; Stats things
;;
(when I-am-at-work
  (use-package ess
    :ensure t
    :config (setq
             auto-mode-alist
                                        ; don't override asm-mode
             (delete '("\\.[qsS]\\'" . S-mode) auto-mode-alist))))

;; See http://emacs.stackexchange.com/questions/499/finding-and-executing-org-babel-snippets-programatically
(defun my-babel-hashed-confirm (lang body)
  "Check against known hashes before prompting for confirmation.
See `org-confirm-babel-evaluate'."
  (let ((check (list lang (md5 body))))
    ;; If not hashed, prompt
    (if (not (member check my-org-babel-hashes))
        ;; Ask if you want to hash
        (if (yes-or-no-p "Store hash for block? ")
            ;; Hash is added, proceed with evaluation
            (progn
              (add-to-list 'my-org-babel-hashes check)
              'nil)
          ;; Return 't to prompt for evaluation
          't)
      (message "Valid hash auto-confirmed for %s @ %s" lang org-babel-current-src-block-location)
      'nil)))

(setq org-confirm-babel-evaluate 'my-babel-hashed-confirm)

;; via https://kitchingroup.cheme.cmu.edu/blog/2016/02/26/Adding-captions-and-attributes-to-figures-and-tables-from-code-blocks-in-org-mode/
(defun my-org-src-decorate (&optional caption attributes)
  "A wrap function for src blocks."
  (concat
   "ORG\n"
   (when attributes
     (concat (mapconcat 'identity attributes "\n") "\n"))
   (when caption
     (format "#+caption: %s" caption))))


(defun my-invoke-babel-named (name)
  "Evaluate named babel block"
  (interactive)
  (save-excursion
    (org-babel-goto-named-src-block name)
    (org-babel-execute-src-block-maybe)))

(provide 'my-org)
;;; my-org.el ends here
