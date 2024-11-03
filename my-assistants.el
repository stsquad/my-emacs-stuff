;;; my-assistants --- integration of LLMs and similar tools
;;
;;; Commentary:
;;
;; This is the configuration of various digital assistants namely LLMs
;; and other similar tools.
;;
;;; Code:

;; AI!
(use-package pcsv
  :ensure t)

;;
;; llm provides an abstraction for various local and remote LLM APIs
;; which is then used by the slowly growing number of front-ends there
;; are.
;;
(use-package llm
  :ensure t)

(use-package llm-openai
  :config (setq my-openai-llm
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "gpt-4o")))

(use-package llm-ollama
  :config (setq my-ollama-codellama
                (make-llm-ollama :chat-model "codellama")
                my-ollama-mistral
                (make-llm-ollama :chat-model "mistrel")))

(use-package llm-gemini
  :config (setq my-gemini-llm
                (make-llm-gemini :key (my-pass-password "api.gemini.google.com")
                                 :chat-model
                                 "gemini-1.5-flash")
                my-gemini-pro-llm
                (make-llm-gemini :key (my-pass-password "api.gemini.google.com")
                                 :chat-model "gemini-1.5-pro-latest")
                my-gemma2-llm
                (make-llm-gemini :key (my-pass-password "api.gemini.google.com")
                                 :chat-model "gemma-2-27b-it")))

;;
;; The OG ChatGpt integration
;;

(defun my-project-name (project)
  "Return the name of the current project."
  (file-name-sans-extension
   (file-name-nondirectory (directory-file-name (cdr project)))))

(defun my-define-project-prompt ()
  "Defined a ChatGPT prompt tailored for the current project."
  (let* ((project (project-current t))
         (proj-name (my-project-name project))
         (origin-url (magit-get "remote" "origin" "url"))
         (prompt-title (format "Project assistant for %s" proj-name))
         (prompt (concat
                  "Your goal is to help the user working on the current project.\n"
                  (format "the project is in the directory %s and its origin repo is %s\n" proj-name origin-url)
                  "You are positive and encouraging.\n"
                  "You do not repeat obvious things, including their query.\n"
                  "You are as concise in responses. You always guide the user go one level deeper and help them see patterns.\n"
                  "You never apologise for confusions because it would waste their time.\n"
                  "You use markdown liberally to structure responses.\n"
                  "Always show code snippets in markdown blocks with language labels.\n"
                  "Don't explain code snippets.\n"
                  "Whenever you output updated code for the user, only show diffs,instead of entire snippets.\n"
                  "You can assume the user is running on Debian Linux and is using Emacs as their editor\n")))
    (add-to-list 'chatgpt-shell-system-prompts `(,prompt-title . ,prompt))))

(use-package chatgpt-shell
  :load-path (lambda () (my-return-path-if-ok
                         "~/src/emacs/chatgpt-shell.git/"))
  :config (setq
           chatgpt-shell-chatgpt-model-version "gpt-4"
           chatgpt-shell-openai-key '(lambda () (my-pass-password "api.openai.com"))))

; "code-davinci-edit-001"
; gpt-3.5-turbo is the cheaper faster one

(use-package dall-e-shell
  :load-path (lambda () (my-return-path-if-ok
                         "~/src/emacs/chatgpt-shell.git/"))
  :config (setq dall-e-shell-openai-key '(lambda () (my-pass-password "api.openai.com"))))

;;
;; Ellama
;;
(use-package ellama
  :ensure t
  :init (setopt ellama-language "English"
                ellama-provider my-gemini-llm
                ellama-providers '(("Gemini Pro" . my-gemini-pro-llm)
                                   ("Gemini" . my-gemini-llm)
                                   ("Gemma2" . my-gemma2-llm)
                                   ("ChatGPT" . my-openai-llm))
                ellama-keymap-prefix "C-c C-l l"))

;;
;; Codeium
;;;

;; https://github.com/Exafunction/codeium.el/issues/97#issuecomment-2354092579
(defun my-codeium-wrapper ()
  "Decouple codeium from other completions"
  (interactive)
  (cape-interactive #'codeium-completion-at-point))

(use-package codeium
  :load-path (lambda () (my-return-path-if-ok
                         "~/src/emacs/codeium.el.git/"))
  :bind (:map prog-mode-map
              ("C-x c" . my-codeium-wrapper))
  :config
  (setq codeium/metadata/api_key (my-pass-password "api.codeium.com")
        codeium-mode-line-enable (lambda (api)
                                   (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))

  (setq codeium/document/text 'my-codeium/document/text
        codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(provide 'my-assistants)
;;; my-assistants.el ends here
