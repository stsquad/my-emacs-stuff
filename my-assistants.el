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
  :config (setq my-gpt4
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "gpt-4o")
                ; cheaper version of gpt4
                my-gpt4-mini
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "gpt-4o-mini")
                ; aliases to the latest chatgpt model
                my-chatgpt
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "chatgpt-4o-latest")
                ; broad general reasoning model, expensive
                my-openai-o1
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "o1")
                ; cheaper version of o1 focused on coding, math,science
                my-openai-o1-mini
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "o1-mini")))

;; Also some hosted models
(setq my-textsynth-mistral
      (make-llm-openai-compatible :url "http://api.textsynth.com/v1/"
                                  :chat-model "mistral_7B"
                                  :key (my-pass-password
                                        "api.textsynth.com"))
      my-textsynth-llama-70b
      (make-llm-openai-compatible :url "http://api.textsynth.com/v1/"
                                  :chat-model "llama3.3_70B_instruct"
                                  :key (my-pass-password
                                        "api.textsynth.com")))


(use-package llm-ollama
  :config (setq my-ollama-codellama
                (make-llm-ollama :port 8080 :chat-model "codellama")
                my-ollama-mistral
                (make-llm-ollama :port 8080
                                 :chat-model "mistral"
                                 :embedding-model "mistral")))

;; https://ai.google.dev/gemini-api/docs/models
(use-package llm-gemini
  :config (setq
           ;; cost efficient, low latency
           my-gemini-llm-flash-lite
           (make-llm-gemini
            :key (my-pass-password "api.gemini.google.com")
            :chat-model
            "gemini-2.5-flash-lite")
           ;; general purpose
           my-gemini-llm-flash
           (make-llm-gemini
            :key (my-pass-password "api.gemini.google.com")
            :chat-model
            "gemini-2.5-flash")
           ;; advanced coding model
           my-gemini-pro-llm
           (make-llm-gemini
            :key (my-pass-password "api.gemini.google.com")
            :chat-model "gemini-2.5-pro")
           my-gemini-pro-preview-llm
           (make-llm-gemini
            :key (my-pass-password "api.gemini.google.com")
            :chat-model "gemini-3-pro-preview")))

;; See https://docs.anthropic.com/en/docs/about-claude/models/all-models
(use-package llm-claude
  :config (setq
           ;; Powerful model for highly complex tasks. most expensive
           my-claude-opus
           (make-llm-claude :key (my-pass-password "api.anthropic.com")
                            :chat-model "claude-opus-4-1")
           ;; Fast an relatively cheaper model
           my-claude-haiku
           (make-llm-claude :key (my-pass-password "api.anthropic.com")
                            :chat-model "claude-3-5-haiku-latest")
           ;; Intelligent, a little cheaper than opus
           my-claude-sonnet
           (make-llm-claude :key (my-pass-password "api.anthropic.com")
                            :chat-model "claude-sonnet-4-5")))

;;
;; The OG ChatGpt integration
;;

(defun my-project-name (project)
  "Return the name of the current `PROJECT'."
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

;; we want posframe to handle the positioning of the context
(use-package posframe
  :ensure t)

(use-package ellama
  ;; :ensure t
  :load-path (lambda () (my-return-path-if-ok
                         "~/src/emacs/ellama.git"))
  :bind ("C-c a" . ellama-transient-main-menu)
  :custom
  (ellama-provider
   my-gemini-llm-flash "Fast-ish general purpose")
  (ellama-completion-provider
   my-gemini-llm-flash-lite "Favour latency for completion")
  (ellama-spinner-type 'moon)
  (ellama-spinner-enabled t)
  :init (setopt ellama-language "English"
                ellama-providers
                '(
                  ;; Google Gemini Models
                  ("Gemini Pro 3 Preview" . my-gemini-pro-preview-llm)
                  ("Gemini Pro (coding)" . my-gemini-pro-llm)
                  ("Gemini Flash (general)" . my-gemini-llm-flash)
                  ("Gemini Flash Lite (cheaper, low latency)" . my-gemini-llm-flash-lite)
                  ;; OpenAI Models
                  ("ChatGPT (latest)" . my-chatgpt)
                  ("OpenAI GPT4o" . my-gpt4)
                  ("OpenAI GPT4o-mini" . my-gpt4-mini)
                  ;; not available on API unless a pro member
                  ;; ("OpenAI o1" . my-openai-o1)
                  ("OpenAI o1-mini" . my-openai-o1-mini)
                  ;; Hosted open models
                  ("Mistral 7B (textsynth)" . my-textsynth-mistral)
                  ("Llama 70B (textsynth)". my-textsynth-llama-70b)
                  ;; Local ramalama provided model
                  ("Local Llama" . my-local-openai-llama)
                  ("Local Mistral" . my-ollama-mistral)
                  ;; Anthropic
                  ("Claude Sonnet" . my-claude-sonnet)
                  ("Claude Haiku (cheap, fast)" . my-claude-haiku)
                  ("Claude Opus (slow, expensive)" . my-claude-opus)
                  ;;
                  )
                ellama-keymap-prefix "C-c C-l l"
                ellama-context-poshandler 'posframe-poshandler-frame-bottom-right-corner))

;;
;; Codeium
;;;

;; https://github.com/Exafunction/codeium.el/issues/97#issuecomment-2354092579
(defun my-codeium-wrapper ()
  "Decouple codeium from other completions."
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
