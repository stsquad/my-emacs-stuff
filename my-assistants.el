;;; my-assistants --- integration of LLMs and similar tools -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This is the configuration of various digital assistants namely
;; Large Language Models and the associated agentic tools.
;;
;;; Code:

(eval-when-compile (require 'use-package))
(use-package my-utils
  :commands my-pass-password my-pass-auth-helper)
(use-package my-tracking)
(use-package my-git)

;; AI!
(use-package pcsv
  :ensure t)

;; import useful env vars
(setenv "GITLAB_TOKEN" (my-pass-auth-helper "gitlab-qemu-api"))

;;
;; llm provides an abstraction for various local and remote LLM APIs
;; which is then used by the slowly growing number of front-ends there
;; are.
;;
(use-package llm
  :ensure t)

;; See https://developers.openai.com/api/docs/models
(use-package llm-openai
  ;; most eye-watering expensive pro reasoning model
  :config (setq my-gpt-pro
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "gpt-5.5-pro")
                ; Pretty expensive
                my-gpt5-latest
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "gpt-5.5")
                ; Slightly older, half price model
                my-gpt5-last
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "gpt-5.4")
                ; Third cheaper than the main 5.4
                my-gpt5-mini
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "gpt-5.4-mini")
                ; Cheapest high volume model
                my-gpt5-nano
                (make-llm-openai :key (my-pass-password "api.openai.com")
                                 :chat-model "gpt-5.4-nano")
                ;; Also some hosted models
                my-textsynth-mistral
                (make-llm-openai-compatible :url "http://api.textsynth.com/v1/"
                                            :chat-model "mistral_7B"
                                            :key (my-pass-password
                                                  "api.textsynth.com"))
                my-textsynth-llama-70b
                (make-llm-openai-compatible :url "http://api.textsynth.com/v1/"
                                            :chat-model "llama3.3_70B_instruct"
                                            :key (my-pass-password
                                                  "api.textsynth.com"))))


(use-package llm-ollama
  :config (setq my-ollama-codellama
                (make-llm-ollama :port 8080 :chat-model "codellama")
                my-ollama-mistral
                (make-llm-ollama :port 8080
                                 :chat-model "mistral"
                                 :embedding-model "mistral")))

;; See https://ai.google.dev/gemini-api/docs/models
(use-package llm-gemini
  :config (setq
           ;; cost efficient, low latency
           my-gemini-llm-flash-lite
           (make-llm-gemini
            :key (my-pass-password "api.gemini.google.com")
            :chat-model
            "gemini-3.5-flash-lite")
           ;; general purpose
           my-gemini-llm-flash
           (make-llm-gemini
            :key (my-pass-password "api.gemini.google.com")
            :chat-model
            "gemini-3.8-flash")
           ;; advanced reasoning model for deep thinking
           my-gemini-argon
           (make-llm-gemini
            :key (my-pass-password "api.gemini.google.com")
            :chat-model "gemini-4-argon")))

;; See https://docs.anthropic.com/en/docs/about-claude/models/all-models
(use-package llm-claude
  :config (setq
           ;; The most expensive (and powerful) model.
           my-claude-fable
           (make-llm-claude :key (my-pass-password "api.anthropic.com")
                            :chat-model "claude-fable-5")
           ;; Powerful model for highly complex tasks. pretty expensive
           my-claude-opus
           (make-llm-claude :key (my-pass-password "api.anthropic.com")
                            :chat-model "claude-opus-4-8")
           ;; Intelligent, a little cheaper than opus
           my-claude-sonnet
           (make-llm-claude :key (my-pass-password "api.anthropic.com")
                            :chat-model "claude-sonnet-5")
           ;; Fast an relatively cheaper model
           my-claude-haiku
           (make-llm-claude :key (my-pass-password "api.anthropic.com")
                            :chat-model "claude-haiku-4-5")))

;;
;; The OG ChatGpt integration
;;

(defun my-project-name (project)
  "Return the name of the current `PROJECT'."
  (file-name-sans-extension
   (file-name-nondirectory (directory-file-name (cdr project)))))

;;
;; Ellama
;;

;; we want posframe to handle the positioning of the context
(use-package posframe
  :ensure t)

(use-package ellama
  :ensure t
  :bind ("C-c a" . ellama-transient-main-menu)
  :custom
  (ellama-provider
   my-gemini-llm-flash-lite "Fast and cheapest general purpose")
  (ellama-completion-provider
   my-gemini-llm-flash-lite "Favour latency for completion")
  (ellama-spinner-type 'moon)
  (ellama-spinner-enabled t)
  ;; Safe tools to use
  (ellama-tools '(project_root
                  read_file
                  grep
                  directory_tree))
  ;; how to find skills
  (ellama-skills-local-path ".agents/skills")
  (ellama-skills-global-path "/home/alex/.config/eca/skills")
  ;; Data Loss Protection setting
  (ellama-tools-dlp-enabled t)
  (ellama-tools-dlp-mode 'monitor)
  (ellama-tools-dlp-log-targets '(memory))
  :init (setopt ellama-language "English"
                ellama-providers
                '(
                  ;; Google Gemini Models
                  ("Gemini Argon (deep thinking)" . my-gemini-argon)
                  ("Gemini Flash (general)" . my-gemini-llm-flash)
                  ("Gemini Flash Lite (cheaper, low latency)" . my-gemini-llm-flash-lite)
                  ;; OpenAI Models
                  ("ChatGPT Pro (most expensive)" . my-gpt-pro)
                  ("ChatGPT (latest)" . my-gpt5-latest)
                  ("ChatGPT (cheaper, older)" . my-gpt5-last)
                  ("ChatGPT mini (much cheaper)" . my-gpt5-mini)
                  ("ChatGPT nano (cheapest)" . my-gpt5-nano)
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
                  ("Claude Fable (really expensive)" . my-claude-fable)
                  ;;
                  )
                ellama-keymap-prefix "C-c C-l l"
                ellama-context-poshandler 'posframe-poshandler-frame-bottom-right-corner))

;;
;; Editor Code Assistant - MCP enabled chat interface
;;

(defun my-eca-add-tracking-buffer ()
  "Add the eca chat buffer to tracking when chat completes."
  (tracking-add-buffer (current-buffer)))

(defun my-eca-chat-flyspell-setup ()
  "Enable Flyspell during typing and disable on submit in `eca-chat-mode`."
  (when (derived-mode-p 'eca-chat-mode)
    ;; Disable Flyspell when submitting prompts
    (add-hook 'pre-command-hook
              (lambda ()
                (when (and (memq this-command '(eca-chat--key-pressed-return
                                                eca-chat-send-prompt-at-chat))
                           flyspell-mode)
                  (flyspell-mode -1)))
              nil t)
    ;; Re-enable Flyspell when typing
    (add-hook 'pre-command-hook
              (lambda ()
                (when (and (eq this-command 'self-insert-command)
                           (not flyspell-mode))
                  (flyspell-mode 1)))
              nil t)))

(use-package eca
  :ensure t
  :hook ((eca-chat-mode . my-eca-chat-flyspell-setup)
         (eca-chat-finished . my-eca-add-tracking-buffer))
  :config (setq eca-extra-args '("--verbose" "--log-level" "debug")
                eca-completion-mode nil
                eca-chat-use-side-window nil))

;; Some local helpers for hacking elisp (see .eca/config.json customTools)

(defun my-eca-search-symbols (pattern &optional limit-type)
  "Search for symbols matching a regex `PATTERN'.
Optional `LIMIT-TYPE'."
  (let ((matches '()))
    (mapatoms
     (lambda (sym)
       (when (string-match-p pattern (symbol-name sym))
         (let ((keep (pcase limit-type
                       ('nil      t)
                       ('function (fboundp sym))
                       ('variable (boundp sym))
                       ('command  (commandp sym))
                       ('face     (facep sym))
                       (_         t))))
           (when keep
             (push
              (format "%s (%s)"
                      sym
                      (cond ((commandp sym) "command")
                            ((fboundp sym) "function")
                            ((boundp sym) "variable")
                            ((facep sym) "face")
                            (t "symbol")))
              matches))))))
    (let ((lms (length matches)))
      (if (<= lms 0)
          "no symbols found."
        (concat
         (mapconcat #'identity
                    (seq-take (sort matches #'string<) 30)
                    "\n")
         (when (< 30 lms)
           (format "\n%d results not show, please narrow your search." (- lms 30))))))))

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
  :commands codeium-completion-at-point
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

;;
;; agent-shell config
;;
(use-package agent-shell
  :ensure t
  :bind  (:map agent-shell-mode-map
               ("C-c C-l" . agent-shell-restart))
  :config (setq agent-shell-google-authentication
                (agent-shell-google-make-authentication
                 :api-key
                 (lambda ()
                   (auth-source-pass-get 'secret "api.gemini.google.com")))
                agent-shell-preferred-agent-config (agent-shell-goose-make-agent-config)))

(provide 'my-assistants)
;;; my-assistants.el ends here
