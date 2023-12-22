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
                                 :chat-model "gpt-4")))

(use-package llm-ollama
  :config (setq my-ollama-codellama
                (make-llm-ollama :chat-model "codellama")
                my-ollama-mistral
                (make-llm-ollama :chat-model "mistrel")))

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
  :init
  (setopt ellama-language "English")
  (setopt ellama-provider my-ollama-mistral))


(provide 'my-assistants)
;;; my-assistants.el ends here
