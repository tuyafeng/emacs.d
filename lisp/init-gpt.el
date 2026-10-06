;;; init-gpt.el --- gpt.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :commands
  (gptel gptel-menu gptel-mode gptel-request)
  :bind
  (("C-c g" . gptel-menu)
   :map gptel-mode-map
   ("C-c m" . gptel-menu))
  :init
  (setq gptel-directives
        `((default
           .
           ,(format "You are a large language model living in Emacs on %s and a helpful assistant. Respond concisely."
                    (pcase system-type
                      ('darwin "macOS")
                      ('gnu/linux "Linux")
                      ('windows-nt "Windows")
                      (_ (symbol-name system-type))))
           )))
  :config
  (setq gptel-use-curl t)
  ;;(setq gptel-log-level 'debug)
  (gptel-make-openai "AxonHub"
    :host "axonhub.pi.com"
    :curl-args '("--insecure")
    :endpoint "/v1/chat/completions"
    :stream t
    :key #'gptel-api-key
    :models '(deepseek-v4-flash deepseek-v4-pro))
  (setq gptel-prompt-prefix-alist
        '((markdown-mode . "## ")
          (org-mode . "** ")
          (text-mode . "## ")))
  (setq
   gptel-default-mode 'org-mode
   gptel-backend (gptel-get-backend "AxonHub")
   gptel-model 'deepseek-v4-flash)

  ;; Reference: https://github.com/karthink/gptel/issues/649#issuecomment-2742700136
  ;; Remove ChatGPT backend
  (delete (assoc "ChatGPT" gptel--known-backends) gptel--known-backends)

  (defun my/gptel-rewrite-settings (orig-fun &rest args)
    (let ((gptel-tools nil)
          (gptel-use-tools nil)
          (gptel-model 'deepseek-v4-flash))
      (apply orig-fun args)))
  (advice-add 'gptel-rewrite :around #'my/gptel-rewrite-settings)

  (gptel-make-tool
   :function (lambda (command &optional working_dir)
               (with-temp-message (format "Executing command: `%s`" command)
                 (let ((default-directory (if (and working_dir (not (string= working_dir "")))
                                              (expand-file-name working_dir)
                                            default-directory)))
                   (shell-command-to-string command))))
   :name "run_command"
   :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
   :args (list
          '(:name "command"
                  :type string
                  :description "The complete shell command to execute.")
          '(:name "working_dir"
                  :type string
                  :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
   :category "command"
   :confirm t
   :include t)

  (gptel-make-preset 'chat
    :model 'deepseek-v4-flash
    :use-tools t
    :tools '("run_command"))
  )

;;; Chats

(defcustom my/gptel-chat-directory
  (expand-file-name "~/data/gptel/")
  "Directory for gptel chat files."
  :type 'directory
  :group 'gptel)

(defun my/get-all-headings ()
  (cond
   ((derived-mode-p 'org-mode)
    (org-element-map (org-element-parse-buffer) 'headline
                     (lambda (h)
                       (format "%s %s"
                               (make-string (org-element-property :level h) ?*)
                               (org-element-property :raw-value h)))))
   ((derived-mode-p 'markdown-mode)
    (let (headings)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\(#+\\)\\s-+\\(.+\\)$" nil t)
          (push (format "%s %s"
                        (match-string 1)
                        (match-string 2))
                headings)))
      (nreverse headings)))
   (t
    (user-error "Unsupported mode"))))

(defun my/gptel-rename-chat ()
  "Rename current gptel chat file using an LLM."
  (interactive)
  (unless gptel-mode
    (user-error "This command is intended to be used in gptel chat buffers."))
  (let ((gptel-backend (gptel-get-backend "AxonHub"))
        (gptel-model 'deepseek-v4-flash)
        (gptel-tools ()))
    (gptel-request
        (concat "```" (if (eq major-mode 'org-mode) "org" "markdown") "\n"
                (string-join (my/get-all-headings) "\n")
                "\n```")
      :system
      (list (format
             "I will provide a transcript of a chat with an LLM.  \
Suggest a short and informative name for a file to store this chat in.  \
Use the following guidelines:
- be very concise, one very short sentence at most
- use the same language as the chat content
- return ONLY the title, no explanation or summary
- append the extension .%s"
             (if (eq major-mode 'org-mode) "org" "md")))
      :callback
      (lambda (resp info)
        (if (stringp resp)
            (let ((buf (plist-get info :buffer)))
              (when (and (buffer-live-p buf)
                         (y-or-n-p (format "Rename buffer %s to %s? " (buffer-name buf) resp)))
                (with-current-buffer buf (rename-visited-file resp))))
          (message "Error(%s): did not receive a response from the LLM."
                   (plist-get info :status)))))))

(defun my/gptel-new-chat ()
  "Create a timestamped gptel chat."
  (interactive)
  (let ((file (expand-file-name
               (format-time-string "%Y-%m-%d-%H-%M-%S.org")
               my/gptel-chat-directory)))
    (make-directory my/gptel-chat-directory t)
    (find-file file)
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (gptel-mode)
    (gptel-preset 'chat)
    (insert "\n**")
    (save-buffer)))

(defun my/gptel-list-chats ()
  "Select and open a gptel chat."
  (interactive)
  (let ((file (read-file-name
               "Open gptel chat: "
               my/gptel-chat-directory
               nil
               t
               nil
               (lambda (f)
                 (or (file-directory-p f)
                     (string-suffix-p ".org" f))))))
    (find-file file)
    (gptel-mode)
    (gptel-preset 'chat)))

;;; Commit message helpers

(defun my/git-root ()
  "Return current Git repository root."
  (or (locate-dominating-file default-directory ".git")
      (user-error "Not in a Git repository")))

(defun my/git-staged-diff-for-ai ()
  "Return staged diff, excluding common lock files."
  (let ((default-directory (my/git-root)))
    (shell-command-to-string
     (concat
      "git diff --cached --no-ext-diff --unified=3 -- . "
      "':(exclude)package-lock.json' "
      "':(exclude)yarn.lock' "
      "':(exclude)pnpm-lock.yaml' "
      "':(exclude)bun.lock' "
      "':(exclude)bun.lockb' "
      "':(exclude)Cargo.lock' "
      "':(exclude)Gemfile.lock' "
      "':(exclude)Podfile.lock' "
      "':(exclude)composer.lock'"))))

(defun my/git-commit-template ()
  "Return existing commit template/message text from current buffer.

Git comment lines are excluded."
  (save-excursion
    (goto-char (point-min))
    (let ((end (or (save-excursion
                     (when (re-search-forward "^#" nil t)
                       (line-beginning-position)))
                   (point-max))))
      (string-trim
       (buffer-substring-no-properties (point-min) end)))))

(defun my/gptel-commit-message-callback (buffer response info)
  "Insert generated commit message into BUFFER."
  (cond
   ((eq (car-safe response) 'reasoning)
    nil)

   ((null response)
    (message "Failed to generate commit message: %S" info))

   ((stringp response)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (let ((end (or (save-excursion
                           (when (re-search-forward "^#" nil t)
                             (line-beginning-position)))
                         (point-max))))
            (delete-region (point-min) end)
            (insert (string-trim response) "\n\n")))))
    (message "Commit message generated"))))

(defun my/gptel-generate-commit-message ()
  "Generate a Git commit message from staged changes."
  (interactive)
  (let* ((diff (my/git-staged-diff-for-ai))
         (template (my/git-commit-template))
         (gptel-backend (gptel-get-backend "AxonHub"))
         (gptel-model 'deepseek-v4-flash)
         (gptel-tools ()))
    (when (string-empty-p (string-trim diff))
      (user-error "No staged changes"))
    (message "Generating commit message...")
    (gptel-request
        (concat
         "Generate a concise Git commit message for the staged changes below.

Requirements:
- Output only the final commit message.
- Do not use Markdown.
- Do not explain anything.
- Start with a lowercase letter.
- Do not end with punctuation.
- Keep the subject concise, preferably under 72 characters.
- Describe the intent of the change rather than mechanically listing files.
"
         (unless (string-empty-p template)
           (concat
            "
The repository has an existing commit message/template.
Follow its structure and preserve any meaningful required sections or fields.
Do not blindly copy placeholder text.

Existing template/message:

--- TEMPLATE ---
"
            template
            "
--- END TEMPLATE ---
"))
         "
Staged diff:

--- DIFF ---
"
         diff
         "
--- END DIFF ---
")

      :callback
      (apply-partially
       #'my/gptel-commit-message-callback
       (current-buffer)))))

(with-eval-after-load 'git-commit
  (define-key git-commit-mode-map
              (kbd "C-c C-g")
              #'my/gptel-generate-commit-message))

(provide 'init-gpt)
;;; init-gpt.el ends here
