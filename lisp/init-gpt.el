;;; init-gpt.el --- gpt.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :commands
  (gptel gptel-menu my/gptel-rewrite my/gptel-new-chat my/gptel-list-chats)
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
    :models '(deepseek-v4-flash deepseek-v4-pro kimi-for-coding))
  (setq gptel-prompt-prefix-alist
        '((markdown-mode . "## ")
          (org-mode . "** ")
          (text-mode . "## ")))
  (setq
   gptel-default-mode 'org-mode
   gptel-backend (gptel-get-backend "AxonHub")
   gptel-model 'deepseek-v4-pro)

  ;; Reference: https://github.com/karthink/gptel/issues/649#issuecomment-2742700136
  ;; Remove ChatGPT backend
  (delete (assoc "ChatGPT" gptel--known-backends) gptel--known-backends)

  (defun my/gptel-rewrite ()
    (interactive)
    (let ((gptel-tools nil)
          (gptel-use-tools nil)
          (gptel-model 'deepseek-v4-flash))
      (call-interactively #'gptel-rewrite)))

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

  (defun my/gptel-new-chat ()
    "Create a timestamped gptel chat."
    (interactive)
    (let* ((dir (expand-file-name "~/data/gptel/"))
           (file (expand-file-name
                  (format-time-string "%Y-%m-%d-%H-%M-%S.org")
                  dir)))
      (make-directory dir t)
      (find-file file)
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (gptel-mode)
      (insert "\n**")
      (save-buffer)))

  (defun my/gptel-list-chats ()
    "Select and open a gptel chat."
    (interactive)
    (let ((file (read-file-name
                 "Open gptel chat: "
                 (expand-file-name "~/data/gptel/")
                 nil
                 t
                 nil
                 (lambda (f)
                   (or (file-directory-p f)
                       (string-suffix-p ".org" f))))))
      (find-file file)
      (gptel-mode)))

  )

(provide 'init-gpt)
;;; init-gpt.el ends here
