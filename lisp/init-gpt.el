;;; init-gpt.el --- gpt.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :defer t
  :bind
  (:map gptel-mode-map
        ("C-c m" . #'gptel-menu))
  :init
  (setq gptel-directives
        '((default . "你是 Emacs 中的一个大型语言模型，也是一位得力助手。请简明扼要地回答。")
          (programming . "你是一个大型语言模型和细心的程序员。请提供代码，并且只提供代码作为输出，不提供任何附加文本、提示或注释。")
          (writing . "你是一个大型语言模型和写作助手。请简明扼要地回答。")))
  :config
  (gptel-make-deepseek "Bytedance"
    :host "ark.cn-beijing.volces.com"
    :endpoint "/api/v3/chat/completions"
    :stream t
    :key #'gptel-api-key
    :models '(deepseek-r1-250120 deepseek-v3-250324))
  (setq gptel-prompt-prefix-alist
        '((markdown-mode . "## ")
          (org-mode . "** ")
          (text-mode . "## ")))
  (setq
   gptel-default-mode 'org-mode
   gptel-backend (gptel-get-backend "Bytedance")
   gptel-model 'deepseek-v3-250324)

  ;; Reference: https://github.com/karthink/gptel/issues/649#issuecomment-2742700136
  ;; Remove ChatGPT backend
  (delete (assoc "ChatGPT" gptel--known-backends) gptel--known-backends))

(provide 'init-gpt)
;;; init-gpt.el ends here
