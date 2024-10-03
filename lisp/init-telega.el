;;; init-telega.el --- telega.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package telega
  :commands (telega)
  :config
  (unless (display-graphic-p) (setq telega-use-images nil))
  (setq telega-server-libs-prefix (expand-file-name "~/.local/tdlib"))
  (setq telega-proxies
        (list '(:server "127.0.0.1" :port 1090 :enable t
                        :type (:@type "proxyTypeSocks5"))))
  (setq
   telega-mode-line-mode 1
   telega-avatar-workaround-gaps-for '(return t)
   telega-root-keep-cursor 'track
   telega-root-show-avatars nil
   telega-translate-to-language-by-default "zh"
   telega-root-keep-cursor 'track)
  (when (featurep 'nerd-icons)
    ;; Reference: https://emacs-china.org/t/telega/25759
    ;; Reference: https://github.com/LuciusChen/.emacs.d/blob/main/lisp/init-telega.el
    ;; Reference: https://www.nerdfonts.com/cheat-sheet
    (setq
     telega-symbols-emojify (assq-delete-all 'forward telega-symbols-emojify)
     telega-symbols-emojify (assq-delete-all 'reply telega-symbols-emojify)
     telega-symbols-emojify (assq-delete-all 'reply-quote telega-symbols-emojify)
     telega-symbol-forward (nerd-icons-octicon "nf-oct-cross_reference")
     telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
     telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
     telega-symbols-emojify (assq-delete-all 'checkmark telega-symbols-emojify)
     telega-symbols-emojify (assq-delete-all 'heavy-checkmark telega-symbols-emojify)
     telega-symbol-checkmark (nerd-icons-mdicon "nf-md-check")
     telega-symbol-heavy-checkmark (nerd-icons-mdicon "nf-md-check_all"))))

(provide 'init-telega)
;;; init-telega.el ends here
