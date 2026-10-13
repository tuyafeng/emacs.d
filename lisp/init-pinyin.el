;;; init-pinyin.el --- Pinyin helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package pinyinlib
  :after orderless
  :config
  (defun my/orderless-pinyin (component)
    (orderless-regexp
     (pinyinlib-build-regexp-string component)))
  (add-to-list 'orderless-matching-styles
               #'my/orderless-pinyin))

(provide 'init-pinyin)
;;; init-pinyin.el ends here
