;;; init-speed-type.el --- Speed type configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package speed-type
  :defer t
  :init
  (defun my/speed-type-char-set (char-set)
    "Use CHAR-SET characters for speed typing."
    (interactive "sEnter character set: ")
    (with-temp-buffer
      (dotimes (i 160)
        (insert (format "%c" (elt char-set (random (length char-set)))))
        (when (= (mod (1+ i) 80) 0)
          (insert "\n")))
      (insert "\n")
      (speed-type-buffer (current-buffer))))

  (defun my/speed-type-number ()
    "Use numbers for speed typing."
    (interactive)
    (my/speed-type-char-set "0123456789"))

  (defun my/speed-type-symbol ()
    "Use symbols for speed typing."
    (interactive)
    (my/speed-type-char-set "~!@#$%^&*()`|\\:;'\",.<>/?-={}_+/[]"))
  )

(provide 'init-speed-type)
;;; init-speed-type.el ends here
