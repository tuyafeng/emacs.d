;;; init-org.el --- Org-mode configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :defer t
  :config
  (setq org-link-descriptive 'nil)
  (setq org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t))))

(use-package ox-html
  :ensure nil
  :defer t
  :config
  (setq org-html-validation-link nil))

(use-package org-download
  :after org
  :config
  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir "./images")
  (defun dummy-org-download-annotate-function (link) "")
  (setq org-download-annotate-function
        #'dummy-org-download-annotate-function))

(use-package plantuml-mode
  :when (file-exists-p "~/data/system/plantuml/plantuml-1.2022.5.jar")
  :after org
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path
        (expand-file-name "~/data/system/plantuml/plantuml-1.2022.5.jar"))
  (setq plantuml-output-type "png")
  (with-eval-after-load "org"
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(use-package calendar
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today)
  :config
  (setq calendar-chinese-all-holidays-flag t)
  (setq calendar-week-start-day 1))

;; Reference: https://github.com/tumashu/cnfonts/issues/84
;; (setq fontset-orgtable
;;       (create-fontset-from-ascii-font "Monaco 24"))
;; (dolist (charset '(han symbol cjk-misc))
;;   (set-fontset-font fontset-orgtable charset
;; 		    (font-spec :family "Hiragino Sans GB W3"
;; 			       :size 20)))

(add-hook 'org-mode-hook
          '(lambda ()
             (set-face-attribute 'org-table nil
                                 :font "Ubuntu Mono 15"
                                 :fontset nil)))

(provide 'init-org)
;;; init-org.el ends here
