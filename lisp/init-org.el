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
   'org-babel-load-languages '((python . t)))
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)))
  (setq org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%Y-%m-%d %H:%M>" . "<%Y-%m-%d %H:%M:%S>"))
  (setq org-image-actual-width 'nil)
  (setq org-cycle-separator-lines -1)
  (setq org-list-allow-alphabetical t)
  (setq org-export-with-section-numbers nil))

(use-package ox-html
  :ensure nil
  :defer t
  :config
  (setq org-html-validation-link nil)
  (setq org-html-postamble t)
  (setq org-html-postamble-format
        '(("en" "<p class=\"author\">Author: %a</p>
<p class=\"date\">Created: %d</p>
<p class=\"date\">Last Updated: %C</p>"))))

(use-package org-download
  :after org
  :hook (org-mode . (lambda ()
                      (setq-local org-download-image-dir
                                    (concat "./" (file-name-base buffer-file-name) ".assets"))))
  :config
  (setq org-download-heading-lvl nil)
  (setq org-download-image-attr-list
        '("#+attr_org: :width 300px"
          "#+attr_html: :width 50% :align center"))
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

(use-package ox-hugo
  :defer t
  :after ox)

(provide 'init-org)
;;; init-org.el ends here
