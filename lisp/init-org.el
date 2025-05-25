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
  (setq org-time-stamp-custom-formats
        '("<%Y-%m-%d %H:%M>" . "<%Y-%m-%d %H:%M:%S>"))
  (setq org-log-into-drawer t)
  (setq org-export-with-drawers '("LOGBOOK"))
  (setq org-image-actual-width 'nil)
  (setq org-cycle-separator-lines -1)
  (setq org-list-allow-alphabetical t)
  (setq org-export-with-section-numbers nil)
  :custom-face
  (org-level-1 ((t (:height 1.15))))
  (org-level-2 ((t (:height 1.13))))
  (org-level-3 ((t (:height 1.11))))
  (org-level-4 ((t (:height 1.09))))
  (org-level-5 ((t (:height 1.07))))
  (org-level-6 ((t (:height 1.05))))
  (org-level-7 ((t (:height 1.03))))
  (org-level-8 ((t (:height 1.01)))))

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
  :config
  (setq org-download-heading-lvl nil)
  (setq org-download-image-attr-list
        '("#+caption: caption"
          "#+attr_org: :width 300px"
          "#+attr_html: :width 50% :align center"))
  (defun dummy-org-download-annotate-function (link) "")
  (setq org-download-annotate-function
        #'dummy-org-download-annotate-function)
  (defun my/org-download-org-mode-hook()
    (when buffer-file-name
      (setq-local org-download-heading-lvl nil)
      (setq-local org-download-image-dir
                  (concat "./" (file-name-base buffer-file-name) ".assets"))))
  (add-hook 'org-mode-hook #'my/org-download-org-mode-hook))

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
