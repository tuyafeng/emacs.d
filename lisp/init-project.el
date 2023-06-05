;;; init-project.el --- Settings and helpers for project.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package project
  :ensure nil
  :defer t
  :config

  ;; Recognize directories as projects by defining a new project backend `local`
  ;; Reference: https://github.com/karthink/project-x/blob/master/project-x.el
  (defcustom project-x-local-identifier ".project"
    "Filename(s) that identifies a directory as a project.
You can specify a single filename or a list of names."
    :type '(choice (string :tag "Single file")
                   (repeat (string :tag "Filename")))
    :group 'project-x)

  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))

  (defun project-x-try-local (dir)
    "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
    (if-let ((root (if (listp project-x-local-identifier)
                       (seq-some (lambda (n)
                                   (locate-dominating-file dir n))
                                 project-x-local-identifier)
                     (locate-dominating-file dir project-x-local-identifier))))
        (cons 'local root)))
  (add-hook 'project-find-functions 'project-x-try-local 90)
  )

(provide 'init-project)
;;; init-project.el ends here
