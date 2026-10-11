;;; init-server.el --- Server configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init-server)
;;; init-server.el ends here
