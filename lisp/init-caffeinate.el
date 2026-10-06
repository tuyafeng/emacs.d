;;; init-caffeinate.el --- Helpers for controlling macOS caffeinate -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/caffeinate-running-p ()
  "Return non-nil if any caffeinate process is running."
  (zerop
   (call-process "pgrep" nil nil nil "-x" "caffeinate")))

(defun my/caffeinate-start ()
  "Prevent macOS from sleeping."
  (interactive)
  (if (my/caffeinate-running-p)
      (message "caffeinate is already running")
    (start-process
     "caffeinate"
     nil
     "caffeinate"
     "-dimsu")
    (message "caffeinate started")))

(defun my/caffeinate-stop ()
  "Stop all caffeinate processes."
  (interactive)
  (if (my/caffeinate-running-p)
      (progn
        (call-process "pkill" nil nil nil "-x" "caffeinate")
        (message "all caffeinate processes stopped"))
    (message "caffeinate is not running")))

(defun my/caffeinate-toggle ()
  "Toggle caffeinate."
  (interactive)
  (if (my/caffeinate-running-p)
      (my/caffeinate-stop)
    (my/caffeinate-start)))

(defun my/caffeinate-status ()
  "Show whether caffeinate is running."
  (interactive)
  (message "caffeinate: %s"
           (if (my/caffeinate-running-p)
               "running"
             "stopped")))

(provide 'init-caffeinate)
;;; init-caffeinate.el ends here
