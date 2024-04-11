;;; eww-url.el --- Support viewing URLs visited by EWW -*- lexical-binding: t -*-

;; Copyright (C) 2024  Yafeng Tu

;; Author: Yafeng Tu <yafengtu@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Just let EWW record the visited URLs.

;;; Code:

(require 'eww)

(defun eww-url-copy ()
  "Copy the current URL."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (copy-region-as-kill start (point))
      (message "%s" (current-kill 0)))))

(defun eww-url--write-urls()
  (when (eq major-mode 'eww-url-mode)
    (let ((file (expand-file-name "eww-visited-urls" eww-bookmarks-directory))
          (start (point-min))
          (end (point-max)))
      (reverse-region start end)
      (write-region start end file)
      (reverse-region start end))))

(defun eww-url-kill ()
  "Kill the current URL."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "^\\s-*$")
      (user-error "No URL on the current line"))
    (let ((start (point))
          (inhibit-read-only t))
      (forward-line 1)
      (delete-region start (point))
      (eww-url--write-urls))))

(defun eww-url-browse ()
  "Browser the URL under point in eww."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "^\\s-*$")
      (user-error "No URL on the current line"))
    (let ((url (thing-at-point 'line t)))
      (quit-window)
      (eww-browse-url url))))

(defvar-keymap eww-url-mode-map
  "n" #'forward-line
  "p" #'previous-line
  "g" #'eww-url-list-urls
  "w" #'eww-url-copy
  "C-k" #'eww-url-kill
  "RET" #'eww-url-browse)

(define-derived-mode eww-url-mode special-mode "eww-url"
  "Mode for listing EWW visited URLs.

\\{eww-url-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t))

;;;###autoload
(defun eww-url-list-urls ()
  "Displayed visited URLs."
  (interactive)
  (let ((file (expand-file-name "eww-visited-urls" eww-bookmarks-directory))
        (inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*eww urls*")
      (eww-url-mode)
      (erase-buffer)
      (when (file-readable-p file)
        (insert-file-contents file)
        (reverse-region (point-min) (point-max))
        (delete-duplicate-lines (point-min) (point-max)))
      (pop-to-buffer (current-buffer)))))

(defun eww-url--add ()
  (when-let ((url (plist-get eww-data :url))
             (file (expand-file-name "eww-visited-urls" eww-bookmarks-directory)))
    (write-region (concat url "\n") nil file 'append)))

(add-hook 'eww-after-render-hook #'eww-url--add)

(provide 'eww-url)
;;; eww-url.el ends here
