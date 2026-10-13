;;; launcher-icons.el --- Native macOS app icons for launcher -*- lexical-binding: t; -*-

;;; Commentary:

;; Each app's own icon, as macOS draws it, before its name in the
;; launcher's list.
;;
;; The packaged script assets/launcher-icons.js asks AppKit for icons
;; (NSWorkspace iconForFile:) through macOS's built-in osascript, and
;; writes them as PNGs, in the background.  A PNG's name is a hash of
;; the app's path and of its Info.plist's modification time, so that an
;; updated app gets a new icon, and making missing PNGs is all there is
;; to do.  An icon that is not ready yet shows as blank space.
;;
;; Loading this file starts nothing: icons are made when a graphical
;; launcher asks for them.

;;; Code:

(defvar launcher-builtin-icons)

(defcustom launcher-show-icons t
  "Non-nil means show application icons in the launcher.
Icons need macOS and a graphical frame that displays PNG images;
elsewhere the launcher shows text only, as when this is nil."
  :type 'boolean
  :group 'launcher)

(defcustom launcher-icon-size 20
  "Size of application icons in the launcher's list, in logical pixels.
Icons stay sharp up to 32 pixels on a Retina display."
  :type 'natnum
  :group 'launcher)

(defcustom launcher-icon-cache-directory
  (locate-user-emacs-file "cache/launcher/icons/")
  "Directory in which the launcher keeps application icons.
The launcher writes only in its \"v1\" subdirectory, and deletes only
the PNGs it made there."
  :type 'directory
  :group 'launcher)

(defconst launcher-icons--pixels 64
  "Width and height in pixels of the PNGs the launcher makes.")

(defconst launcher-icons--osascript "/usr/bin/osascript"
  "Program that runs the worker.")

(defconst launcher-icons--script
  (expand-file-name "assets/launcher-icons.js"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "The packaged worker, next to this library.")

(defconst launcher-icons--timeout 60
  "Seconds after which a worker still running is stopped.")

(defconst launcher-icons--png-regexp "\\`[0-9a-f]\\{40\\}\\.png\\'"
  "Matches the names of the PNGs the launcher makes.")

(defvar launcher--apps nil)

(defvar launcher-icons--requested (make-hash-table :test #'equal)
  "PNG files a worker of this Emacs was asked to make, and did not.
An icon that could not be made is not asked for again until its app
changes or the app index is refreshed.  A PNG that was made, and then
deleted, as by another Emacs sharing the cache, is made again.")

(defvar launcher-icons--process nil
  "The running worker, or nil.")

(defun launcher-icons-enabled-p (&optional frame)
  "Return non-nil if the launcher shows icons in FRAME.
FRAME defaults to the selected frame."
  (and launcher-show-icons
       (eq system-type 'darwin)
       (display-images-p frame)
       (image-type-available-p 'png)
       (file-executable-p launcher-icons--osascript)
       (file-readable-p launcher-icons--script)))

(defun launcher-icons--directory ()
  "Return the directory of the launcher's PNGs, which only it writes."
  (file-name-as-directory (expand-file-name "v1" launcher-icon-cache-directory)))

(defun launcher-icons--file (path)
  "Return the PNG file of the icon of the app at PATH, or nil if it is gone."
  (when-let* ((attributes (or (file-attributes (expand-file-name "Contents/Info.plist" path))
                              (file-attributes path))))
    (expand-file-name
     (concat (secure-hash 'sha1 (encode-coding-string
                                 (format "%d\0%s\0%s" launcher-icons--pixels path
                                         (float-time (file-attribute-modification-time
                                                      attributes)))
                                 'utf-8-unix))
             ".png")
     (launcher-icons--directory))))

(defun launcher-icons--pngs ()
  "Return the PNGs the launcher made."
  (let ((directory (launcher-icons--directory)))
    (and (file-directory-p directory)
         (directory-files directory t launcher-icons--png-regexp))))

(defun launcher-icons-app (app)
  "Return (:file PNG-PATH) for application name or path APP, or nil.
Names match the cached display name or bundle base name exactly.  An
ambiguous name returns nil.  Paths need not be in the application index.
The PNG may not exist yet.  This function neither discovers applications
nor starts an icon worker."
  (when (stringp app)
    (let ((path
           (if (and (string-suffix-p ".app" (directory-file-name app))
                    (or (string-match-p "/" app) (file-directory-p app)))
               (directory-file-name (expand-file-name app))
             (let (matches)
               (dolist (entry launcher--apps)
                 (when (or (equal app (car entry))
                           (equal app (file-name-base (cdr entry))))
                   (push (directory-file-name (expand-file-name (cdr entry))) matches)))
               (setq matches (delete-dups matches))
               (when (= (length matches) 1) (car matches))))))
      (when (and path (string-suffix-p ".app" path) (file-directory-p path))
        (when-let* ((file (launcher-icons--file path)))
          (list :file file))))))

(defun launcher-icons--stop-stuck ()
  "Stop a worker running for longer than `launcher-icons--timeout'.
The icons it was asked for are asked for again."
  (when (and (process-live-p launcher-icons--process)
             (> (- (float-time) (process-get launcher-icons--process 'start))
                launcher-icons--timeout))
    (delete-process launcher-icons--process)
    (clrhash launcher-icons--requested)))

(defun launcher-icons-prepare (paths)
  "Make the missing icons of the apps at PATHS, in the background.
Call when a launcher with icons starts."
  (when launcher-show-icons
    (launcher-icons--stop-stuck)
    (let (missing)
      (dolist (path paths)
        (when-let* ((file (launcher-icons--file path)))
          (unless (or (file-exists-p file) (gethash file launcher-icons--requested))
            (push (cons path file) missing))))
      (when missing
        (launcher-icons--start (nreverse missing))))))

(defun launcher-icons--start (icons)
  "Start a worker making ICONS, a list of (APP-PATH . PNG-FILE).
If a worker is still running, leave ICONS to a later launcher."
  (unless (process-live-p launcher-icons--process)
    (make-directory (launcher-icons--directory) t)
    (let ((default-directory "/")
          (log (get-buffer-create " *launcher-icons*")))
      (with-current-buffer log (erase-buffer))
      ;; Paths are arguments, never code: the worker prints the apps it
      ;; failed on into the log.
      (setq launcher-icons--process
            (make-process :name "launcher-icons" :buffer log :noquery t
                          :connection-type 'pipe
                          :command `(,launcher-icons--osascript "-l" "JavaScript"
                                     ,launcher-icons--script
                                     ,(number-to-string launcher-icons--pixels)
                                     ,@(mapcan (lambda (icon) (list (car icon) (cdr icon)))
                                               icons))
                          :sentinel (lambda (process event)
                                      (unless (process-live-p process)
                                        (launcher-icons--forget icons
                                                                (equal event "finished\n"))))))
      (process-put launcher-icons--process 'start (float-time))
      (dolist (icon icons)
        (puthash (cdr icon) t launcher-icons--requested)))))

(defun launcher-icons--forget (icons finished)
  "Forget the requests of ICONS, once their worker exited.
If it FINISHED, keep those whose PNGs it failed to make; otherwise,
as when it crashed or was killed, forget all."
  (dolist (icon icons)
    (when (or (not finished) (file-exists-p (cdr icon)))
      (remhash (cdr icon) launcher-icons--requested))))

(defun launcher-icons-valid-p (icon)
  "Return non-nil if ICON is a supported text, file or built-in icon."
  (pcase icon
    ((or 'nil (pred stringp)) t)
    (`(:file ,path) (stringp path))
    (`(:builtin ,key) (memq key '(calculator search url)))))

(defun launcher-icons-prefix (icon)
  "Return a completion prefix for ICON, respecting `launcher-show-icons'.
ICON is a propertized string, (:file PNG-PATH), (:builtin KEY), or nil.
Built-in references resolve through `launcher-builtin-icons' at display
time, without following further references.  Missing or unusable images
show an equally sized blank.  Text keeps its properties
and is centered in a slot at least `launcher-icon-size' pixels wide."
  (when (and launcher-show-icons (consp icon) (eq (car icon) :builtin))
    (setq icon (and (boundp 'launcher-builtin-icons)
                    (alist-get (cadr icon) launcher-builtin-icons))))
  (cond
   ((not launcher-show-icons) "")
   ((and (stringp icon) (not (string-empty-p icon)))
    (let* ((padding (max 0 (- launcher-icon-size
                             (string-pixel-width icon (current-buffer)))))
           (half (/ padding 2.0)))
      (concat (propertize " " 'display `(space :width (,half)))
              icon
              (propertize " " 'display
                          `(space :width (,half) :height (,launcher-icon-size)))
              " ")))
   (t
    (let* ((size launcher-icon-size)
           (file (and (consp icon) (plist-get icon :file)))
           (image (condition-case nil
                      (when (and file (file-regular-p file) (file-readable-p file))
                        (create-image file 'png nil :width size :height size
                                      :scale 1 :ascent 'center))
                    (error nil))))
      (concat (propertize " " 'display
                          (or image `(space :width (,size) :height (,size))))
              " ")))))

(defun launcher-icons-index-refreshed (paths)
  "Note that the app index now holds PATHS: delete the icons of other apps.
Icons that could not be made are tried again.  If PATHS is empty, as
when Spotlight failed, delete nothing."
  (clrhash launcher-icons--requested)
  (when paths
    (let ((keep (make-hash-table :test #'equal)))
      (dolist (path paths)
        (when-let* ((file (launcher-icons--file path)))
          (puthash file t keep)))
      (dolist (file (launcher-icons--pngs))
        (unless (gethash file keep)
          (delete-file file))))))

;;;###autoload
(defun launcher-clear-icon-cache ()
  "Delete the launcher's app icons; it makes them again when next shown.
Delete only the PNGs the launcher made."
  (interactive)
  (when (process-live-p launcher-icons--process)
    (delete-process launcher-icons--process))
  (dolist (file (launcher-icons--pngs))
    (delete-file file)
    (clear-image-cache file))
  (clrhash launcher-icons--requested)
  (message "Cleared the launcher's icon cache"))

(provide 'launcher-icons)
;;; launcher-icons.el ends here
