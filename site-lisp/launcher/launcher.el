;;; launcher.el --- Launch macOS apps from Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'url-util)

(defgroup launcher nil
  "Launch macOS applications from Emacs."
  :group 'external)

;; After the group, which its options belong to.
(require 'launcher-icons)

(defcustom launcher-app-discovery-method 'spotlight
  "Method used to discover applications.
`spotlight' queries Spotlight and `filesystem' scans application directories.
Run `launcher-refresh' after changing this option to rebuild the session's
cached application list."
  :type '(choice (const :tag "Spotlight" spotlight)
                 (const :tag "File system" filesystem))
  :group 'launcher)

(defcustom launcher-mdfind-query
  "kMDItemContentTypeTree == \"com.apple.application-bundle\""
  "Spotlight query used to discover application bundles.
Only used when `launcher-app-discovery-method' is `spotlight'."
  :type 'string
  :group 'launcher)

(defcustom launcher-app-directories
  '("/Applications"
    "/System/Applications"
    "/System/Library/CoreServices"
    "~/Applications")
  "Directories to search for applications.
These are the standard macOS application directories.
User-specific directories can be added here."
  :type '(repeat string)
  :group 'launcher)

(defcustom launcher-fallback-search-url
  "https://www.google.com/search?q=%s"
  "URL template for unprefixed search when all tools return no candidates.
%s will be replaced with the URL-encoded search query.
Input starting with http:// or https:// (ignoring case) opens directly instead."
  :type 'string
  :group 'launcher)

(defcustom launcher-builtin-icons nil
  "Icons for built-in calculator, search and URL actions.
Each entry is (KEY . ICON), where KEY is `calculator', `search' or `url'.
ICON is nil, a string or (:file PNG-PATH).  Missing entries show blank space.
Search tools without an explicit icon also use the `search' entry.
References (:builtin KEY) resolve when displayed, so changing this option
also updates tools that were created earlier.  Values must not be references."
  :type '(alist :key-type (choice (const calculator) (const search) (const url))
                :value-type (choice (const nil) string (list (const :file) file)))
  :group 'launcher)

(cl-defun launcher-search-tool (prefix &key name url icon)
  "Return a search tool entry for PREFIX, NAME, URL and optional ICON.
PREFIX must be a nonempty token and NAME a nonblank label.
URL is an Emacs format string: %s receives the URL-encoded query and
%% represents a literal percent sign.  Blank queries produce no search
result.  ICON uses the same formats as `launcher-tools'.  A nil or omitted
ICON uses the `search' entry of `launcher-builtin-icons'; an empty string
keeps the icon blank.
This function does not modify `launcher-tools'; add its return value
there to configure the search tool."
  (unless (and (stringp prefix) (not (string-empty-p prefix))
               (not (string-match-p "[[:space:]]" prefix)))
    (user-error "Search PREFIX must be a nonempty string without whitespace"))
  (unless (and (stringp name) (not (string-blank-p name)))
    (user-error "Search :name must be a nonblank string"))
  (unless (and (stringp url) (not (string-blank-p url)))
    (user-error "Search :url must be a nonblank format string"))
  (unless (launcher-icons-valid-p icon)
    (user-error "Search :icon must be nil, a string, (:file PNG-PATH), or (:builtin KEY)"))
  (list prefix :name name :icon (or icon '(:builtin search)) :submit #'launcher--url-submit
        :query (lambda (query)
                 (unless (string-blank-p query)
                   (list (list :id prefix :title (format "Search %s: %s" name query)
                               :data (format url (url-hexify-string query))))))))

(defcustom launcher-tools
  (list (launcher-search-tool "g" :name "Google"
                              :url "https://www.google.com/search?q=%s"))
  "Tools following the built-in applications and calculator.
Defaults to the Google search tool.  Setting this to nil
removes all configured tools; unprefixed fallback search remains available.
Use `launcher-search-tool' to construct search entries.
Replacing this list also replaces the default search tools.
Each entry is (PREFIX :name NAME :icon ICON :candidates SOURCE :query QUERY
:submit SUBMIT :debounce SECONDS).  PREFIX is nil, empty, or one token.
SOURCE is a full list of candidate plists or a no-argument function;
its value (including an empty list or failure) is cached per session.
Static titles are matched using completion styles, category `launcher-tool'.
QUERY receives the input after PREFIX and its space, or all unprefixed
input.  It returns dynamic candidates, already matched.  Both sources
may be configured; dynamic candidates replace static matches with the
same ID.  Each source must have unique IDs.  With neither source there
are no content candidates.  Blank input queries no dynamic sources.

Candidates require :id and :title, with optional :subtitle, :icon and
arbitrary :data.  ICON is nil, a string, (:file PNG-PATH) or (:builtin KEY),
where KEY is calculator, search or url from `launcher-builtin-icons'.
The tool's ICON appears on its prefix entry and is the default for
candidates without :icon.  An explicit candidate :icon, including nil
or an empty string, overrides it.  Text icon properties are preserved.
SUBMIT receives the selected candidate and returns
nil, a live non-minibuffer buffer, or (:input STRING).  Tools with a source
require SUBMIT.  The former tool/candidate :function is not supported.
:debounce is a finite nonnegative delay for QUERY only (default 0).

An explicit prefix routes exclusively to its tool.  A blank query shows
all static candidates, or the prefix entry for a tool without SOURCE.
Successful selections are recorded separately from full input history."
  :type '(repeat (cons :tag "Tool"
                       (choice (const :tag "No prefix" nil) string)
                       (plist :tag "Properties"
                              :options ((:name string) (:icon sexp) (:candidates sexp)
                                        (:query function) (:submit function)
                                        (:debounce number)))))
  :group 'launcher)

(defvar launcher--apps nil
  "Cached app entries as an alist of (display-name . app-path).")

(defun launcher--collect-paths-spotlight ()
  "Return discovered .app paths from standard Application directories.
Searches only in `launcher-app-directories' using Spotlight's -onlyin flag."
  (unless (executable-find "mdfind")
    (user-error "Cannot find `mdfind` in PATH"))
  (let (results)
    (dolist (dir launcher-app-directories)
      (let ((expanded-dir (expand-file-name dir)))
        (when (file-directory-p expanded-dir)
          (setq results
                (append results
                        (process-lines "mdfind"
                                       "-onlyin" expanded-dir
                                       launcher-mdfind-query))))))
    (seq-filter
     (lambda (path)
       (and (string-suffix-p ".app" path)
            (file-directory-p path)))
     results)))

(defun launcher--collect-paths-filesystem ()
  "Scan `launcher-app-directories' for application bundles.
Stop at .app directories.  Follow explicitly configured roots, but do
not recurse into directory symlinks encountered below them."
  (let ((pending (mapcar #'expand-file-name launcher-app-directories))
        results)
    (while pending
      (let ((dir (pop pending)))
        (when (file-directory-p dir)
          (if (string-suffix-p ".app" (directory-file-name dir))
              (push (directory-file-name dir) results)
            (condition-case err
                (dolist (path (directory-files dir t directory-files-no-dot-files-regexp))
                  (when (file-directory-p path)
                    (cond
                     ((string-suffix-p ".app" path) (push path results))
                     ((not (file-symlink-p path)) (push path pending)))))
              (file-error
               (message "Launcher cannot scan %s: %s"
                        dir (error-message-string err))))))))
    (nreverse results)))

(defun launcher--collect-paths ()
  "Discover apps using `launcher-app-discovery-method'.
Return unique absolute paths without resolving symlinks."
  (let ((paths
         (pcase launcher-app-discovery-method
           ('spotlight (launcher--collect-paths-spotlight))
           ('filesystem (launcher--collect-paths-filesystem))
           (_ (user-error "Invalid `launcher-app-discovery-method': %S"
                          launcher-app-discovery-method)))))
    (delete-dups (mapcar (lambda (path)
                          (directory-file-name (expand-file-name path)))
                        paths))))

(defun launcher--build-entries (paths)
  "Convert PATHS to an alist of (display-name . app-path)."
  (let ((counts (make-hash-table :test #'equal))
        entries)
    (dolist (path paths)
      (let ((name (file-name-base (directory-file-name path))))
        (puthash name (1+ (gethash name counts 0)) counts)
        (push (cons name path) entries)))
    (dolist (entry entries)
      (when (> (gethash (car entry) counts) 1)
        (setcar entry (format "%s  (%s)" (car entry) (abbreviate-file-name (cdr entry))))))
    (sort entries (lambda (a b) (string-lessp (car a) (car b))))))

(defun launcher-refresh ()
  "Refresh the cached list of launchable macOS apps."
  (interactive)
  (setq launcher--apps
        (launcher--build-entries (launcher--collect-paths)))
  (condition-case err
      (launcher-icons-index-refreshed (mapcar #'cdr launcher--apps))
    (error (message "Launcher icons not refreshed: %s" (error-message-string err))))
  (message "Indexed %d applications" (length launcher--apps)))

(defun launcher--launch (app-path)
  "Launch APP-PATH with macOS `open`."
  (unless (executable-find "open")
    (user-error "Cannot find `open` in PATH"))
  (unless (and app-path (file-directory-p app-path))
    (user-error "Invalid app path: %S" app-path))
  (let ((exit-code (call-process "open" nil nil nil "-a" app-path)))
    (unless (zerop exit-code)
      (user-error "Failed to launch app: %s" app-path))))

;;; Tools

(cl-defstruct (launcher--tool
               (:constructor launcher--tool-make)
               (:copier nil))
  "A validated entry of `launcher-tools'."
  prefix name icon candidates candidates-p query submit (debounce 0) (category 'launcher-tool))

(defun launcher--callable-p (value)
  "Whether VALUE is a function or a nonempty function name."
  (or (functionp value)
      (and value (symbolp value) (not (keywordp value)) (not (eq value t)))))

(defun launcher--tool-from-entry (entry)
  "Validate ENTRY and return its tool."
  (cl-flet ((invalid (format &rest args)
              (user-error "Invalid `launcher-tools' entry %S: %s"
                          entry (apply #'format format args))))
    (unless (and (consp entry) (proper-list-p (cdr entry))
                 (cl-evenp (length (cdr entry))))
      (invalid "expected (PREFIX :name NAME ...)"))
    (let* ((prefix (car entry)) (props (cdr entry))
           (name (plist-get props :name))
           (icon (plist-get props :icon))
           (candidates (plist-get props :candidates))
           (candidates-p (and (plist-member props :candidates) t))
           (submit (plist-get props :submit))
           (query (plist-get props :query))
           (debounce (or (plist-get props :debounce) 0)))
      (unless (or (null prefix)
                  (and (stringp prefix)
                       (not (string-match-p "[[:space:]]" prefix))))
        (invalid "PREFIX must be nil or a single string without whitespace"))
      (when (equal prefix "") (setq prefix nil))
      (cl-loop for key in props by #'cddr
               when (eq key :function)
               do (invalid "migrate :function to :submit and candidate :data")
               unless (memq key '(:name :icon :candidates :query :submit :debounce))
               do (invalid "unknown property %S" key))
      (unless (and (stringp name) (not (string-blank-p name)))
        (invalid ":name must be a nonempty string"))
      (unless (launcher-icons-valid-p icon)
        (invalid ":icon must be nil, a string, (:file PNG-PATH), or (:builtin KEY)"))
      (unless (and (numberp debounce) (>= debounce 0) (<= debounce most-positive-fixnum))
        (invalid ":debounce must be a finite nonnegative number"))
      (when (and query (not (launcher--callable-p query)))
        (invalid ":query must be a function or its name"))
      (when (and candidates-p
                 (not (or (proper-list-p candidates)
                          (launcher--callable-p candidates))))
        (invalid ":candidates must be a list or a no-argument function"))
      (when (and submit (not (launcher--callable-p submit)))
        (invalid ":submit must be a function or its name"))
      (when (and (or candidates-p query) (not submit))
        (invalid "a candidate source requires :submit"))
      (launcher--tool-make
       :prefix prefix :name name :icon icon :candidates candidates :candidates-p candidates-p
       :query query :submit submit :debounce debounce))))

(defun launcher--tools ()
  "Return validated custom tools, in configuration order."
  (unless (proper-list-p launcher-tools)
    (user-error "`launcher-tools' must be a list of tools: %S" launcher-tools))
  (let (tools)
    (dolist (entry launcher-tools)
      (let* ((tool (launcher--tool-from-entry entry))
             (prefix (launcher--tool-prefix tool)))
        (when (and prefix
                   (seq-find (lambda (other) (equal (launcher--tool-prefix other) prefix)) tools))
          (user-error "Two `launcher-tools' entries use the prefix %S" prefix))
        (push tool tools)))
    (nreverse tools)))

(defun launcher--route (input tools)
  "Return (TOOL . QUERY) if INPUT starts with the prefix of one of TOOLS.
The prefix must be followed by an ASCII space, and matches exactly,
case included.  QUERY is the rest of INPUT, unchanged.  Return nil for
any other INPUT, which is ordinary app or web input."
  (when-let* (((stringp input))
              (space (string-search " " input))
              (prefix (substring input 0 space))
              (tool (seq-find (lambda (tool) (equal (launcher--tool-prefix tool) prefix))
                              tools)))
    (cons tool (substring input (1+ space)))))

(defun launcher--entries (refresh)
  "Return cached apps, refreshing if requested; keep other tools usable."
  (condition-case err
      (progn
        (when (or refresh (null launcher--apps)) (launcher-refresh))
        (or launcher--apps
            (user-error "No apps discovered from configured application directories")))
    (error
     (message "Launcher apps unavailable: %s" (error-message-string err))
     nil)))

(require 'launcher-query)

;;;###autoload
(defun launcher (&optional refresh initial)
  "Query apps, calculator, custom tools and web searches as you type.
Empty input shows no candidates.  Return executes the selected result.
A tool prefix stays in the input box.  Results that return a buffer
are shown with `pop-to-buffer' after the minibuffer closes.
With prefix argument REFRESH, rebuild the application index first.
INITIAL, when supplied from Lisp, is the full starting input."
  (interactive "P")
  (let* ((tools (launcher--tools))
         (entries (launcher--entries refresh))
         (answer (launcher--read entries tools initial)))
    (when (bufferp (car answer)) (pop-to-buffer (car answer)))))

(provide 'launcher)
;;; launcher.el ends here
