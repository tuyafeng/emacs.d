;;; launcher-osx-dictionary.el --- Apple Dictionary tool for launcher -*- lexical-binding: t; -*-

;;; Commentary:

;; `launcher-osx-dictionary-lookup' is a handler for `launcher-tools': it
;; looks a word up in Apple's dictionaries with the osx-dictionary package
;; (https://github.com/xuchunyang/osx-dictionary.el) and returns the
;; package's rendering of the result, without displaying it.  Register it
;; under a prefix of your choice:
;;
;;   (require 'launcher-osx-dictionary)
;;   (setq launcher-tools
;;         '(("d" :name "Dictionary"
;;                 :query launcher-osx-dictionary-query :debounce 0.2
;;                 :submit launcher-osx-dictionary-submit)))
;;
;; Loading this file loads neither osx-dictionary nor its helper; the
;; first lookup does, and reports what is missing instead of looking up.
;; Needs macOS, osx-dictionary and Xcode's command line tools, with which
;; the first lookup builds the package's helper if it is missing.
;;
;; Tested with osx-dictionary commit 655bca5 (2026-09-06).  It has no
;; public function that looks a word up without prompting or displaying,
;; so this file uses its private `osx-dictionary--insert-search-result',
;; `osx-dictionary--current-dictionary-description',
;; `osx-dictionary--current-word' and `osx-dictionary--load-dir', and
;; reports a version that lacks them.

;;; Code:

(require 'launcher)
(require 'nadvice)
(require 'url-util)

(defvar osx-dictionary-cli)
(defvar osx-dictionary--load-dir)
(defvar osx-dictionary--current-word)
(defvar osx-dictionary-current-dictionary)
(defvar osx-dictionary-search-log-file)
(declare-function osx-dictionary-mode "osx-dictionary" ())
(declare-function osx-dictionary--insert-search-result "osx-dictionary" (word))
(declare-function osx-dictionary--current-dictionary-description "osx-dictionary" ())
(declare-function osx-dictionary-select-dictionary "osx-dictionary" (&optional dictionary))

(defcustom launcher-osx-dictionary-buffer-name "*Launcher Dictionary*"
  "Name of the buffer `launcher-osx-dictionary-lookup' shows results in.
It differs from osx-dictionary's own buffer, so that lookups from the
launcher leave the package's own session alone."
  :type 'string
  :group 'launcher)

(defconst launcher-osx-dictionary--functions
  '(osx-dictionary-mode osx-dictionary--insert-search-result
    osx-dictionary--current-dictionary-description osx-dictionary-select-dictionary)
  "Functions of osx-dictionary this file uses.")

(defconst launcher-osx-dictionary--variables
  '(osx-dictionary-cli osx-dictionary--load-dir osx-dictionary--current-word
    osx-dictionary-current-dictionary)
  "Variables of osx-dictionary this file uses.")

(defvar-local launcher-osx-dictionary--speech-process nil
  "Speech process for this result buffer.")

(defvar-local launcher-osx-dictionary--result-prefix nil
  "Launcher tool prefix that produced the displayed result.")

(defun launcher-osx-dictionary--query-prefix ()
  "Return the prefix of the active launcher query, if any."
  (when launcher--query-session
    (when-let* ((route (launcher--query-session-route launcher--query-session)))
      (launcher--tool-prefix (car route)))))

(defun launcher-osx-dictionary--require ()
  "Load osx-dictionary, or signal an error saying what is missing."
  (unless (eq system-type 'darwin)
    (error "Apple dictionaries are only available on macOS"))
  (unless (require 'osx-dictionary nil t)
    (error "Needs the osx-dictionary package: M-x package-install RET osx-dictionary"))
  (when-let* ((missing (append (seq-remove #'fboundp launcher-osx-dictionary--functions)
                              (seq-remove #'boundp launcher-osx-dictionary--variables))))
    (error "This osx-dictionary lacks %s; launcher-osx-dictionary was tested with commit 655bca5"
           (mapconcat #'symbol-name missing ", "))))

(defun launcher-osx-dictionary--find-helper ()
  "Return the osx-dictionary helper, where the package looks for it, or nil."
  (or (executable-find (expand-file-name osx-dictionary-cli osx-dictionary--load-dir))
      (executable-find osx-dictionary-cli)))

(defun launcher-osx-dictionary--developer-tools-p ()
  "Return non-nil if Xcode's command line tools are installed.
Without them, Apple's clang stub would offer to install them instead."
  (and (executable-find "xcode-select")
       (eql 0 (call-process "xcode-select" nil nil nil "-p"))))

(defun launcher-osx-dictionary--build-helper ()
  "Build the osx-dictionary helper where the package looks for it first.
Return its file name, or signal an error saying why it could not."
  (let ((source (expand-file-name "osx-dictionary.m" osx-dictionary--load-dir))
        (output (expand-file-name osx-dictionary-cli osx-dictionary--load-dir)))
    (unless (file-readable-p source)
      (error "The osx-dictionary helper source %s is missing; reinstall the package"
             (abbreviate-file-name source)))
    (unless (and (executable-find "clang") (launcher-osx-dictionary--developer-tools-p))
      (error "Building the osx-dictionary helper needs the Xcode command line tools: \
run xcode-select --install"))
    (unless (file-writable-p output)
      (error "Cannot build the osx-dictionary helper: %s is not writable"
             (abbreviate-file-name osx-dictionary--load-dir)))
    (message "Building the osx-dictionary helper...")
    (with-temp-buffer
      ;; The command of `osx-dictionary-recompile', which hides failures.
      (let ((status (call-process "clang" nil t nil "-O3"
                                  "-framework" "CoreServices" "-framework" "Foundation"
                                  source "-o" output)))
        (unless (and (eql status 0) (file-executable-p output))
          (error "Building the osx-dictionary helper failed (%s): %s" status
                 (string-trim (buffer-substring (point-min)
                                                (min (point-max) 1000)))))))
    (message "Building the osx-dictionary helper...done")
    output))

(defun launcher-osx-dictionary--dictionaries (helper)
  "Return the dictionaries HELPER lists, or signal an error if it fails."
  (with-temp-buffer
    (let ((status (call-process helper nil '(t nil) nil "-l")))
      (unless (eql status 0)
        (error "The osx-dictionary helper failed (%s)" status))
      (split-string (buffer-string) "\n" t))))

(defun launcher-osx-dictionary--no-result (word helper)
  "Signal an error saying why HELPER found nothing for WORD."
  (let ((installed (launcher-osx-dictionary--dictionaries helper)))
    (cond ((null installed)
           (error "Dictionary.app lists no installed dictionaries"))
          ((and osx-dictionary-current-dictionary
                (not (member osx-dictionary-current-dictionary installed)))
           (error "Dictionary %S is not installed; choose another with M-x \
osx-dictionary-select-dictionary" osx-dictionary-current-dictionary))
          (t
           (error "No definition for %S (searched: %s)" word
                  (osx-dictionary--current-dictionary-description))))))

(defun launcher-osx-dictionary--search (word)
  "Return osx-dictionary's rendering of WORD's definitions, as a string.
Signal an error if it finds none."
  (let ((helper (or (launcher-osx-dictionary--find-helper)
                    (launcher-osx-dictionary--build-helper))))
    (with-temp-buffer
      ;; Rendered as `osx-dictionary--view-result' does, but here.
      (osx-dictionary--insert-search-result word)
      (when (string-blank-p (buffer-string))
        (launcher-osx-dictionary--no-result word helper))
      (whitespace-cleanup)
      (buffer-string))))

(defvar-keymap launcher-osx-dictionary-result-mode-map
  :doc "Keys of launcher's dictionary results, over `osx-dictionary-mode-map'."
  "q" #'launcher-osx-dictionary-quit
  "s" #'launcher-osx-dictionary-search
  "S" #'launcher-osx-dictionary-select-dictionary
  "r" #'launcher-osx-dictionary-read-word
  "o" #'launcher-osx-dictionary-open-dictionary)

(define-minor-mode launcher-osx-dictionary-result-mode
  "Minor mode of launcher's dictionary results.
It replaces the `osx-dictionary-mode' keys that would display the
package's own buffer, or restore the window configuration it saved,
in this buffer only, and provides asynchronous speech and URL opening:
\\{launcher-osx-dictionary-result-mode-map}"
  :lighter nil)

(defun launcher-osx-dictionary--render (word text &optional prefix)
  "Show TEXT, the definitions of WORD, in the result buffer and return it.
Display nothing.  Remember the originating tool PREFIX for further searches."
  (let ((buffer (get-buffer-create launcher-osx-dictionary-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'osx-dictionary-mode)
        (osx-dictionary-mode))
      ;; User hooks can enable View mode for read-only buffers.  Its
      ;; minor-mode map shadows dictionary keys (notably r with backward
      ;; isearch); the dictionary mode already provides read-only results.
      (when (bound-and-true-p view-mode)
        (view-mode -1))
      (launcher-osx-dictionary--stop-speech)
      (add-hook 'kill-buffer-hook #'launcher-osx-dictionary--stop-speech nil t)
      ;; These generated results have no edits to undo or save.
      (setq buffer-undo-list t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      ;; For the package's commands that act on the word, such as `o'.
      (setq osx-dictionary--current-word word)
      (setq launcher-osx-dictionary--result-prefix prefix)
      (launcher-osx-dictionary-result-mode 1)
      (goto-char (point-min))
      (dolist (window (get-buffer-window-list buffer nil t))
        (set-window-point window (point-min))
        (set-window-start window (point-min))))
    buffer))

(defun launcher-osx-dictionary--text (word &optional preview)
  "Return definitions for trimmed WORD without changing a result buffer.
With PREVIEW, leave the package's search log untouched."
  (launcher-osx-dictionary--require)
  (let* ((osx-dictionary-search-log-file
          (and (not preview) osx-dictionary-search-log-file
               (expand-file-name osx-dictionary-search-log-file)))
         (default-directory (file-name-as-directory osx-dictionary--load-dir)))
    (launcher-osx-dictionary--search word)))

;;;###autoload
(defun launcher-osx-dictionary-query (query)
  "Return a definition preview for QUERY, without displaying or rendering a buffer.
Use this as a tool's :query with :debounce 0.2.  Selecting the candidate
renders the captured definitions without looking the word up again."
  (let ((word (string-trim query)))
    (unless (string-empty-p word)
      (let* ((prefix (launcher-osx-dictionary--query-prefix))
             (text (launcher-osx-dictionary--text word t))
             (log-file (and osx-dictionary-search-log-file
                            (expand-file-name osx-dictionary-search-log-file)))
             (summary (truncate-string-to-width
                       (string-join (split-string (substring-no-properties text) "[[:space:]]+" t) " ")
                       160 nil nil "…")))
        (list (list :id word :title word :subtitle summary
                    :data (list :word word :text text :prefix prefix :log-file log-file)))))))

;;;###autoload
(defun launcher-osx-dictionary-submit (candidate)
  "Render CANDIDATE's captured definition without another lookup."
  (let* ((data (plist-get candidate :data))
         (word (plist-get data :word))
         (log-file (plist-get data :log-file)))
    (when log-file (append-to-file (concat word "\n") nil log-file))
    (launcher-osx-dictionary--render word (plist-get data :text)
                                    (plist-get data :prefix))))

;;;###autoload
(defun launcher-osx-dictionary-lookup (query)
  "Look QUERY up in Apple's dictionaries; return the result buffer.
A handler for `launcher-tools'.  Look up QUERY without surrounding
whitespace, in the dictionaries osx-dictionary is set to search, and
render the result as the package does, in the buffer named by
`launcher-osx-dictionary-buffer-name', without displaying it.  The
buffer is in `osx-dictionary-mode', with the keys of
`launcher-osx-dictionary-result-mode'.

Signal an error, leaving the buffer as it was, if osx-dictionary or
its helper is missing or fails, or if there is no definition.  Build
the helper first if it is missing, which blocks Emacs meanwhile, as
does the lookup."
  (let ((word (string-trim query)))
    (when (string-empty-p word)
      (error "Nothing to look up"))
    (launcher-osx-dictionary--render word (launcher-osx-dictionary--text word)
                                     (launcher-osx-dictionary--query-prefix))))

(defun launcher-osx-dictionary-quit ()
  "Quit the result window without restoring the package's saved layout."
  (interactive)
  (launcher-osx-dictionary--stop-speech)
  (quit-window))

(defun launcher-osx-dictionary--stop-speech ()
  "Stop any speech belonging to this result buffer."
  (when (and (processp launcher-osx-dictionary--speech-process)
             (process-live-p launcher-osx-dictionary--speech-process))
    (delete-process launcher-osx-dictionary--speech-process))
  (setq launcher-osx-dictionary--speech-process nil))

(defun launcher-osx-dictionary--word ()
  "Return the displayed query, or report that there is no result."
  (unless (and (boundp 'osx-dictionary--current-word)
               (stringp osx-dictionary--current-word)
               (not (string-blank-p osx-dictionary--current-word)))
    (user-error "No dictionary word to act on"))
  osx-dictionary--current-word)

(defun launcher-osx-dictionary-read-word ()
  "Read the displayed word asynchronously; repeated calls restart speech."
  (interactive)
  (let ((word (launcher-osx-dictionary--word)))
    (unless (executable-find "say")
      (user-error "The macOS speech command `say' is unavailable"))
    (launcher-osx-dictionary--stop-speech)
    (setq launcher-osx-dictionary--speech-process
          (make-process :name "launcher-dictionary-speech" :buffer nil
                        :command '("say" "-f" "-") :connection-type 'pipe
                        :coding 'utf-8-unix :noquery t
                        :sentinel (lambda (process _event)
                                    (when (and (eq (process-status process) 'exit)
                                               (not (zerop (process-exit-status process))))
                                      (message "Dictionary speech failed (%s)"
                                               (process-exit-status process))))))
    (process-send-string launcher-osx-dictionary--speech-process (concat word "\n"))
    (process-send-eof launcher-osx-dictionary--speech-process)))

(defun launcher-osx-dictionary-open-dictionary ()
  "Open the displayed word or phrase in Dictionary.app."
  (interactive)
  (let ((word (launcher-osx-dictionary--word)))
    (unless (executable-find "open")
      (user-error "The macOS command `open' is unavailable"))
    (make-process :name "launcher-dictionary-open" :buffer nil :noquery t
                  :sentinel (lambda (process _event)
                              (when (and (eq (process-status process) 'exit)
                                         (not (zerop (process-exit-status process))))
                                (message "Opening Dictionary.app failed (%s)"
                                         (process-exit-status process))))
                  :command (list "open" (concat "dict://" (url-hexify-string word))))))

(defun launcher-osx-dictionary-search ()
  "Return to launcher with the configured dictionary prefix ready for input."
  (interactive)
  (let* ((tools (seq-filter
                 (lambda (tool)
                   (and (launcher--tool-prefix tool)
                        (or (eq (launcher--tool-query tool) #'launcher-osx-dictionary-query)
                            (eq (launcher--tool-submit tool) #'launcher-osx-dictionary-submit))))
                 (launcher--tools)))
         (tool (or (seq-find (lambda (tool)
                              (equal (launcher--tool-prefix tool)
                                     launcher-osx-dictionary--result-prefix))
                            tools)
                   (car tools))))
    (unless tool
      (user-error "Configure a prefixed dictionary tool in `launcher-tools' first"))
    (launcher nil (concat (launcher--tool-prefix tool) " "))))

(defun launcher-osx-dictionary-select-dictionary ()
  "Choose the dictionaries to search, then look this word up again here.
Prompt as `osx-dictionary-select-dictionary', which saves the choice
for later lookups, but show the word in this buffer, not in the
package's own."
  (interactive)
  (let* ((word (launcher-osx-dictionary--word))
         (prefix launcher-osx-dictionary--result-prefix)
         (default-directory (file-name-as-directory osx-dictionary--load-dir))
         (args (advice-eval-interactive-spec
                (cadr (interactive-form #'osx-dictionary-select-dictionary)))))
    ;; Outside its mode, the command does not show the word itself.
    (with-temp-buffer
      (apply #'osx-dictionary-select-dictionary args))
    (launcher-osx-dictionary--render word (launcher-osx-dictionary--text word) prefix)))

(provide 'launcher-osx-dictionary)
;;; launcher-osx-dictionary.el ends here
