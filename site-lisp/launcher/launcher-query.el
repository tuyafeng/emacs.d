;;; launcher-query.el --- Dynamic launcher results -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared synchronous query engine and completion adapter.  Loaded by
;; launcher after its tool and application definitions, without Vertico.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defvar launcher-fallback-search-url)
(declare-function launcher--launch "launcher" (app-path))
(declare-function launcher--callable-p "launcher" (value))
(declare-function launcher--route "launcher" (input tools))

(defvar vertico--input)
(defvar vertico--index)
(defvar vertico--candidates)
(defvar vertico--lock-candidate)
(defvar vertico--base)
(defvar vertico-sort--history)
(defvar vertico-sort-function)
(defvar vertico-sort-override-function)
(declare-function vertico--update "vertico" (&optional interruptible))
(declare-function vertico--exhibit "vertico" ())
(declare-function vertico--candidate "vertico" ())

(cl-defstruct (launcher--query-session (:constructor launcher--query-session-create))
  tools styles overrides defaults buffer input (version 0)
  slots static-cache timers errors pending results candidates closed answer
  status-overlay selection route)

(defvar-local launcher--query-session nil "This minibuffer's launcher query session.")
(defvar launcher-selection-history nil
  "Recently executed candidate IDs, newest first.
Add this variable to `savehist-additional-variables' to persist selections.")

(defun launcher--results-history-sort (sorter)
  "Adapt SORTER to selection history without changing input history."
  (when sorter
    (lambda (candidates)
      ;; Candidates are full IDs; do not strip the query or reuse its history cache.
      (let ((minibuffer-history-variable 'launcher-selection-history)
            (minibuffer-default nil)
            (minibuffer-completion-base "")
            (vertico--base "")
            (vertico-sort--history nil))
        (funcall sorter candidates)))))

(defun launcher--apps-candidates (entries)
  "Convert cached app ENTRIES into static candidates."
  (mapcar (lambda (entry)
            (let ((name (substring-no-properties (car entry))) (path (cdr entry)))
              (list :id path :title name :subtitle (abbreviate-file-name path)
                    :icon (launcher-icons-app path))))
          entries))

(defun launcher--apps-submit (candidate)
  "Launch the app represented by CANDIDATE."
  (launcher--launch (plist-get candidate :id))
  (message "Launching %s" (plist-get candidate :title))
  nil)

(defun launcher--static-match (session tool input results)
  "Match INPUT against RESULTS' titles using SESSION's styles for TOOL.
Keep source order and all candidates sharing a matching title."
  (if (string-blank-p input) results
    (let* ((completion-styles (launcher--query-session-styles session))
           (completion-category-overrides (launcher--query-session-overrides session))
           (completion-category-defaults (launcher--query-session-defaults session))
           (completion-ignore-case t)
           (titles (delete-dups (mapcar (lambda (item) (plist-get item :title)) results)))
           (category (launcher--tool-category tool))
           (table (lambda (str pred action)
                    (if (eq action 'metadata) `(metadata (category . ,category))
                      (complete-with-action action titles str pred))))
           (matches (completion-all-completions input table nil (length input)))
           (base (or (cdr (last matches)) 0))
           (prefix (substring input 0 base))
           (selected (make-hash-table :test #'equal)))
      (while (consp matches)
        (puthash (downcase (concat prefix (substring-no-properties (pop matches)))) t selected))
      (seq-filter (lambda (item) (gethash (downcase (plist-get item :title)) selected)) results))))

(defun launcher--sources-merge (static dynamic)
  "Union STATIC and DYNAMIC by ID, replacing static matches in place."
  (let ((remaining (make-hash-table :test #'equal)))
    (dolist (item dynamic) (puthash (plist-get item :id) item remaining))
    (append (mapcar (lambda (item)
                      (let* ((id (plist-get item :id))
                             (replacement (gethash id remaining)))
                        (remhash id remaining)
                        (or replacement item))) static)
            (seq-filter (lambda (item) (gethash (plist-get item :id) remaining)) dynamic))))

(defun launcher--static-run (session index tool input)
  "Load TOOL once and match its static candidates for INPUT in SESSION."
  (when (launcher--tool-candidates-p tool)
    (let ((cached (gethash index (launcher--query-session-static-cache session) 'unloaded)))
      (when (eq cached 'unloaded)
        (let ((launcher--query-session session) results failure)
          (condition-case err
              (progn
                (setq results (if (launcher--callable-p (launcher--tool-candidates tool))
                                  (funcall (launcher--tool-candidates tool))
                                (launcher--tool-candidates tool)))
                (launcher--query-validate-results results tool))
            (error (setq results nil failure (error-message-string err))))
          (setq cached (list results failure))
          (puthash index cached (launcher--query-session-static-cache session))))
      (let ((failure (cadr cached)))
        (unless failure
          (condition-case err
              (let ((matches (launcher--static-match session tool input (car cached))))
                (puthash index matches (launcher--query-session-slots session)))
            (error (setq failure (error-message-string err)))))
        (when failure
          (puthash index (format "%s (candidates): %s" (launcher--tool-name tool) failure)
                   (launcher--query-session-errors session)))))))

(defun launcher--arithmetic (text)
  "Parse and calculate TEXT using only decimal numbers and + - * / ().
Return nil for incomplete or invalid syntax; signal an error for division
by zero.  No Lisp forms, variables, function calls or evaluation are used."
  (let ((pos 0) (len (length text)) division-by-zero)
    (cl-labels
        ((space () (while (and (< pos len) (memq (aref text pos) '(?\s ?\t ?\n)))
                     (cl-incf pos)))
         (peek () (space) (and (< pos len) (aref text pos)))
         (factor ()
           (pcase (peek)
             ((or ?+ ?-) (let ((negative (= (aref text pos) ?-)))
                           (cl-incf pos)
                           (* (if negative -1 1) (factor))))
             (?\( (cl-incf pos)
                  (let ((value (sum)))
                    (unless (eq (peek) ?\)) (throw 'invalid nil))
                    (cl-incf pos) value))
             (_ (space)
                (unless (and (string-match "\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)" text pos)
                             (= (match-beginning 0) pos))
                  (throw 'invalid nil))
                (let ((number (match-string 0 text)))
                  (setq pos (match-end 0)) (string-to-number number)))))
         (product ()
           (let ((value (factor)))
             (while (memq (peek) '(?* ?/))
               (let ((op (aref text pos)))
                 (cl-incf pos)
                 (let ((right (factor)))
                   (setq value (cond ((= op ?*) (* value right))
                                     ((zerop right) (setq division-by-zero t) 0)
                                     (t (/ (float value) right)))))))
             value))
         (sum ()
           (let ((value (product)))
             (while (memq (peek) '(?+ ?-))
               (let ((op (aref text pos)))
                 (cl-incf pos)
                 (let ((right (product)))
                   (setq value (if (= op ?+) (+ value right) (- value right))))))
             value)))
      (catch 'invalid
        (let ((value (sum)))
          (space)
          (when (= pos len)
            (when division-by-zero (error "Division by zero"))
            value))))))

(defun launcher--calculator-query (input)
  "Return a calculation candidate, including INVALID for unfinished INPUT.
Invalid candidates retain the calculation identity and refuse execution."
  (let* ((explicit (string-prefix-p "=" input))
         (expression (if explicit (substring input 1) input)))
    (when (or explicit
              (and (string-match-p "\\`[0-9.() +*/\t\n-]+\\'" expression)
                   (string-match-p "[()+*/-]" expression)))
      (let ((number (condition-case nil (launcher--arithmetic expression)
                      (error nil))))
        (if (null number)
            (list (list :id 'calculation :title "= INVALID"
                        :subtitle "Incomplete or invalid expression"
                        :data nil))
          (let ((value (if (integerp number) (number-to-string number)
                         (format "%.12g" number))))
            (list (list :id 'calculation :title (concat "= " value)
                        :subtitle "Copy result"
                        :data value))))))))

(defun launcher--calculator-submit (candidate)
  "Copy CANDIDATE's calculated value, refusing incomplete expressions."
  (let ((value (plist-get candidate :data)))
    (unless value (user-error "Complete or correct the expression first"))
    (kill-new value)
    (message "Copied %s" value)
    nil))

(defun launcher--url-submit (candidate)
  "Open the URL stored in CANDIDATE."
  (browse-url (plist-get candidate :data))
  nil)

(defun launcher--prefix-result (tool &optional input)
  "Return an entry that fills TOOL's prefix and a separating space.
With INPUT, preserve that full input when selecting the entry."
  (let ((input (or input (concat (launcher--tool-prefix tool) " "))))
    (list :id 'prefix-entry :title (launcher--tool-prefix tool)
          :subtitle (launcher--tool-name tool) :icon (launcher--tool-icon tool)
          :completion input)))

(defun launcher--fallback-result (input)
  "Make a URL-opening or default search action for unmatched INPUT.
Open INPUT unchanged when it starts with http:// or https://, ignoring case."
  (let* ((case-fold-search t)
         (direct-url (string-match-p "\\`https?://" input))
         (url (if direct-url input
                (format launcher-fallback-search-url (url-hexify-string input)))))
    (list :id 'fallback :title (concat (if direct-url "Open URL: " "Search web: ") input)
          :data url :icon (if direct-url '(:builtin url) '(:builtin search)))))

(defun launcher--query-create (entries tools)
  "Create a query session for ENTRIES and custom TOOLS."
  (launcher--query-session-create
   :tools (append (list (launcher--tool-make :name "Applications"
                                             :candidates (lambda () (launcher--apps-candidates entries))
                                             :candidates-p t :category 'launcher-app
                                             :submit #'launcher--apps-submit)
                        (launcher--tool-make :name "Calculator" :icon '(:builtin calculator)
                                             :query #'launcher--calculator-query
                                             :submit #'launcher--calculator-submit))
                  tools)
   :styles completion-styles :overrides completion-category-overrides
   :defaults completion-category-defaults
   :static-cache (make-hash-table :test #'eql)
   :slots (make-hash-table :test #'eql) :timers (make-hash-table :test #'eql)
   :errors (make-hash-table :test #'eql) :pending (make-hash-table :test #'eql)
   :candidates (make-hash-table :test #'equal)))

(defun launcher--query-close (session)
  "Cancel pending queries and invalidate SESSION."
  (setf (launcher--query-session-closed session) t)
  (maphash (lambda (_ timer) (cancel-timer timer)) (launcher--query-session-timers session))
  (clrhash (launcher--query-session-timers session))
  (clrhash (launcher--query-session-pending session))
  (when (overlayp (launcher--query-session-status-overlay session))
    (delete-overlay (launcher--query-session-status-overlay session))))

(defun launcher--query-valid-p (session version)
  "Whether VERSION can still publish into SESSION."
  (and (not (launcher--query-session-closed session))
       (= version (launcher--query-session-version session))
       (or (null (launcher--query-session-buffer session))
           (and (buffer-live-p (launcher--query-session-buffer session))
                (eq (buffer-local-value 'launcher--query-session
                                        (launcher--query-session-buffer session)) session)))))

(defun launcher--query-validate-results (results tool)
  "Validate RESULTS returned by TOOL, before publishing any of them."
  (unless (proper-list-p results) (error "Query must return a list of results"))
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (result results)
      (when (and (proper-list-p result) (plist-member result :function))
        (error "Migrate candidate :function to tool :submit and candidate :data"))
      (unless (and (proper-list-p result) (cl-evenp (length result))
                   (plist-get result :id)
                   (stringp (plist-get result :title))
                   (not (string-blank-p (plist-get result :title)))
                   (cl-loop for key in result by #'cddr
                            always (memq key '(:id :title :subtitle :icon :data)))
                   (or (null (plist-get result :subtitle)) (stringp (plist-get result :subtitle)))
                   (launcher-icons-valid-p (plist-get result :icon)))
        (error "%s returned an invalid result: %S" (launcher--tool-name tool) result))
      (let ((id (plist-get result :id)))
        (when (gethash id ids) (error "Duplicate result id: %S" id))
        (puthash id t ids)))))

(defun launcher--query-run (session index tool query version)
  "Run TOOL for QUERY in SESSION VERSION without publishing.
Return t if results were committed to the still-valid session."
  (when (launcher--query-valid-p session version)
    (remhash index (launcher--query-session-timers session))
    (let ((launcher--query-session session)
          ;; Each dynamic source runs once per input, replacing these matches.
          (static (gethash index (launcher--query-session-slots session)))
          results failure)
      (condition-case err
          (progn
            (setq results (funcall (launcher--tool-query tool) query))
            (launcher--query-validate-results results tool))
        (error (setq results nil
                     failure (format "%s: %s" (launcher--tool-name tool) (error-message-string err)))))
      (when (launcher--query-valid-p session version)
        (remhash index (launcher--query-session-pending session))
        (puthash index (launcher--sources-merge static results)
                 (launcher--query-session-slots session))
        (when failure
          (let ((static-error (gethash index (launcher--query-session-errors session))))
            (puthash index (if static-error (concat static-error "; " failure) failure)
                     (launcher--query-session-errors session))))
        t))))

(defun launcher--query-update (session input)
  "Schedule queries for SESSION if INPUT changed; clear obsolete results."
  (unless (equal input (launcher--query-session-input session))
    (when-let* ((buffer (launcher--query-session-buffer session))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (when (and (boundp 'vertico--input) vertico--input)
          (setf (launcher--query-session-selection session)
                (and (>= vertico--index 0) (nth vertico--index vertico--candidates))))))
    (setf (launcher--query-session-input session) input
          (launcher--query-session-route session)
          (launcher--route input (launcher--query-session-tools session)))
    (cl-incf (launcher--query-session-version session))
    (maphash (lambda (_ timer) (cancel-timer timer)) (launcher--query-session-timers session))
    (clrhash (launcher--query-session-timers session))
    (clrhash (launcher--query-session-slots session))
    (clrhash (launcher--query-session-errors session))
    (clrhash (launcher--query-session-pending session))
    ;; Invalidate old actions immediately, but publish only after the immediate
    ;; queries finish so the UI never displays their intermediate empty state.
    (clrhash (launcher--query-session-candidates session))
    (setf (launcher--query-session-results session) nil)
    (unless (string-blank-p input)
      (cl-loop for tool in (launcher--query-session-tools session) for index from 0
               for prefix = (launcher--tool-prefix tool)
               for route = (launcher--query-session-route session)
               do (cond
                   ((and route (eq tool (car route)) (string-blank-p (cdr route))
                         (not (launcher--tool-candidates-p tool)))
                    (puthash index (list (launcher--prefix-result tool input))
                             (launcher--query-session-slots session)))
                   ((if route (eq tool (car route)) (null prefix))
                    (let* ((query (if route (cdr route) input))
                           (version (launcher--query-session-version session))
                           (delay (launcher--tool-debounce tool)))
                      (launcher--static-run session index tool query)
                      (when (and (launcher--tool-query tool) (not (string-blank-p query)))
                        (if (zerop delay)
                            (launcher--query-run session index tool query version)
                          (puthash index (format "Querying %s..." (launcher--tool-name tool))
                                   (launcher--query-session-pending session))
                          (puthash index
                                   (run-at-time delay nil #'launcher--query-timer
                                                session index tool query version)
                                   (launcher--query-session-timers session))))))
                   ((and (null route) prefix (not (string-search " " input))
                         (string-prefix-p input prefix))
                    (puthash index (list (launcher--prefix-result tool))
                             (launcher--query-session-slots session))))))
    (launcher--query-publish session)))

(defun launcher--query-timer (session index tool query version)
  "Run a debounced query, retaining the minibuffer's dynamic bindings."
  (when (launcher--query-valid-p session version)
    (let ((buffer (launcher--query-session-buffer session)))
      (with-current-buffer (or buffer (current-buffer))
        ;; A key event may have changed text before its post-command hook.
        (when (and (or (null buffer)
                       (equal (minibuffer-contents-no-properties)
                              (launcher--query-session-input session)))
                   (launcher--query-run session index tool query version))
          (launcher--query-publish session))))))

(defun launcher--query-item (session tool item)
  "Give ITEM from TOOL a completion identity stable across sessions.
The identity includes the tool name, prefix and result ID.
Inherit TOOL's icon only when ITEM has no :icon, leaving ITEM unchanged."
  (let* ((identity (list (and tool (launcher--tool-name tool))
                         (and tool (launcher--tool-prefix tool))
                         (plist-get item :id)))
         (key (concat "launcher:"
                      (secure-hash 'sha256 (prin1-to-string identity)))))
    ;; Titles may change independently of the completion identity.
    (setq key (propertize key 'display (plist-get item :title)))
    (let ((result (append (list :tool tool :candidate key :item item) item
                          (unless (plist-member item :icon)
                            (list :icon (and tool (launcher--tool-icon tool)))))))
      (puthash key result (launcher--query-session-candidates session))
      result)))

(defun launcher--query-publish (session)
  "Merge tools in order; offer a URL or search fallback when results are empty."
  (let (results)
    (clrhash (launcher--query-session-candidates session))
    (cl-loop for tool in (launcher--query-session-tools session) for index from 0
             do (dolist (item (gethash index (launcher--query-session-slots session)))
                  (push (launcher--query-item session tool item) results)))
    (when (and (null results)
               (null (launcher--query-session-route session))
               (launcher--query-session-input session)
               (not (string-blank-p (launcher--query-session-input session))))
      (push (launcher--query-item session (launcher--tool-make :submit #'launcher--url-submit)
                                  (launcher--fallback-result (launcher--query-session-input session)))
            results))
    (setf (launcher--query-session-results session) (nreverse results))
    (let ((buffer (launcher--query-session-buffer session)))
      (when (and (buffer-live-p buffer)
                 (eq (buffer-local-value 'launcher--query-session buffer) session))
        (with-current-buffer buffer
          (when (and (boundp 'vertico--input) vertico--input)
            (setq-local vertico--lock-candidate t vertico--input nil)
            (vertico--update)
            (setq-local vertico--index
                        (or (seq-position vertico--candidates (launcher--query-session-selection session))
                            (if vertico--candidates 0 -1)))
            (vertico--exhibit))
          (launcher--query-status session))))))

(defun launcher--query-status (session &optional notice)
  "Display SESSION's pending queries, errors or NOTICE without a candidate."
  (let ((overlay (launcher--query-session-status-overlay session)) messages)
    (unless (overlayp overlay)
      (setq overlay (make-overlay (point-max) (point-max) nil t t))
      (setf (launcher--query-session-status-overlay session) overlay))
    (cl-loop for index from 0 below (length (launcher--query-session-tools session))
             for status = (string-join
                           (delq nil (list (gethash index (launcher--query-session-errors session))
                                           (gethash index (launcher--query-session-pending session))))
                           "; ")
             unless (string-empty-p status) do (push status messages))
    (move-overlay overlay (point-max) (point-max))
    (overlay-put overlay 'after-string
                 (when-let* ((text (or notice (and messages (string-join (nreverse messages) "; "))
                                      (when (and (launcher--query-session-route session)
                                                 (null (launcher--query-session-results session))
                                                 (zerop (hash-table-count (launcher--query-session-timers session))))
                                        "No results"))))
                   (propertize (concat "  " text) 'face
                               (if (or notice (> (hash-table-count (launcher--query-session-errors session)) 0))
                                   'warning 'shadow))))))

(defun launcher--results-all (_string table pred _point)
  "Completion style: return already matched results in their original order."
  (let ((items (all-completions "" table pred)))
    (when items (nconc items 0))))

(defun launcher--results-try (string _table _pred point)
  "Keep STRING and POINT unchanged; result actions do not complete text."
  (cons string point))

(defun launcher--results-collection (session)
  "Return SESSION's identity-preserving dynamic completion table."
  (lambda (string pred action)
    (cond
     ((eq action 'metadata)
      '(metadata (category . launcher-result)))
     ((eq action t)
      (seq-filter (lambda (candidate) (or (null pred) (funcall pred candidate)))
                  (mapcar (lambda (result) (plist-get result :candidate))
                          (launcher--query-session-results session))))
     ((eq action 'lambda) (and (gethash string (launcher--query-session-candidates session)) t))
     (t nil))))

(defun launcher--result-for-candidate (candidate)
  "Find the current structured result for CANDIDATE."
  (and launcher--query-session
       (gethash candidate (launcher--query-session-candidates launcher--query-session))))

(defun launcher--results-annotation (candidate)
  "Annotate CANDIDATE using its structured result."
  (when-let* ((subtitle (plist-get (launcher--result-for-candidate candidate) :subtitle)))
    (concat "  " subtitle)))

(defun launcher--results-affixation (candidates)
  "Affix CANDIDATES with their tool-provided icons and subtitles."
  (mapcar (lambda (candidate)
            (list candidate
                  (launcher-icons-prefix (plist-get (launcher--result-for-candidate candidate) :icon))
                  (or (launcher--results-annotation candidate) ""))) candidates))

(defun launcher--results-setup-history ()
  "Initialize a private input history snapshot for minibuffer navigation."
  (setq-local minibuffer-history-variable (make-symbol "launcher-input-history")
              minibuffer-history-position 0
              minibuffer-text-before-history nil)
  ;; Emacs can resolve history in the buffer that opened the minibuffer.
  ;; Keep completion's automatic history writes in a private symbol.
  (set minibuffer-history-variable (copy-sequence minibuffer-history)))

(defun launcher--results-post-command ()
  "Update queries only when the actual minibuffer text changes."
  (when launcher--query-session
    (let* ((input (minibuffer-contents-no-properties))
           (inserted (launcher--result-for-candidate input)))
      ;; Stock completion can insert an opaque action key.  Keep the typed
      ;; query and remember the chosen identity instead of querying its label.
      (when inserted
        (delete-minibuffer-contents)
        (insert (launcher--query-session-input launcher--query-session))
        (setq input (minibuffer-contents-no-properties)))
      (when (and (equal input (launcher--query-session-input launcher--query-session))
                 (boundp 'vertico--input) vertico--input)
        (setf (launcher--query-session-selection launcher--query-session)
              (and (>= vertico--index 0) (nth vertico--index vertico--candidates))))
      (launcher--query-update launcher--query-session input)
      (when inserted
        (setf (launcher--query-session-selection launcher--query-session)
              (plist-get inserted :candidate))))))

(defun launcher--results-current ()
  "Return the selected structured result, or the first for stock completion."
  (let ((session launcher--query-session))
    (when (and (boundp 'vertico--input) vertico--input)
      (vertico--update))
    (or (launcher--result-for-candidate (minibuffer-contents-no-properties))
        (when (and (boundp 'vertico--input) vertico--input (>= vertico--index 0))
          (launcher--result-for-candidate (vertico--candidate)))
        (when (not (and (boundp 'vertico--input) vertico--input))
          (launcher--result-for-candidate (launcher--query-session-selection session)))
        (car (launcher--query-session-results session)))))

(defun launcher--result-execute (result)
  "Execute RESULT and validate its action return value."
  (let ((value (if (plist-member result :completion)
                   (list :input (plist-get result :completion))
                 (funcall (launcher--tool-submit (plist-get result :tool))
                          (plist-get result :item)))))
    (unless (or (null value)
                (and (bufferp value) (buffer-live-p value) (not (minibufferp value)))
                (and (proper-list-p value) (= (length value) 2)
                     (eq (car value) :input) (stringp (cadr value))))
      (error "Result action must return nil, a live buffer, or (:input STRING)"))
    value))

(defun launcher--results-record-selection (result)
  "Record RESULT's successfully executed candidate identity."
  (let ((history-delete-duplicates t))
    (add-to-history 'launcher-selection-history
                    (substring-no-properties (plist-get result :candidate)))))

(defun launcher--results-exit (session)
  "Record the final minibuffer query and close SESSION."
  (unless (launcher--query-session-closed session)
    (unwind-protect
        (let* ((input (minibuffer-contents-no-properties))
               (query (if (gethash input (launcher--query-session-candidates session))
                          (launcher--query-session-input session)
                        input)))
          (unless (string-blank-p query)
            (add-to-history 'minibuffer-history query)))
      (launcher--query-close session))))

(defun launcher-results-submit ()
  "Execute only a current candidate; keep input when an action fails."
  (interactive)
  (let* ((session launcher--query-session)
         (input (minibuffer-contents-no-properties)))
    (when (string-blank-p input) (signal 'quit nil))
    (launcher--query-update session input)
    (if-let* ((result (launcher--results-current)))
        (condition-case err
            (let ((value (launcher--result-execute result)))
              (if (and (consp value) (eq (car value) :input))
                  (progn (delete-minibuffer-contents) (insert (cadr value))
                         (launcher--results-post-command))
                (launcher--results-record-selection result)
                (setf (launcher--query-session-answer session) (cons value input))
                (exit-minibuffer)))
          (error (launcher--query-status session (error-message-string err))))
      (launcher--query-status session
                              (unless (launcher--query-session-route session) "No result yet")))))

(defun launcher-results-insert ()
  "Insert the selected result's title, preserving the active tool prefix.
Prefix entries insert their completion text unchanged."
  (interactive)
  (when-let* ((result (launcher--results-current)))
    (let* ((route (launcher--query-session-route launcher--query-session))
           (text (cond
                  ((plist-member result :completion)
                   (plist-get result :completion))
                  (route
                   (concat (launcher--tool-prefix (car route)) " "
                           (plist-get result :title)))
                  (t (plist-get result :title)))))
      (delete-minibuffer-contents)
      (insert text))))

(defvar-keymap launcher-query-map
  :doc "Keys in the unified launcher input box."
  "RET" #'launcher-results-submit
  "<return>" #'launcher-results-submit
  "<remap> <exit-minibuffer>" #'launcher-results-submit
  "<remap> <minibuffer-complete-and-exit>" #'launcher-results-submit
  "<remap> <vertico-exit>" #'launcher-results-submit
  "<remap> <vertico-exit-input>" #'launcher-results-submit
  "<remap> <vertico-insert>" #'launcher-results-insert)

(defun launcher--read (entries tools &optional initial)
  "Read and execute dynamic results for ENTRIES and TOOLS.
INITIAL is the full starting input.  Return (ACTION-VALUE . FULL-INPUT),
after closing the minibuffer.  No raw input is executed."
  (let* ((session (launcher--query-create entries tools))
         (completion-styles-alist
          (cons '(launcher-result launcher--results-try launcher--results-all
                                  "Already matched launcher actions.") completion-styles-alist))
         (completion-category-overrides
          (cons '(launcher-result (styles launcher-result))
                (assq-delete-all 'launcher-result (copy-tree completion-category-overrides))))
         (icons (launcher-icons-enabled-p))
         choice
         (completion-extra-properties
          (if icons '(:affixation-function launcher--results-affixation)
            '(:annotation-function launcher--results-annotation))))
    (when (and icons entries)
      (condition-case err (launcher-icons-prepare (mapcar #'cdr entries))
        (error (message "Launcher icons unavailable: %s" (error-message-string err)))))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq-local launcher--query-session session)
              (setf (launcher--query-session-buffer session) (current-buffer))
              (when (eq (key-binding " ") #'minibuffer-complete-word)
                (let ((map (make-sparse-keymap)))
                  (set-keymap-parent map (current-local-map))
                  (define-key map " " #'self-insert-command)
                  (use-local-map map)))
              (use-local-map (make-composed-keymap launcher-query-map (current-local-map)))
              (when (boundp 'vertico--input)
                (setq-local vertico-preselect 'first))
              (add-hook 'post-command-hook #'launcher--results-post-command -10 t)
              (add-hook 'minibuffer-exit-hook (lambda () (launcher--results-exit session)) nil t)
              (when (boundp 'vertico-sort-function)
                (setq-local vertico-sort-function
                            (launcher--results-history-sort vertico-sort-function)))
              (when (boundp 'vertico-sort-override-function)
                (setq-local vertico-sort-override-function
                            (launcher--results-history-sort vertico-sort-override-function)))
              (launcher--results-setup-history)
              (launcher--results-post-command))
          (setq choice
                (completing-read (if entries "Launch: " "Launch (apps unavailable): ")
                                 (launcher--results-collection session) nil nil initial t)))
      (launcher--query-close session))
    (or (launcher--query-session-answer session)
        ;; Stock mouse completion and alternative readers may exit directly
        ;; with an opaque candidate.  Resolve only a current identity, never
        ;; a visible label or arbitrary text, and keep actions single-shot.
        (when-let* ((result (gethash choice (launcher--query-session-candidates session))))
          (let ((value (condition-case err
                           (launcher--result-execute result)
                         (error
                          (message "%s" (error-message-string err))
                          (list :input (launcher--query-session-input session))))))
            (if (and (consp value) (eq (car value) :input))
                (launcher--read entries tools (cadr value))
              (launcher--results-record-selection result)
              (cons value (launcher--query-session-input session)))))
        (signal 'quit nil))))

(provide 'launcher-query)
;;; launcher-query.el ends here
