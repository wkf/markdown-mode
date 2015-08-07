;;; markdown-indent.el --- Dynamic indentation for markdown-mode

;;; Commentary:

;; This is a an implementation of dynamic virtual indentation. Inspiration (and most code) taken from org-mode.

;;; Code:

(require 'markdown-mode)

(eval-when-compile
  (require 'cl))

(defgroup markdown-indent nil
  "Options concerning dynamic virtual outline indentation"
  :tag "Markdown Indent"
  :group 'markdown)

(defconst markdown-indent-max 40
  "Maximum indentation in characters.")

(defconst markdown-indent-max-levels 20
  "Maximum added level through virtual indentation in characters.")

(defvar markdown-indent-strings nil
  "Vector with all indentation strings.")

(defvar markdown-indent-headers nil
  "Vector with all indentation header strings.")

(defvar markdown-indent-initial-marker nil
  "Position of initialization before interrupt.")

(defvar markdown-indent-agent-timer nil
  "Timer running the initialize agent.")

(defvar markdown-indent-agentized-buffers nil
  "List of buffers watched by the initialize agent.")

(defvar markdown-indent-agent-resume-timer nil
  "Timer to reschedule agent after switching to other idle processes.")

(defvar markdown-indent-agent-active-delay '(0 2 0)
  "Time to run agent before switching to other idle processes.
Delay used when the buffer to initialize is current.")

(defvar markdown-indent-agent-passive-delay '(0 0 400000)
  "Time to run agent before switching to other idle processes.
Delay used when the buffer to initialize isn't current.")

(defvar markdown-indent-agent-resume-delay '(0 0 100000)
  "Minimal time for other idle processes before switching back to agent.")

(defvar markdown-indent-outline-regexp "#+ "
  "Regexp to match markdown headlines.")

(defvar markdown-indent-outline-regexp-bol "^#+ "
  "Regexp to match markdown headlines.
This is similar to `markdown-indent-outline-regexp' but additionally makes
sure that we are at the beginning of the line.")

(defvar markdown-indent-heading-regexp "^\\(#+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Matches a headline, putting pounds and text into groups.
Headers are put in group 1 and the trimmed body in group 2.")

(defcustom markdown-indent-boundary-char ?\  ; comment to protect space char
  "The end of the virtual indentation strings."
  :group 'markdown-indent
  :set (lambda (var val)
         (set var val)
         (and markdown-indent-strings (markdown-indent-initialize)))
  :type 'character)

(defcustom markdown-indent-indentation-per-level 2
  "Indentation per level in number of characters."
  :group 'markdown-indent
  :type 'integer)

(defgroup markdown-indent-faces nil
  "Options concerning dynamic virtual outline indentation"
  :tag "Markdown Indent"
  :group 'markdown-indent)

(defface markdown-indent-space nil
  "Face for outline indentation."
  :group 'markdown-indent-faces)

(defface markdown-indent-hide nil
  "Face for outline indentation."
  :group 'markdown-indent-faces)

(defun markdown-indent-restart-font-lock ()
  "Restart `font-lock-mode', to force refontification."
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1)
    (font-lock-mode 1)))

(defun markdown-indent-add-props (string plist &rest props)
  "Add text properties to entire string, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (add-text-properties
   0 (length string) (if props (append plist props) plist) string)
  string)

(defun markdown-indent-initialize ()
  "Initialize the indentation strings."
  (setq markdown-indent-strings (make-vector (1+ markdown-indent-max) nil))
  (setq markdown-indent-headers (make-vector (1+ markdown-indent-max) nil))
  (aset markdown-indent-strings 0 nil)
  (aset markdown-indent-headers 0 nil)
  (loop for i from 1 to markdown-indent-max do
        (aset markdown-indent-strings i
              (markdown-indent-add-props
               (concat (make-string (1- i) ?\ ))
               nil 'face 'markdown-indent-space)))
  (loop for i from 1 to markdown-indent-max do
        (aset markdown-indent-headers i
              (markdown-indent-add-props
               (concat (make-string i ?#))
               nil 'face 'markdown-indent-hide))))

(defsubst markdown-indent-remove-properties (beg end)
  "Remove indentations between BEG and END"
  (with-silent-modifications
    (remove-text-properties beg end '(line-prefix nil wrap-prefix nil))))

(defun markdown-indent-remove-properties-from-string (string)
  "Remove indentation properties from STRING."
  (remove-text-properties 0 (length string)
                          '(line-prefix nil wrap-prefix nil) string)
  string)

(defmacro markdown-indent-with-wide-buffer (&rest body)
  "Execute body while temporarily widening the buffer."
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))
(def-edebug-spec markdown-indent-with-wide-buffer (body))

(defmacro markdown-indent-bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))
(def-edebug-spec markdown-indent-bound-and-true-p (symbolp))

(defun markdown-indent-remove-if-not (predicate seq)
  "Remove everything from SEQ that does not fulfill PREDICATE."
  (let (res e)
    (while seq
      (setq e (pop seq))
      (if (funcall predicate e) (push e res)))
    (nreverse res)))

(defun markdown-indent-back-to-heading (&optional invisible-ok)
  "Call `outline-back-to-heading', but provide a better error message."
  (condition-case nil
      (let ((outline-regexp "#+ "))
        (outline-back-to-heading invisible-ok))
    (error (error "Before first headline at position %d in buffer %s"
                  (point) (current-buffer)))))

(defun markdown-indent-at-item-p ()
  "Is point in a line starting a hand-formatted item?"
  (save-excursion
    (beginning-of-line)
    (looking-at markdown-regex-list)))

(defun markdown-indent-list-item-body-column (item)
  "Return column at which body of ITEM should start."
  (let (bpos bcol tpos tcol)
    (save-excursion
      (goto-char item)
      (looking-at "[ \t]*\\(\\S-+\\)\\(.*[ \t]+::\\)?\\([ \t]+\\|$\\)")
      (setq tpos (match-end 0)
            bpos (match-beginning 1)
            bcol (progn (goto-char bpos) (current-column))
            tcol (progn (goto-char tpos) (current-column)))
      (when (> tcol (+ bcol 20))
        (setq tcol (+ bcol 5))))
    tcol))

(defun markdown-indent-remove-tabs (s &optional width)
  "Replace tabulators in S with spaces.
Assumes that s is a single line, starting in column 0."
  (setq width (or width tab-width))
  (while (string-match "\t" s)
    (setq s (replace-match
             (make-string
              (- (* width (/ (+ (match-beginning 0) width) width))
                 (match-beginning 0)) ?\ )
             t t s)))
  s)

(defun markdown-indent-get-indentation (&optional line)
  "Get the indentation of the current line, interpreting tabs.
When LINE is given, assume it represents a line and compute its indentation."
  (if line
      (if (string-match "^ #" (markdown-indent-remove-tabs line))
          (match-end 0))
    (save-excursion
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (current-column))))

;;;###autoload
(define-minor-mode markdown-indent-mode
  "When active, indent text according to outline structure."
  nil " Indent" nil
  (cond
   (markdown-indent-mode
    ;; mode was turned on
    (set (make-local-variable 'indent-tabs-mode) nil)
    (or markdown-indent-strings (markdown-indent-initialize))
    (set (make-local-variable 'markdown-indent-initial-marker) (copy-marker 1))
    (add-hook 'filter-buffer-substring-functions
              (lambda (fun start end delete)
                (markdown-indent-remove-properties-from-string
                 (funcall fun start end delete)))
              nil t)
    (add-hook 'after-change-functions
              'markdown-indent-refresh-maybe nil 'local)
    (add-hook 'before-change-functions
              'markdown-indent-notify-modified-headline nil 'local)
    (and font-lock-mode (markdown-indent-restart-font-lock))
    (markdown-indent-remove-properties (point-min) (point-max))
    (if markdown-indent-agentized-buffers
        (push (current-buffer) markdown-indent-agentized-buffers)
      (push (current-buffer) markdown-indent-agentized-buffers)
      (setq markdown-indent-agent-timer
            (run-with-idle-timer 0.2 t #'markdown-indent-initialize-agent))))
   (t
    ;; mode was turned off
    (setq markdown-indent-agentized-buffers
          (delq (current-buffer) markdown-indent-agentized-buffers))
    (when (markerp markdown-indent-initial-marker)
      (set-marker markdown-indent-initial-marker nil))
    (remove-hook 'filter-buffer-substring-functions
                 (lambda (fun start end delete)
                   (markdown-indent-remove-properties-from-string
                    (funcall fun start end delete))))
    (remove-hook 'after-change-functions
                 'markdown-indent-refresh-maybe 'local)
    (remove-hook 'before-change-functions
                 'markdown-indent-notify-modified-headline 'local)
    (markdown-indent-with-wide-buffer
     (markdown-indent-remove-properties (point-min) (point-max)))
    (and font-lock-mode (markdown-indent-restart-font-lock))
    (redraw-display))))

(defun markdown-indent-indent-buffer ()
  "Add indentation properties to the accessible part of the buffer."
  (interactive)
  (if (not (derived-mode-p 'markdown-mode))
      (error "Not in Markdown mode")
    (message "Setting buffer indentation.  It may take a few seconds...")
    (markdown-indent-remove-properties (point-min) (point-max))
    (markdown-indent-add-properties (point-min) (point-max))
    (message "Indentation of buffer set.")))

(defun markdown-indent-initialize-agent ()
  "Start or resume current buffer initialization.
Only buffers in `markdown-indent-agentized-buffers' trigger an action.
When no more buffer is being watched, the agent suppress itself."
  (when markdown-indent-agent-resume-timer
    (cancel-timer markdown-indent-agent-resume-timer))
  (setq markdown-indent-agentized-buffers
        (markdown-indent-remove-if-not #'buffer-live-p markdown-indent-agentized-buffers))
  (cond
   ;; Job done:  kill agent.
   ((not markdown-indent-agentized-buffers) (cancel-timer markdown-indent-agent-timer))
   ;; Current buffer is agentized: start/resume initialization
   ;; somewhat aggressively.
   ((memq (current-buffer) markdown-indent-agentized-buffers)
    (markdown-indent-initialize-buffer (current-buffer)
                                       markdown-indent-agent-active-delay))
   ;; Else, start/resume initialization of the last agentized buffer,
   ;; softly.
   (t (markdown-indent-initialize-buffer (car markdown-indent-agentized-buffers)
                                         markdown-indent-agent-passive-delay))))

(defun markdown-indent-initialize-buffer (buffer delay)
  "Set virtual indentation for the buffer BUFFER, asynchronously.
Give hand to other idle processes if it takes longer than DELAY,
a time value."
  (with-current-buffer buffer
    (when markdown-indent-mode
      (markdown-indent-with-wide-buffer
       (let ((interruptp
              ;; Always nil unless interrupted.
              (catch 'interrupt
                (and markdown-indent-initial-marker
                     (marker-position markdown-indent-initial-marker)
                     (markdown-indent-add-properties markdown-indent-initial-marker
                                                     (point-max)
                                                     delay)
                     nil))))
         (move-marker markdown-indent-initial-marker interruptp)
         ;; Job is complete: un-agentize buffer.
         (unless interruptp
           (setq markdown-indent-agentized-buffers
                 (delq buffer markdown-indent-agentized-buffers))))))))

(defsubst markdown-indent-set-line-properties (l w h)
  "Set prefix properties on current line an move to next one.
Prefix properties `line-prefix' and `wrap-prefix' in current line
are set to, respectively, length L and W.
If H is non-nil, `line-prefix' will be starred.  If H is
`inline', the first star will have `org-warning' face.
Assume point is at beginning of line."
  (let ((line (cond
               ((eq 'inline h)
                (let ((headers (aref markdown-indent-headers
                                     (min l markdown-indent-max-levels))))))
               (h (aref markdown-indent-headers
                        (min l markdown-indent-max-levels)))
               (t (aref markdown-indent-strings
                        (min l markdown-indent-max)))))
        (wrap (aref markdown-indent-strings (min w markdown-indent-max))))
    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (point) (min (1+ (point-at-eol)) (point-max))
                         `(line-prefix ,line wrap-prefix ,wrap)))
  (forward-line 1))

(defun markdown-indent-add-properties (beg end &optional delay)
  "Add indentation properties between BEG and END.
When DELAY is non-nil, it must be a time value.  In that case,
the process is asynchronous and can be interrupted, either by
user request, or after DELAY.  This is done by throwing the
`interrupt' tag along with the buffer position where the process
stopped."
  (save-match-data
    (markdown-indent-with-wide-buffer
     (goto-char beg)
     (beginning-of-line)
     ;; 1. Initialize prefix at BEG.  This is done by storing two
     ;;    variables: INLINE-PF and PF, representing respectively
     ;;    length of current `line-prefix' when line is inside an
     ;;    inline task or not.
     (let* ((case-fold-search t)
            (added-ind-per-lvl (abs (1- markdown-indent-indentation-per-level)))
            (pf (save-excursion
                  (and (ignore-errors (markdown-indent-back-to-heading t))
                       (+ (* markdown-indent-indentation-per-level
                             (- (match-end 0) (match-beginning 0) 2)) 2))))
            (time-limit (and delay (time-add (current-time) delay))))
       ;; 2. For each line, set `line-prefix' and `wrap-prefix'
       ;;    properties depending on the type of line (headline,
       ;;    inline task, item or other).
       (with-silent-modifications
         (while (and (<= (point) end) (not (eobp)))
           (cond
            ;; When in asynchronous mode, check if interrupt is
            ;; required.
            ((and delay (input-pending-p)) (throw 'interrupt (point)))
            ;; In asynchronous mode, take a break of
            ;; `markdown-indent-agent-resume-delay' every DELAY to avoid
            ;; blocking any other idle timer or process output.
            ((and delay (time-less-p time-limit (current-time)))
             (setq markdown-indent-agent-resume-timer
                   (run-with-idle-timer
                    (time-add (current-idle-time)
                              markdown-indent-agent-resume-delay)
                    nil #'markdown-indent-initialize-agent))
             (throw 'interrupt (point)))
            ;; Headline or inline task.
            ((looking-at markdown-indent-outline-regexp)
             (let* ((nheaders (- (match-end 0) (match-beginning 0) 1))
                    (line (* added-ind-per-lvl (1- nheaders)))
                    (wrap (+ line (1+ nheaders))))
               (markdown-indent-set-line-properties line wrap t)
               (setq pf wrap)))
            ;; List item: `wrap-prefix' is set where body starts.
            ((markdown-indent-at-item-p)
             (let* ((line (or pf 0))
                    (wrap (+ (markdown-indent-list-item-body-column (point)) line)))
               ;; (print "INDENT LIST ITEM")
               ;; (print pf)
               (markdown-indent-set-line-properties line wrap nil)))
            ;; Normal line: use PF-INLINE, PF or nil as prefixes.
            (t (let* ((line (or pf 0))
                      (wrap (+ line (markdown-indent-get-indentation))))
                 ;; (print "INDENT REGULAR LINE")
                 ;; (print pf)
                 (markdown-indent-set-line-properties line wrap nil))))))))))

(defun markdown-indent-notify-modified-headline (beg end)
  "Set `markdown-indent-modified-headline-flag' depending on context.
BEG and END are the positions of the beginning and end of the
range of deleted text.
This function is meant to be called by `before-change-functions'.
Flag will be non-nil if command is going to modify or delete an
headline."
  (when markdown-indent-mode
    (setq markdown-indent-modified-headline-flag
          (save-excursion
            (goto-char beg)
            (save-match-data
              (or (and (outline-on-heading-p t) (< beg (match-end 0)))
                  (re-search-forward markdown-indent-outline-regexp-bol end t)))))))

(defun markdown-indent-refresh-maybe (beg end dummy)
  "Refresh indentation properties in an adequate portion of buffer.
BEG and END are the positions of the beginning and end of the
range of inserted text.  DUMMY is an unused argument.
This function is meant to be called by `after-change-functions'."
  (when markdown-indent-mode
    (save-match-data
      ;; If a headline was modified or inserted, set properties until
      ;; next headline.
      (if (or markdown-indent-modified-headline-flag
              (save-excursion
                (goto-char beg)
                (beginning-of-line)
                (re-search-forward markdown-indent-outline-regexp-bol end t)))
          (let ((end (save-excursion
                       (goto-char end)
                       (outline-next-heading)
                       (point))))
            (setq markdown-indent-modified-headline-flag nil)
            (markdown-indent-add-properties beg end))
        ;; Otherwise, only set properties on modified area.
        (markdown-indent-add-properties beg end)))))

(provide 'markdown-indent-mode)
