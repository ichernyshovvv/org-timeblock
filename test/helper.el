;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'org-timeblock)

(defmacro org-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with Org mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1) (debug t))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	 (org-mode-hook nil))
     (with-temp-buffer
       (org-mode)
       (let ((point (string-match "<point>" inside-text)))
	 (if point
	     (progn
	       (insert (replace-match "" nil nil inside-text))
	       (goto-char (1+ (match-beginning 0))))
	   (insert inside-text)
	   (goto-char (point-min))))
       (font-lock-ensure (point-min) (point-max))
       ,@body)))

(defun ot-test-encode-time(timestring)
  (when-let ((time (parse-time-string timestring)))
    (unless (decoded-time-hour time)
      (setf (decoded-time-hour time) 0
	    (decoded-time-minute time) 0))
    (setf (decoded-time-second time) 0)
    (encode-time time)))

(defun ot-get-file-contents(filename)
  (with-temp-buffer (insert-file-contents filename) (buffer-string)))

(defun ot-org-test-settings()
  "Orgmode settings"
  (setq org-log-done nil
	org-id-files nil
	org-property-format "%s %s"
        org-tags-column 0
	org-id-locations-file ,(make-temp-file "org-push-caldav-")
	org-directory (make-temp-file "org-push-caldav-" t)
	org-agenda-files `(,org-directory)))

(defmacro ot-with-temp-org-file (&rest body)
  "Move to buffer and point of point-or-marker POM for the duration of BODY."
  `(let((org-property-format "%s %s")
	(org-tags-column 0)
	(org-file (make-temp-file "org-ts-functions-" nil ".org")))
     (with-current-buffer (find-file-noselect org-file)
       ,@body
       (save-buffer))
     (prog1
	 (with-temp-buffer
	   (insert-file-contents-literally org-file)
	   (buffer-substring-no-properties (point-min) (point-max)))
       (delete-file org-file)
       (and (get-file-buffer org-file)
	    (kill-buffer (get-file-buffer org-file))))))

(defmacro ot-with-temp-org-file2 (&rest body)
  "Move to buffer and point of point-or-marker POM for the duration of BODY."
  `(let((org-property-format "%s %s")
	(org-tags-column 0)
	(org-file (make-temp-file "org-timeblock-" nil ".org")))
     (prog1
	 (with-current-buffer (find-file-noselect org-file)
	   ,@body)
       (delete-file org-file)
       (and (get-file-buffer org-file)
	    (kill-buffer (get-file-buffer org-file))))))

;; If the code under test has side effects on Emacs’s current state,
;; such as on the current buffer or window configuration, the test should
;; create a temporary buffer for the code to manipulate (using with-temp-buffer),
;; or save and restore the window configuration (using save-window-excursion),

(provide 'helper)

;; Local Variables:
;; read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:
