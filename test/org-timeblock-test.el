;; -*- lexical-binding: t; -*-
(add-to-list 'load-path (expand-file-name "."))
(require 'org-timeblock)
(require 'ert)

;; The code is borrowed from `org-test-with-temp-text' which is a macro from
;; org-test.el
(defmacro org-timeblock-test-with-temp-text (text &rest body)
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

(defmacro org-timeblock-with-temp-org-file (&rest body)
  "Move to buffer and point of point-or-marker POM for the duration of BODY."
  `(let ((org-property-format "%s %s")
	 (org-tags-column 0)
	 (org-file (make-temp-file "org-timeblock-" nil ".org")))
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

(dolist (file (directory-files "test" t "^test-"))
  (load file))
