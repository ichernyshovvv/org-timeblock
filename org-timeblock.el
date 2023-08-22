;;; org-timeblock.el --- Schedule your day visually, using timeblocking technique inside Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (compat "29.1") (org-ql "0.7") (org "9.0") (svg "1.1") (persist "0.5"))
;; Keywords: org, calendar, timeblocking, agenda
;; URL: https://github.com/ichernyshovvv/org-timeblock

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-timeblock.el is a package that allows you to visually
;; understand your day schedule, quickly reschedule your tasks and set
;; TODO statuses

;;; Code:

;;;; Requirements

(require 'org)
(require 'svg)
(require 'color)
(require 'seq)
(require 'org-ql)
(require 'text-property-search)
(require 'persist)
(require 'compat)
(require 'compat-macs)

;;;; Faces

(defface ot-list-header '((t (:inherit org-agenda-structure)))
  "Face used in agenda for `org-super-agenda' group name header."
  :group 'org-timeblock)

;;;; Custom Variables

(defgroup org-timeblock nil
  "Customization for `org-timeblock'."
  :group 'org
  :link '(url-link "https://github.com/ichernyshovvv/org-timeblock"))

(defcustom ot-show-outline-path nil
  "Non-nil means show outline path in echo area for the selected item."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Don't show outline path with prepended file name." nil)
	  (const :tag "Show outline path." t)))

(defcustom ot-n-days-view 3
  "Number of days displayed in org-timeblock."
  :group 'org-timeblock
  :type 'integer)

(defcustom ot-display-time t
  "Non-nil means show end and start time of events or tasks inside timeblocks."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Show time." t)
	  (const :tag "Do not show time." nil)))

(defcustom ot-inbox-file
  (expand-file-name "inbox.org" org-directory)
  "Org file in which new tasks are created via `org-timeblock-new-task'."
  :group 'org-timeblock
  :type 'file)

(defcustom ot-new-task-time
  'pick
  "Time to which new tasks are scheduled via `org-timeblock-new-task'."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Unspecified.  The new task will be scheduled to a date with no time" nil)
	  (const :tag "The new task will be scheduled to a time picked by user." pick)
	  (string :tag "Time of the format \"HH:MM\".  The new task will be scheduled to a time.")))

(defcustom ot-view-options t
  "Options that are used to decide which part of visual schedule must be hidden."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Hide hours in the past (if there are no timeblocks)." t)
	  (const :tag "Do not hide anything.  All 24 hours will be displayed." nil)
	  (const :tag "Hide all free hours before the first timeblock." hide-all)))

(defcustom ot-fast-todo-commands
  '(("TODO" . "1")
    ("DONE" . "5"))
  "Fast TODO keyword selection with single keys.

Alist where each cons is an orgmode todo state and a key.  For
each car a command is created (\"TODO\" -> `org-timeblock-todo')
and then bound inside `org-timeblock-list-mode-map' and
`org-timeblock-mode-map' respectively to a key in cdr.

Keys should be a string in the format returned by commands such
as `describe-key'."
  :group 'org-timeblock
  :type 'alist)

(defcustom ot-current-time-indicator t
  "Whether to show current time indicator in the `org-timeblock-list' buffer."
  :group 'org-timeblock
  :type 'boolean)

(defcustom ot-tag-colors
  nil
  "Colors for specific tags.

List of lists where each list is of the form
  (\"tagname\" \"background color\" \"foreground color\").
Colors are set in hex format.  Example:

  ((\"tag1\" \"#f3d000\" \"#000000\")
   (\"tag2\" \"#ff8f88\" \"#000000\"))

In `org-timeblock-mode', timeblocks tagged with a tag in car are
painted in background color.  In `org-timeblock-list-mode', both
background and foreground colors are used to colorize items that
are tagged with a tag in car."
  :type 'list
  :group 'org-timeblock)

;;;; Variables

(defvar ot-sel-block-color-light "#f3d000")

(defvar ot-sel-block-color-dark "#3f1651")

(defvar ot-sel-block-color ot-sel-block-color-light)

(defvar ot-background-color (face-attribute 'default :background))

(defvar ot-colors nil)

(defvar ot-data nil)
(defvar ot-current-column 1)

(defvar ot-svg-width 0)
(defvar ot-svg-height 0)
(defvar ot-svg-obj nil)

(persist-defvar ot-list-entries-pos nil
  "Saved positions for entries in `org-timeblock-list-mode'.
Nested alist of saved positions of the entries for each date that
a user have previously opened in `org-timeblock-list-mode'.")

(defvar ot-prev-selected-block-color nil)

(defvar ot-sort-function #'ot-order<)

(defvar ot-daterange nil
  "The date range that is used to get and display schedule data.")

(defvar ot-duration-multipliers
  '((?w . 10080)
    (?d . 1440)
    (?h . 60)
    (?m . 1))
  "Duration multipliers used in `ot-read-duration'.")

(defvar ot-buffer "*org-timeblock*" "The name of the buffer displaying visual schedule.")

(defvar ot-list-buffer "*org-timeblock-list*"
  "The name of the buffer displaying the list of tasks and events.")

(persist-defvar ot-list-sortline-pos nil
  "Sort indicator line position.
The line position of the sort line which is displayed in
`org-timeblock-list-mode' when orgmode tasks are manually
placed.  Used as a simple separator to distinguish manually sorted
tasks and those tasks that have not been sorted yet.")

;;;; Keymaps

(defvar-keymap ot-mode-map
  "+" #'ot-new-task
  "<mouse-1>" #'ot-select-block-under-mouse
  "<down>" #'ot-forward-block
  "<right>" #'ot-forward-column
  "<left>" #'ot-backward-column
  "<up>" #'ot-backward-block
  "C-<down>" #'ot-day-later
  "C-<up>" #'ot-day-earlier
  "RET" #'ot-goto
  "TAB" #'ot-goto-other-window
  "d" #'ot-set-duration
  "g" #'ot-redraw-buffers
  "j" #'ot-jump-to-day
  "s" #'ot-schedule
  "t" #'ot-toggle-timeblock-list
  "v" #'ot-switch-scaling
  "V" #'ot-switch-view)

(defvar-keymap ot-list-mode-map
  "+" #'ot-new-task
  "<down>" #'ot-list-next-line
  "<up>" #'ot-list-previous-line
  "C-<down>" #'ot-day-later
  "C-<up>" #'ot-day-earlier
  "C-s" #'ot-list-save
  "M-<down>" #'ot-list-drag-line-forward
  "M-<up>" #'ot-list-drag-line-backward
  "RET" #'ot-list-goto
  "TAB" #'ot-list-goto-other-window
  "S" #'ot-list-toggle-sort-function
  "d" #'ot-list-set-duration
  "g" #'ot-redraw-buffers
  "j" #'ot-jump-to-day
  "q" #'ot-quit
  "s" #'ot-list-schedule
  "t" #'ot-list-toggle-timeblock
  "v" #'ot-switch-scaling
  "V" #'ot-switch-view)

;; Generate todo commands and bind them to a corresponding key
(dolist (elem ot-fast-todo-commands)
  (let ((command-name (intern (format "%s%s" 'ot- (downcase (car elem))))))
    (defalias command-name
      (lambda ()
	(interactive)
	(when-let ((m (pcase major-mode
			(`ot-list-mode
			 (get-text-property (line-beginning-position) 'marker))
			(`ot-mode
			 (ot-selected-block-marker)))))
	  (ot--set-todo m (car elem))
	  (ot-redraw-buffers))))
    (define-key ot-list-mode-map (kbd (cdr elem)) command-name)
    (define-key ot-mode-map (kbd (cdr elem)) command-name)))

;;;; Modes

(defvar image-transform-resize)
(define-derived-mode org-timeblock-mode image-mode "Org-Timeblock" :interactive nil
  (if-let ((window (get-buffer-window ot-buffer))
	   ((or (< (window-body-height window t) ot-svg-height)
		(< (window-body-width window t) ot-svg-width))))
      (ot-redraw-timeblocks)
    (setq image-transform-resize nil
	  header-line-format
	  (let* ((window (get-buffer-window ot-buffer))
		 (window-width (window-body-width window t))
		 (dates (ot-get-dates))
		 (right-margin (format "%% -%ds" (/ window-width (default-font-width) (length dates))))
		 (result (make-string (/ (car (window-edges window t nil t)) (default-font-width)) ? )))
	    (dotimes (iter (length dates) result)
	      (cl-callf concat result
		(propertize (format right-margin (ts-format "[%Y-%m-%d %a]" (nth iter dates))) 'face
			    (when (= ot-current-column (1+ iter)) (list :background ot-sel-block-color))))))
	  buffer-read-only t)))

(define-derived-mode org-timeblock-list-mode special-mode "Org-Timeblock-List" :interactive nil
  (setq truncate-lines t))

;;;; Functions

(compat-version "29.1")

(compat-defun org-fold-show-context (&optional key)
  "Make sure point and context are visible."
  (org-show-context key))

(defun ot-show-context ()
  "Make sure point and context are visible."
  (compat-call org-fold-show-context 'agenda))

(cl-defsubst ot-get-sched (&optional object (position 0))
  "Return the value of POSITION's \\='sched property, in OBJECT.
If OBJECT is nil, try to get the property from current buffer at POSITION.

\\='sched property is an org-element timestamp object."
  (get-text-property position 'sched object))

(cl-defsubst ot-get-event (&optional object (position 0))
  "Return the value of POSITION's \\='event property, in OBJECT.
If OBJECT is nil, try to get the property from current buffer at POSITION.

\\='event property is an org-element timestamp object."
  (get-text-property position 'event object))

(cl-defsubst ot-get-sched-or-event (&optional object (position 0))
  "Return POSITION's \\='sched or \\='event property, in OBJECT."
  (or (ot-get-sched object position)
      (ot-get-event object position)))

(defun ot-mouse-pixel-pos ()
  "Return current mouse position in the window of the *org-timeblock* buffer.
If mouse position is outside of the window, return nil.

Mouse position is of the form (X . Y)."
  (when-let ((mouse-pos (cdr (mouse-pixel-position)))
	     (window (get-buffer-window ot-buffer))
	     (pos (window-edges window t nil t)))
    (when (and (> (- (car mouse-pos) (car pos)) 0)
	       (> (- (cdr mouse-pos) (cadr pos)) 0))
      (cons (- (car mouse-pos) (car pos))
	    (- (cdr mouse-pos) (cadr pos))))))

(defun ot-selected-block-marker ()
  "Return a marker pointing to the org entry of selected timeblock."
  (goto-char (point-min))
  (and
   (re-search-forward (format "<rect .*? id=\"\\([^\"]+\\)\" fill=\"%s\"" ot-sel-block-color) nil t)
   (let ((id (match-string-no-properties 1)))
     (cadr (seq-find (lambda (x) (string= (car x) id)) ot-data)))))

(defun ot-block-eventp (id)
  "Return t if block with ID is an event."
  (caddr (seq-find (lambda (x) (string= (car x) id)) ot-data)))

(defun ot-get-dates ()
  "Return a list of ts.el struct dates between org-timeblock-daterange."
  (let (dates (start-date (car ot-daterange)))
    (while (ot-ts-date< start-date (cdr ot-daterange))
      (push start-date dates)
      (setq start-date (ts-inc 'day 1 start-date)))
    (nreverse dates)))

(defun ot-selected-block-id ()
  "Return an id of the entry of selected timeblock.
id is constructed via `ot-construct-id'"
  (goto-char (point-min))
  (and
   (re-search-forward (format "<rect .*? id=\"\\([^\"]+\\)\" fill=\"%s\"" ot-sel-block-color) nil t)
   (match-string-no-properties 1)))

(defmacro ot-on (accessor op lhs rhs)
  "Run OP on ACCESSOR's return values from LHS and RHS."
  `(,op (,accessor ,lhs) (,accessor ,rhs)))

(defun ot-ts-date= (a b)
  "Return t if dates of ts.el ts objects A and B are equal."
  (cond
   ((and (null a) (null b)))
   ((and a b)
    (and (ot-on ts-year  = a b)
         (ot-on ts-month = a b)
         (ot-on ts-day   = a b)))))

(defun ot-ts-date< (a b)
  "Return t, if A's date is earlier then B's date.
A and B are ts.el ts objects."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (or (ot-on ts-year < a b)
	(and
	 (ot-on ts-year = a b)
	 (or (ot-on ts-month < a b)
	     (and (ot-on ts-month = a b)
		  (ot-on ts-day < a b))))))))

(defsubst ot-get-order (item)
  "Return ITEM's \\='order text property or return 1."
  (or (get-text-property 0 'order item) 1))

(defsubst ot-get-ts (item)
  "Return ITEM's \\='sched or \\='event text property as ts.el object."
  (ot--parse-org-element-ts (ot-get-sched-or-event item)))

(defun ot-order< (a b)
  "Return t, if A's \\='order is less then B's \\='order."
  (ot-on ot-get-order < a b))

(defun ot-sched-or-event< (a b)
  "Return t, if A's \\='sched or \\='event is less then B's.
\\='sched or \\='event are transformed to ts.el objects."
  (ot-on ot-get-ts ts< a b))

(defun ot-select-block-for-current-entry ()
  "Select block for the entry at point in `org-timeblock-list-mode'."
  (when-let (((not
	       (ot--daterangep
                (ot-get-sched-or-event nil (line-beginning-position)))))
	     (id (get-text-property (line-beginning-position) 'id))
	     (inhibit-read-only t)
             ((get-buffer-window ot-buffer)))
    (with-current-buffer ot-buffer
      (goto-char (point-min))
      (when (re-search-forward (format " fill=\"\\(%s\\)\"" ot-sel-block-color) nil t)
	(replace-match ot-prev-selected-block-color nil nil nil 1)
	(goto-char (point-min)))
      (when (re-search-forward (format " id=\"%s\" fill=\"\\([^\"]+\\)\"" id) nil t)
	(setq ot-prev-selected-block-color (match-string 1))
	(replace-match ot-sel-block-color nil nil nil 1)
	(ot-redisplay)))))

(defun ot-intersect-p (entry1 entry2)
  "Return t, if two entries intersect each other.
Otherwise, return nil.
`ENTRY1',`ENTRY2' - strings returned from `org-timeblock-get-entries'."
  (when-let ((y1 (get-text-property 0 'y entry1))
	     (y2 (get-text-property 0 'y entry2)))
    (let ((y1-end (+ (get-text-property 0 'block-height entry1) y1))
	  (y2-end (+ (get-text-property 0 'block-height entry2) y2)))
      (or
       (= y2 y1)
       (and
	y2-end
	(< y2 y1)
	(< y1 y2-end))
       (and
	y1-end
	(< y1 y2)
	(< y2 y1-end))))))

(defun ot--parse-hex-color (hex)
  "Convert a HEX color code to a RGB list of form (R G B)."
  (cl-loop for scale in (color-name-to-rgb hex) collect (* scale 255)))

(defun ot--random-color ()
  "Generate random color based on BASE-COLOR and RANGE.
Default background color is used when BASE-COLOR is nil."
  (let* ((default-background-color (ignore-errors (face-attribute 'default :background)))
	 (base-color
          (cond ((eq 'unspecified default-background-color)
                 "#fff")
		((string-match "^#[0-9a-fA-F]\\{3,6\\}" default-background-color)
                 default-background-color)
		((color-name-to-rgb default-background-color) ;; yellow, LightBlue, etc...
                 default-background-color)
		(t "#fff")))
	 (range 50))
    (when (color-name-to-rgb base-color)
      (let (rgb (hex "#"))
	(mapc (lambda (x)
		(setq rgb (cons (round (* x 255)) rgb)))
	      (color-name-to-rgb base-color))
	(setq rgb (nreverse rgb))
	(mapc (lambda (x)
		(setq hex (concat hex (format "%02x" x))))
	      rgb)
	(setq base-color hex)))
    (let* ((rgb (ot--parse-hex-color base-color))
           (half-range (/ range 2))
           (fn (lambda (n)
		 (let ((base (nth n rgb))
                       (min half-range)
                       (max (- 255 half-range))
                       result)
                   (when (< base min) (setq base min))
                   (when (> base max) (setq base max))
                   (setq result (+ (- (cl-random range) half-range) base))
                   (when (< result 0) (setq result 0))
                   (when (> result 255) (setq result 255))
                   result)))
           (r (funcall fn 0))
           (g (funcall fn 1))
           (b (funcall fn 2)))
      (format "#%02x%02x%02x" r g b))))

(defun ot-redraw-timeblocks ()
  "Redraw *org-timeblock* buffer."
  (with-current-buffer (get-buffer-create ot-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if-let ((entries (ot-get-entries :sort-func #'ot-sched-or-event< :exclude-dateranges t :with-time t))
	       (dates (ot-get-dates))
	       (window (get-buffer-window ot-buffer))
	       (overall-window-height (setq ot-svg-height (window-body-height window t)))
	       (overall-window-width (setq ot-svg-width (window-body-width window t))))
	  (progn
	    (setq ot-svg-obj (svg-create overall-window-width overall-window-height))
	    (dotimes (iter (length dates))
	      (if-let ((entries (seq-filter (lambda (x) (ot-ts-date= (ot--parse-org-element-ts (ot-get-sched-or-event x)) (nth iter dates))) entries)))
		  (let* ((window-height (window-body-height window t))
			 (window-width (/ (window-body-width window t) (length dates)))
			 (timeline-left-padding 25)
			 (block-max-width (- window-width timeline-left-padding))
			 (min-hour
			  (if-let ((ot-view-options)
				   (hours
				    (remove
				     nil
				     (append
				      (list (unless (eq ot-view-options 'hide-all) (ts-hour (ts-now))))
				      (mapcar
				       (lambda (entry)
					 (let ((s-or-e (ot-get-sched-or-event entry)))
					   (if (ot-ts-date< (ot--parse-org-element-ts s-or-e) (nth iter dates))
					       0
					     (org-element-property :hour-start s-or-e))))
				       entries)))))
			      (apply #'min hours)
			    0))
			 (scale (/ (float window-height) (float (* (- 24 min-hour) 60))))
			 (cur-time (ts-now))
			 (cur-time-indicator
			  (* scale
			     (-
			      (+ (* (ts-hour cur-time) 60) (ts-minute cur-time)) ;; minutes
			      (* min-hour 60))))
			 (columns
			  (mapcar (lambda (x) (cons (get-text-property 0 'id x) 1)) entries))
			 placed
			 (bg-rgb-sum (apply #'+ (ot--parse-hex-color ot-background-color)))
			 (get-color
			  (if (string= ot-background-color (face-attribute 'default :background))
			      (lambda (title) (cl-callf (lambda (x) (or x (ot--random-color))) (alist-get title ot-colors nil nil #'equal)))
			    (setq ot-background-color (face-attribute 'default :background))
			    (setq ot-sel-block-color
				  (if (> (setq bg-rgb-sum (apply #'+ (ot--parse-hex-color ot-background-color))) 550)
				      ot-sel-block-color-light
				    ot-sel-block-color-dark))
			    (lambda (title) (setf (alist-get title ot-colors nil nil #'equal) (ot--random-color)))))
			 (hour-lines-color (if (> bg-rgb-sum 550) "#7b435c" "#cdcdcd")))
		    (dolist (entry entries)
		      (let* ((timestamp (ot-get-sched-or-event entry))
			     (start-ts (ot--parse-org-element-ts timestamp))
			     (end-ts (ot--parse-org-element-ts timestamp t))
			     (start-date-earlier-p (ot-ts-date< start-ts (nth iter dates)))
			     (end-date-later-p (ot-ts-date< (nth iter dates) end-ts)))
			(add-text-properties 0 (length entry)
					     `( time-string ,(and ot-display-time
								  (not (or end-date-later-p start-date-earlier-p))
								  (concat (ts-format " %H:%M" start-ts)
									  (and end-ts (ts-format "-%H:%M" end-ts))))
						block-height ,(- (if (and start-ts end-ts)
								     (max
								      (default-font-height)
								      (round
								       (* (/ (ts-diff
									      (if end-date-later-p
										  (ts-apply :hour 23 :minute 59 :second 0 (nth iter dates))
										end-ts)
									      (if start-date-earlier-p
										  (ts-apply :hour 0 :minute 1 :second 0 (nth iter dates))
										start-ts))
									     60)
									  scale)))
								   (default-font-height))
								 (if (ot-get-event entry) 2 1))
						y ,(+ (round (* (- (if start-date-earlier-p
								       0
								     (+ (* 60 (org-element-property :hour-start timestamp))
									(org-element-property :minute-start timestamp)))
								   (* min-hour 60))
								scale))
						      (if (ot-get-event entry) 2 1))
						title ,(concat
							(get-text-property 0 'title entry)
							(cond
							 ((and end-date-later-p start-date-earlier-p) "↕️")
							 (end-date-later-p "⬇️")
							 (start-date-earlier-p "⬆️"))))
					     entry)))
		    ;; Timeblocks layout algorithm
		    (dolist (entry entries)
		      (let ((id (get-text-property 0 'id entry)))
			(push entry placed)
			(setcdr (assoc id columns)
				(catch 'found-column
				  (let ((k 1))
				    (while t
				      (catch 'next-column
					(dolist (el (seq-filter
						     (lambda (x)
						       (eq (cdr (assoc (get-text-property 0 'id x) columns)) k))
						     placed))
					  (and (not (string= (get-text-property 0 'id el) id))
					       (ot-intersect-p entry el)
					       (cl-incf k)
					       (throw 'next-column t)))
					(throw 'found-column k))))))))
		    ;; Drawing hour lines
		    (let ((lines-iter (if (> min-hour 0) (1- min-hour) 0)) y)
		      (while (< (cl-incf lines-iter) 24)
			(setq y (round (* scale (- lines-iter min-hour) 60)))
			(svg-line
			 ot-svg-obj
			 (+ timeline-left-padding (* window-width iter))
			 y
			 (+ window-width (* window-width iter))
			 y
			 :stroke-dasharray "4"
			 :stroke hour-lines-color)
			(svg-text
			 ot-svg-obj (format "%d" lines-iter)
			 :y (+ y 5)
			 :x (+ 5 (* window-width iter))
			 :fill (face-attribute 'default :foreground))))
		    ;; Drawing all the entries inside the timeline
		    (dolist (entry entries)
		      (when-let ((length (1+ (length (seq-uniq
						      (mapcar
						       ;; get columns for those entries
						       (lambda (x)
							 (cdr (assoc (get-text-property 0 'id x) columns)))
						       ;; find those with which current entry is in intersection
						       (seq-filter
							(lambda (x)
							  (unless (equal (get-text-property 0 'id entry) (get-text-property 0 'id x))
							    (ot-intersect-p entry x)))
							entries))
						      #'eq))))
				 (y (get-text-property 0 'y entry))
				 (block-height (get-text-property 0 'block-height entry))
				 ((> (+ y block-height) 0))
				 (x (+ (+ timeline-left-padding (round (* (1- (cdr (assoc (get-text-property 0 'id entry) columns))) (/ block-max-width length))))
				       (* window-width iter)
				       (if (ot-get-event entry) 2 1)))
				 (block-width (- (round (/ block-max-width length)) (if (ot-get-event entry) 2 1)))
				 (title (get-text-property 0 'title entry))
				 ;; Splitting the title of an entry
				 (heading-list
				  (if (> (* (length title) (default-font-width)) block-width)
				      (seq-take
				       (seq-partition title (/ block-width (default-font-width)))
				       (let ((lines-count (round (/ block-height (default-font-height)))))
					 (if (= 0 lines-count) 1 lines-count)))
				    `(,title))))
			(let ((time-string (get-text-property 0 'time-string entry))
			      (colors (ot-get-colors (get-text-property 0 'tags entry))))
			  (when-let ((time-string)
				     ((< (- block-height
					    (* (length heading-list) (default-font-height)))
					 (- (default-font-height) 6)))
				     (diff (-
					    (+ (length (car (last heading-list)))
					       (length time-string))
					    (/ block-width (default-font-width))))
				     ((> diff 0)))
			    (cl-callf
				(lambda (x)
				  (if (> (- (length x) diff) 10)
				      (substring x 0 (- diff))
				    (setq time-string nil)
				    x))
				(car (last heading-list))))
			  (push (list (get-text-property 0 'id entry) (get-text-property 0 'marker entry) (ot-get-event entry)) ot-data)
			  ;; Appending generated rectangle for current entry
			  (svg-rectangle ot-svg-obj x y block-width block-height
					 :column (1+ iter)
					 :stroke (if (ot-get-event entry) "#5b0103" "#cdcdcd")
					 :stroke-width (if (ot-get-event entry) 2 1)
					 :opacity "0.7"
					 :fill (or (car colors) (funcall get-color title))
					 :id (get-text-property 0 'id entry))
			  ;; Setting the title of current entry
			  (let ((y (- y 5)))
			    (dolist (heading-part heading-list)
			      (svg-text ot-svg-obj heading-part
					:x x
					:y (cl-incf y (default-font-height))
					:fill (or (cadr colors) (face-attribute 'default :foreground))
					:font-size (aref (font-info (face-font 'default)) 2))))
			  (when time-string
			    (svg-text ot-svg-obj time-string
				      :x (- (+ x block-width) (* (length time-string) (default-font-width)))
				      :y (- (+ y block-height) 2)
				      :fill (or (cadr colors) hour-lines-color)
				      :font-size (aref (font-info (face-font 'default)) 2))))))
		    ;; Drawing current time indicator
		    (when (and ot-current-time-indicator
			       (ot-ts-date= (nth iter dates) (ts-now)))
		      (svg-polygon
		       ot-svg-obj
		       (list
			(cons (- block-max-width 5) cur-time-indicator)
			(cons (+ block-max-width 25) (- cur-time-indicator 5))
			(cons (+ block-max-width 25) (+ cur-time-indicator 5)))
		       :fill-color "red")))
		(let* ((window (get-buffer-window ot-buffer))
		       (window-height (window-body-height window t))
		       (window-width (/ (window-body-width window t) (length dates)))
		       (message "No data."))
		  (svg-text ot-svg-obj message
			    :y (/ window-height 2)
			    :x (+ (- (/ window-width 2) (/ (* (default-font-width) (length message)) 2))
				  (* window-width iter))
			    :fill (face-attribute 'default :foreground)
			    :font-size (aref (font-info (face-font 'default)) 2)))))
	    (svg-print ot-svg-obj))
	(let* ((window (get-buffer-window ot-buffer))
	       (window-height (window-body-height window t))
	       (window-width (window-body-width window t))
	       (message "No entries found for this date."))
	  (setq ot-svg-obj (svg-create window-width window-height))
	  (svg-text
	   ot-svg-obj message
	   :y (/ window-height 2)
	   :x (- (/ window-width 2) (/ (* (default-font-width) (length message)) 2))
	   :fill (face-attribute 'default :foreground))
	  (svg-print ot-svg-obj)))
      (ot-redisplay))))

(defun ot-redisplay ()
  "Redisplay *org-timeblock* buffer."
  (let((inhibit-message t))
    (ot-mode)))

(defun ot-show-timeblocks ()
  "Switch to *org-timeblock* buffer in another window."
  (switch-to-buffer-other-window ot-buffer)
  (other-window 1))

(defun ot-show-timeblock-list ()
  "Switch to *org-timeblock-list* buffer in another window."
  (switch-to-buffer-other-window ot-list-buffer)
  (other-window 1))

(defun ot-list-toggle-sort-function ()
  "Toggle the sorting strategy in *org-timeblock-list*.
Available sorting strategies:
1. Sort by SCHEDULED property.\\<org-timeblock-list-mode-map>
2. Sort by \\='order text property applied to each entry inside
*org-timeblock-list* which can be changed via `\\[org-timeblock-list-drag-line-forward]'/`\\[org-timeblock-list-drag-line-backward]'
commands"
  (interactive)
  (setq ot-sort-function
	(if (eq ot-sort-function #'ot-order<)
            #'ot-sched-or-event<
	  #'ot-order<))
  (ot-redraw-buffers))

(defun ot-list-save ()
  "Save orgmode files, sorting line and tasks positions."
  (interactive)
  (unless (eq major-mode 'org-timeblock-list-mode)
    (user-error "Not in org-timeblock buffer"))
  (let ((count 0)
	(inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when-let ((m (get-text-property (point) 'marker))
		   (id (ot-construct-id m (ot-get-event nil (line-beginning-position)))))
	  (setf (alist-get id (alist-get (ts-format "%Y-%m-%d" (car ot-daterange)) ot-list-entries-pos nil nil #'equal) nil nil #'equal)
		(cl-incf count)))
	(when (get-text-property (point) 'sort-ind)
	  (setf (alist-get (ts-format "%Y-%m-%d" (car ot-daterange)) ot-list-sortline-pos nil nil 'equal)
		(cl-incf count)))
	(forward-line))))
  (org-save-all-org-buffers))

(defun ot-quit ()
  "Exit `org-timeblock-list-mode'."
  (interactive)
  (quit-window t))

(cl-defun ot--schedule-time (&optional marker eventp (date (car ot-daterange)))
  "Interactively change time in DATE for Org entry timestamp at MARKER.
If MARKER is nil, use entry at point.
If EVENTP is non-nil, change timestamp of the event.
If DATE is not specified, use `org-timeblock-date'.

Schedule or event date won't be changed.  The time might be a
timerange which depends on user interactive choice.

Time format is \"HHMM\""
  (when marker
    (unless (marker-buffer marker)
      (user-error "Non-existent marker's buffer")))
  (org-with-point-at (or marker (point))
    (ot-show-context)
    (let (ts-type prev-date)
      (while (null ts-type)
	(pcase (read-char-from-minibuffer
		(format "Schedule (%s): time[s]tamp, time[r]ange, other [d]ay" (ts-format "%Y-%m-%d" date))
		'(?s ?r ?d))
	  (?r (setq ts-type 'timerange))
	  (?s (setq ts-type 'timestamp))
	  (?d (setq prev-date date
		    date (ts-parse (org-read-date nil nil nil nil (ts-internal date))))
	      (ot-jump-to-day date))))
      (let* ((timestamp (if eventp
			    (ot-get-event-timestamp)
			  (org-element-property :scheduled (org-element-at-point))))
	     (start-ts (ot--parse-org-element-ts timestamp))
	     (end-ts (ot--parse-org-element-ts timestamp t))
	     (duration (when (and start-ts end-ts) (/ (round (ts-diff end-ts start-ts)) 60)))
	     (new-start-ts (ot-read-ts date "START-TIME: "))
	     (new-end-ts
	      (if (eq ts-type 'timerange)
		  (ot-read-ts date "END-TIME: ")
		(when duration (ts-inc 'minute duration new-start-ts)))))
	(when prev-date
	  (ot-jump-to-day prev-date))
	(if eventp
	    (progn
	      (save-excursion
		(ot-delete-event-timestamp)
		(insert
		 (ot-ts-to-org-timerange new-start-ts new-end-ts)))
	      (ot-get-event-timestamp))
	  (ot--schedule new-start-ts new-end-ts)
	  (org-element-property :scheduled (org-element-at-point)))))))

(defun ot--daterangep (timestamp)
  "Return t if org timestamp object TIMESTAMP is a daterange with no time."
  (when-let ((day-end (org-element-property :day-end timestamp))
	     (month-end (org-element-property :month-end timestamp))
	     (year-end (org-element-property :year-end timestamp)))
    (and
     (or
      (/= (org-element-property :day-start timestamp) day-end)
      (/= (org-element-property :month-start timestamp) month-end)
      (/= (org-element-property :year-start timestamp) year-end))
     (null (org-element-property :hour-start timestamp))
     (null (org-element-property :hour-end timestamp)))))

(defun ot--construct-entry-prefix (timestamp &optional eventp)
  "Construct an entry prefix for *org-timeblock-list* buffer.

TIMESTAMP is org-element timestamp object which is used to
construct a timerange inside the prefix.  If EVENTP is non-nil,
insert \"EVENT\" in the prefix."
  (let ((hstart (org-element-property :hour-start timestamp))
	(mstart (org-element-property :minute-start timestamp))
	(hend (org-element-property :hour-end timestamp))
	(mend (org-element-property :minute-end timestamp)))
    (propertize
     (format
      (if eventp " EVENT % -12s    " "       % -12s    ")
      (if (ot--daterangep timestamp)
	  ""
	(concat (and hstart mstart
		     (format
		      "%02d:%02d"
		      hstart
		      mstart))
		(and hend mend
		     (or (/= hend hstart)
			 (/= mend mstart))
		     (format
		      "-%02d:%02d"
		      hend
		      mend)))))
     'prefix t)))

(cl-defun ot-read-ts (ts &optional (prompt "TIME:"))
  "Read a time in \"HHMM\" format and apply it to ts.el struct TS.
Return the changed time struct.

PROMPT can overwrite the default prompt."
  (let (time)
    (catch 'exit
      (while t
	(let ((len (length time))
	      (ch (read-char-exclusive (concat "[format: HHMM] " prompt (reverse time)))))
	  (cond
	   ((or (and (= len 0) (<= ?0 ch ?2))
		(and (= len 1) (if (< (car time) ?2) (<= ?0 ch ?9) (<= ?0 ch ?3)))
		(and (= len 2) (<= ?0 ch ?5)))
	    (push ch time))
	   ((and (= len 3) (<= ?0 ch ?9))
	    (push ch time)
	    (throw 'exit t))
	   ((and (/= len 0) (eq ch ?\C-?))
	    (pop time))
	   (t (ding))))))
    (cl-macrolet ((pop-digit () '(- (pop time) 48)))
      (ts-apply
       :minute
       (+ (pop-digit) (* 10 (pop-digit)))
       :hour
       (+ (pop-digit) (* 10 (pop-digit)))
       ts))))

(defun ot-construct-id (&optional marker eventp)
  "Construct identifier for the org entry at MARKER.
If MARKER is nil, use entry at point.
If EVENTP is non-nil, use entry's TIMESTAMP property."
  (let ((element (org-with-point-at marker (org-element-at-point))))
    (md5
     (concat
      (org-element-property :raw-value element)
      (if eventp
	  (org-entry-get marker "TIMESTAMP")
	(concat
	 "S"
	 (org-element-property
	  :raw-value
	  (org-element-property :scheduled element)))))
     nil nil 'utf-8)))

(defun ot-get-event-timestamp ()
  "Return an org-element timestamp object of an event at point."
  (when-let ((ts (org-entry-get nil "TIMESTAMP")))
    (with-temp-buffer
      (insert ts)
      (goto-char (point-min))
      (org-element-timestamp-parser))))

(cl-defun ot-get-entries (&key sort-func exclude-dateranges with-time)
  "Return entries relevant to `org-timeblock-date'.

SORT-FUNC is either nil, in which case items are sorted via
`ot-sort-function'; or a function that accepts two items as
arguments and returns nil or non-nil.

When EXCLUDE-DATERANGES is non-nil, exclude scheduled entries or
events with a daterange with no times.

When WITH-TIME is non-nil, each entry must contain a timestamp
with time (timerange or just start time)."
  (when-let ((files (org-agenda-files)))
    (sort
     (mapcar
      (lambda (entry)
	;; setting 'order property not inside of `org-ql-select' call
	;; because when buffers have not been changed, org-ql uses
	;; cached results and therefore does not update 'order property,
	;; which is the only property that's not stored in org buffers
	(put-text-property
	 0 (length entry)
	 'order (alist-get
		 (get-text-property 0 'id entry)
		 (alist-get
		  (ts-format "%Y-%m-%d" (car ot-daterange))
		  ot-list-entries-pos
		  nil nil #'equal)
		 nil nil #'equal)
	 entry)
	entry)
      (let ((markers
	     (org-ql-select
	       files
	       `(and (not (done))
		     (ot-active-ts
		      :exclude-dateranges ,exclude-dateranges
		      :with-time ,with-time))
	       :action (lambda () (copy-marker (point) t))))
	    propertized-entries)
	(cl-macrolet ((get-propertized-entry (timestamp &optional eventp)
			`(when-let ((,timestamp)
				    (title (org-get-heading t nil t t)))
			   (propertize (concat (ot--construct-entry-prefix ,timestamp ,eventp) title)
				       (if ,eventp 'event 'sched) ,timestamp
				       'marker marker
				       'tags (mapcar 'substring-no-properties (org-element-property :tags (org-element-at-point)))
				       'id (ot-construct-id nil ,eventp)
				       'title title))))
	  (dolist (marker markers (remove nil propertized-entries))
	    (org-with-point-at marker
	      (push (get-propertized-entry (ot-get-event-timestamp) t) propertized-entries)
	      (push (get-propertized-entry (org-element-property :scheduled (org-element-at-point))) propertized-entries))))))
     (or sort-func ot-sort-function))))

(defun ot-get-colors (tags)
  "Return the colors for TAGS.
Return value is of the form (\"background color\" \"foreground color\")."
  (catch 'found
    (dolist (tag tags)
      (when-let ((colors (cdr (seq-find (lambda (x) (string= (car x) tag)) ot-tag-colors))))
	(throw 'found colors)))))

(defun ot--parse-org-element-ts (ts &optional end)
  "Convert TS to ts.el ts object.
If END is non-nil, use end part of the timestamp.

TS is a org-element timestamp object."
  (let ((year-start (org-element-property :year-start ts))
	(month-start (org-element-property :month-start ts))
	(day-start (org-element-property :day-start ts))
	(hour-start (org-element-property :hour-start ts))
	(minute-start (org-element-property :minute-start ts)))
    (if end
	(when-let ((year-end (org-element-property :year-end ts))
		   (month-end (org-element-property :month-end ts))
		   (day-end (org-element-property :day-end ts)))
	  (let ((hour-end (org-element-property :hour-end ts))
		(minute-end (org-element-property :minute-end ts)))
	    (when (or
		   (/= day-start day-end)
		   (/= month-start month-end)
		   (/= year-start year-end)
		   (and hour-end hour-start (/= hour-start hour-end))
		   (and minute-end minute-start (/= minute-start minute-end)))
	      (make-ts :year year-end :month month-end :day day-end
		       :hour (or hour-end 0) :minute (or minute-end 0) :second 0))))
      (make-ts :year year-start :month month-start :day day-start
	       :hour (or hour-start 0) :minute (or minute-start 0) :second 0))))

(defun ot--schedule (start-ts &optional end-ts)
  "Schedule the entry at point.
START-TS and END-TS are ts.el time objects."
  (let* ((timestamp (org-element-property :scheduled (org-element-at-point)))
	 (repeat-string (org-get-repeat))
	 (warning-string
	  (concat
	   (pcase (org-element-property :warning-type timestamp)
	     (`first "--") (`all "-"))
	   (let ((val (org-element-property :warning-value timestamp)))
	     (and val (number-to-string val)))
	   (pcase (org-element-property :warning-unit timestamp)
	     (`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))
	 (dates-equal-p (ot-ts-date= start-ts end-ts)))
    (cond
     ((or (not end-ts) dates-equal-p)
      (org-schedule nil (ot-ts-to-org-timerange start-ts end-ts)))
     ((and end-ts (not dates-equal-p))
      (org-schedule nil (ot-ts-to-org-timerange start-ts))
      (org-back-to-heading t)
      (forward-line)
      (when (re-search-forward org-scheduled-time-regexp (line-end-position) t)
	(insert "--" (ot-ts-to-org-timerange end-ts nil repeat-string warning-string)))))))

(defun ot-delete-event-timestamp ()
  "Delete event timestamp for the entry at point.
Leave point where the timestamp was."
  (let ((end (save-excursion (outline-next-heading) (point))))
    (while
	(not (and
	      (or (re-search-forward org-tr-regexp end t)
		  (re-search-forward org-ts-regexp end t))
	      (not (org-in-regexp org-scheduled-time-regexp))
	      (not (org-in-regexp org-deadline-time-regexp))))
      t)
    (beginning-of-line)
    (when (or (re-search-forward org-tr-regexp end t)
	      (re-search-forward org-ts-regexp end t))
      (replace-match ""))))

(defun ot-ts-to-org-timerange (ts-start &optional ts-end repeat-string warning-string)
  "Create an Org timestamp range string.

TS-START and TS-END are ts.el time objects.
REPEAT-STRING is a repeater string.
WARNING-STRING is a warning string of the form \"-[0-9]+[hdwmy]\""
  (when-let ((start-date (ts-format "%Y-%m-%d %a" ts-start)))
    (let ((start-time
	   (let ((res (ts-format "%R" ts-start)))
	     (and (not (string= res "00:00")) res)))
	  (end-date (and ts-end (ts-format "%Y-%m-%d %a" ts-end)))
	  (end-time (and ts-end
			 (let ((res (ts-format "%R" ts-end)))
			   (and (not (string= res "00:00")) res))))
	  (timestamp-end
           (concat
            (and (org-string-nw-p repeat-string) (concat " " repeat-string))
            (and (org-string-nw-p warning-string) (concat " " warning-string))
            ">")))
      (concat
       "<" start-date (and start-time (concat " " start-time))
       (if (equal end-date start-date)
	   (and end-time (not (equal end-time start-time)) (concat "-" end-time))
	 (and
	  end-date
	  (concat
	   timestamp-end
	   "--<" end-date
	   (and end-time (concat " " end-time)))))
       timestamp-end))))

(defun ot-read-duration ()
  "Read time duration and return minutes as an integer.

Beep or flash the screen when an invalid character is typed.  The
prompt shows currently valid characters for the next input
character.

Valid duration formats:
2h
2h30m
2h30
45
1d
1w3h30m"
  (let* ((dur "")
	 (all-multipliers (mapcar #'car ot-duration-multipliers))
	 (valid-multipliers all-multipliers)
	 typed-multipliers)
    (catch 'dur
      (while-let ((multipliers (apply #'propertize
				      (concat "[" valid-multipliers "]")
				      (and (or (length= dur 0) (member (string-to-char (substring dur -1)) all-multipliers))
					   '(face org-agenda-dimmed-todo-face))))
		  (ch (read-char-exclusive (concat "DURATION ([0-9]+" multipliers "):" dur))))
	(cond
	 ((<= ?0 ch ?9)
	  (setq dur (format "%s%c" dur ch)))
	 ((or (and (eq ch ?\C-m) (length> dur 0))
	      (and (member ch valid-multipliers) (string-match-p "[0-9]+$" dur)))
	  (when-let (((member ch '(?m ?\C-m)))
		     (minutes 0)
		     (start 0))
	    (setq dur (concat dur "m"))
	    (while (string-match (concat "\\([0-9]+\\)\\([" typed-multipliers "m]\\)") dur start)
	      (cl-incf minutes (* (cdr (assq (string-to-char (match-string 2 dur)) ot-duration-multipliers))
				  (string-to-number (match-string 1 dur))))
	      (setq start (match-end 0)))
	    (throw 'dur minutes))
	  (setq dur (format "%s%c" dur ch)
		valid-multipliers (cdr (member ch valid-multipliers)))
	  (push ch typed-multipliers))
	 ((and (eq ?\C-? ch) (not (length= dur 0)))
	  (when (eq (string-to-char (substring dur -1)) (car typed-multipliers))
	    (pop typed-multipliers)
	    (setq valid-multipliers
		  (let ((ms all-multipliers))
		    (when typed-multipliers
		      (while (not (eq (pop ms) (car typed-multipliers)))))
		    ms)))
	  (setq dur (substring dur 0 -1)))
	 (t (ding)))))))

(defun ot--duration (duration marker &optional eventp)
  "Set SCHEDULED duration to DURATION for the org entry at MARKER.
Change SCHEDULED timestamp duration of the org entry at MARKER.
Return the changed org-element timestamp object.
If EVENTP is non-nil, use entry's timestamp."
  (unless (marker-buffer marker)
    (user-error "Non-existent marker's buffer"))
  (org-with-point-at marker
    (ot-show-context)
    (let* ((timestamp
	    (if eventp
		(ot-get-event-timestamp)
	      (org-element-property :scheduled (org-element-at-point))))
	   (start-ts (ts-parse-org-element timestamp))
	   (new-end-ts (when duration (ts-inc 'minute duration start-ts))))
      (unless (and (org-element-property :hour-start timestamp)
		   (org-element-property :minute-start timestamp))
	(user-error "No scheduled time specified for this entry"))
      (if eventp
	  (progn
	    (save-excursion
	      (ot-delete-event-timestamp)
	      (insert
	       (ot-ts-to-org-timerange start-ts new-end-ts)))
	    (ot-get-event-timestamp))
	(ot--schedule start-ts new-end-ts)
	(org-element-property :scheduled (org-element-at-point))))))

(defun ot-list-drag-line-backward ()
  "Drag an agenda line backward by ARG lines."
  (interactive)
  (ot-list-drag-line-forward t))

(defun ot--set-todo (marker todo)
  "Set TODO state to the entry at MARKER."
  (when (and marker todo)
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (ot-show-context)
      (org-todo todo))))

(defun ot-list-drag-line-forward (&optional backward)
  "Drag an agenda line forward by ARG lines.
When BACKWARD is non-nil, move backward."
  (interactive)
  (unless (eq ot-sort-function #'ot-order<)
    (user-error "Can't drag lines if entries aren't displayed and sorted by `SORTING-PROPERTY' property"))
  (unless (or (get-text-property (point) 'marker)
	      (get-text-property (point) 'sort-ind))
    (user-error "Can not move this line"))
  (when (or (and backward (= (line-number-at-pos) 2))
	    (and (not backward) (= (count-lines (point-min) (point-max))
				   (line-number-at-pos))))
    (user-error "Can not move further"))
  (let ((inhibit-read-only t)
	(end (save-excursion (move-beginning-of-line 2) (point)))
	line)
    (move-beginning-of-line 1)
    (setq line (buffer-substring (point) end))
    (delete-region (point) end)
    (move-beginning-of-line (funcall (if backward '1- '1+) 1))
    (insert line)
    (move-beginning-of-line 0)))

;;;; Main commands

;;;###autoload
(defun org-timeblock-list ()
  "Enter `org-timeblock-list-mode'."
  (interactive)
  (switch-to-buffer ot-list-buffer)
  (setq ot-daterange (cons (ts-now) (ts-inc 'day ot-n-days-view (ts-now))))
  (ot-redraw-buffers))

;;;###autoload
(defun org-timeblock ()
  "Enter `org-timeblock-mode'."
  (interactive)
  (switch-to-buffer ot-buffer)
  (setq ot-daterange (cons (ts-now) (ts-inc 'day ot-n-days-view (ts-now))))
  (ot-redraw-buffers))

;;;; Planning commands

(defun ot-new-task ()
  "Create a task scheduled to the date in the current view.
The new task is created in `org-timeblock-inbox-file'"
  (interactive)
  (unless (member ot-inbox-file (org-agenda-files))
    (user-error "`org-timeblock-inbox-file' must be present in `org-agenda-files'"))
  (let ((title ""))
    (while (string-empty-p title)
      (setq title (read-string "Heading: ")))
    (with-current-buffer (find-file-noselect ot-inbox-file)
      (goto-char (point-max))
      (insert "\n")
      (org-insert-heading nil t t)
      (insert "TODO " title " ")
      (pcase ot-new-task-time
	(`pick (ot--schedule-time))
	((pred stringp) (unless (string-match-p "\\([01][0-9]\\|2[0-3]\\):[0-5][0-9]" ot-new-task-time)
			  (user-error "Wrong time format specified in `org-timeblock-new-task-time'"))
	 (org-schedule nil (concat (ts-format "%Y-%m-%d " (car ot-daterange)) ot-new-task-time)))
	(`nil (org-schedule nil (ts-format "%Y-%m-%d" (car ot-daterange))))
	(_ (user-error "Invalid custom variable value")))
      (save-buffer)))
  (ot-redraw-buffers))

(defun ot-list-set-duration ()
  "Interactively change SCHEDULED duration for the task at point.

Change SCHEDULED timestamp duration of the task at point in
`org-timeblock-list-mode'.

Duration format:
2h
2h30m
2h30
45"
  (interactive)
  (when (ot--daterangep (ot-get-sched-or-event nil (line-beginning-position)))
    (user-error "Can not reschedule entries with daterange timestamp"))
  (let ((eventp (ot-get-event nil (line-beginning-position))))
    (when-let ((duration (ot-read-duration))
	       (timestamp (ot--duration duration (get-text-property (line-beginning-position) 'marker) eventp)))
      (ot-list-update-entry eventp)
      (when (get-buffer-window ot-buffer)
	(ot-redraw-timeblocks)))))

(defun ot-schedule ()
  "Change the timestamp for the selected block.
The org-element timestamp object may be from an event or from a
SCHEDULED property."
  (interactive)
  (when-let ((id (ot-selected-block-id))
	     (marker (ot-selected-block-marker)))
    (ot--schedule-time marker (ot-block-eventp id))
    (ot-redraw-buffers)
    (ot-redisplay)))

(defun ot-set-duration ()
  "Interactively change SCHEDULED duration of the selected block.

Change SCHEDULED timestamp duration of the task bound to the selected
block in `org-timeblock-mode'.

Duration format:
2h
2h30m
2h30
45"
  (interactive)
  (when-let ((marker (ot-selected-block-marker))
	     (id (ot-selected-block-id))
	     (duration (ot-read-duration)))
    (ot--duration duration marker (ot-block-eventp id))
    (ot-redraw-buffers)
    (ot-redisplay)))

(defun ot-list-schedule ()
  "Reschedule the entry at point in *org-timeblock-list* buffer.
The org-element timestamp object may be from an event or from a
SCHEDULED property."
  (interactive)
  (when (ot--daterangep (ot-get-sched nil (line-beginning-position)))
    (user-error "Can not reschedule entries with daterange timestamp"))
  (let ((eventp (ot-get-event nil (line-beginning-position))))
    (when-let ((timestamp (ot--schedule-time (get-text-property (line-beginning-position) 'marker) eventp)))
      (ot-list-update-entry eventp)
      (when (get-buffer-window ot-buffer)
	(ot-redraw-timeblocks)))))

(defun ot-list-update-entry (eventp)
  "Update text and text properties for the entry at point in *org-timeblock-list*.
If EVENTP is non-nil the entry is considered as an event."
  (let ((marker (get-text-property (line-beginning-position) 'marker)) colors)
    (unless (marker-buffer marker)
      (user-error "Non-existent marker's buffer"))
    (let ((inhibit-read-only t)
	  (new-entry
	   (org-with-point-at marker
	     (let ((timestamp (if eventp
				  (ot-get-event-timestamp)
				(org-element-property :scheduled (org-element-at-point))))
		   (title (org-get-heading t nil t t))
		   (tags (mapcar 'substring-no-properties (org-element-property :tags (org-element-at-point)))))
	       (setq colors (ot-get-colors tags))
	       (propertize
		(concat (ot--construct-entry-prefix timestamp eventp) title)
		(if eventp 'event 'sched) timestamp
		'marker marker
		'tags tags
		'id (ot-construct-id nil eventp)
		'title title)))))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (insert (propertize
	       (concat new-entry "\n")
	       'face
	       `(:extend t ,@(and (car colors) (list :background (car colors))) ,@(and (cadr colors) (list :foreground (cadr colors)))))))))

;;;; Navigation commands

(defun ot-select-block-under-mouse ()
  "Select timeblock under current position of mouse cursor."
  (interactive)
  (when-let ((pos (ot-mouse-pixel-pos))
	     (inhibit-read-only t))
    (goto-char (point-min))
    (when (re-search-forward (format " fill=\"\\(%s\\)\"" ot-sel-block-color) nil t)
      (replace-match (or ot-prev-selected-block-color "#ffffff") nil nil nil 1)
      (goto-char (point-min)))
    (when-let ((found (car (dom-search
			    ot-svg-obj
			    (lambda (node)
			      (let ((x (dom-attr node 'x))
				    (y (dom-attr node 'y)))
				(and (eq (dom-tag node) 'rect)
				     (> (car pos) x)
				     (<= (car pos) (+ x (dom-attr node 'width)))
				     (<= (cdr pos) (+ y (dom-attr node 'height)))
				     (> (cdr pos) y))))))))
      (re-search-forward (format "id=\"%s\" fill=\"\\([^\"]+\\)\"" (dom-attr found 'id)) nil t)
      (setq ot-prev-selected-block-color (match-string-no-properties 1))
      (replace-match ot-sel-block-color nil nil nil 1))
    (ot-redisplay)
    (ot-show-olp-maybe (ot-selected-block-marker))))

(defun ot-list-next-line ()
  "Move cursor to the next line."
  (interactive)
  (funcall-interactively 'next-line)
  (ot-select-block-for-current-entry)
  (ot-show-olp-maybe (get-text-property (line-beginning-position) 'marker)))

(defun ot-list-previous-line ()
  "Move cursor to the previous line."
  (interactive)
  (funcall-interactively 'previous-line)
  (ot-select-block-for-current-entry)
  (ot-show-olp-maybe (get-text-property (line-beginning-position) 'marker)))

(defun ot-forward-block ()
  "Select the next timeblock in *org-timeblock* buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (when (re-search-forward (format " fill=\"\\(%s\\)\"" ot-sel-block-color) nil t)
      (replace-match ot-prev-selected-block-color nil nil nil 1)
      (unless (save-excursion (re-search-forward (format "<rect .+? column=\"%d\"" ot-current-column) nil t))
	(goto-char (point-min))))
    (when (and (re-search-forward (format "<rect .*? column=\"%d\"" ot-current-column) nil t)
	       (re-search-backward "<rect .*? id=\"[^\"]+\" fill=\"\\([^\"]+\\)\"" nil t))
      (setq ot-prev-selected-block-color (match-string-no-properties 1))
      (replace-match ot-sel-block-color nil nil nil 1)
      (ot-show-olp-maybe (ot-selected-block-marker)))
    (ot-redisplay)))

(defun ot-forward-column ()
  "Select the next column in *org-timeblock* buffer."
  (interactive)
  (if (= ot-current-column ot-n-days-view)
      (setq ot-current-column 1)
    (cl-incf ot-current-column))
  (ot-forward-block))

(defun ot-backward-column ()
  "Select the next column in *org-timeblock* buffer."
  (interactive)
  (if (= ot-current-column 1)
      (setq ot-current-column ot-n-days-view)
    (cl-decf ot-current-column))
  (ot-forward-block))

(defun ot-show-olp-maybe (marker)
  "Show outline path in echo area for the selected item.
If `ot-show-outline-path' is non-nil, display the path of the
heading at MARKER in the echo area."
  (when (and ot-show-outline-path marker (marker-buffer marker)
	     (buffer-live-p (marker-buffer marker)))
    (org-with-point-at marker (org-display-outline-path t))))

(defun ot-backward-block ()
  "Select the previous timeblock in *org-timeblock* buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (when (re-search-backward (format " fill=\"\\(%s\\)\"" ot-sel-block-color) nil t)
      (replace-match ot-prev-selected-block-color nil nil nil 1)
      (unless (save-excursion (re-search-backward (format "<rect .+? column=\"%d\"" ot-current-column) nil t))
	(goto-char (point-max))))
    (when (and (re-search-backward (format "<rect .*? column=\"%d\"" ot-current-column) nil t)
	       (re-search-forward "<rect .*? id=\"[^\"]+\" fill=\"\\([^\"]+\\)\"" nil t))
      (setq ot-prev-selected-block-color (match-string-no-properties 1))
      (replace-match ot-sel-block-color nil nil nil 1)
      (ot-redisplay)
      (ot-show-olp-maybe (ot-selected-block-marker)))))

(defun ot-day-later ()
  "Go forward in time by one day in `org-timeblock-mode'."
  (interactive)
  (setq ot-daterange
	(cons (ts-inc 'day 1 (car ot-daterange))
	      (ts-inc 'day 1 (cdr ot-daterange))))
  (ot-redraw-buffers))

(defun ot-jump-to-day (date)
  "Jump to DATE in *org-timeblock-list* or *org-timeblock* buffers.

When called interactively, prompt for the date.
When called from Lisp, DATE should be a date as returned by
`org-read-date'"
  (interactive (list (ts-parse (org-read-date))))
  (when date
    (setq ot-daterange (cons date (ts-inc 'day ot-n-days-view date)))
    (ot-redraw-buffers)))

(defun ot-day-earlier ()
  "Go backward in time by one day in `org-timeblock-mode'."
  (interactive)
  (setq ot-daterange
	(cons (ts-dec 'day 1 (car ot-daterange))
	      (ts-dec 'day 1 (cdr ot-daterange))))
  (ot-redraw-buffers))

;;;; View commands

(defun ot-list-goto-other-window ()
  "Jump to the org heading of the entry at point."
  (interactive)
  (unless (eq major-mode 'org-timeblock-list-mode)
    (user-error "Not in *org-timeblock-list* buffer"))
  (let ((marker (get-text-property (line-beginning-position) 'marker)))
    (switch-to-buffer-other-window (marker-buffer marker))
    (goto-char (marker-position marker))
    (ot-show-context)
    (recenter)))

(defun ot-list-goto ()
  "Go to the heading of the entry at point in the same window."
  (interactive)
  (unless (eq major-mode 'org-timeblock-list-mode)
    (user-error "Not in *org-timeblock-list* buffer"))
  (when-let ((marker (get-text-property (line-beginning-position) 'marker))
	     (buffer (marker-buffer marker))
	     (pos (marker-position marker)))
    (unless buffer (user-error "Trying to switch to non-existent buffer"))
    (pop-to-buffer-same-window buffer)
    (widen)
    (goto-char pos)
    (ot-show-context)))

(defun ot-goto-other-window ()
  "Jump to the org heading of selected timeblock."
  (interactive)
  (unless (eq major-mode 'org-timeblock-mode)
    (user-error "Not in *org-timeblock* buffer"))
  (goto-char (point-min))
  (when (re-search-forward (format "<rect .*? id=\"\\([^\"]+\\)\" fill=\"%s\"" ot-sel-block-color) nil t)
    (when-let ((inhibit-read-only t)
	       (id (match-string-no-properties 1))
	       (m (cadr (seq-find (lambda (x) (string= (car x) id)) ot-data))))
      (switch-to-buffer-other-window (marker-buffer m))
      (goto-char (marker-position m))
      (ot-show-context)
      (recenter))))

(defun ot-goto ()
  "Go to the heading of the selected block in the same window."
  (interactive)
  (unless (eq major-mode 'org-timeblock-mode)
    (user-error "Not in *org-timeblock* buffer"))
  (when-let ((marker (ot-selected-block-marker))
	     (buffer (marker-buffer marker))
	     (pos (marker-position marker)))
    (unless buffer (user-error "Trying to switch to non-existent buffer"))
    (pop-to-buffer-same-window buffer)
    (widen)
    (goto-char pos)
    (ot-show-context)))

(defun ot-switch-scaling ()
  "Switch between different scaling modes in `org-timeblock-mode'.

Available view options:
1. Do not hide anything.  All 24 hours will be displayed.
2. Hide hours in the past (if there are no timeblocks).
3. Hide all free hours before the first timeblock."
  (interactive)
  (setq ot-view-options
	(pcase ot-view-options
	  (`nil 'hide-all)
	  (`hide-all 't)
	  (`t 'nil)))
  (ot-redraw-timeblocks))

(defun ot-switch-view ()
  ""
  (interactive)
  (pcase (string-to-number (completing-read "Number of days in the view: " '("1" "2" "3" "4" "5" "6" "7") nil t))
    (`1
     (setq ot-n-days-view 1
	   ot-daterange (list (car ot-daterange))))
    ((and days _)
     (setq ot-n-days-view days
	   ot-daterange (cons (car ot-daterange) (ts-inc 'day days (car ot-daterange))))))
  (ot-redraw-buffers))

(defun ot-list-toggle-timeblock ()
  "Toggle the display of the window with `org-timeblock-mode'."
  (interactive)
  (if-let ((window (get-buffer-window ot-buffer)))
      (delete-window window)
    (ot-show-timeblocks)
    (ot-redraw-timeblocks)))

(defun ot-toggle-timeblock-list ()
  "Toggle the display of the window with `org-timeblock-list-mode'."
  (interactive)
  (if-let ((window (get-buffer-window ot-list-buffer)))
      (delete-window window)
    (ot-show-timeblock-list))
  (ot-redraw-buffers))

(defun ot-redraw-buffers ()
  "Redraw `org-timeblock-list-mode' and `org-timeblock-timeline-mode' buffers."
  ;; org-timeblock-list-mode and org-timeblock-mode
  (interactive)
  (with-current-buffer (get-buffer-create ot-list-buffer)
    (let ((inhibit-read-only t)
	  (entries (ot-get-entries)))
      (erase-buffer)
      (ot-list-mode)
      (setq
       header-line-format
       (substitute-command-keys
	(format "\\<org-timeblock-list-mode-map>Sorted by %s property. Toggle sorting: `\\[org-timeblock-list-toggle-sort-function]'"
		(pcase ot-sort-function
		  (`ot-order< "SORTING-ORDER")
		  (`ot-sched-or-event< "SCHEDULED")
		  ((pred symbolp) (symbol-name ot-sort-function))))))
      (insert
       (propertize
	(concat
	 (ts-format "[%Y-%m-%d %a]" (car ot-daterange))
	 (and (ot-ts-date= (car ot-daterange) (ts-now)) " Today"))
	'face 'ot-list-header))
      (insert "\n")
      (dolist (entry entries)
	(let ((colors (ot-get-colors (get-text-property 0 'tags entry))))
	  (insert (propertize
		   (concat entry "\n")
		   'face
		   `(:extend t ,@(and (car colors) (list :background (car colors))) ,@(and (cadr colors) (list :foreground (cadr colors))))))))
      (goto-char (point-min))
      (when (eq ot-sort-function #'ot-order<)
	(forward-line
	 (alist-get (ts-format "%Y-%m-%d" (car ot-daterange)) ot-list-sortline-pos nil nil #'equal))
	(insert (propertize (format "% 37s" "^^^ SORTED ^^^\n") 'sort-ind t 'face '(:extend t :background "#8b0000" :foreground "#ffffff")))
	(goto-char (point-min)))
      (when (get-buffer-window ot-buffer)
	(ot-redraw-timeblocks)))))

;;;; Predicates

(org-ql-defpred ot-active-ts (&key exclude-dateranges with-time)
  "Search for org entries that have TIMESTAMP or SCHEDULED property set to ON.
OT-DATERANGE is a ts.el struct.

When EXCLUDE-DATERANGES is non-nil, exclude entries with daterange and no time.

When WITH-TIME is non-nil, each entry must contain a timestamp
with time (timerange or just start time)."
  :preambles
  ((`(ot-active-ts)
    (list
     :query query
     :regexp org-ts-regexp)))
  :body
  (when-let ((timestamp (or (org-element-property :scheduled (org-element-at-point))
			    (ot-get-event-timestamp)))
	     ((not (and exclude-dateranges (ot--daterangep timestamp))))
	     ((or (not with-time) (org-element-property :hour-start timestamp)))
	     (start-ts (ot--parse-org-element-ts timestamp)))
    (let ((end-ts (ot--parse-org-element-ts timestamp t)))
      (or
       (ot-ts-date= start-ts (car ot-daterange))
       (ot-ts-date= end-ts (car ot-daterange))
       (ot-ts-date= start-ts (cdr ot-daterange))
       (and end-ts (cdr ot-daterange)
	    (ot-ts-date= end-ts (cdr ot-daterange)))
       (and
	(ot-ts-date< (car ot-daterange) start-ts)
	(ot-ts-date< start-ts (cdr ot-daterange)))
       (and
	(ot-ts-date< (car ot-daterange) end-ts)
	end-ts (cdr ot-daterange)
	(ot-ts-date< end-ts (cdr ot-daterange)))))))

;;;; Footer

(provide 'org-timeblock)

;; Local Variables:
;;   outline-regexp: "^\\(;\\{3,\\} \\)"
;;   read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:

;;; org-timeblock.el ends here
