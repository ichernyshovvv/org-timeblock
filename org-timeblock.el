;;; org-timeblock.el --- Interactive SVG calendar for orgmode tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.1") (org "9.0") (svg "1.1"))
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
(require 'compat)
(require 'compat-macs)

;;;; Faces

(defface org-timeblock-list-header '((t (:inherit org-agenda-structure)))
  "Face used in org-timeblock-list for dates."
  :group 'org-timeblock)

;;;; Custom Variables

(defgroup org-timeblock nil
  "Customization for `org-timeblock'."
  :group 'org
  :link '(url-link "https://github.com/ichernyshovvv/org-timeblock"))

(defcustom org-timeblock-show-future-repeats nil
  "Non-nil shows repeated entries in the future dates of repeat.
When set to the symbol `next' only the first future repeat is shown."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Show all repeated entries" t)
	  (const :tag "Show next repeated entry" next)
	  (const :tag "Do not show repeated entries" nil)))

(defcustom org-timeblock-files 'agenda
  "Non-nil shows repeated entries in the future dates of repeat.
When set to the symbol `next' only the first future repeat is shown."
  :group 'org-timeblock
  :type '(choice
	  (repeat :tag "List of files" file)
	  (const :tag "Files from (org-agenda-files)" agenda)))

(defcustom org-timeblock-show-outline-path nil
  "Non-nil means show outline path in echo area for the selected item."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Don't show outline path with prepended file name." nil)
	  (const :tag "Show outline path." t)))

(defcustom org-timeblock-span 3
  "Number of days displayed in `org-timeblock'."
  :group 'org-timeblock
  :type 'integer)

(defcustom org-timeblock-display-time t
  "Non-nil means show end and start time inside timeblocks."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Show time." t)
	  (const :tag "Do not show time." nil)))

(defcustom org-timeblock-inbox-file
  (expand-file-name "inbox.org" org-directory)
  "Org file in which new tasks are created via `org-timeblock-new-task'."
  :group 'org-timeblock
  :type 'file)

(defcustom org-timeblock-new-task-time
  'pick
  "Time to which new tasks are scheduled via `org-timeblock-new-task'."
  :group 'org-timeblock
  :type
  '(choice
    (const :tag "Unspecified.  The new task will be scheduled to a date with no time" nil)
    (const :tag "The new task will be scheduled to a time picked by user." pick)
    (string :tag "Time of the format \"HH:MM\".  The new task will be scheduled to a time.")))

(defcustom org-timeblock-scale-options t
  "Options that are used to decide which part of visual schedule must be hidden."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Hide hours in the past (if there are no timeblocks)." t)
	  (const :tag "Do not hide anything.  All 24 hours will be displayed." nil)
	  (const :tag "Hide all free hours before the first timeblock." hide-all)
	  (cons :tag "Display specified range of hours [earliest; latest)."
		(integer :tag "Min Hour")
		(integer :tag "Max Hour"))))

(defcustom org-timeblock-current-time-indicator t
  "Whether to show current time indicator in the `org-timeblock-list' buffer."
  :group 'org-timeblock
  :type 'boolean)

(defcustom org-timeblock-tag-colors
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

(defvar org-timeblock-mark-color "#7b435c")

(defvar org-timeblock-markers nil)

(defvar org-timeblock-buffers nil)

(defvar org-timeblock-mark-count 0)

(defvar org-timeblock-select-color-light "#f3d000")

(defvar org-timeblock-select-color-dark "#3f1651")

(defvar org-timeblock-select-color org-timeblock-select-color-light)

(defvar org-timeblock-background-color (face-attribute 'default :background))

(defvar org-timeblock-colors nil)

(defvar org-timeblock-data nil)
(defvar org-timeblock-column 1
  "Currently selected column.")

(defvar org-timeblock-cache nil)
(defvar org-timeblock-svg nil)
(defvar org-timeblock-svg-width 0)
(defvar org-timeblock-svg-height 0)

(defvar org-timeblock-daterange nil
  "The date range that is used to get and display schedule data.")

(defvar org-timeblock-duration-multipliers
  '((?w . 10080)
    (?d . 1440)
    (?h . 60)
    (?m . 1))
  "Duration multipliers used in `org-timeblock-read-duration'.")

(defvar org-timeblock-buffer
  "*org-timeblock*" "The name of the buffer displaying visual schedule.")

(defvar org-timeblock-list-buffer "*org-timeblock-list*"
  "The name of the buffer displaying the list of tasks and events.")

;;;; Keymaps

(defvar-keymap org-timeblock-mode-map
  "+" #'org-timeblock-new-task
  "<mouse-1>" #'org-timeblock-select-block-with-cursor
  "<down>" #'org-timeblock-forward-block
  "n" #'org-timeblock-forward-block
  "<up>" #'org-timeblock-backward-block
  "p" #'org-timeblock-backward-block
  "<right>" #'org-timeblock-forward-column
  "f" #'org-timeblock-forward-column
  "<left>" #'org-timeblock-backward-column
  "b" #'org-timeblock-backward-column
  "C-<right>" #'org-timeblock-day-later
  "C-f" #'org-timeblock-day-later
  "C-<left>" #'org-timeblock-day-earlier
  "C-b" #'org-timeblock-day-earlier
  "RET" #'org-timeblock-goto
  "TAB" #'org-timeblock-goto-other-window
  "d" #'org-timeblock-set-duration
  "i" #'org-timeblock-clock-in
  "o" #'org-clock-out
  "g" #'org-timeblock-redraw-buffers
  "j" #'org-timeblock-jump-to-day
  "C-s" #'org-save-all-org-buffers
  "s" #'org-timeblock-schedule
  "T" #'org-timeblock-toggle-timeblock-list
  "t" #'org-timeblock-todo
  "v" #'org-timeblock-switch-scaling
  "V" #'org-timeblock-change-span
  "m" #'org-timeblock-mark-block
  "%" #'org-timeblock-mark-by-regexp
  "u" #'org-timeblock-unmark-block
  "U" #'org-timeblock-unmark-all-blocks
  "w" #'org-timeblock-write)

(defvar-keymap org-timeblock-list-mode-map
  "+" #'org-timeblock-new-task
  "<remap> <next-line>" #'org-timeblock-list-next-line
  "n" #'org-timeblock-list-next-line
  "<remap> <previous-line>" #'org-timeblock-list-previous-line
  "p" #'org-timeblock-list-previous-line
  "C-<right>" #'org-timeblock-day-later
  "f" #'org-timeblock-day-later
  "C-<left>" #'org-timeblock-day-earlier
  "b" #'org-timeblock-day-earlier
  "C-s" #'org-save-all-org-buffers
  "RET" #'org-timeblock-list-goto
  "TAB" #'org-timeblock-list-goto-other-window
  "d" #'org-timeblock-list-set-duration
  "i" #'org-timeblock-list-clock-in
  "o" #'org-clock-out
  "g" #'org-timeblock-redraw-buffers
  "j" #'org-timeblock-jump-to-day
  "q" #'org-timeblock-quit
  "s" #'org-timeblock-list-schedule
  "T" #'org-timeblock-list-toggle-timeblock
  "t" #'org-timeblock-todo
  "v" #'org-timeblock-switch-scaling
  "V" #'org-timeblock-change-span)

;;;; Modes

(define-derived-mode org-timeblock-mode
  special-mode "Org-Timeblock" :interactive nil
  (setq
   org-timeblock-daterange
   (cons (decode-time)
	 (org-timeblock-time-inc 'day (1- org-timeblock-span)
				 (decode-time)))
   cursor-type nil
   buffer-read-only t)
  (org-timeblock-redisplay))

(define-derived-mode org-timeblock-list-mode
  special-mode "Org-Timeblock-List" :interactive nil
  (setq truncate-lines t))

;;;; Functions

(compat-version "29.1")

(compat-defun org-fold-show-context (&optional key)
  "Make sure point and context are visible."
  (org-show-context key))

(defun org-timeblock-show-context ()
  "Make sure point and context are visible."
  (compat-call org-fold-show-context 'agenda))

(defsubst org-timeblock-format-time (format-string time)
  "Use FORMAT-STRING to format the time value TIME."
  (let ((time (copy-sequence time)))
    (unless (decoded-time-second time)
      (setf (decoded-time-second time) 0))
    (unless (decoded-time-minute time)
      (setf (decoded-time-minute time) 0))
    (unless (decoded-time-hour time)
      (setf (decoded-time-hour time) 0))
    (format-time-string format-string (encode-time time))))

(cl-defsubst org-timeblock-get-ts-prop (&optional object (position 0))
  "Return POSITION's \\='timestamp property, in OBJECT."
  (get-text-property position 'timestamp object))

(cl-defsubst org-timeblock-get-ts-type (&optional object (position 0))
  "Return POSITION's \\='type property, in OBJECT."
  (get-text-property position 'type object))

(defun org-timeblock-cursor-pos ()
  "Return cursor position in the window of the *org-timeblock* buffer.
If cursor position is outside of the window, return nil.

Cursor position is of the form (X . Y)."
  (when-let ((cursor-pos (cdr (mouse-pixel-position)))
	     (window (get-buffer-window org-timeblock-buffer))
	     (pos (window-edges window t nil t)))
    (when (and (> (- (car cursor-pos) (car pos)) 0)
	       (> (- (cdr cursor-pos) (cadr pos)) 0))
      (cons (- (car cursor-pos) (car pos))
	    (- (cdr cursor-pos) (cadr pos))))))

(defun org-timeblock-selected-block-marker ()
  "Return a marker pointing to the org entry of selected timeblock."
  (when-let ((node (org-timeblock-selected-block))
	     (id (dom-attr node 'id)))
    (org-timeblock-get-marker-by-id
     (car (split-string id "_")))))

(defun org-timeblock-get-marker-by-id (id)
  "Return a marker of entry with ID."
  (cadr (seq-find (lambda (x) (string= (car x) id)) org-timeblock-data)))

(defun org-timeblock-get-dates (from to)
  "Return a list of decoded-time dates between FROM and TO."
  (let (dates)
    (while (and
	    (push from dates)
	    (setq from (org-timeblock-time-inc 'day 1 from))
	    (org-timeblock-date<= from to)))
    (nreverse dates)))

(defun org-timeblock-selected-block ()
  "Return an id of the entry of selected timeblock.
id is constructed via `org-timeblock-construct-id'"
  (car (dom-search
	org-timeblock-svg
	(lambda (node) (dom-attr node 'select)))))

(defmacro org-timeblock-on (accessor op lhs rhs)
  "Run OP on ACCESSOR's return values from LHS and RHS."
  `(,op (,accessor ,lhs) (,accessor ,rhs)))

(defun org-timeblock-date= (a b)
  "Return non-nil if dates of A and B time values are equal."
  (cond
   ((and (null a) (null b)))
   ((and a b)
    (and (org-timeblock-on decoded-time-year  = a b)
         (org-timeblock-on decoded-time-month = a b)
         (org-timeblock-on decoded-time-day   = a b)))))

(defun org-timeblock-date< (a b)
  "Return non-nil if A's date is less than B's date."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (or (org-timeblock-on decoded-time-year < a b)
	(and
	 (org-timeblock-on decoded-time-year = a b)
	 (or (org-timeblock-on decoded-time-month < a b)
	     (and (org-timeblock-on decoded-time-month = a b)
		  (org-timeblock-on decoded-time-day < a b))))))))

(defun org-timeblock-time-diff (a b)
  "Return difference between times A and B in minutes."
  (when-let ((a (encode-time a))
	     (b (encode-time b)))
    (/ (time-convert (time-subtract a b) 'integer) 60)))

(defun org-timeblock-decoded< (a b)
  "Return non-nil if A is earlier then B."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (time-less-p
     (encode-time a)
     (encode-time b)))))

(defun org-timeblock-decoded= (a b)
  "Return non-nil if A is earlier then B."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (time-equal-p
     (encode-time a)
     (encode-time b)))))

(defun org-timeblock-time< (a b)
  "Return non-nil if A's time is earlier then B's time.
Compare only hours and minutes."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (or (org-timeblock-on decoded-time-hour < a b)
	(and
	 (org-timeblock-on decoded-time-hour = a b)
	 (org-timeblock-on decoded-time-minute < a b))))))

(defun org-timeblock-date<= (a b)
  "Return non-nil if A's date is <= B's date."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (or (org-timeblock-on decoded-time-year < a b)
	(and
	 (org-timeblock-on decoded-time-year = a b)
	 (or (org-timeblock-on decoded-time-month < a b)
	     (and (org-timeblock-on decoded-time-month = a b)
		  (org-timeblock-on decoded-time-day <= a b))))))))

(defsubst org-timeblock-get-ts (item)
  "Return ITEM's \\='timestamp text property as decoded time."
  (org-timeblock-timestamp-to-time (org-timeblock-get-ts-prop item)))

(defun org-timeblock-timestamp< (a b)
  "Return t, if A's \\='timestamp is less then B's."
  (org-timeblock-on org-timeblock-get-ts org-timeblock-time< a b))

(defun org-timeblock-select-block-for-current-entry ()
  "Select block for the entry at point in `org-timeblock-list-mode'."
  (when-let (((get-buffer-window org-timeblock-buffer))
	     (timestamp (org-timeblock-get-ts-prop
			 nil (line-beginning-position)))
	     ((org-element-property :hour-start timestamp))
	     ((not (org-timeblock--daterangep timestamp)))
	     (id (get-text-property (line-beginning-position) 'id))
	     (column-number
	      (save-excursion
		(let ((count 0))
		  (while (not (bobp))
		    (while (not (eq (get-text-property (point) 'face)
				    'org-timeblock-list-header))
		      (forward-line -1))
		    (cl-incf count)
		    (forward-line -1))
		  count)))
	     (inhibit-read-only t))
    (org-timeblock-unselect-block)
    (when-let ((node (car (dom-by-id org-timeblock-svg id))))
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill))
      (dom-set-attribute node 'fill org-timeblock-select-color)
      (dom-set-attribute node 'select t)
      (setq org-timeblock-column (dom-attr node 'column)))
    (with-current-buffer org-timeblock-buffer
      (org-timeblock-redisplay))))

(defun org-timeblock-intersect-p (entry1 entry2)
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

(defun org-timeblock--parse-hex-color (hex)
  "Convert a HEX color code to a RGB list of form (R G B)."
  (cl-loop for scale in (color-name-to-rgb hex) collect (* scale 255)))

(defun org-timeblock--random-color ()
  "Generate random color based on BASE-COLOR and RANGE.
Default background color is used when BASE-COLOR is nil."
  (let* ((default-background-color
	  (ignore-errors (face-attribute 'default :background)))
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
    (let* ((rgb (org-timeblock--parse-hex-color base-color))
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

(defun org-timeblock--timestamp-relevant-p (timestamp date)
  "Check if org-element TIMESTAMP is relevant to DATE.
DATE is decoded-time value."
  (let ((start-ts (org-timeblock-timestamp-to-time timestamp)))
    (or
     (org-timeblock-date= start-ts date)
     (when-let ((org-timeblock-show-future-repeats)
		(value (org-element-property
			:repeater-value timestamp))
		(unit
		 (pcase (org-element-property
			 :repeater-unit timestamp)
		   (`week
		    (setq value (* value 7))
		    'day)
		   ((and _ u) u)))
		(start start-ts))
       (or
	(and (eq unit 'day)
	     (= value 1)
	     (if (eq org-timeblock-show-future-repeats 'next)
		 (org-timeblock-date= (org-timeblock-time-inc 'day 1 start) date)
	       (org-timeblock-date<= start date)))
	(progn
	  (if (eq org-timeblock-show-future-repeats 'next)
	      (setq start (org-timeblock-time-inc unit value start))
	    (while (org-timeblock-date< start date)
	      (setq start (org-timeblock-time-inc unit value start))))
	  (org-timeblock-date= start date))))
     (when-let ((end-ts (org-timeblock-timestamp-to-time timestamp t)))
       (and (org-timeblock-date< start-ts date)
	    (org-timeblock-date<= date end-ts))))))

(defun org-timeblock-redraw-timeblocks ()
  "Redraw *org-timeblock* buffer."
  (with-current-buffer (get-buffer-create org-timeblock-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if-let ((entries (org-timeblock-get-entries
			 (car org-timeblock-daterange)
			 (cdr org-timeblock-daterange)
			 t))
	       (dates (org-timeblock-get-dates
		       (car org-timeblock-daterange)
		       (cdr org-timeblock-daterange)))
	       (window (get-buffer-window org-timeblock-buffer))
	       ((setq org-timeblock-svg-height (window-body-height window t)
		      org-timeblock-svg-width (window-body-width window t))))
	  (let* ((column-width (/ org-timeblock-svg-width (length dates)))
		 (timeline-left-padding (* 2 (default-font-width)))
		 (block-max-width (- column-width timeline-left-padding))
		 (max-hour (if (consp org-timeblock-scale-options)
			       (if (= (cdr org-timeblock-scale-options) 24)
				   24
				 (1+ (cdr org-timeblock-scale-options)))
			     24))
		 (cur-time (decode-time)))
	    (setq org-timeblock-svg (svg-create org-timeblock-svg-width org-timeblock-svg-height))
	    (dotimes (iter (length dates))
	      (if-let ((entries
			(seq-filter
			 (lambda (x)
			   (let ((timestamp (org-timeblock-get-ts-prop x)))
			     (and
			      (org-timeblock--timestamp-relevant-p
			       timestamp (nth iter dates))
			      (or (not (consp org-timeblock-scale-options))
				  (<= (car org-timeblock-scale-options)
				      (org-element-property :hour-start timestamp)
				      (cdr org-timeblock-scale-options))
				  (and
				   (org-element-property :hour-end timestamp)
				   (or (<=
					(org-element-property :hour-start timestamp)
					(car org-timeblock-scale-options)
					(org-element-property :hour-end timestamp))
				       (<= (car org-timeblock-scale-options)
					   (org-element-property :hour-end timestamp)
					   (cdr org-timeblock-scale-options))))))))
			 entries)))
		  (let* ((order -1)
			 (min-hour
			  (pcase org-timeblock-scale-options
			    ((pred consp) (car org-timeblock-scale-options))
			    (`nil 0)
			    (_ (apply #'min (remove
					     nil
					     (append
					      (list (unless (eq org-timeblock-scale-options 'hide-all) (decoded-time-hour (decode-time))))
					      (mapcar
					       (lambda (entry)
						 (let ((s-or-e (org-timeblock-get-ts-prop entry)))
						   (if (and (org-timeblock-date< (org-timeblock-timestamp-to-time s-or-e) (nth iter dates))
							    (not (org-element-property :repeater-type s-or-e)))
						       0
						     (org-element-property :hour-start s-or-e))))
					       entries)))))))
			 (scale (/ org-timeblock-svg-height (float (* (- max-hour min-hour) 60))))
			 (cur-time-indicator
			  (and org-timeblock-current-time-indicator
			       (* scale
				  (-
				   (+ (* (decoded-time-hour cur-time) 60)
				      (decoded-time-minute cur-time)) ;; minutes
				   (* min-hour 60)))))
			 (columns
			  (mapcar (lambda (x) (cons (get-text-property 0 'id x) 1)) entries))
			 placed
			 (bg-rgb-sum (apply #'+ (org-timeblock--parse-hex-color org-timeblock-background-color)))
			 (get-color
			  (if (string= org-timeblock-background-color
				       (face-attribute 'default :background))
			      (lambda (title)
				(cl-callf (lambda (x) (or x (org-timeblock--random-color)))
				    (alist-get title org-timeblock-colors nil nil #'equal)))
			    (setq org-timeblock-background-color
				  (face-attribute 'default :background))
			    (setq org-timeblock-select-color
				  (if (> (setq bg-rgb-sum
					       (apply #'+
						      (org-timeblock--parse-hex-color
						       org-timeblock-background-color)))
					 550)
				      org-timeblock-select-color-light
				    org-timeblock-select-color-dark))
			    (lambda (title)
			      (setf (alist-get title org-timeblock-colors
					       nil nil #'equal)
				    (org-timeblock--random-color)))))
			 (hour-lines-color
			  (if (> bg-rgb-sum 550) "#7b435c" "#cdcdcd")))
		    (dolist (entry entries)
		      (let* ((timestamp (org-timeblock-get-ts-prop entry))
			     (repeated (org-element-property
					:repeater-type timestamp))
			     (start-ts (org-timeblock-timestamp-to-time
					timestamp))
			     (end-ts (org-timeblock-timestamp-to-time
				      timestamp t))
			     (start-date-earlier-p (org-timeblock-date<
						    start-ts (nth iter dates)))
			     (end-date-later-p (org-timeblock-date<
						(nth iter dates) end-ts)))
			(add-text-properties
			 0 (length entry)
			 `( time-string
			    ,(and
			      org-timeblock-display-time
			      (or repeated (not
					    (or end-date-later-p
						start-date-earlier-p)))
			      (concat
			       (org-timeblock-format-time " %H:%M" start-ts)
			       (and end-ts (org-timeblock-format-time "-%H:%M" end-ts))
			       (and repeated
				    (concat
				     " "
				     (pcase (org-element-property :repeater-type timestamp)
				       (`cumulate "+") (`catch-up "++") (`restart ".+"))
				     (let ((val (org-element-property :repeater-value timestamp)))
				       (and val (number-to-string val)))
				     (pcase (org-element-property :repeater-unit timestamp)
				       (`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))))
			    block-height
			    ,(- (if (and start-ts end-ts)
				    (max
				     (default-font-height)
				     (round
				      (* (org-timeblock-time-diff
					  (if (or (and end-date-later-p (not repeated))
						  (org-timeblock-decoded<
						   (org-timeblock-time-apply
						    (nth iter dates)
						    :hour (1- max-hour)
						    :minute 59 :second 0)
						   (org-timeblock-time-apply
						    (nth iter dates)
						    :hour (decoded-time-hour end-ts)
						    :minute (decoded-time-minute end-ts))))
					      (org-timeblock-time-apply
					       (nth iter dates)
					       :hour (1- max-hour)
					       :minute 59
					       :second 0)
					    end-ts)
					  (if (or (and start-date-earlier-p (not repeated))
						  (org-timeblock-decoded<
						   (org-timeblock-time-apply
						    (nth iter dates)
						    :hour (decoded-time-hour start-ts)
						    :minute (decoded-time-minute start-ts))
						   (org-timeblock-time-apply
						    (nth iter dates)
						    :hour min-hour :minute 0
						    :second 0)))
					      (org-timeblock-time-apply
					       (nth iter dates)
					       :hour min-hour :minute 0
					       :second 0)
					    start-ts))
					 scale)))
				  (default-font-height))
				(if (eq 'event (org-timeblock-get-ts-type entry)) 2 1))
			    y
			    ,(if-let ((value (+ (round (* (if (or (and start-date-earlier-p (not repeated))
								  (org-timeblock-decoded<
								   (org-timeblock-time-apply
								    (nth iter dates)
								    :hour (decoded-time-hour start-ts)
								    :minute (decoded-time-minute start-ts))
								   (org-timeblock-time-apply
								    (nth iter dates)
								    :hour min-hour :minute 0
								    :second 0)))
							      0
							    (- (+ (* 60 (org-element-property :hour-start timestamp))
								  (org-element-property :minute-start timestamp))
							       (* min-hour 60)))
							  scale))
						(if (eq 'event (org-timeblock-get-ts-type entry)) 2 1)))
				      ((< (- org-timeblock-svg-height value) (default-font-height))))
				 (- org-timeblock-svg-height (default-font-height))
			       value)
			    n-day-indicator
			    ,(and (not repeated)
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
					       (org-timeblock-intersect-p entry el)
					       (cl-incf k)
					       (throw 'next-column t)))
					(throw 'found-column k))))))))
		    ;; Drawing hour lines
		    (let ((lines-iter (if (> min-hour 0) (1- min-hour) 0)) y)
		      (while (< (cl-incf lines-iter) max-hour)
			(setq y (round (* scale (- lines-iter min-hour) 60)))
			(svg-line
			 org-timeblock-svg
			 (+ timeline-left-padding (* column-width iter))
			 y
			 (+ column-width (* column-width iter))
			 y
			 :stroke-dasharray "4"
			 :stroke hour-lines-color)
			(svg-text
			 org-timeblock-svg (format "%d" lines-iter)
			 :y (+ y 5)
			 :x (* column-width iter)
			 :fill (face-attribute 'default :foreground))))
		    ;; Drawing current time indicator
		    (and cur-time-indicator
			 (org-timeblock-date= (nth iter dates) cur-time)
			 (svg-line
			  org-timeblock-svg
			  (* column-width iter)
			  cur-time-indicator
			  (+ column-width (* column-width iter))
			  cur-time-indicator
			  :stroke "red"))
		    ;; Drawing all the entries inside the timeline
		    (dolist (entry entries)
		      (when-let ((length
				  (1+ (length
				       (seq-uniq
					(mapcar
					 ;; get columns for those entries
					 (lambda (x)
					   (cdr (assoc (get-text-property 0 'id x) columns)))
					 ;; find those with which current entry is in intersection
					 (seq-filter
					  (lambda (x)
					    (unless
						(equal
						 (get-text-property 0 'id entry)
						 (get-text-property 0 'id x))
					      (org-timeblock-intersect-p entry x)))
					  entries))
					#'eq))))
				 (y (get-text-property 0 'y entry))
				 (block-height
				  (get-text-property 0 'block-height entry))
				 ((> (+ y block-height) 0))
				 (x (+ (+ timeline-left-padding
					  (round
					   (* (1- (cdr
						   (assoc
						    (get-text-property 0 'id entry)
						    columns)))
					      (/ block-max-width length))))
				       (* column-width iter)
				       (if (eq 'event (org-timeblock-get-ts-type entry)) 2 1)))
				 (block-width
				  (- (round (/ block-max-width length))
				     (if (eq 'event (org-timeblock-get-ts-type entry)) 2 1)))
				 (title
				  (concat (get-text-property 0 'title entry)
					  (get-text-property 0 'n-day-indicator entry)))
				 ;; Splitting the title of an entry
				 (heading-list
				  (if (> (* (length title) (default-font-width))
					 block-width)
				      (seq-take
				       (seq-partition title (/ block-width (default-font-width)))
				       (let ((lines-count
					      (round
					       (/ block-height (default-font-height)))))
					 (if (= 0 lines-count) 1 lines-count)))
				    `(,title))))
			(let ((time-string
			       (get-text-property 0 'time-string entry))
			      (colors (org-timeblock-get-colors
				       (get-text-property 0 'tags entry))))
			  (when (< (/ block-width (default-font-width))
				   (length time-string))
			    (setq time-string nil))
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
			  (push (list (get-text-property 0 'id entry)
				      (get-text-property 0 'marker entry))
				org-timeblock-data)
			  ;; Appending generated rectangle for current entry
			  (svg-rectangle
			   org-timeblock-svg x y block-width block-height
			   :column (1+ iter)
			   :stroke
			   (if (eq 'event (org-timeblock-get-ts-type entry))
			       "#5b0103" "#cdcdcd")
			   :stroke-width
			   (if (eq 'event (org-timeblock-get-ts-type entry))
			       2 1)
			   :opacity "0.7"
			   :order (cl-incf order)
			   :fill
			   (or
			    (and
			     (eq 'deadline
				 (org-timeblock-get-ts-type entry)) "#5b0103")
			    (car colors) (funcall get-color title))
			   ;; Same timestamp can be displayed in multiple
			   ;; columns, so _column-number postfix is used to tell
			   ;; that blocks apart
			   :id (format "%s_%d"
				       (get-text-property 0 'id entry)
				       (1+ iter))
			   :type (org-timeblock-get-ts-type entry))
			  ;; Setting the title of current entry
			  (let ((y (- y 5)))
			    (dolist (heading-part heading-list)
			      (svg-text org-timeblock-svg heading-part
					:x x
					:y (cl-incf y (default-font-height))
					:fill
					(or
					 (and
					  (eq 'deadline
					      (org-timeblock-get-ts-type entry))
					  "#ffffff")
					 (cadr colors)
					 (face-attribute 'default :foreground))
					:font-size
					(aref (font-info (face-font 'default)) 2))))
			  (when time-string
			    (svg-text org-timeblock-svg time-string
				      :x (- (+ x block-width)
					    (* (length time-string)
					       (default-font-width)))
				      :y (- (+ y block-height) 2)
				      :fill
				      (or
				       (and
					(eq 'deadline
					    (org-timeblock-get-ts-type entry))
					"#ffffff")
				       (cadr colors) hour-lines-color)
				      :font-size
				      (aref (font-info (face-font 'default)) 2)))))))
		(let ((message "No data."))
		  (svg-text org-timeblock-svg message
			    :y (/ org-timeblock-svg-height 2)
			    :x (+ (- (/ column-width 2)
				     (/ (* (default-font-width)
					   (length message))
					2))
				  (* column-width iter))
			    :fill (face-attribute 'default :foreground)
			    :font-size
			    (aref (font-info (face-font 'default)) 2)))))
	    (svg-insert-image org-timeblock-svg))
	(let* ((window (get-buffer-window org-timeblock-buffer))
	       (window-height (window-body-height window t))
	       (window-width (window-body-width window t))
	       (message "No data."))
	  (setq org-timeblock-svg (svg-create window-width window-height))
	  (svg-text
	   org-timeblock-svg message
	   :y (/ window-height 2)
	   :x (- (/ window-width 2)
		 (/ (* (default-font-width) (length message)) 2))
	   :fill (face-attribute 'default :foreground))
	  (svg-insert-image org-timeblock-svg)))
      (setq org-timeblock-mark-count 0)
      (org-timeblock-redisplay))))

(defun org-timeblock-redisplay ()
  "Redisplay *org-timeblock* buffer."
  (let ((inhibit-read-only t))
    (when-let ((window (get-buffer-window org-timeblock-buffer)))
      (if (or (< (window-body-height window t) org-timeblock-svg-height)
	      (< (window-body-width window t) org-timeblock-svg-width))
	  (org-timeblock-redraw-timeblocks)
	(setq header-line-format
	      (let* ((dates (org-timeblock-get-dates
			     (car org-timeblock-daterange)
			     (cdr org-timeblock-daterange)))
		     (left-fringe (/ (car (window-fringes window))
				     (default-font-width)))
		     (max-length (/ (+ (/ (window-body-width window t)
					  (default-font-width))
				       left-fringe)
				    (length dates)))
		     (date-format
		      (pcase max-length
			((pred (< 15)) "[%Y-%m-%d %a]")
			((pred (< 11)) "[%Y-%m-%d]")
			((pred (< 6)) "[%m-%d]")
			((pred (< 3)) "[%d]")))
		     (right-margin (format "%% -%ds" max-length))
		     (result (make-string left-fringe ? )))
		(dotimes (iter (length dates))
		  (cl-callf concat result
		    (propertize
		     (format right-margin
			     (org-timeblock-format-time date-format (nth iter dates)))
		     'face
		     (and (= org-timeblock-column (1+ iter))
			  `(:background ,org-timeblock-select-color)))))
		result))))
    (svg-possibly-update-image org-timeblock-svg)))

(defun org-timeblock-show-timeblocks ()
  "Switch to *org-timeblock* buffer in another window."
  (switch-to-buffer-other-window org-timeblock-buffer)
  (other-window 1))

(defun org-timeblock-show-timeblock-list ()
  "Switch to *org-timeblock-list* buffer in another window."
  (switch-to-buffer-other-window org-timeblock-list-buffer)
  (other-window 1))

(defun org-timeblock-quit ()
  "Exit `org-timeblock-list-mode'."
  (interactive)
  (quit-window t))

(defun org-timeblock--schedule-time (&optional marker)
  "Interactively change time in DATE for Org entry timestamp at MARKER.
If MARKER is nil, use entry at point.
If DATE is not specified, use `org-timeblock-date'.

Date won't be changed.  The time might be a
timerange which depends on user choice.

Time format is \"HHMM\""
  (when marker
    (unless (marker-buffer marker)
      (user-error "Non-existent marker's buffer")))
  (org-with-point-at (or marker (point))
    (org-timeblock-show-context)
    (let* (ts-type
	   go-back-p
	   prev-date
	   (timestamp (org-element-timestamp-parser))
	   (date (org-timeblock-timestamp-to-time timestamp)))
      (while (null ts-type)
	(pcase (read-char-from-minibuffer
		"Change timestamp: time[s]tamp, time[r]ange, other [d]ay"
		'(?s ?r ?d))
	  (?r (setq ts-type 'timerange))
	  (?s (setq ts-type 'timestamp))
	  (?d (setq prev-date date
		    date (decode-time (org-read-date nil t nil nil (encode-time date))))
	      (unless (and
		       (org-timeblock-date<=
			date (cdr org-timeblock-daterange))
		       (org-timeblock-date<=
			(car org-timeblock-daterange) date))
		(org-timeblock-jump-to-day date)
		(setq go-back-p t)))))
      (let* ((start-ts (org-timeblock-timestamp-to-time timestamp))
	     (end-ts (org-timeblock-timestamp-to-time timestamp t))
	     (duration (when (and start-ts end-ts)
			 (org-timeblock-time-diff end-ts start-ts)))
	     (new-start-ts (org-timeblock-read-ts date "START-TIME: "))
	     (new-end-ts
	      (if (eq ts-type 'timerange)
		  (let (ts)
		    (while (progn (setq ts (org-timeblock-read-ts date "END-TIME: "))
				  (org-timeblock-decoded< ts new-start-ts))
		      (ding))
		    ts)
		(when duration (org-timeblock-time-inc
				'minute duration new-start-ts)))))
	(when (and prev-date go-back-p)
	  (org-timeblock-jump-to-day prev-date))
	(org-timeblock--schedule new-start-ts new-end-ts)))))

(defun org-timeblock--daterangep (timestamp)
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

(defun org-timeblock--construct-prefix (timestamp type)
  "Construct prefix for TIMESTAMP of type TYPE.

TIMESTAMP is org-element timestamp object which is used to
construct a timerange inside the prefix."
  (let ((hstart (org-element-property :hour-start timestamp))
	(mstart (org-element-property :minute-start timestamp))
	(hend (org-element-property :hour-end timestamp))
	(mend (org-element-property :minute-end timestamp))
	(type-str (pcase type
		    (`event "EVENT")
		    (`sched "SCHED")
		    (`deadline "DEADL"))))
    (propertize
     (format
      " %s % -12s % -6s "
      type-str
      (if (org-timeblock--daterangep timestamp)
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
		      mend))))
      (concat
       ""
       (pcase (org-element-property :repeater-type timestamp)
	 (`cumulate "+") (`catch-up "++") (`restart ".+"))
       (when-let ((val (org-element-property :repeater-value timestamp)))
	 (number-to-string val))
       (pcase (org-element-property :repeater-unit timestamp)
	 (`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))
     'prefix t)))

(cl-defun org-timeblock-read-ts (ts &optional (prompt "TIME:"))
  "Read a time in \"HHMM\" format and apply it to TS.
Return the changed time struct.

PROMPT can overwrite the default prompt."
  (let (time)
    (catch 'exit
      (while t
	(let ((len (length time))
	      (ch (read-char-exclusive
		   (concat "[format: HHMM] " prompt (reverse time)))))
	  (cond
	   ((or (and (= len 0) (<= ?0 ch ?2))
		(and (= len 1)
		     (if (< (car time) ?2) (<= ?0 ch ?9) (<= ?0 ch ?3)))
		(and (= len 2) (<= ?0 ch ?5)))
	    (push ch time))
	   ((and (= len 3) (<= ?0 ch ?9))
	    (push ch time)
	    (throw 'exit t))
	   ((and (/= len 0) (eq ch ?\C-?))
	    (pop time))
	   (t (ding))))))
    (cl-macrolet ((pop-digit () '(- (pop time) 48)))
      (org-timeblock-time-apply
       ts
       :minute (+ (pop-digit) (* 10 (pop-digit)))
       :hour (+ (pop-digit) (* 10 (pop-digit)))))))

(defun org-timeblock-construct-id (&optional marker)
  "Construct identifier for the timestamp at MARKER.
If MARKER is nil, use timestamp at point."
  (org-with-point-at (or marker (point))
    (md5
     (format
      "%s%d%s"
      (buffer-file-name (buffer-base-buffer))
      (point)
      (when (or (looking-at org-tr-regexp)
		(looking-at org-ts-regexp))
	(match-string 0)))
     nil nil 'utf-8)))

(defun org-timeblock-files ()
  "Get a list of files in which tasks are searched."
  (pcase org-timeblock-files
    (`agenda (org-agenda-files))
    ((and list (pred listp)) list)))

(defun org-timeblock-get-buffer-entries-all (buffer)
  "Get all not done and not archived entries with any active timestamp in BUFFER."
  (let (entries tags
		update-markers-alist-p
		(buffer-markers
		 (alist-get buffer
			    org-timeblock-markers nil nil #'equal)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-ts-regexp nil t)
	 (if (save-match-data
	       (or (org-entry-is-done-p)
		   (progn
		     (setq tags
			   (mapcar #'substring-no-properties
				   (org-get-tags)))
		     (member org-archive-tag tags))))
	     (org-get-next-sibling)
	   (when-let
	       ((timestamp-and-type
		 (save-excursion
		   (goto-char (match-beginning 0))
		   (list
		    (org-element-timestamp-parser)
		    (save-excursion
		      (cond
		       ((re-search-backward
			 (concat org-scheduled-regexp "[ \t]*\\=")
			 nil
			 t)
			'sched)
		       ((re-search-backward
			 (concat org-deadline-regexp "[ \t]*\\=")
			 nil
			 t)
			'deadline)
		       (t 'event)))
		    (or (seq-find (lambda (x) (= x (point))) buffer-markers)
			(let ((marker (copy-marker (point) t)))
			  (setq update-markers-alist-p t)
			  (push marker buffer-markers)
			  marker)))))
		(timestamp (car timestamp-and-type))
		(type (cadr timestamp-and-type))
		(marker (caddr timestamp-and-type))
		(start-ts (org-timeblock-timestamp-to-time
			   timestamp))
		(title (or (org-get-heading t nil t t) "no title")))
	     (save-excursion
	       (org-back-to-heading-or-point-min t)
	       (push
		(propertize
		 (concat
		  (org-timeblock--construct-prefix timestamp type)
		  title)
		 'type type
		 'timestamp timestamp
		 'marker marker
		 'tags tags
		 'id (org-timeblock-construct-id marker)
		 'title title)
		entries)))))))
    (when update-markers-alist-p
      (setf
       (alist-get buffer
		  org-timeblock-markers nil nil #'equal)
       buffer-markers))
    entries))

(defun org-timeblock-get-entries (from to &optional timeblocks)
  "Return scheduled tasks or events in [FROM;TO] timerange.
FROM and TO are decoded-time values.

When TIMEBLOCKS is non-nil, exclude entries with daterange or
without time."
  (org-timeblock-update-cache)
  (seq-filter
   (lambda (x)
     (when-let
	 ((timestamp (org-timeblock-get-ts-prop x))
	  ((or (not timeblocks)
	       (and
		(not (org-timeblock--daterangep timestamp))
		(org-element-property :hour-start timestamp))))
	  (start-ts (org-timeblock-timestamp-to-time
		     timestamp)))
       (or
	(and
	 (org-element-property
	  :repeater-type timestamp)
	 (org-timeblock-date<= start-ts from))
	(and
	 (org-timeblock-date<= from start-ts)
	 (org-timeblock-date<= start-ts to))
	(let ((end-ts
	       (org-timeblock-timestamp-to-time
		timestamp t)))
	  (and
	   end-ts
	   (org-timeblock-date<= from end-ts)
	   (org-timeblock-date<= end-ts to))))))
   org-timeblock-cache))

(defun org-timeblock-update-cache ()
  "Update org-timeblock-cache.
Return nil if buffers are up-to-date."
  (when-let
      ((buffers-to-update
	(mapcar
	 #'find-file-noselect
	 (seq-filter
	  (lambda (file)
	    (let* ((buffer (find-file-noselect file))
		   (modified-tick
		    (alist-get file org-timeblock-buffers nil nil #'equal))
		   (new-modified-tick (buffer-chars-modified-tick buffer)))
	      (and (not (eq new-modified-tick modified-tick))
		   (setf
		    (alist-get file org-timeblock-buffers nil nil #'equal)
		    new-modified-tick))))
	  (org-timeblock-files)))))
    (setq org-timeblock-cache
	  (sort
	   (apply
	    #'append
	    (seq-remove
	     (lambda (x)
	       (member (marker-buffer (get-text-property 0 'marker x))
		       buffers-to-update))
	     org-timeblock-cache)
	    (mapcar
	     #'org-timeblock-get-buffer-entries-all
	     buffers-to-update))
	   #'org-timeblock-timestamp<))
    t))

(defun org-timeblock-get-colors (tags)
  "Return the colors for TAGS.
Return value is of the form (\"background color\" \"foreground color\")."
  (catch 'found
    (dolist (tag tags)
      (when-let ((colors (cdr (seq-find (lambda (x) (string= (car x) tag))
					org-timeblock-tag-colors))))
	(throw 'found colors)))))

(defun org-timeblock-timestamp-to-time (ts &optional end)
  "Convert TS into an Emacs decoded time value.
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
	      (make-decoded-time
	       :year year-end :month month-end :day day-end
	       :hour (or hour-end 0) :minute (or minute-end 0) :second 0))))
      (make-decoded-time
       :year year-start :month month-start :day day-start
       :hour (or hour-start 0) :minute (or minute-start 0) :second 0))))

(defun org-timeblock--schedule (start-ts &optional end-ts)
  "Schedule org entry at point.
START-TS and END-TS are Emacs decoded time values.

Return new org-element timestamp object."
  (let* ((planning-func
	  (cond ((save-excursion
		   (re-search-backward
		    (concat org-deadline-regexp "[ \t]*\\=")
		    nil
		    t))
		 #'org-deadline)
		((save-excursion
		   (re-search-backward
		    (concat org-scheduled-regexp "[ \t]*\\=")
		    nil
		    t))
		 #'org-schedule)))
	 (regexp-with-ts
	  (if (eq planning-func #'org-schedule)
	      org-scheduled-time-regexp
	    org-deadline-time-regexp))
	 (keyword-regexp
	  (concat
	   (if (eq planning-func #'org-schedule)
	       org-scheduled-regexp
	     org-deadline-regexp)
	   "[ \t]*")))
    (if planning-func
	(save-excursion
	  (let* ((timestamp (org-element-timestamp-parser))
		 (repeat-string (org-get-repeat
				 (org-element-property :raw-value timestamp)))
		 (warning-string
		  (concat
		   (pcase (org-element-property :warning-type timestamp)
		     (`first "--") (`all "-"))
		   (let ((val (org-element-property :warning-value timestamp)))
		     (and val (number-to-string val)))
		   (pcase (org-element-property :warning-unit timestamp)
		     (`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))
		 (dates-equal-p (org-timeblock-date= start-ts end-ts)))
	    (cond
	     ((or (not end-ts) dates-equal-p)
	      (funcall-interactively
	       planning-func nil
	       (org-timeblock-ts-to-org-timerange start-ts end-ts)))
	     ((and end-ts (not dates-equal-p))
	      (funcall-interactively
	       planning-func nil
	       (org-timeblock-ts-to-org-timerange start-ts))
	      (when (re-search-forward
		     regexp-with-ts
		     (line-end-position) t)
		(insert "--"
			(org-timeblock-ts-to-org-timerange
			 end-ts
			 nil repeat-string warning-string)))))
	    (org-back-to-heading t)
	    (forward-line)
	    (re-search-forward keyword-regexp (line-end-position) t)
	    (org-element-timestamp-parser)))
      (when (or (looking-at org-tr-regexp)
		(looking-at org-ts-regexp))
	(replace-match
	 (org-timeblock-ts-to-org-timerange start-ts end-ts)))
      (org-element-timestamp-parser))))

(defun org-timeblock-ts-to-org-timerange
    (ts-start &optional ts-end repeat-string warning-string)
  "Create an Org timestamp range string.

TS-START and TS-END are decoded time values.
REPEAT-STRING is a repeater string.
WARNING-STRING is a warning string of the form \"-[0-9]+[hdwmy]\""
  (when-let ((start-date (org-timeblock-format-time "%Y-%m-%d %a" ts-start)))
    (let ((start-time
	   (and (decoded-time-hour ts-start)
		(decoded-time-minute ts-start)
		(org-timeblock-format-time "%R" ts-start)))
	  (end-date (and ts-end (org-timeblock-format-time
				 "%Y-%m-%d %a" ts-end)))
	  (end-time (and ts-end
			 (and (decoded-time-hour ts-end)
			      (decoded-time-minute ts-end)
			      (org-timeblock-format-time "%R" ts-end))))
	  (timestamp-end
           (concat
            (and (org-string-nw-p repeat-string) (concat " " repeat-string))
            (and (org-string-nw-p warning-string) (concat " " warning-string))
            ">")))
      (concat
       "<" start-date (and start-time (concat " " start-time))
       (if (equal end-date start-date)
	   (and end-time (not (equal end-time start-time))
		(concat "-" end-time))
	 (and
	  end-date
	  (concat
	   timestamp-end
	   "--<" end-date
	   (and end-time (concat " " end-time)))))
       timestamp-end))))

(defun org-timeblock-read-duration ()
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
	 (all-multipliers (mapcar #'car org-timeblock-duration-multipliers))
	 (valid-multipliers all-multipliers)
	 typed-multipliers)
    (catch 'dur
      (while-let ((multipliers
		   (apply #'propertize
			  (concat "[" valid-multipliers "]")
			  (and (or (length= dur 0)
				   (member (string-to-char (substring dur -1))
					   all-multipliers))
			       '(face org-agenda-dimmed-todo-face))))
		  (ch (read-char-exclusive
		       (concat "DURATION ([0-9]+" multipliers "):" dur))))
	(cond
	 ((<= ?0 ch ?9)
	  (setq dur (format "%s%c" dur ch)))
	 ((or (and (eq ch ?\C-m) (length> dur 0))
	      (and (member ch valid-multipliers)
		   (string-match-p "[0-9]+$" dur)))
	  (when-let (((member ch '(?m ?\C-m)))
		     (minutes 0)
		     (start 0))
	    (setq dur (concat dur "m"))
	    (while (string-match
		    (concat "\\([0-9]+\\)\\([" typed-multipliers "m]\\)")
		    dur start)
	      (cl-incf minutes (* (cdr
				   (assq (string-to-char (match-string 2 dur))
					 org-timeblock-duration-multipliers))
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

(defun org-timeblock--duration (duration marker)
  "Set SCHEDULED duration to DURATION for the org entry at MARKER.
Change SCHEDULED timestamp duration of the org entry at MARKER.
Return the changed org-element timestamp object."
  (unless (marker-buffer marker)
    (user-error "Non-existent marker's buffer"))
  (org-with-point-at marker
    (org-timeblock-show-context)
    (let ((timestamp (org-element-timestamp-parser)))
      (unless (and (org-element-property :hour-start timestamp)
		   (org-element-property :minute-start timestamp))
	(user-error "The timestamp does not have time"))
      (let* ((start-ts (org-timeblock-timestamp-to-time timestamp))
	     (new-end-ts (org-timeblock-time-inc
			  'minute duration start-ts)))
	(org-timeblock--schedule start-ts new-end-ts)))))

(defun org-timeblock-todo (&optional arg)
  "Change the TODO state of an item in org-timeblock.

Check `org-todo' for more information, including on the values of
ARG."
  (interactive "P")
  (when-let ((marker (pcase major-mode
		       (`org-timeblock-list-mode
			(get-text-property (line-beginning-position) 'marker))
		       (`org-timeblock-mode
			(org-timeblock-selected-block-marker)))))
    (org-with-point-at marker
      (funcall-interactively #'org-todo arg))
    (org-timeblock-redraw-buffers)))

;;;; Main commands

;;;###autoload
(defun org-timeblock-list ()
  "Enter `org-timeblock-list-mode'."
  (interactive)
  (switch-to-buffer org-timeblock-list-buffer)
  (setq org-timeblock-daterange
	(cons (decode-time)
	      (org-timeblock-time-inc 'day (1- org-timeblock-span)
				      (decode-time))))
  (org-timeblock-redraw-buffers))

;;;###autoload
(defun org-timeblock ()
  "Enter `org-timeblock-mode'."
  (interactive)
  (switch-to-buffer org-timeblock-buffer)
  (org-timeblock-mode)
  (org-timeblock-redraw-buffers))

;;;; Planning commands

(defun org-timeblock-clock-in (&optional select)
  "Start the clock on the currently selected block.
See `org-clock-in' to read about what tasks selection options
SELECT prefix argument provides."
  (interactive "P")
  (if (equal select '(4))
      (org-clock-in select)
    (when-let ((marker (org-timeblock-selected-block-marker)))
      (org-with-point-at marker
	(org-timeblock-show-context)
	(org-clock-in select)))))

(defun org-timeblock-list-clock-in (&optional select)
  "Start the clock on the item at point.
See `org-clock-in' to read about what tasks selection options
SELECT prefix argument provides."
  (interactive "P")
  (if (equal select '(4))
      (org-clock-in select)
    (when-let ((marker (get-text-property (line-beginning-position) 'marker)))
      (org-with-point-at marker
	(org-timeblock-show-context)
	(org-clock-in select)))))

(defun org-timeblock-list-get-current-date ()
  "Get date at point."
  (save-excursion
    (while (not (eq (get-text-property (point) 'face)
		    'org-timeblock-list-header))
      (forward-line -1))
    (parse-time-string (buffer-substring
			(line-beginning-position) (line-end-position)))))

(cl-defun org-timeblock-new-task
    (&optional (date (pcase major-mode
		       (`org-timeblock-mode
			(nth (1- org-timeblock-column)
			     (org-timeblock-get-dates
			      (car org-timeblock-daterange)
			      (cdr org-timeblock-daterange))))
		       (`org-timeblock-list-mode
			(org-timeblock-list-get-current-date)))))
  "Create a task scheduled to DATE.
If DATE is nil, use the date in the current view.

The new task is created in `org-timeblock-inbox-file'"
  (interactive)
  (unless (member org-timeblock-inbox-file (org-timeblock-files))
    (user-error "`org-timeblock-inbox-file' must be present in `org-timeblock-files'"))
  (let ((title ""))
    (while (string-empty-p title)
      (setq title (read-string "Heading: ")))
    (with-current-buffer (find-file-noselect org-timeblock-inbox-file)
      (goto-char (point-max))
      (insert "\n")
      (org-insert-heading nil t t)
      (insert "TODO " title " ")
      (pcase org-timeblock-new-task-time
	(`pick (funcall-interactively #'org-schedule nil))
	((pred stringp)
	 (unless
	     (string-match-p "\\([01][0-9]\\|2[0-3]\\):[0-5][0-9]"
			     org-timeblock-new-task-time)
	   (user-error "Wrong time format specified in `org-timeblock-new-task-time'"))
	 (org-schedule nil (concat (org-timeblock-format-time "%Y-%m-%d " date)
				   org-timeblock-new-task-time)))
	(`nil (org-schedule nil (org-timeblock-format-time "%Y-%m-%d" date)))
	(_ (user-error "Invalid custom variable value")))
      (save-buffer)))
  (org-timeblock-redraw-buffers))

(defun org-timeblock-time-inc (slot value time)
  "Return a new time object based on TIME with its SLOT incremented by VALUE.

SLOT should be specified as a plain symbol, not a keyword."
  (let ((time (copy-sequence time)))
    (decoded-time-add
     time
     (make-decoded-time (intern (format ":%s" slot)) value))))

(cl-defun org-timeblock-time-apply (time &key second minute hour
					 day month year)
  "Return new timestamp based on TIME with new slot values from keys."
  (let ((time (copy-sequence time)))
    (when second
      (setf (decoded-time-second time) second))
    (when minute
      (setf (decoded-time-minute time) minute))
    (when hour
      (setf (decoded-time-hour time) hour))
    (when day
      (setf (decoded-time-day time) day))
    (when month
      (setf (decoded-time-month time) month))
    (when year
      (setf (decoded-time-year time) year))
    time))

(defun org-timeblock-list-set-duration ()
  "Interactively change SCHEDULED duration for the task at point.

Change SCHEDULED timestamp duration of the task at point in
`org-timeblock-list-mode'.

Duration format:
2h
2h30m
2h30
45"
  (interactive)
  (when (org-timeblock--daterangep
	 (org-timeblock-get-ts-prop nil (line-beginning-position)))
    (user-error "Can not reschedule entries with daterange timestamp"))
  (when-let ((duration (org-timeblock-read-duration))
	     (timestamp
	      (org-timeblock--duration
	       duration
	       (get-text-property (line-beginning-position) 'marker))))
    (org-timeblock-list-update-entry)
    (when (get-buffer-window org-timeblock-buffer)
      (org-timeblock-redraw-timeblocks))))

(defun org-timeblock-schedule ()
  "Change the timestamp for selected block or all marked blocks."
  (interactive)
  (if (> org-timeblock-mark-count 0)
      (let* ((mark-data
	      (sort (dom-search
		     org-timeblock-svg
		     (lambda (node) (dom-attr node 'mark)))
		    (lambda (x y)
		      (cl-macrolet
			  ((get-ts (x)
			     `(org-with-point-at
				  (org-timeblock-get-marker-by-id
				   (car (split-string (dom-attr ,x 'id) "_")))
				(org-timeblock-timestamp-to-time
				 (org-element-timestamp-parser)))))
			(or
			 (org-timeblock-decoded< (get-ts x) (get-ts y))
			 (org-timeblock-decoded= (get-ts x) (get-ts y)))))))
	     (id (car (split-string (dom-attr (car mark-data) 'id) "_")))
	     (marker (org-timeblock-get-marker-by-id id))
	     (interval
	      (let* ((choices '(("side-by-side (0 mins between blocks)" ?s 0)
				("save time between blocks" ?b savetime)
				("enter your interval" ?i user-input)))
		     (answer
		      (read-char-from-minibuffer
		       (mapconcat
			(lambda (x)
			  (concat
			   (propertize (char-to-string (cadr x)) 'face 'error)
			   " " (car x) "\n"))
			choices)
		       (mapcar #'cadr choices))))
		(message "")
		(pcase (caddr
			(seq-find (lambda (x) (eq (cadr x) answer)) choices))
		  (`user-input (read-number "Interval (minutes): "))
		  ((and n _) n))))
	     (prev-timestamp
	      (org-with-point-at marker
		(org-element-timestamp-parser)))
	     (prev-end-or-start-ts
	      (or (org-timeblock-timestamp-to-time prev-timestamp t)
		  (org-timeblock-timestamp-to-time prev-timestamp)))
	     (timestamp (org-timeblock--schedule-time marker))
	     (new-end-or-start-ts
	      (or (org-timeblock-timestamp-to-time timestamp t)
		  (org-timeblock-timestamp-to-time timestamp))))
	(pop mark-data)
	(pcase interval
	  ((pred integerp)
	   (dolist (block mark-data)
	     (let* ((id (car (split-string (dom-attr block 'id) "_")))
		    (marker (org-timeblock-get-marker-by-id id)))
	       (org-with-point-at marker
		 (let* ((timestamp (org-element-timestamp-parser))
			(start-ts
			 (org-timeblock-timestamp-to-time timestamp))
			(end-ts
			 (org-timeblock-timestamp-to-time timestamp t))
			(duration
			 (when (and start-ts end-ts)
			   (org-timeblock-time-diff end-ts start-ts)))
			(new-start-ts
			 (org-timeblock-time-inc
			  'minute
			  interval new-end-or-start-ts))
			(new-end-ts
			 (when duration
			   (org-timeblock-time-inc
			    'minute duration new-start-ts))))
		   (org-timeblock--schedule new-start-ts new-end-ts)
		   (setq new-end-or-start-ts
			 (or new-end-ts new-start-ts)))))))
	  (`savetime
	   (dolist (block mark-data)
	     (let* ((id (car (split-string (dom-attr block 'id) "_")))
		    (marker (org-timeblock-get-marker-by-id id)))
	       (org-with-point-at marker
		 (let* ((timestamp (org-element-timestamp-parser))
			(start-ts (org-timeblock-timestamp-to-time
				   timestamp))
			(end-ts (org-timeblock-timestamp-to-time
				 timestamp t))
			(duration
			 (when (and start-ts end-ts)
			   (org-timeblock-time-diff end-ts start-ts)))
			(int (org-timeblock-time-diff
			      start-ts prev-end-or-start-ts))
			(new-start-ts
			 (org-timeblock-time-inc
			  'minute int new-end-or-start-ts))
			(new-end-ts
			 (when duration
			   (org-timeblock-time-inc
			    'minute duration new-start-ts))))
		   (org-timeblock--schedule new-start-ts new-end-ts)
		   (setq new-end-or-start-ts (or new-end-ts new-start-ts))
		   (setq prev-end-or-start-ts (or end-ts start-ts)))))))))
    (when-let ((block (org-timeblock-selected-block))
	       (marker (org-timeblock-selected-block-marker)))
      (org-timeblock--schedule-time marker)))
    (org-timeblock-redraw-buffers))

(defun org-timeblock-set-duration ()
  "Interactively change SCHEDULED duration of the selected block.

Change SCHEDULED timestamp duration of the task bound to the selected
block in `org-timeblock-mode'.

Duration format:
2h
2h30m
2h30
45"
  (interactive)
  (when-let ((marker (org-timeblock-selected-block-marker))
	     (block (org-timeblock-selected-block))
	     (duration (org-timeblock-read-duration)))
    (org-timeblock--duration duration marker)
    (org-timeblock-redraw-buffers)))

(defun org-timeblock-list-schedule ()
  "Reschedule the entry at point in *org-timeblock-list* buffer.
The org-element timestamp object may be from an event or from a
SCHEDULED property."
  (interactive)
  (when (org-timeblock--daterangep
	 (org-timeblock-get-ts-prop nil (line-beginning-position)))
    (user-error "Can not reschedule entries with daterange timestamp"))
  (when-let ((date (org-timeblock-list-get-current-date))
	     (timestamp (org-timeblock--schedule-time
			 (get-text-property (line-beginning-position) 'marker))))
    (org-timeblock-list-update-entry)
    (when (get-buffer-window org-timeblock-buffer)
      (org-timeblock-redraw-timeblocks))))

(defun org-timeblock-list-update-entry ()
  "Update text and text properties for the entry at point in *org-timeblock-list*."
  (let ((marker (get-text-property (line-beginning-position) 'marker)) colors)
    (unless (marker-buffer marker)
      (user-error "Non-existent marker's buffer"))
    (let* ((inhibit-read-only t)
	   (type (get-text-property (line-beginning-position) 'type))
	   (new-entry
	    (org-with-point-at marker
	      (let ((timestamp (org-element-timestamp-parser))
		    (title (org-get-heading t nil t t))
		    (tags (mapcar #'substring-no-properties (org-get-tags))))
		(setq colors (org-timeblock-get-colors tags))
		(propertize
		 (concat (org-timeblock--construct-prefix timestamp type)
			 title)
		 'timestamp timestamp
		 'type type
		 'marker marker
		 'tags tags
		 'id (org-timeblock-construct-id marker)
		 'title title)))))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (insert (propertize
	       (concat new-entry "\n")
	       'face
	       `(:extend t ,@(and (car colors) (list :background (car colors)))
			 ,@(and (cadr colors)
				(list :foreground (cadr colors)))))))))

;;;; Navigation commands

(defun org-timeblock-select-block-with-cursor ()
  "Select the block under current position of the cursor."
  (interactive)
  (when-let ((pos (org-timeblock-cursor-pos))
	     (window (get-buffer-window org-timeblock-buffer))
	     (window-width (window-body-width window t)))
    (org-timeblock-unselect-block)
    (when-let ((node
		(car (dom-search
		      org-timeblock-svg
		      (lambda (node)
			(let ((x (dom-attr node 'x))
			      (y (dom-attr node 'y)))
			  (and (eq (dom-tag node) 'rect)
			       (> (car pos) x)
			       (<= (car pos) (+ x (dom-attr node 'width)))
			       (<= (cdr pos) (+ y (dom-attr node 'height)))
			       (> (cdr pos) y))))))))
      (unless (dom-attr node 'mark)
	(dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
      (dom-set-attribute node 'fill org-timeblock-select-color)
      (dom-set-attribute node 'select t))
    (setq org-timeblock-column
	  (1+ (/ (car pos) (/ window-width org-timeblock-span))))
    (org-timeblock-redisplay)
    (org-timeblock-show-olp-maybe (org-timeblock-selected-block-marker))))

(defun org-timeblock-unselect-block ()
  "Unselect selected block.
Return the numerical order of the unselected block on success.
Otherwise, return nil."
  (when-let ((node (car (dom-search
			 org-timeblock-svg
			 (lambda (node)
			   (dom-attr node 'select))))))
    (dom-set-attribute
     node
     'fill (if (dom-attr node 'mark)
	       org-timeblock-mark-color
	     (or (dom-attr node 'orig-fill) "#ffffff")))
    (dom-remove-attribute node 'select)
    (cons (dom-attr node 'order)
	  (dom-attr node 'column))))

(defun org-timeblock-list-next-line ()
  "Move cursor to the next line."
  (interactive)
  (funcall-interactively 'next-line)
  (org-timeblock-select-block-for-current-entry)
  (org-timeblock-show-olp-maybe
   (get-text-property (line-beginning-position) 'marker)))

(defun org-timeblock-list-previous-line ()
  "Move cursor to the previous line."
  (interactive)
  (funcall-interactively 'previous-line)
  (org-timeblock-select-block-for-current-entry)
  (org-timeblock-show-olp-maybe
   (get-text-property (line-beginning-position) 'marker)))

(defun org-timeblock-mark-block ()
  "Mark selected block."
  (interactive)
  (when-let ((node (org-timeblock-selected-block))
	     ((not (dom-attr node 'mark))))
    (dom-set-attribute node 'fill org-timeblock-mark-color)
    (dom-set-attribute node 'mark t)
    (cl-incf org-timeblock-mark-count))
  (org-timeblock-forward-block))

(defun org-timeblock-mark-by-regexp (regexp)
  "Mark blocks by REGEXP."
  (interactive "sMark entries matching regexp: ")
  (dolist (entry
	   (seq-filter
	    (lambda (x) (string-match regexp x))
	    (org-timeblock-get-entries
	     (car org-timeblock-daterange)
	     (cdr org-timeblock-daterange)
	     t)))
    (when-let ((id (get-text-property 0 'id entry))
	       (node (car (dom-by-id org-timeblock-svg id))))
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill))
      (dom-set-attribute node 'fill org-timeblock-mark-color)
      (dom-set-attribute node 'mark t)
      (cl-incf org-timeblock-mark-count)))
  (org-timeblock-redisplay))

(defun org-timeblock-unmark-block ()
  "Unmark selected block."
  (interactive)
  (when-let ((node (org-timeblock-selected-block))
	     ((dom-attr node 'mark)))
    (dom-remove-attribute node 'mark)
    (dom-set-attribute node 'fill (dom-attr node 'orig-fill))
    (cl-decf org-timeblock-mark-count))
  (org-timeblock-forward-block))

(defun org-timeblock-unmark-all-blocks ()
  "Unmark all marked blocks."
  (interactive)
  (when-let ((marked-blocks
	      (dom-search org-timeblock-svg
			  (lambda (node)
			    (dom-attr node 'mark)))))
    (dolist (node marked-blocks)
      (dom-remove-attribute node 'mark)
      (unless (dom-attr node 'select)
	(dom-set-attribute node 'fill (dom-attr node 'orig-fill))))
    (setq org-timeblock-mark-count 0)
    (org-timeblock-redisplay)))

(defun org-timeblock-forward-block ()
  "Select the next timeblock in *org-timeblock* buffer."
  (interactive)
  (when-let ((unsel-order (let ((unselected-info (org-timeblock-unselect-block)))
			    (or (and unselected-info
				     (= (cdr unselected-info)
					org-timeblock-column)
				     (car unselected-info))
				-1)))
	     (node (car (or (dom-search org-timeblock-svg
					(lambda (node)
					  (and
					   (dom-attr node 'order)
					   (= (dom-attr node 'column)
					      org-timeblock-column)
					   (= (dom-attr node 'order)
					      (1+ unsel-order)))))
			    (dom-search org-timeblock-svg
					(lambda (node)
					  (and
					   (dom-attr node 'order)
					   (= (dom-attr node 'column)
					      org-timeblock-column)
					   (= (dom-attr node 'order) 0))))))))
    (unless (dom-attr node 'mark)
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
    (dom-set-attribute node 'fill org-timeblock-select-color)
    (dom-set-attribute node 'select t)
    (org-timeblock-show-olp-maybe (org-timeblock-selected-block-marker))
    (org-timeblock-redisplay) t))

(defun org-timeblock-forward-column ()
  "Select the next column in *org-timeblock* buffer."
  (interactive)
  (if (= org-timeblock-column org-timeblock-span)
      (setq org-timeblock-column 1)
    (cl-incf org-timeblock-column))
  (org-timeblock-forward-block)
  (org-timeblock-redisplay))

(defun org-timeblock-backward-column ()
  "Select the next column in *org-timeblock* buffer."
  (interactive)
  (if (= org-timeblock-column 1)
      (setq org-timeblock-column org-timeblock-span)
    (cl-decf org-timeblock-column))
  (org-timeblock-forward-block)
  (org-timeblock-redisplay))

(defun org-timeblock-show-olp-maybe (marker)
  "Show outline path in echo area for the selected item.
If `org-timeblock-show-outline-path' is non-nil, display the path of the
heading at MARKER in the echo area."
  (when (and org-timeblock-show-outline-path marker (marker-buffer marker)
	     (buffer-live-p (marker-buffer marker)))
    (org-with-point-at marker (org-display-outline-path t t))))

(defun org-timeblock-backward-block ()
  "Select the previous timeblock in *org-timeblock* buffer.
Return t on success, otherwise - nil."
  (interactive)
  (when-let ((unsel-order
	      (let ((unselected-info (org-timeblock-unselect-block)))
		(or (and unselected-info
			 (= (cdr unselected-info)
			    org-timeblock-column)
			 (car unselected-info))
		    0)))
	     (node (car (or
			 (dom-search org-timeblock-svg
				     (lambda (node)
				       (and
					(dom-attr node 'order)
					(= (dom-attr node 'column)
					   org-timeblock-column)
					(= (dom-attr node 'order)
					   (1- unsel-order)))))
			 (let ((len (length
				     (seq-filter
				      (lambda (x)
					(= (dom-attr x 'column)
					   org-timeblock-column))
				      (dom-by-tag org-timeblock-svg 'rect)))))
			   (dom-search
			    org-timeblock-svg
			    (lambda (node)
			      (and
			       (dom-attr node 'order)
			       (= (dom-attr node 'column)
				  org-timeblock-column)
			       (= (dom-attr node 'order) (1- len))))))))))
    (unless (dom-attr node 'mark)
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
    (dom-set-attribute node 'fill org-timeblock-select-color)
    (dom-set-attribute node 'select t)
    (org-timeblock-show-olp-maybe (org-timeblock-selected-block-marker))
    (org-timeblock-redisplay) t))

(defun org-timeblock-day-later ()
  "Go forward in time by one day in `org-timeblock-mode'."
  (interactive)
  (setq org-timeblock-daterange
	(cons (org-timeblock-time-inc 'day 1 (car org-timeblock-daterange))
	      (and (cdr org-timeblock-daterange)
		   (org-timeblock-time-inc
		    'day 1 (cdr org-timeblock-daterange)))))
  (unless (= org-timeblock-column 1)
    (org-timeblock-backward-column))
  (org-timeblock-redraw-buffers))

(defun org-timeblock-jump-to-day (date)
  "Jump to DATE in *org-timeblock-list* or *org-timeblock* buffers.

When called interactively, prompt for the date.
When called from Lisp, DATE should be a date as returned by
`org-read-date'"
  (interactive (list (decode-time (org-read-date
				   nil t nil nil
				   (encode-time
				    (nth (1- org-timeblock-column)
					 (org-timeblock-get-dates
					  (car org-timeblock-daterange)
					  (cdr org-timeblock-daterange))))))))
  (when date
    (setq org-timeblock-daterange
	  (cons date
		(org-timeblock-time-inc
		 'day (1- org-timeblock-span) date)))
    (org-timeblock-redraw-buffers)))

(defun org-timeblock-day-earlier ()
  "Go backward in time by one day in `org-timeblock-mode'."
  (interactive)
  (setq org-timeblock-daterange
	(cons
	 (org-timeblock-time-inc 'day -1 (car org-timeblock-daterange))
	 (and (cdr org-timeblock-daterange)
	      (org-timeblock-time-inc 'day -1 (cdr org-timeblock-daterange)))))
  (unless (= org-timeblock-column org-timeblock-span)
    (org-timeblock-forward-column))
  (org-timeblock-redraw-buffers))

;;;; View commands

(defun org-timeblock-list-goto-other-window ()
  "Jump to the org heading of the entry at point."
  (interactive)
  (unless (eq major-mode 'org-timeblock-list-mode)
    (user-error "Not in *org-timeblock-list* buffer"))
  (let ((marker (get-text-property (line-beginning-position) 'marker)))
    (switch-to-buffer-other-window (marker-buffer marker))
    (goto-char (marker-position marker))
    (org-timeblock-show-context)
    (recenter)))

(defun org-timeblock-list-goto ()
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
    (org-timeblock-show-context)))

(defun org-timeblock-goto-other-window ()
  "Jump to the org heading of selected timeblock."
  (interactive)
  (unless (eq major-mode 'org-timeblock-mode)
    (user-error "Not in *org-timeblock* buffer"))
  (when-let ((marker (org-timeblock-selected-block-marker)))
    (unless (marker-buffer marker)
      (user-error "Non-existent marker's buffer"))
    (switch-to-buffer-other-window (marker-buffer marker))
    (goto-char (marker-position marker))
    (org-timeblock-show-context)
    (recenter)))

(defun org-timeblock-goto ()
  "Go to the heading of the selected block in the same window."
  (interactive)
  (unless (eq major-mode 'org-timeblock-mode)
    (user-error "Not in *org-timeblock* buffer"))
  (when-let ((marker (org-timeblock-selected-block-marker))
	     (buffer (marker-buffer marker))
	     (pos (marker-position marker)))
    (unless buffer (user-error "Trying to switch to non-existent buffer"))
    (pop-to-buffer-same-window buffer)
    (widen)
    (goto-char pos)
    (org-timeblock-show-context)))

(defun org-timeblock-read-number-in-range (min max)
  "Read a number in [MIN; MAX] and return it."
  (cl-loop as n = (read-number
		   (format "Number [%d; %d]: " min max))
	   until (<= min n max)
	   finally return n))

(defun org-timeblock-switch-scaling ()
  "Switch between different scaling modes in `org-timeblock-mode'.

Available view options:
1. Do not hide anything.  All 24 hours will be displayed.
2. Hide hours in the past (if there are no timeblocks).
3. Hide all free hours before the first timeblock."
  (interactive)
  (let* ((choices
	  '(("Hide hours in the past (if there are no timeblocks)." ?c t)
	    ("Do not hide anything.  All 24 hours will be displayed." ?a nil)
	    ("Hide all free hours before the first timeblock." ?h hide-all)
	    ("Display specified range of hours [earliest; latest]." ?v user)))
	 (answer
	  (read-char-from-minibuffer
	   (mapconcat
	    (lambda (x)
	      (concat (propertize (char-to-string (cadr x)) 'face 'error)
		      " " (car x) "\n"))
	    choices)
	   (mapcar #'cadr choices))))
    (message "")
    (setq org-timeblock-scale-options
	  (pcase (caddr (seq-find (lambda (x) (eq (cadr x) answer)) choices))
	    (`user (let* ((min-hour
			   (org-timeblock-read-number-in-range 0 23))
			  (max-hour
			   (org-timeblock-read-number-in-range min-hour 24)))
		     (cons min-hour max-hour)))
	    ((and n _) n)))
    (org-timeblock-redraw-timeblocks)))

(defun org-timeblock-change-span ()
  "Change view span."
  (interactive)
  (let ((cur-date (nth (1- org-timeblock-column)
		       (org-timeblock-get-dates
			(car org-timeblock-daterange)
			(cdr org-timeblock-daterange))))
	(span (- (read-char-from-minibuffer
		  "Span span [1-7]: " '(?1 ?2 ?3 ?4 ?5 ?6 ?7))
		 48)))
    (setq org-timeblock-daterange
	  (cons cur-date (org-timeblock-time-inc 'day (1- span) cur-date))
	  org-timeblock-span span
	  org-timeblock-column 1))
  (org-timeblock-redraw-buffers))

(defun org-timeblock-list-toggle-timeblock ()
  "Toggle the display of the window with `org-timeblock-mode'."
  (interactive)
  (if-let ((window (get-buffer-window org-timeblock-buffer)))
      (delete-window window)
    (org-timeblock-show-timeblocks)
    (org-timeblock-redraw-timeblocks)))

(defun org-timeblock-toggle-timeblock-list ()
  "Toggle the display of the window with `org-timeblock-list-mode'."
  (interactive)
  (if-let ((window (get-buffer-window org-timeblock-list-buffer)))
      (delete-window window)
    (org-timeblock-show-timeblock-list))
  (org-timeblock-redraw-buffers))

(defun org-timeblock-redraw-buffers ()
  "Redraw `org-timeblock-list-mode' and `org-timeblock-timeline-mode' buffers."
  ;; org-timeblock-list-mode and org-timeblock-mode
  (interactive)
  (with-current-buffer (get-buffer-create org-timeblock-list-buffer)
    (let ((inhibit-read-only t)
	  (entries (org-timeblock-get-entries
		    (car org-timeblock-daterange)
		    (cdr org-timeblock-daterange)))
	  (dates (org-timeblock-get-dates
		  (car org-timeblock-daterange)
		  (cdr org-timeblock-daterange))))
      (erase-buffer)
      (org-timeblock-list-mode)
      (dolist (date dates)
	(let ((entries
	       (seq-filter
		(lambda (x)
		  (org-timeblock--timestamp-relevant-p
		   (org-timeblock-get-ts-prop x) date))
		entries)))
	  (insert
	   (propertize
	    (concat
	     (org-timeblock-format-time "[%Y-%m-%d %a]" date)
	     (and (org-timeblock-date= date (decode-time)) " Today"))
	    'face 'org-timeblock-list-header))
	  (insert "\n")
	  (dolist (entry entries)
	    (let ((colors (org-timeblock-get-colors
			   (get-text-property 0 'tags entry))))
	      (insert (propertize
		       (concat entry "\n")
		       'face
		       `(:extend t ,@(and (car colors)
					  (list :background (car colors)))
				 ,@(and (cadr colors)
					(list :foreground (cadr colors))))))))
	  (goto-char (point-max))))
      (goto-char (point-min))
      (when (get-buffer-window org-timeblock-buffer)
	(org-timeblock-redraw-timeblocks)))))

(defun org-timeblock-write (file)
  "Write the current *org-timeblock* buffer to FILE.

Depending on the extension of the file name, PNG image (.png),
SVG image (.svg), PDF (.pdf) is produced."
  (interactive "FWrite timeblocks to [PDF|SVG|PNG] file : \n")
  (unless (eq major-mode 'org-timeblock-mode)
    (user-error "Not in org-timeblock buffer"))
  (if (or (not (file-writable-p file))
	  (and (file-exists-p file)
	       (if (called-interactively-p 'any)
		   (not (y-or-n-p
			 (format "Overwrite existing file %s? " file))))))
      (user-error "Cannot write agenda to file %s" file))
  (let ((file (expand-file-name file)))
    (with-temp-buffer
      (let* ((dates (org-timeblock-get-dates
		     (car org-timeblock-daterange)
		     (cdr org-timeblock-daterange)))
	     (max-length (/ (+ (/ org-timeblock-svg-width (default-font-width)))
			    (length dates)))
	     (date-format
	      (pcase max-length
		((pred (< 15)) "[%Y-%m-%d %a]")
		((pred (< 11)) "[%Y-%m-%d]")
		((pred (< 6)) "[%m-%d]")
		((pred (< 3)) "[%d]"))))
	(dom-add-child-before
	 org-timeblock-svg
	 (dom-node
	  'rect
	  (list
	   (cons 'x 0)
	   (cons 'y 0)
	   (cons 'width org-timeblock-svg-width)
	   (cons 'height org-timeblock-svg-height)
	   (cons 'fill (face-attribute 'default :background)))))
	(dotimes (iter (length dates))
	  (svg-text
	   org-timeblock-svg (org-timeblock-format-time date-format (nth iter dates))
	   :y org-timeblock-svg-height
	   :x (+ 5 (* (/ org-timeblock-svg-width (length dates)) iter))
	   :fill (face-attribute 'default :foreground)))
	(svg-print org-timeblock-svg))
      (pcase (file-name-extension file)
	((or "pdf" "png")
	 (unless (executable-find "inkscape")
	   (user-error "Inkscape executable not found"))
	 (call-process-region (point-min) (point-max)
			      "inkscape" nil nil nil "--pipe"
			      (concat "--export-filename=" file)))
	((or "svg" `nil) (write-region nil nil file)))
      (org-timeblock-redraw-timeblocks))))

;;;; Footer

(provide 'org-timeblock)

;; Local Variables:
;;   outline-regexp: "\\(;\\{3,\\} \\)"
;; End:

;;; org-timeblock.el ends here
