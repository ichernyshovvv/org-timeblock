;;; org-timeblock.el --- Schedule your day visually, using timeblocking technique inside Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (org-ql "0.7") (org "9.6.7") svg)
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
(require 'org-ql)

;;;; Faces

(defface otl-header '((t (:inherit org-agenda-structure)))
  "Face used in agenda for `org-super-agenda' group name header.")

;;;; Custom Variables

(defgroup org-timeblock nil
  "Customization for `org-timeblock'."
  :group 'org
  :link '(url-link "https://github.com/ichernyshovvv/org-timeblock"))

(defcustom ot-inbox-file
  (expand-file-name "inbox.org" org-directory)
  "Orgmode file path used to create a new task via
`org-timeblock-new-task' command"
  :group 'org-timeblock
  :type 'file)

(defcustom ot-view-options t
  "Options that are used to decide which part of visual schedule must be hidden."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Hide passed and free time" t)
	  (const :tag "Do not hide anything" nil)
	  (const :tag "Hide space of free, passed and not passed time" hide-all)))

(defcustom ot-todo-commands
  '(("TODO" . "1")
    ("DONE" . "5"))
  "Alist where each cons is an orgmode todo state and a
key. For each car a command is created (\"TODO\" ->
`org-timeblock-todo') and then bound inside
`org-timeblock-list-mode-map' and `org-timeblock-mode-map'
respectively to a key in cdr.

cdr (a key) should be a string in the format returned by commands such
as `describe-key'."
  :group 'org-timeblock
  :type 'alist)

(defcustom ot-current-time-indicator t
  "Whether to show current time indicator in the `org-timeblock-list' buffer."
  :group 'org-timeblock
  :type 'boolean)

(defcustom ot-color-tag-alist
  nil
  "List of lists where each list is of the form `(\"tagname\"
\"background color\" \"foreground color\")'. Colors are set in hex
format. Example:

`((\"tag1\" \"#f3d000\" \"#000000\")
  (\"tag2\" \"#ff8f88\" \"#000000\"))'"
  :type 'alist
  :group 'org-timeblock)

;;;; Variables

(defvar ot-sel-block-color-light "#f3d000")

(defvar ot-sel-block-color-dark "#3f1651")

(defvar ot-sel-block-color ot-sel-block-color-light)

(defvar ot-background-color (face-attribute 'default :background))

(defvar ot-colors nil "")

(defvar ot-markers nil)

(defvar otl-entries-pos nil
  "Nested alist of saved positions of the entries for each date that
a user have previously opened.")

(defvar ot-prev-selected-block-color nil)

(defvar ot-sort-function #'ot-order<)

(defvar ot-date nil
  "The date that is used to get and display schedule data.")

(defvar ot-buffer "*org-timeblock*" "The name of the buffer displaying visual schedule.")

(defvar otl-buffer "*org-timeblock-list*"
  "The name of the buffer displaying the list of tasks and events.")

(defvar otl-sort-line-position nil
  "The line position of the sort line which is displayed in
`org-timeblock-list-mode', when orgmode tasks are manually
placed. Used as a simple separator to distinguish manually sorted
tasks and those tasks that have not been sorted yet.")

(defvar otl-order-cache-file (expand-file-name ".cache/org-timeblock.el" user-emacs-directory))

;;;; Keymaps

(defvar org-timeblock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'ot-redraw-buffers)
    (define-key map (kbd "s") 'ot-schedule)
    (define-key map (kbd "d") 'ot-set-duration)
    (define-key map (kbd "v") 'ot-switch-view)
    (define-key map (kbd "<down>") 'ot-forward-block)
    (define-key map (kbd "<up>") 'ot-backward-block)
    (define-key map (kbd "<tab>") 'ot-goto-other-window-svg)
    (define-key map [mouse-1] 'ot-select-block-under-mouse)
    (define-key map (kbd "C-<up>") 'ot-backward-day)
    (define-key map (kbd "C-<down>") 'ot-forward-day)
    (define-key map (kbd "t") 'ot-toggle-timeblock-list)
    (define-key map (kbd "j") 'ot-jump-to-day)
    (define-key map (kbd "+") 'ot-new-task)
    map))

(defvar org-timeblock-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'otl-schedule)
    (define-key map (kbd "r") 'otl-bulk-reschedule)
    (define-key map (kbd "<tab>") 'ot-goto-other-window)
    (define-key map (kbd "q") 'ot-quit)
    (define-key map (kbd "C-s") 'otl-save)
    (define-key map (kbd "M-<down>") 'otl-drag-line-forward)
    (define-key map (kbd "M-<up>") 'otl-drag-line-backward)
    (define-key map (kbd "d") 'otl-set-duration)
    (define-key map (kbd "g") 'ot-redraw-buffers)
    (define-key map (kbd "t") 'otl-toggle-timeblock)
    (define-key map (kbd "<down>") 'otl-next-line)
    (define-key map (kbd "<up>") 'otl-previous-line)
    (define-key map (kbd "S") 'otl-toggle-sort-function)
    (define-key map (kbd "5") 'otl-done)
    (define-key map (kbd "1") 'otl-todo)
    (define-key map (kbd "+") 'ot-new-task)
    (define-key map (kbd "v") 'ot-switch-view)
    (define-key map (kbd "C-<up>") 'ot-backward-day)
    (define-key map (kbd "C-<down>") 'ot-forward-day)
    (define-key map (kbd "j") 'ot-jump-to-day)
    map))

;; Generate todo commands and bind them to a corresponding key
(dolist (elem ot-todo-commands)
  (let((command-name (intern (concat "org-timeblock-" (downcase (car elem))))))
    (defalias command-name
      (lambda()
	(interactive)
	(when-let((m (pcase major-mode
		       (`org-timeblock-list-mode
			(get-text-property (line-beginning-position) 'marker))
		       (`org-timeblock-mode
			(ot-selected-block-marker)))))
	  (ot--set-todo m (car elem))
	  (ot-redraw-buffers))))
    (define-key org-timeblock-list-mode-map (kbd (cdr elem)) command-name)
    (define-key org-timeblock-mode-map (kbd (cdr elem)) command-name)))

;;;; Modes

(define-derived-mode org-timeblock-mode image-mode "Org-Timeblock" :interactive nil
  (setq image-auto-resize nil
	header-line-format
	(format-time-string "[%Y-%m-%d %a]" ot-date)
	buffer-read-only t))

(define-derived-mode org-timeblock-list-mode special-mode "Org-Timeblock-List" :interactive nil
  (setq truncate-lines t))

;;;; Functions

(defun ot-mouse-pixel-pos()
  (when-let ((mouse-pos (cdr (mouse-pixel-position)))
	     (window (get-buffer-window ot-buffer))
	     (pos (window-edges window t nil t)))
    (when (and (> (- (car mouse-pos) (car pos)) 0)
	       (> (- (cdr mouse-pos) (cadr pos)) 0))
      (cons (- (car mouse-pos) (car pos))
	    (- (cdr mouse-pos) (cadr pos))))))

(defun ot-selected-block-marker()
  ""
  (goto-char (point-min))
  (and
   (re-search-forward (format " fill=\"%s\".*? id=\"\\(.+?\\)\"" ot-sel-block-color) nil t)
   (let((id (match-string-no-properties 1)))
     (cdr
      (seq-find
       (lambda(x)
	 (string=
	  (car x)
	  id))
       ot-markers)))))

(defun ot-selected-block-id()
  ""
  (goto-char (point-min))
  (and
   (re-search-forward (format " fill=\"%s\".*? id=\"\\(.+?\\)\"" ot-sel-block-color) nil t)
   (match-string-no-properties 1)))

(cl-macrolet ((on (accessor op lhs rhs)
                `(,op (,accessor ,lhs)
                      (,accessor ,rhs))))
  (defun ot-ts-date= (a b)
    (cond
     ((and (null a)
           (null b)))
     ((and a b)
      (let ((a (decode-time a))
            (b (decode-time b)))
        (and (on decoded-time-year  = a b)
             (on decoded-time-month = a b)
             (on decoded-time-day   = a b))))))

  (defun ot-ts-date< (a b)
    (cond
     ;; nil is less than non-nil
     ((null b) nil)
     ((null a) t)
     (t
      (let ((a (decode-time a))
            (b (decode-time b)))
        (and (on decoded-time-year  < a b)
             (on decoded-time-month < a b)
             (on decoded-time-day   < a b))))))

  (defun ot-order< (a b)
    (on (lambda (item)
          (or (get-text-property 0 'order item) 1))
        < a b))

  (defun ot-sched-or-event< (a b)
    (on (lambda (item)
          (ot--timestamp-encode
           (or (get-text-property 0 'sched item)
               (get-text-property 0 'event item))))
        time-less-p a b)))

(defun ot-select-block-for-current-entry()
  (when-let(((not
	      (ot--daterangep
	       (or (get-text-property (line-beginning-position) 'event)
		   (get-text-property (line-beginning-position) 'sched)))))
	    (id (get-text-property (line-beginning-position) 'id))
	    (inhibit-read-only t)
            ((get-buffer-window ot-buffer)))
    (with-current-buffer ot-buffer
      (goto-char (point-min))
      (when (re-search-forward (format " fill=\"\\(%s\\)\"" ot-sel-block-color) nil t)
	(replace-match ot-prev-selected-block-color nil nil nil 1)
	(goto-char (point-min)))
      (when (and (search-forward (format " id=\"%s\"" id) nil t)
		 (search-backward "<rect " nil t)
		 (re-search-forward " fill=\"\\([^\"]+?\\)\"" nil t))
	(setq ot-prev-selected-block-color (match-string 1))
	(replace-match ot-sel-block-color nil nil nil 1)
	(org-timeblock-mode)))))

(defun ot-tss-are-in-intersection-p(oe-ts1 oe-ts2)
  "`TS1-START',`TS2-START' - org-element timestamp objects"
  (when-let((ts1-start (ot--timestamp-encode oe-ts1))
	    (ts2-start (ot--timestamp-encode oe-ts2)))
    (let((ts1-end (ot--timestamp-encode oe-ts1 t))
	 (ts2-end (ot--timestamp-encode oe-ts2 t)))
      (or
       (time-equal-p ts2-start ts1-start)
       (and
	ts2-end
	(time-less-p ts2-start ts1-start)
	(time-less-p ts1-start ts2-end))
       (and
	ts1-end
	(time-less-p ts1-start ts2-start)
	(time-less-p ts2-start ts1-end))))))

(defun ot--parse-hex-color (hex)
  "Convert a HEX color code to a RGB list.
i.e.
#99ccff => (153 204 255)
#33a    => (51 51 170)"
  (let (result)
    (when (string-match
           "^\\s-*\\#\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\s-*$"
           hex)
      (let ((m1 (match-string 1 hex))
            (m2 (match-string 2 hex))
            (m3 (match-string 3 hex)))
        (setq result (list (read (format "#x%s%s" m1 m1))
                           (read (format "#x%s%s" m2 m2))
                           (read (format "#x%s%s" m3 m3))))))
    (when (string-match
           "^\\s-*\\#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\s-*$"
           hex)
      (setq result (list (read (format "#x%s" (match-string 1 hex)))
                         (read (format "#x%s" (match-string 2 hex)))
                         (read (format "#x%s" (match-string 3 hex))))))
    result))

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

(defun ot-redraw-timeblocks()
  (with-current-buffer (get-buffer-create ot-buffer)
    (let((inhibit-read-only t))
      (erase-buffer)
      (if-let((entries (ot-get-entries :sort-func #'ot-sched-or-event< :exclude-dateranges t :with-time t)))
	  (let* ((window (get-buffer-window ot-buffer))
		 (window-height (window-body-height window t))
		 (window-width (window-body-width window t))
		 (timeline-left-padding 25)
		 (entry-max-width (- window-width timeline-left-padding))
		 (svg-obj (svg-create window-width window-height))
		 (min-hour
		  (if-let((ot-view-options)
			  (hours
			   (remove
			    nil
			    (append
			     (list (unless (eq ot-view-options 'hide-all)
				     (decoded-time-hour (decode-time (current-time)))))
			     (mapcar (lambda(entry)
				       (if (ot-ts-date<
					    (ot--timestamp-encode
					     (or (get-text-property 0 'sched entry)
						 (get-text-property 0 'event entry)))
					    ot-date)
					   0
					 (org-element-property
					  :hour-start
					  (or (get-text-property 0 'sched entry)
					      (get-text-property 0 'event entry)))))
				     entries)))))
		      (apply #'min hours)
		    0))
		 (scale (/ (float window-height) (float (* (- 24 min-hour) 60))))
		 (cur-time (decode-time (current-time)))
		 (cur-time-indicator
		  (* scale
		     (-
		      (+ (* (decoded-time-hour cur-time) 60) (decoded-time-minute cur-time)) ;; minutes
		      (* min-hour 60))))
		 (columns
		  (mapcar
		   (lambda(x)
		     (cons (get-text-property 0 'marker x) 1))
		   entries))
		 placed
		 (bg-rgb-sum (apply #'+ (ot--parse-hex-color ot-background-color)))
		 (get-color
		  (if (string= ot-background-color (face-attribute 'default :background))
		      (lambda(title) (cl-callf (lambda(x) (or x (ot--random-color))) (alist-get title ot-colors nil nil #'equal)))
		    (setq ot-background-color (face-attribute 'default :background))
		    (setq ot-sel-block-color
			  (if (> (setq bg-rgb-sum (apply #'+ (ot--parse-hex-color ot-background-color))) 550)
			      ot-sel-block-color-light
			    ot-sel-block-color-dark))
		    (lambda(title) (setf (alist-get title ot-colors nil nil #'equal) (ot--random-color)))))
		 (hour-lines-color
		  (if (> bg-rgb-sum 550) "#7b435c" "#cdcdcd")))
	    (dolist (entry entries)
	      (let ((m (get-text-property 0 'marker entry)))
		(push entry placed)
		(setcdr (assq m columns)
			(catch 'found-column
			  (let((k 1))
			    (while t
			      (catch 'next-column
				(dolist(el (seq-filter
					    (lambda(x)
					      (eq (cdr (assq (get-text-property 0 'marker x) columns)) k))
					    placed))
				  (and (/= (get-text-property 0 'marker el) m)
				       (ot-tss-are-in-intersection-p
					(or (get-text-property 0 'sched entry)
					    (get-text-property 0 'event entry))
					(or (get-text-property 0 'sched el)
					    (get-text-property 0 'event el)))
				       (cl-incf k)
				       (throw 'next-column t)))
				(throw 'found-column k))))))))
	    (svg-rectangle svg-obj 0 0 window-width window-height :fill (face-attribute 'default :background))
	    ;; Drawing all the entries inside the timeline
	    (dolist (entry entries)
	      (let* ((length (1+ (length (seq-uniq
					  (mapcar
					   ;; get columns for those entries
					   (lambda(x)
					     (cdr (assq (get-text-property 0 'marker x) columns)))
					   ;; find those with which current entry is in intersection
					   (seq-filter
					    (lambda(x)
					      (unless (equal (get-text-property 0 'marker entry) (get-text-property 0 'marker x))
						(ot-tss-are-in-intersection-p
						 (or (get-text-property 0 'sched entry)
						     (get-text-property 0 'event entry))
						 (or (get-text-property 0 'sched x)
						     (get-text-property 0 'event x)))))
					    entries))
					  #'eq))))
		     (x (+ timeline-left-padding (round (* (1- (cdr (assq (get-text-property 0 'marker entry) columns))) (/ entry-max-width length)))))
		     (entry-width (round (/ entry-max-width length)))
		     (timestamp (or (get-text-property 0 'sched entry)
				    (get-text-property 0 'event entry)))
		     (start-ts (ot--timestamp-encode timestamp))
		     (end-ts (ot--timestamp-encode timestamp t))
		     (duration (if (and start-ts end-ts)
				   (round
				    (* (/ (time-convert
					   (time-subtract
					    (if (ot-ts-date< ot-date end-ts)
						(let((decoded (decode-time ot-date)))
						  (setf
						   (decoded-time-hour decoded) 23
						   (decoded-time-minute decoded) 59
						   (decoded-time-second decoded) 0)
						  (encode-time decoded))
					      end-ts)
					    (if (ot-ts-date< start-ts ot-date)
						(let((decoded (decode-time ot-date)))
						  (setf
						   (decoded-time-hour decoded) 0
						   (decoded-time-minute decoded) 1
						   (decoded-time-second decoded) 0)
						  (encode-time decoded))
					      start-ts))
					   'integer)
					  60)
				       scale))
				 5))
		     (title (get-text-property 0 'title entry))
		     ;; Splitting the title of an entry
		     (heading-list
		      (append
		       (if (> (* (length title) (default-font-width)) entry-width)
			   (seq-take
			    (seq-split title (1- (/ entry-width (default-font-width))))
			    (let((lines-count (round (/ duration (default-font-height)))))
			      (if (= 0 lines-count) 1 lines-count)))
			 `(,title))
		       (cond
			((and
			  (ot-ts-date< ot-date end-ts)
			  (ot-ts-date< start-ts ot-date))
			 '("↕️"))
			((ot-ts-date< ot-date end-ts)
			 '("⬇️"))
			((ot-ts-date< start-ts ot-date)
			 '("⬆️"))
			(t nil))))
		     (hstart (if (ot-ts-date< start-ts ot-date)
				 0
			       (org-element-property :hour-start timestamp)))
		     (minutes (+ (* hstart 60) (if (ot-ts-date< start-ts ot-date)
						   0
						 (org-element-property :minute-start timestamp))))
		     (y (round (* scale (- minutes (* min-hour 60))))))
		(push (cons (get-text-property 0 'id entry) (get-text-property 0 'marker entry)) ot-markers)
		;; Appending generated rectangle for current entry
		(svg--append
		 svg-obj
		 (dom-node
		  'rect
		  `((height . ,duration)
		    (width . ,entry-width)
		    (stroke . ,(if (get-text-property 0 'event entry) "#5b0103" "#cdcdcd"))
		    (stroke-width . ,(if (get-text-property 0 'event entry) 2 1))
		    (opacity . "0.95")
		    (y . ,y)
		    (x . ,x)
		    (fill . ,(or (car (ot-get-colors (get-text-property 0 'tags entry)))
				 (funcall get-color title)))
		    (id . ,(get-text-property 0 'id entry)))))
		;; Setting the title of current entry
		(svg--append
		 svg-obj
		 (apply
		  'dom-node
		  'g
		  `((transform . ,(format "translate(%d,%d)" x y)))
		  (let ((y-start -5) result)
		    (dolist (heading-part heading-list (nreverse result))
		      (push
		       (dom-node
			'text
			`((x . 0)
			  (y . ,(cl-incf y-start (default-font-height)))
			  (fill . ,(face-attribute 'default :foreground))
			  (font-size . 14))
			(svg--encode-text heading-part))
		       result)))))))
	    ;; Drawing hour lines
	    (let ((iter (if (> min-hour 0) (1- min-hour) 0)) y)
	      (while (< (cl-incf iter) 24)
		(setq y (round (* scale (- iter min-hour) 60)))
		(svg-line
		 svg-obj
		 timeline-left-padding
		 y
		 window-width
		 y
		 :stroke-dasharray "4"
		 :stroke hour-lines-color)
		(svg-text
		 svg-obj (format "%d" iter)
		 :y (+ y 5)
		 :x 5
		 :fill (face-attribute 'default :foreground))))
	    ;; Drawing current time indicator
	    (when (and ot-current-time-indicator
		       (ot-ts-date= ot-date (current-time)))
	      (svg-polygon
	       svg-obj
	       (list
		(cons (- entry-max-width 5) cur-time-indicator)
		(cons (+ entry-max-width 25) (- cur-time-indicator 5))
		(cons (+ entry-max-width 25) (+ cur-time-indicator 5)))
	       :fill-color "red"))
	    (svg-print svg-obj))
	(let*((window (get-buffer-window ot-buffer))
	      (window-height (window-body-height window t))
	      (window-width (window-body-width window t))
	      (message "No entries found for this date.")
	      (svg-obj (svg-create window-width window-height)))
	  (svg-text
	   svg-obj message
	   :y (/ window-height 2)
	   :x (- (/ window-width 2) (/ (* (default-font-width) (length message)) 2))
	   :fill (face-attribute 'default :foreground))
	  (svg-print svg-obj)))
      (org-timeblock-mode))))

(defun ot-show-timeblocks()
  (switch-to-buffer-other-window ot-buffer)
  (other-window 1))

(defun ot-show-timeblock-list()
  (switch-to-buffer-other-window otl-buffer)
  (other-window 1))

(defun otl-toggle-sort-function()
  (interactive)
  (setq ot-sort-function
	(if (eq ot-sort-function #'ot-order<)
            #'ot-sched-or-event<
	  #'ot-order<))
  (ot-redraw-buffers))

(defun otl-save()
  "Save orgmode files, sorting line and tasks positions."
  (interactive)
  (unless (eq major-mode 'org-timeblock-list-mode)
    (user-error "Not in org-timeblock buffer"))
  (let((count 0)
       (inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when-let((m (get-text-property (point) 'marker))
		  (id (ot-construct-id m)))
	  (setf (alist-get id (alist-get (format-time-string "%Y-%m-%d" ot-date) otl-entries-pos nil nil #'equal) nil nil #'equal)
		(cl-incf count)))
	(when (get-text-property (point) 'sort-ind)
	  (setf (alist-get (format-time-string "%Y-%m-%d" ot-date) otl-sort-line-position nil nil 'equal)
		(cl-incf count)))
	(forward-line))
      (with-temp-file otl-order-cache-file
	(insert (format "(setq org-timeblock-entries-positions-alist '%S)" otl-entries-pos)
		(format "\n(setq org-timeblock/sort-line-position '%S)" otl-sort-line-position)))))
  (org-save-all-org-buffers))

(defun ot-quit()
  (interactive)
  ;; (ot-reset-markers)
  (quit-window t))

(defun ot--schedule-time(marker)
  (let ((timerangep
	 (eq
	  (read-char-from-minibuffer
	   "Schedule: [s]tart timestamp only, time[r]ange"
	   '(?s ?r))
	  ?r))
	(new-start-ts (ot-read-ts ot-date "START-TIME: "))
	new-end-ts start-ts end-ts duration timestamp)
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (org-fold-show-context 'agenda)
      (setq timestamp (org-element-property :scheduled (org-element-at-point)))
      (unless timestamp
	(user-error "Scheduled property not found"))
      (setq start-ts (ot--timestamp-encode timestamp))
      (setq end-ts (ot--timestamp-encode timestamp t))
      (setq duration (when (and start-ts end-ts) (/ (time-convert (time-subtract end-ts start-ts) 'integer) 60)))
      (setq new-end-ts (if timerangep
			   (ot-read-ts ot-date "END-TIME: ")
			 (when duration (time-add new-start-ts (* 60 duration)))))
      (ot--schedule new-start-ts new-end-ts)
      (org-element-property :scheduled (org-element-at-point)))))

(defun ot--daterangep (timestamp)
  "Return t, if org timestamp object is a daterange with no time."
  (when-let((day-end (org-element-property :day-end timestamp))
	    (month-end (org-element-property :month-end timestamp))
	    (year-end (org-element-property :year-end timestamp)))
    (and
     (or
      (/= (org-element-property :day-start timestamp) day-end)
      (/= (org-element-property :month-start timestamp) month-end)
      (/= (org-element-property :year-start timestamp) year-end))
     (null (org-element-property :hour-start timestamp))
     (null (org-element-property :hour-end timestamp)))))

(defun ot--construct-entry-prefix(timestamp &optional eventp)
  "Construct an entry prefix for *org-timeblock* buffer."
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

(defun ot-read-ts(ts &optional prompt)
  "Change time for TS interactively and return the changed ts object."
  (let((decoded (decode-time ts)))
    (cl-loop
     as
     res = (let(time)
	     (while (< (length time) 4)
	       (setq time (concat time (char-to-string (read-char-exclusive (concat (or prompt "TIME:") time))))))
	     (when (string-match-p "[[:digit:]]\\{4\\}" time)
	       (setf
		(decoded-time-hour decoded)
		(string-to-number (substring time 0 2))
		(decoded-time-minute decoded)
		(string-to-number (substring time 2 4)))
	       (encode-time decoded)))
     until res
     finally return res)))

(defun ot-construct-id(&optional marker)
  (let* ((element (org-element-at-point marker))
	 (title (org-element-property :title element)))
    (md5
     (concat
      (if (stringp title) title (car title)) ;; TODO
      (or
       (org-element-property
	:raw-value
	(org-element-property :scheduled element))
       (org-entry-get marker "TIMESTAMP")))
     nil nil 'utf-8)))

(defun ot-get-event-timestamp()
  "Return an org-element timestamp object of an event at point."
  (when-let((ts (org-entry-get nil "TIMESTAMP")))
    (with-temp-buffer
      (insert ts)
      (goto-char (point-min))
      (org-element-timestamp-parser))))

(cl-defun ot-get-entries (&key sort-func exclude-dateranges with-time)
  "Get entries relevant to `org-timeblock-date'."
  (when-let ((files (org-agenda-files)))
    (sort
     (mapcar
      (lambda(entry)
	;; setting 'order property not inside of `org-ql-select' call
	;; because when buffers have not been changed, org-ql uses
	;; cached results and therefore does not update 'order property,
	;; which is the only property that's not stored in org buffers
	(put-text-property
	 0 (length entry)
	 'order (alist-get
		 (get-text-property 0 'id entry)
		 (alist-get
		  (format-time-string "%Y-%m-%d" ot-date)
		  otl-entries-pos
		  nil nil #'equal)
		 nil nil #'equal)
	 entry)
	entry)
      (when files
	(append
	 (org-ql-select
	   files
	   `(and (active-timestamp
		  :on ,(format-time-string "%Y-%m-%d" ot-date)
		  :exclude-dateranges ,exclude-dateranges
		  :with-time ,with-time))
	   :action
	   (lambda()
	     (let ((id (ot-construct-id))
		   (event (ot-get-event-timestamp))
		   (title (org-get-heading t nil t t)))
	       (propertize
		(concat
		 (ot--construct-entry-prefix event t)
		 title)
		'tags (mapcar 'substring-no-properties (org-element-property :tags (org-element-at-point)))
		'marker (copy-marker (point) t)
		'title title
		'id id
		'event event))))
	 (org-ql-select
	   files
	   `(and (not (done))
		 (ot-org-ql-scheduled
		  :on ,(format-time-string "%Y-%m-%d" ot-date)
		  :exclude-dateranges ,exclude-dateranges
		  :with-time ,with-time))
	   :action
	   (lambda()
	     (let* ((elem (org-element-at-point))
		    (sched (org-element-property :scheduled elem))
		    (id (ot-construct-id))
		    (title (org-get-heading t nil t t)))
	       (propertize
		(concat
		 (ot--construct-entry-prefix sched)
		 title)
		'tags (mapcar 'substring-no-properties (org-element-property :tags (org-element-at-point)))
		'marker (copy-marker (point) t)
		'title title
		'id id
		'sched sched)))))))
     (or sort-func ot-sort-function))))

(defun ot-get-colors(tags)
  (catch 'found
    (dolist (tag tags)
      (when-let ((colors (cdr (assoc tag ot-color-tag-alist))))
	(throw 'found colors)))))

(defun ot--timestamp-encode(ts &optional end)
  (let((year-start (org-element-property :year-start ts))
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
	      (encode-time (list 0 (or minute-end 0) (or hour-end 0) day-end month-end year-end 0 nil (car (current-time-zone)))))))
      (encode-time (list 0 (or minute-start 0) (or hour-start 0) day-start month-start year-start 0 nil (car (current-time-zone)))))))

(defun ot--schedule(start-ts &optional end-ts marker)
  (save-window-excursion
    (save-excursion
      (save-restriction
	(let*((marker-valid-p
	       (and marker (marker-buffer marker)
		    (buffer-live-p (marker-buffer marker))))
	      (point (or (when marker-valid-p marker) (point)))
	      (buffer (or (when marker-valid-p (marker-buffer marker)) (current-buffer)))
	      repeat-string warning-string
	      timestamp)
	  (with-current-buffer buffer
	    (widen)
	    (goto-char point)
	    (setq
	     timestamp (org-element-property :scheduled (org-element-at-point))
	     repeat-string (org-get-repeat)
	     warning-string
	     (concat
	      (pcase (org-element-property :warning-type timestamp)
		(`first "--") (`all "-"))
	      (let ((val (org-element-property :warning-value timestamp)))
		(and val (number-to-string val)))
	      (pcase (org-element-property :warning-unit timestamp)
		(`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))
	    (org-schedule nil (ot-ts-to-org-timerange start-ts end-ts repeat-string warning-string))
	    (cond
	     ((or (not end-ts) (ot-ts-date= start-ts end-ts))
	      (org-schedule nil (ot-ts-to-org-timerange start-ts end-ts repeat-string warning-string)))
	     ((and end-ts (not (ot-ts-date= start-ts end-ts)))
	      (org-back-to-heading t)
	      (save-excursion
		(forward-line)
		(when (re-search-forward "^SCHEDULED:.+>\n" (1+ (line-end-position)) t)
		  (replace-match "")))
	      (org-schedule nil (ot-ts-to-org-timerange start-ts nil repeat-string warning-string))
	      (forward-line)
	      (when (re-search-forward "^SCHEDULED:.+>" (line-end-position) t)
		(insert "--" (ot-ts-to-org-timerange end-ts nil repeat-string warning-string)))))))))))

(defun ot-delete-event-timestamp()
  "Delete event timestamp for an entry at point and leave the point
where the timestamp was."
  (let((end (save-excursion (outline-next-heading) (point))))
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
  "Create an Org timestamp range from START-D/T, END-D/T."
  (when-let((start-date (format-time-string "%Y-%m-%d %a" ts-start)))
    (let ((start-time
	   (let((res (format-time-string "%R" ts-start)))
	     (and (not (string= res "00:00")) res)))
	  (end-date (and ts-end (format-time-string "%Y-%m-%d %a" ts-end)))
	  (end-time (and ts-end
			 (let((res (format-time-string "%R" ts-end)))
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

(defun ot--update-prefix(timestamp &optional eventp)
  (let((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (text-property-search-forward 'prefix nil)
      (delete-region (line-beginning-position) (point))
      (insert (apply
	       'propertize
	       (ot--construct-entry-prefix timestamp eventp)
	       (text-properties-at (point)))))))

(defun ot--duration(marker)
  (let (new-end-ts timestamp start-ts duration)
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (org-fold-show-context 'agenda)
      (setq timestamp (or (org-element-property :scheduled (org-element-at-point))
			  (ot-get-event-timestamp)))
      (unless (and (org-element-property :hour-start timestamp)
		   (org-element-property :minute-start timestamp))
	(user-error "No scheduled time specified for this entry"))
      (setq start-ts (ot--timestamp-encode timestamp))
      (while (not
	      (setq duration
		    (let((time ""))
		      (catch 'dur
			(while t
			  (when-let((char (read-char-exclusive (concat "DURATION:" time))))
			    (cond
			     ((eq ?q char)
			      (setq time "")
			      (throw 'dur t))
			     ((eq ? char)
			      (let(hours minutes)
				(if (not (string-match "^\\(?:\\([0-9]+\\)h\\)?\\([0-9]+\\)?" time))
				    (setq time "")
				  (setq hours (and (match-string 1 time) (string-to-number (match-string 1 time))))
				  (setq minutes (and (match-string 2 time) (string-to-number (match-string 2 time))))
				  (unless (= (length time) 0)
				    (setq time (number-to-string (+ (or minutes 0) (if hours (* hours 60) 0))))
				    (throw 'dur t)))))
			     ((eq ?m char)
			      (let(hours minutes)
				(if (not (string-match "^\\(?:\\([0-9]+\\)h\\)?\\([0-9]+\\)" time))
				    (setq time "")
				  (setq hours (and (match-string 1 time) (string-to-number (match-string 1 time))))
				  (setq minutes (and (match-string 2 time) (string-to-number (match-string 2 time))))
				  (setq time (number-to-string (+ minutes (if hours (* hours 60) 0))))
				  (throw 'dur t))))
			     ((and (< char 58) (> char 47))
			      (setq time (concat time (char-to-string char))))
			     ((eq ?h char)
			      (unless (= 0 (length time))
				(setq time (concat time (char-to-string char)))))))))
		      (and time (string-to-number time))))))
      (setq new-end-ts (when duration (time-add start-ts (* 60 duration))))
      (if (org-element-property :scheduled (org-element-at-point))
	  (ot--schedule start-ts new-end-ts)
	(save-excursion
	  (ot-delete-event-timestamp)
	  (insert
	   (ot-ts-to-org-timerange start-ts new-end-ts))))
      (or
       (org-element-property :scheduled (org-element-at-point))
       (ot-get-event-timestamp)))))

(defun otl-drag-line-backward()
  "Drag an agenda line backward by ARG lines."
  (interactive)
  (otl-drag-line-forward t))

(defun ot--set-todo(m todo)
  (when(and m todo)
    (with-current-buffer (marker-buffer m)
      (goto-char m)
      (org-fold-show-context 'agenda)
      (org-todo todo))))

(defun otl-drag-line-forward (&optional backward)
  "Drag an agenda line forward by ARG lines.
When the optional argument `backward' is non-nil, move backward."
  (interactive)
  (unless (eq ot-sort-function #'ot-order<)
    (user-error "Can't drag lines if entries aren't displayed
and sorted by `SORTING-PROPERTY' property."))
  (unless (or (get-text-property (point) 'marker)
	      (get-text-property (point) 'sort-ind))
    (user-error "Can not move this line"))
  (if (or (and backward (= (line-number-at-pos) 2))
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
(defun org-timeblock-list()
  (interactive)
  (switch-to-buffer otl-buffer)
  (setq ot-date (current-time))
  (unless (file-exists-p otl-order-cache-file)
    (make-directory (file-name-directory otl-order-cache-file) t)
    (with-temp-file otl-order-cache-file))
  (load (expand-file-name otl-order-cache-file) nil t t)
  (ot-redraw-buffers))

;;;###autoload
(defun org-timeblock()
  (interactive)
  (switch-to-buffer ot-buffer)
  (setq ot-date (current-time))
  (unless (file-exists-p otl-order-cache-file)
    (make-directory (file-name-directory otl-order-cache-file) t)
    (with-temp-file otl-order-cache-file))
  (load (expand-file-name otl-order-cache-file) nil t t)
  (ot-redraw-buffers))

;;;; Planning commands

(defun ot-new-task()
  "Create a task scheduled to the date in the current view"
  (interactive)
  (let(title)
    (while (or (not title)
	       (string-empty-p title))
      (setq title (read-string "Heading: ")))
    (find-file ot-inbox-file)
    (goto-char (point-max))
    (insert "\n")
    (org-insert-heading nil t t)
    (insert "TODO " title " ")
    (org-schedule nil (format-time-string "%Y-%m-%d" ot-date))
    (save-buffer)
    (setq org-ql-cache (clrhash org-ql-cache))
    (kill-buffer))
  (ot-redraw-buffers))

(defun otl-set-duration()
  "Change schedule property duration for a task at point inside
`org-timeblock-list-mode'"
  (interactive)
  (when (ot--daterangep (get-text-property (line-beginning-position) 'sched))
    (user-error "Can not reschedule entries with daterange timestamp"))
  (when-let ((timestamp (ot--duration (get-text-property (line-beginning-position) 'marker))))
    (ot--update-prefix timestamp (get-text-property (line-beginning-position) 'event))
    (forward-line)
    (when (get-buffer-window ot-buffer)
      (ot-redraw-timeblocks))))

(defun ot-schedule()
  "Reschedule a selected block inside `org-timeblock-mode'"
  (interactive)
  (when-let((marker (ot-selected-block-marker)))
    (ot--schedule-time marker)
    (ot-redraw-buffers)
    (org-timeblock-mode)))

(defun ot-set-duration()
  "Change schedule property duration for a task bound to a selected
block inside `org-timeblock-mode'"
  (interactive)
  (when-let((marker (ot-selected-block-marker)))
    (ot--duration marker)
    (ot-redraw-buffers)
    (org-timeblock-mode)))

(defun otl-schedule()
  (interactive)
  (when (ot--daterangep (get-text-property (line-beginning-position) 'sched))
    (user-error "Can not reschedule entries with daterange timestamp"))
  (when-let((sched (ot--schedule-time (get-text-property (line-beginning-position) 'marker))))
    (ot--update-prefix sched)
    (forward-line)
    (when (get-buffer-window ot-buffer)
      (ot-redraw-timeblocks))))

;;;; Navigation commands

(defun ot-select-block-under-mouse()
  (interactive)
  (cl-macrolet ((get-number (n) `(string-to-number (match-string-no-properties ,n))))
    (when-let((pos (ot-mouse-pixel-pos))
	      (inhibit-read-only t))
      (goto-char (point-min))
      (when (re-search-forward (format " fill=\"\\(%s\\)\"" ot-sel-block-color) nil t)
	(replace-match (or ot-prev-selected-block-color "#ffffff") nil nil nil 1)
	(goto-char (point-min)))
      (when (catch 'found
	      (goto-char (point-min))
	      (search-forward "</rect>" nil t)
	      (while (re-search-forward " height=\"\\(.+?\\)\" width=\"\\(.+?\\)\".*? y=\"\\(.+?\\)\" x=\"\\(.+?\\)\".*? id=\"\\(.+?\\)\"" nil t)
		(and (> (car pos) (get-number 4))
		     (<= (car pos) (+ (get-number 4) (get-number 2)))
		     (<= (cdr pos) (+ (get-number 3) (get-number 1)))
		     (> (cdr pos) (get-number 3))
		     (throw 'found t))))
	(re-search-backward " fill=\"\\(.+?\\)\"" nil t)
	(setq ot-prev-selected-block-color (match-string-no-properties 1))
	(replace-match ot-sel-block-color nil nil nil 1))
      (org-timeblock-mode))))

(defun otl-next-line()
  (interactive)
  (funcall-interactively 'next-line)
  (ot-select-block-for-current-entry))

(defun otl-previous-line()
  (interactive)
  (funcall-interactively 'previous-line)
  (ot-select-block-for-current-entry))

(defun ot-forward-block()
  (interactive)
  (let((inhibit-read-only t))
    (goto-char (point-min))
    (when (re-search-forward (format " fill=\"\\(%s\\)\"" ot-sel-block-color) nil t)
      (replace-match ot-prev-selected-block-color nil nil nil 1)
      (unless (save-excursion
		(search-forward "</rect>" nil t)
		(re-search-forward " id=\".+?\"" nil t))
	(goto-char (point-min))))
    (search-forward "</rect>" nil t)
    (when (re-search-forward "<rect .*?fill=\"\\(.+?\\)\".*? id=\".+?\"" nil t)
      (setq ot-prev-selected-block-color (match-string-no-properties 1))
      (replace-match ot-sel-block-color nil nil nil 1)
      (org-timeblock-mode))))

(defun ot-backward-block()
  (interactive)
  (let((inhibit-read-only t))
    (goto-char (point-max))
    (when (re-search-backward (format " fill=\"\\(%s\\)\"" ot-sel-block-color) nil t)
      (replace-match ot-prev-selected-block-color nil nil nil 1)
      (unless (save-excursion
		(search-backward "</rect>" nil t)
		(re-search-backward " id=\".+?\"" nil t))
	(goto-char (point-max))))
    (search-backward "</rect>" nil t)
    (when (re-search-backward " fill=\"\\(.+?\\)\"*+? id=\".+?\"" nil t)
      (setq ot-prev-selected-block-color (match-string-no-properties 1))
      (replace-match ot-sel-block-color nil nil nil 1)
      (org-timeblock-mode))))

(defun ot-forward-day()
  (interactive)
  (setq ot-date (time-add ot-date (* 24 60 60)))
  (ot-redraw-buffers))

(defun ot-jump-to-day()
  (interactive)
  (when-let((date (org-read-date nil t)))
    (setq ot-date date)
    (ot-redraw-buffers)))

(defun ot-backward-day()
  (interactive)
  (setq ot-date (time-subtract ot-date (* 24 60 60)))
  (ot-redraw-buffers))

;;;; View commands

(defun ot-goto-other-window()
  (interactive)
  (unless (eq major-mode 'org-timeblock-list-mode)
    (user-error "Not in org-timeblock buffer"))
  (let((marker (get-text-property (point) 'marker)))
    (switch-to-buffer-other-window (marker-buffer marker))
    (goto-char (marker-position marker))
    (ignore-errors (org-fold-core--isearch-reveal (point)))
    (recenter)))

(defun ot-goto-other-window-svg()
  (interactive)
  (goto-char (point-min))
  (search-forward "</rect>" nil t)
  (when (re-search-forward (format " fill=\"%s\".*? id=\"\\(.+?\\)\"" ot-sel-block-color) nil t)
    (when-let((inhibit-read-only t)
	      (id (match-string-no-properties 1))
	      (m (cdr (seq-find (lambda(x) (string= (car x) id)) ot-markers))))
      (switch-to-buffer-other-window (marker-buffer m))
      (goto-char (marker-position m))
      (ignore-errors (org-fold-core--isearch-reveal (point)))
      (recenter))))

(defun ot-switch-view()
  (interactive)
  (setq ot-view-options
	(pcase ot-view-options
	  (`nil 'hide-all)
	  (`hide-all 't)
	  (`t 'nil)))
  (ot-redraw-timeblocks))

(defun otl-toggle-timeblock()
  (interactive)
  (if-let ((window (get-buffer-window ot-buffer)))
      (delete-window window)
    (ot-show-timeblocks)
    (ot-redraw-timeblocks)))

(defun ot-toggle-timeblock-list()
  (interactive)
  (if-let ((window (get-buffer-window otl-buffer)))
      (delete-window window)
    (ot-show-timeblock-list))
  (ot-redraw-buffers))

(defun ot-redraw-buffers()
  "Redraw org-timeblock-list-mode and org-timeblock-timeline-mode buffer"
  ;; org-timeblock-list-mode and org-timeblock-mode
  (interactive)
  (with-current-buffer (get-buffer-create otl-buffer)
    (let((inhibit-read-only t)
	 (entries (ot-get-entries)))
      (erase-buffer)
      (org-timeblock-list-mode)
      (setq
       header-line-format
       (substitute-command-keys
	(format "\\<org-timeblock-list-mode-map>Sorted by %s property. Toggle sorting: `\\[org-timeblock-list-toggle-sort-function]'"
		(pcase ot-sort-function
		  ('ot-order< "SORTING-ORDER") ;; TODO replace with backticks?
		  ('ot-sched-or-event< "SCHEDULED")
		  ((pred symbolp) (symbol-name ot-sort-function))))))
      (insert
       (propertize
	(concat
	 (format-time-string "[%Y-%m-%d %a]" ot-date)
	 (and (ot-ts-date= ot-date (current-time)) " Today"))
	'face 'otl-header))
      (insert "\n")
      (dolist (entry entries)
	(let((colors (ot-get-colors (get-text-property 0 'tags entry))))
	  (insert
	   (propertize
	    (concat entry "\n")
	    'face
	    `(:background ,(car colors) :foreground ,(cdr colors))))))
      (goto-char (point-min))
      (when (eq ot-sort-function #'ot-order<)
	(forward-line
	 (alist-get (format-time-string "%Y-%m-%d" ot-date) otl-sort-line-position nil nil #'equal))
	(insert (propertize (format "% 37s" "^^^ SORTED ^^^\n") 'sort-ind t 'face '(:extend t :background "#8b0000" :foreground "#ffffff")))
	(goto-char (point-min)))
      (when (get-buffer-window ot-buffer)
	(ot-redraw-timeblocks)))))

;;;; Predicates

(org-ql-defpred active-timestamp(&key on exclude-dateranges with-time)
  "Search for entries that are `SCHEDULED' to ON
ON format: YYYY-MM-DD"
  :preambles
  ((`(active-timestamp . ,on)
    (list
     :query query ;; run :body on regexp-matched headings after :preambles
     :regexp
     (concat "<.+?>\\(--<.+?>\\)?"))))
  :body
  (when-let((on-ts
	     (when (stringp on)
	       (if-let (((string-match-p "^[0-9]\\{4\\}-[01][0-9]-[0-3][0-9]$" on))
			(time (parse-time-string on))
			((setf (decoded-time-hour time) 0
			       (decoded-time-minute time) 0
			       (decoded-time-second time) 0)))
		   (encode-time time)
		 (user-error "wrong date format = %s" on))))
	    (timestamp (ot-get-event-timestamp))
	    ((not (and exclude-dateranges (ot--daterangep timestamp))))
	    ((or (not with-time) (org-element-property :hour-start timestamp)))
	    (start-ts (ot--timestamp-encode timestamp)))
    (let((end-ts (ot--timestamp-encode timestamp t)))
      (or
       (ot-ts-date= start-ts on-ts)
       (ot-ts-date= end-ts on-ts)
       (and
	end-ts
	(ot-ts-date< start-ts on-ts)
	(ot-ts-date< on-ts end-ts))))))

;; See https://github.com/alphapapa/org-ql/pull/237
;; TODO delete `org-ql' prefix
(org-ql-defpred ot-org-ql-scheduled(&key on exclude-dateranges with-time)
  "Search for entries that have `SCHEDULED' set to ON date
ON format: YYYY-MM-DD"
  :preambles
  ((`(ot-org-ql-scheduled . ,on)
    (list
     :query query ;; run :body on regexp-matched headings after :preambles
     :regexp
     "^SCHEDULED:[ \t]+<.+?>\\(?:--<.+?>\\)?")))
  :body
  (when-let((on-ts
	     (when (stringp on)
	       (if-let (((string-match-p "^[0-9]\\{4\\}-[01][0-9]-[0-3][0-9]$" on))
			(time (parse-time-string on))
			((setf (decoded-time-hour time) 0
			       (decoded-time-minute time) 0
			       (decoded-time-second time) 0)))
		   (encode-time time)
		 (user-error "wrong date format = %s" on))))
	    (sched (org-element-property :scheduled (org-element-at-point)))
	    ((not (and exclude-dateranges (ot--daterangep sched))))
	    ((or (not with-time) (org-element-property :hour-start sched)))
	    (start-ts (ot--timestamp-encode sched)))
    (let((end-ts (ot--timestamp-encode sched t)))
      (or
       (ot-ts-date= start-ts on-ts)
       (ot-ts-date= end-ts on-ts)
       (and
	end-ts
	(ot-ts-date< start-ts on-ts)
	(ot-ts-date< on-ts end-ts))))))

;;;; Footer

(provide 'org-timeblock)

;; Local Variables:
;;   outline-regexp: "^\\(;\\{3,\\} \\)"
;;   eval: (progn (outline-minor-mode 1) (outline-hide-region-body(point-min)(point-max)))
;;   read-symbol-shorthands: (("ot-" . "org-timeblock-") ("otl-" . "org-timeblock-list-"))
;; End:

;;; org-timeblock.el ends here
