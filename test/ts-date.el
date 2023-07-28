;; -*- lexical-binding: t; -*-

(require 'helper)

(defun ot-encode-date (timestr)
  (let ((time (parse-time-string timestr)))
    (when time
      (setf (decoded-time-hour time) 0
	    (decoded-time-minute time) 0
	    (decoded-time-second time) 0)
      (encode-time time))))

(describe
 "org-timeblock-ts-date="
 (it
  "org-timeblock-ts-date="
  (expect
   (ot-ts-date= nil nil)
   :to-be
   t)
  (expect
   (ot-ts-date= (ot-encode-date "2023-10-10") nil)
   :to-be
   nil)
  (expect
   (ot-ts-date= nil (ot-encode-date "2023-10-10"))
   :to-be
   nil)
  (expect
   (ot-ts-date= (ot-encode-date "2023-10-10") (ot-encode-date "2023-10-10"))
   :to-be
   t)
  (expect
   (ot-ts-date= (ot-encode-date "2023-10-10") (ot-encode-date "2023-10-15"))
   :to-be
   nil)))

(describe
 "org-timeblock-ts-date<"
 (it
  "org-timeblock-ts-date<"
  (expect
   (ot-ts-date< (ot-encode-date "2023-10-10") nil)
   :to-be
   nil)
  (expect
   (ot-ts-date< nil (ot-encode-date "2023-10-10"))
   :to-be
   t)
  (expect
   (ot-ts-date< (ot-encode-date "2023-10-10") (ot-encode-date "2023-10-10"))
   :to-be
   nil)
  (expect
   (ot-ts-date< (ot-encode-date "2023-10-10") (ot-encode-date "2023-10-15"))
   :to-be
   t)
  (expect
   (ot-ts-date< (ot-encode-date "2023-10-15") (ot-encode-date "2023-10-10"))
   :to-be
   nil)))



;;   outline-regexp: "^\\(;\\{3,\\} \\)"
;;   eval: (progn (outline-minor-mode 1) (outline-hide-region-body(point-min)(point-max)))


;; Local Variables:
;;   read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:
