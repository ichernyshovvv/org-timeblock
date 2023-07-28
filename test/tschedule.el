;; -*- lexical-binding: t; -*-

(require 'helper)

(describe
 "org-timeblock-schedule"
 (it
  "Scheduling an entry without SCHEDULED property"

  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:")
    (ot--schedule
     (ot-test-encode-time "2022-10-15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat>"
      "")
    "\n"))

  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:")
    (ot--schedule
     (ot-test-encode-time "2022-10-15 20:15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat 20:15>"
      "")
    "\n"))

  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:")
    (ot--schedule
     (ot-test-encode-time "2022-10-15 20:15")
     (ot-test-encode-time "2022-10-15 21:15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat 20:15-21:15>"
      "")
    "\n"))

  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:")
    (ot--schedule
     (ot-test-encode-time "2022-10-15 20:15")
     (ot-test-encode-time "2022-10-16 21:15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat 20:15>--<2022-10-16 Sun 21:15>"
      "")
    "\n"))

  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:")
    (ot--schedule
     (ot-test-encode-time "2022-10-15 20:15")
     (ot-test-encode-time "2022-10-16 21:15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat 20:15>--<2022-10-16 Sun 21:15>"
      "")
    "\n"))

  (expect
   (ot-with-temp-org-file
    (insert
     (string-join
      '("* test :tag1:"
	"SCHEDULED: <2022-09-02 Fri 12:00>"
	"")
      "\n"))
    (ot--schedule
     (ot-test-encode-time "2022-10-15 20:15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat 20:15>"
      "")
    "\n")))
 (it
  "Rescheduling"
  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:\nSCHEDULED: <2022-10-16 Sun>")
    (ot--schedule
     (ot-test-encode-time "2022-10-15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat>"
      "")
    "\n")))
 (it
  "Rescheduling an entry that is scheduled to a timestamp with a repeater"
  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:\nSCHEDULED: <2022-10-16 Sun +1d>")
    (ot--schedule
     (ot-test-encode-time "2022-10-15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat +1d>"
      "")
    "\n")))
 (it
  "Rescheduling an entry that is scheduled to a timerange with a repeater"
  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:\nSCHEDULED: <2022-10-16 Sun 12:00-13:00 +1d>")
    (ot--schedule
     (ot-test-encode-time "2022-10-15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat +1d>"
      "")
    "\n")))
 (it
  "Rescheduling an entry that is scheduled to a daterange with a repeater"
  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:\nSCHEDULED: <2022-10-14 Fri 12:00 +1d>--<2022-10-15 Sat 13:00 +1d>")
    (ot--schedule
     (ot-test-encode-time "2022-10-15")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-15 Sat +1d>"
      "")
    "\n")))
 (it
  "Rescheduling to a daterange with a repeater"
  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:\nSCHEDULED: <2022-10-15 Sat +1d>")
    (ot--schedule
     (ot-test-encode-time "2022-10-14 12:00")
     (ot-test-encode-time "2022-10-15 13:00")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-14 Fri 12:00 +1d>--<2022-10-15 Sat 13:00 +1d>"
      "")
    "\n")))
 (it
  "Rescheduling to a timerange with a repeater"
  (expect
   (ot-with-temp-org-file
    (insert "* test :tag1:\nSCHEDULED: <2022-10-15 Sat +1d>")
    (ot--schedule
     (ot-test-encode-time "2022-10-14 12:00")
     (ot-test-encode-time "2022-10-14 13:00")))
   :to-equal
   (string-join
    '("* test :tag1:"
      "SCHEDULED: <2022-10-14 Fri 12:00-13:00 +1d>"
      "")
    "\n"))))

;; Local Variables:
;; read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:
