;; -*- lexical-binding: t; -*-

(load "./org-timeblock-test.el")

(describe "org-timeblock-schedule"
  (it "Rescheduling"
    (expect
     (org-timeblock-with-temp-org-file
      (insert "* test :tag1:\nSCHEDULED: <2022-10-16 Sun>")
      (search-backward "<")
      (org-timeblock--schedule
       (make-decoded-time :year 2022 :month 10 :day 15)))
     :to-equal
     (string-join
      '("* test :tag1:"
	"SCHEDULED: <2022-10-15 Sat>"
	"")
      "\n")))
  (it "Rescheduling an entry that is scheduled to a timestamp with a repeater"
    (expect
     (org-timeblock-with-temp-org-file
      (insert "* test :tag1:\nSCHEDULED: <2022-10-16 Sun +1d>")
      (search-backward "<")
      (org-timeblock--schedule
       (make-decoded-time :year 2022 :month 10 :day 15)))
     :to-equal
     (string-join
      '("* test :tag1:"
	"SCHEDULED: <2022-10-15 Sat +1d>"
	"")
      "\n")))
  (it "Rescheduling an entry that is scheduled to a timerange with a repeater"
    (expect
     (org-timeblock-with-temp-org-file
      (insert "* test :tag1:\nSCHEDULED: <2022-10-16 Sun 12:00-13:00 +1d>")
      (search-backward "<")
      (org-timeblock--schedule
       (make-decoded-time :year 2022 :month 10 :day 15)))
     :to-equal
     (string-join
      '("* test :tag1:"
	"SCHEDULED: <2022-10-15 Sat +1d>"
	"")
      "\n")))
  (it "Rescheduling an entry that is scheduled to a daterange with a repeater"
    (expect
     (org-timeblock-with-temp-org-file
      (insert "* test :tag1:\nSCHEDULED: <2022-10-14 Fri 12:00 +1d>--<2022-10-15 Sat 13:00 +1d>")
      (search-backward "<")
      (search-backward "<")
      (org-timeblock--schedule
       (make-decoded-time :year 2022 :month 10 :day 15)))
     :to-equal
     (string-join
      '("* test :tag1:"
	"SCHEDULED: <2022-10-15 Sat +1d>"
	"")
      "\n")))
  (it "Rescheduling to a daterange with a repeater"
    (expect
     (org-timeblock-with-temp-org-file
      (insert "* test :tag1:\nSCHEDULED: <2022-10-15 Sat +1d>")
      (search-backward "<")
      (org-timeblock--schedule
       (org-parse-time-string "2022-10-14 12:00")
       (org-parse-time-string "2022-10-15 13:00")))
     :to-equal
     (string-join
      '("* test :tag1:"
	"SCHEDULED: <2022-10-14 Fri 12:00 +1d>--<2022-10-15 Sat 13:00 +1d>"
	"")
      "\n")))
  (it "Rescheduling to a timerange with a repeater"
    (expect
     (org-timeblock-with-temp-org-file
      (insert "* test :tag1:\nSCHEDULED: <2022-10-15 Sat +1d>")
      (search-backward "<")
      (org-timeblock--schedule
       (org-parse-time-string "2022-10-14 12:00")
       (org-parse-time-string "2022-10-14 13:00")))
     :to-equal
     (string-join
      '("* test :tag1:"
	"SCHEDULED: <2022-10-14 Fri 12:00-13:00 +1d>"
	"")
      "\n"))))
