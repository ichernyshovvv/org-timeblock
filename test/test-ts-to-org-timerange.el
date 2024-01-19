;; -*- lexical-binding: t; -*-

(ert-deftest test-org-timeblock-ts-to-org-timerange ()
  "org-timeblock-ts-to-org-timerange"
  (should
   (string=
    (org-timeblock-ts-to-org-timerange
     (make-decoded-time :year 2022 :month 10 :day 15))
    "<2022-10-15 Sat>"))

  (should
   (string=
    (org-timeblock-ts-to-org-timerange
     (make-decoded-time :year 2022 :month 10 :day 15)
     (make-decoded-time :year 2022 :month 10 :day 15))
    "<2022-10-15 Sat>"))
  
  (should
   (string=
    (org-timeblock-ts-to-org-timerange
     (make-decoded-time :year 2022 :month 10 :day 15)
     (make-decoded-time :year 2022 :month 10 :day 16))
    "<2022-10-15 Sat>--<2022-10-16 Sun>"))
  
  (should
   (string=
    (org-timeblock-ts-to-org-timerange
     (org-parse-time-string "2022-10-15 20:15"))
    "<2022-10-15 Sat 20:15>"))
  
  (should
   (string=
    (org-timeblock-ts-to-org-timerange
     (org-parse-time-string "2022-10-15 20:15")
     (org-parse-time-string "2022-10-15 23:15"))
    "<2022-10-15 Sat 20:15-23:15>"))

  (should
   (string=
    (org-timeblock-ts-to-org-timerange
     (org-parse-time-string "2022-10-15 20:15")
     (make-decoded-time :year 2022 :month 10 :day 16))
    "<2022-10-15 Sat 20:15>--<2022-10-16 Sun>"))
  
  (should
   (string=
    (org-timeblock-ts-to-org-timerange
     (org-parse-time-string "2022-10-15 23:30")
     (org-parse-time-string "2022-10-16"))
    "<2022-10-15 Sat 23:30>--<2022-10-16 Sun 00:00>"))

  (should
   (string=
    (org-timeblock-ts-to-org-timerange
     (org-parse-time-string "2022-10-15 23:30")
     (org-parse-time-string "2022-10-15 24:00"))
    "<2022-10-15 Sat 23:30>--<2022-10-16 Sun 00:00>"))

  (should
   (string=
    (org-timeblock-ts-to-org-timerange
     (org-parse-time-string "2022-10-15 23:45")
     (org-parse-time-string "2022-10-16 00:15"))
    "<2022-10-15 Sat 23:45>--<2022-10-16 Sun 00:15>")))
