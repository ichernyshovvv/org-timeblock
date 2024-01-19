;; -*- lexical-binding: t; -*-

(load "./org-timeblock-test.el")

(describe "org-timeblock-ts-to-org-timerange"
  (it "org-timeblock-ts-to-org-timerange"
    (expect
     (org-timeblock-ts-to-org-timerange
      (make-decoded-time :year 2022 :month 10 :day 15))
     :to-equal
     "<2022-10-15 Sat>")

    (expect
     (org-timeblock-ts-to-org-timerange
      (make-decoded-time :year 2022 :month 10 :day 15)
      (make-decoded-time :year 2022 :month 10 :day 15))
     :to-equal
     "<2022-10-15 Sat>")
    
    (expect
     (org-timeblock-ts-to-org-timerange
      (make-decoded-time :year 2022 :month 10 :day 15)
      (make-decoded-time :year 2022 :month 10 :day 16))
     :to-equal
     "<2022-10-15 Sat>--<2022-10-16 Sun>")
    
    (expect
     (org-timeblock-ts-to-org-timerange
      (org-parse-time-string "2022-10-15 20:15"))
     :to-equal
     "<2022-10-15 Sat 20:15>")
    
    (expect
     (org-timeblock-ts-to-org-timerange
      (org-parse-time-string "2022-10-15 20:15")
      (org-parse-time-string "2022-10-15 23:15"))
     :to-equal
     "<2022-10-15 Sat 20:15-23:15>")

    (expect
     (org-timeblock-ts-to-org-timerange
      (org-parse-time-string "2022-10-15 20:15")
      (make-decoded-time :year 2022 :month 10 :day 16))
     :to-equal
     "<2022-10-15 Sat 20:15>--<2022-10-16 Sun>")
    
    (expect
     (org-timeblock-ts-to-org-timerange
      (org-parse-time-string "2022-10-15 23:30")
      (org-parse-time-string "2022-10-16"))
     :to-equal
     "<2022-10-15 Sat 23:30>--<2022-10-16 Sun 00:00>")

    (expect
     (org-timeblock-ts-to-org-timerange
      (org-parse-time-string "2022-10-15 23:30")
      (org-parse-time-string "2022-10-15 24:00"))
     :to-equal
     "<2022-10-15 Sat 23:30>--<2022-10-16 Sun 00:00>")

    (expect
     (org-timeblock-ts-to-org-timerange
      (org-parse-time-string "2022-10-15 23:45")
      (org-parse-time-string "2022-10-16 00:15"))
     :to-equal
     "<2022-10-15 Sat 23:45>--<2022-10-16 Sun 00:15>")
    ;; TODO this call should return other result
    (expect
     (org-timeblock-ts-to-org-timerange
      (org-parse-time-string "2022-10-15 20:15")
      (org-parse-time-string "2022-10-16 21:15")
      "+1d")
     :to-equal
     "<2022-10-15 Sat 20:15 +1d>--<2022-10-16 Sun 21:15 +1d>")))
