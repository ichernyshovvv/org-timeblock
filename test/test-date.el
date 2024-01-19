;; -*- lexical-binding: t; -*-

(ert-deftest test-org-timeblock-date= ()
  "org-timeblock-date="
  (should
   (org-timeblock-date= nil nil))
  (should-not
   (org-timeblock-date=
    (org-parse-time-string "2023-10-10")
    nil))
  (should-not
   (org-timeblock-date=
    nil
    (org-parse-time-string "2023-10-10")))
  (should
   (org-timeblock-date=
    (org-parse-time-string "2023-10-10")
    (org-parse-time-string "2023-10-10")))
  (should
   (org-timeblock-date=
    (org-parse-time-string "2023-10-10")
    (org-parse-time-string "2023-10-10 12:00")))
  (should-not
   (org-timeblock-date=
    (org-parse-time-string "2023-10-10")
    (org-parse-time-string "2023-10-15"))))

(ert-deftest test-org-timeblock-date< ()
  "org-timeblock-date<"
  (should-not
   (org-timeblock-date<
    (org-parse-time-string "2023-10-10") nil))
  (should
   (org-timeblock-date< nil (org-parse-time-string "2023-10-10")))
  (should-not
   (org-timeblock-date<
    (org-parse-time-string "2023-10-10")
    (org-parse-time-string "2023-10-10")))
  (should
   (org-timeblock-date<
    (org-parse-time-string "2023-10-10")
    (org-parse-time-string "2023-10-15")))
  (should
   (org-timeblock-date<
    (org-parse-time-string "2023-07-31")
    (org-parse-time-string "2023-08-01")))
  (should-not
   (org-timeblock-date<
    (org-parse-time-string "2023-10-15")
    (org-parse-time-string "2023-10-10"))))
