;; -*- lexical-binding: t; -*-

(load "./org-timeblock-test.el")

(describe "org-timeblock-date="
  (it "org-timeblock-date="
    (expect
     (org-timeblock-date= nil nil)
     :to-be
     t)
    (expect
     (org-timeblock-date= (org-parse-time-string "2023-10-10") nil)
     :to-be
     nil)
    (expect
     (org-timeblock-date= nil (org-parse-time-string "2023-10-10"))
     :to-be
     nil)
    (expect
     (org-timeblock-date=
      (org-parse-time-string "2023-10-10")
      (org-parse-time-string "2023-10-10"))
     :to-be
     t)
    (expect
     (org-timeblock-date=
      (org-parse-time-string "2023-10-10")
      (org-parse-time-string "2023-10-10 12:00"))
     :to-be
     t)
    (expect
     (org-timeblock-date=
      (org-parse-time-string "2023-10-10")
      (org-parse-time-string "2023-10-15"))
     :to-be
     nil)))

(describe "org-timeblock-date<"
  (it "org-timeblock-date<"
    (expect
     (org-timeblock-date< (org-parse-time-string "2023-10-10") nil)
     :to-be
     nil)
    (expect
     (org-timeblock-date< nil (org-parse-time-string "2023-10-10"))
     :to-be
     t)
    (expect
     (org-timeblock-date<
      (org-parse-time-string "2023-10-10")
      (org-parse-time-string "2023-10-10"))
     :to-be
     nil)
    (expect
     (org-timeblock-date<
      (org-parse-time-string "2023-10-10")
      (org-parse-time-string "2023-10-15"))
     :to-be
     t)

    (expect
     (org-timeblock-date<
      (org-parse-time-string "2023-07-31")
      (org-parse-time-string "2023-08-01"))
     :to-be
     t)
    
    (expect
     (org-timeblock-date<
      (org-parse-time-string "2023-10-15")
      (org-parse-time-string "2023-10-10"))
     :to-be
     nil)))
