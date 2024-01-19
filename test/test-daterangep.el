;; -*- lexical-binding: t; -*-

(ert-deftest test-org-timeblock--daterangep ()
  "org-timeblock--daterangep"
  (should-not
   (org-timeblock--daterangep
    (org-timeblock-test-with-temp-text
	"<2023-07-02 Sun>"
      (org-element-timestamp-parser))))

  (should-not
   (org-timeblock--daterangep
    (org-timeblock-test-with-temp-text
	"<2023-07-02 Sun>--<2023-07-02 Sun>"
      (org-element-timestamp-parser))))
  
  (should-not
   (org-timeblock--daterangep
    (org-timeblock-test-with-temp-text "<2023-07-02 Sun 12:00>"
      (org-element-timestamp-parser))))

  (should-not
   (org-timeblock--daterangep
    (org-timeblock-test-with-temp-text
	"<2023-07-02 Sun 12:00>--<2023-07-03 Mon>"
      (org-element-timestamp-parser))))

  (should-not
   (org-timeblock--daterangep
    (org-timeblock-test-with-temp-text
	"<2023-07-02 Sun 12:00>--<2023-07-03 Mon 13:00>"
      (org-element-timestamp-parser))))

  (should
   (org-timeblock--daterangep
    (org-timeblock-test-with-temp-text
	"<2023-07-02 Sun>--<2023-07-03 Mon>"
      (org-element-timestamp-parser)))))
