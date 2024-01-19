;; -*- lexical-binding: t; -*-

(load "./org-timeblock-test.el")

(describe "org-timeblock--daterangep"
  (it "org-timeblock--daterangep"
    (expect
     (org-timeblock--daterangep
      (org-timeblock-test-with-temp-text
	  "<2023-07-02 Sun>"
	(org-element-timestamp-parser)))
     :to-be
     nil)

    (expect
     (org-timeblock--daterangep
      (org-timeblock-test-with-temp-text
	  "<2023-07-02 Sun>--<2023-07-02 Sun>"
	(org-element-timestamp-parser)))
     :to-be
     nil)
    
    (expect
     (org-timeblock--daterangep
      (org-timeblock-test-with-temp-text "<2023-07-02 Sun 12:00>"
	(org-element-timestamp-parser)))
     :to-be
     nil)

    (expect
     (org-timeblock--daterangep
      (org-timeblock-test-with-temp-text
	  "<2023-07-02 Sun 12:00>--<2023-07-03 Mon>"
	(org-element-timestamp-parser)))
     :to-be
     nil)

    (expect
     (org-timeblock--daterangep
      (org-timeblock-test-with-temp-text
	  "<2023-07-02 Sun 12:00>--<2023-07-03 Mon 13:00>"
	(org-element-timestamp-parser)))
     :to-be
     nil)

    (expect
     (org-timeblock--daterangep
      (org-timeblock-test-with-temp-text
	  "<2023-07-02 Sun>--<2023-07-03 Mon>"
	(org-element-timestamp-parser)))
     :to-be
     t)))
