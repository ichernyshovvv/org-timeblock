;; -*- lexical-binding: t; -*-

(require 'org-timeblock-test)

(describe "org-timeblock-tss-intersect-p"
  (it "org-timeblock-tss-intersect-p"
    (expect
     (ot-tss-intersect-p
      (ot-test-with-temp-text "<2023-07-02 Sun>"
	(org-element-timestamp-parser))
      (ot-test-with-temp-text "<2023-07-02 Sun>"
	(org-element-timestamp-parser)))
     :to-be
     t)

    (expect
     (ot-tss-intersect-p
      (ot-test-with-temp-text "<2023-07-03 Mon>"
	(org-element-timestamp-parser))
      (ot-test-with-temp-text "<2023-07-02 Sun>"
	(org-element-timestamp-parser)))
     :to-be
     nil)

    (expect
     (ot-tss-intersect-p
      (ot-test-with-temp-text "<2023-07-02 Sun 13:00-14:00>"
	(org-element-timestamp-parser))
      (ot-test-with-temp-text "<2023-07-02 Sun 12:00>"
	(org-element-timestamp-parser)))
     :to-be
     nil)

    (expect
     (ot-tss-intersect-p
      (ot-test-with-temp-text "<2023-07-02 Sun 12:00>"
	(org-element-timestamp-parser))
      (ot-test-with-temp-text "<2023-07-02 Sun 13:00-14:00>"
	(org-element-timestamp-parser)))
     :to-be
     nil)))

;; Local Variables:
;;   read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:
