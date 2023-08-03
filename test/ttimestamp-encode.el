;; -*- lexical-binding: t; -*-

(require 'org-timeblock-test)

(describe
    "ot--parse-org-element-ts"
  (it "ot--parse-org-element-ts"

    (expect
     (ot--parse-org-element-ts
      (ot-test-with-temp-text "<2023-07-02 Sun>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)

    (expect
     (ot--parse-org-element-ts
      (ot-test-with-temp-text "<2023-07-02 Sun>--<2023-07-02 Sun>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)

    (expect
     (ot--parse-org-element-ts
      (ot-test-with-temp-text "<2023-07-02 Sun 12:00-12:00>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)
    
    (expect
     (ot--parse-org-element-ts
      (ot-test-with-temp-text "<2023-07-02 Sun 12:00>--<2023-07-02 Sun 12:00>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)
    
    (expect
     (ot--parse-org-element-ts
      (ot-test-with-temp-text "<2023-07-02 Sun 12:00>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)

    (expect
     (ot--parse-org-element-ts
      (ot-test-with-temp-text "<2023-07-02 Sun 12:00-13:00>"
	(org-element-timestamp-parser))
      t)
     :not
     :to-be
     nil)))

;; Local Variables:
;;   read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:
