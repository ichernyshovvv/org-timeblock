;; -*- lexical-binding: t; -*-

(require 'helper)

(describe
    "org-timeblock--timestamp-encode"
  (it "org-timeblock--timestamp-encode"

    (expect
     (ot--timestamp-encode
      (org-test-with-temp-text "<2023-07-02 Sun>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)

    (expect
     (ot--timestamp-encode
      (org-test-with-temp-text "<2023-07-02 Sun>--<2023-07-02 Sun>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)

    (expect
     (ot--timestamp-encode
      (org-test-with-temp-text "<2023-07-02 Sun 12:00-12:00>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)
    
    (expect
     (ot--timestamp-encode
      (org-test-with-temp-text "<2023-07-02 Sun 12:00>--<2023-07-02 Sun 12:00>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)
    
    (expect
     (ot--timestamp-encode
      (org-test-with-temp-text "<2023-07-02 Sun 12:00>"
	(org-element-timestamp-parser))
      t)
     :to-be
     nil)

    (expect
     (ot--timestamp-encode
      (org-test-with-temp-text "<2023-07-02 Sun 12:00-13:00>"
	(org-element-timestamp-parser))
      t)
     :not
     :to-be
     nil)))

;;   outline-regexp: "^\\(;\\{3,\\} \\)"
;;   eval: (progn (outline-minor-mode 1) (outline-hide-region-body(point-min)(point-max)))


;; Local Variables:
;;   read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:
