;; -*- lexical-binding: t; -*-

(require 'helper)

(describe "org-timeblock-tss-are-in-intersection-p"
  (it "org-timeblock-tss-are-in-intersection-p"
    (expect
     (ot-tss-are-in-intersection-p
      (org-test-with-temp-text "<2023-07-02 Sun>"
	(org-element-timestamp-parser))
      (org-test-with-temp-text "<2023-07-02 Sun>"
	(org-element-timestamp-parser)))
     :to-be
     t)

    (expect
     (ot-tss-are-in-intersection-p
      (org-test-with-temp-text "<2023-07-03 Mon>"
	(org-element-timestamp-parser))
      (org-test-with-temp-text "<2023-07-02 Sun>"
	(org-element-timestamp-parser)))
     :to-be
     nil)

    (expect
     (ot-tss-are-in-intersection-p
      (org-test-with-temp-text "<2023-07-02 Sun 13:00-14:00>"
	(org-element-timestamp-parser))
      (org-test-with-temp-text "<2023-07-02 Sun 12:00>"
	(org-element-timestamp-parser)))
     :to-be
     nil)

    (expect
     (ot-tss-are-in-intersection-p
      (org-test-with-temp-text "<2023-07-02 Sun 12:00>"
	(org-element-timestamp-parser))
      (org-test-with-temp-text "<2023-07-02 Sun 13:00-14:00>"
	(org-element-timestamp-parser)))
     :to-be
     nil)))

;;   outline-regexp: "^\\(;\\{3,\\} \\)"
;;   eval: (progn (outline-minor-mode 1) (outline-hide-region-body(point-min)(point-max)))


;; Local Variables:
;;   read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:
