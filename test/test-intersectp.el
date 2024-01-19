;; -*- lexical-binding: t; -*-

(defun org-timeblock-check-intersection (y1 height1 y2 height2)
  (let ((entry1 (propertize "entry text" 'y y1 'block-height height1))
	(entry2 (propertize "entry text" 'y y2 'block-height height2)))
    (org-timeblock-intersect-p entry1 entry2)))

(ert-deftest test-org-timeblock-intersect-p ()
  "org-timeblock-intersect-p"
  (should-not
   (org-timeblock-check-intersection 0 30 30 45))
  
  (should-not
   (org-timeblock-check-intersection 0 30 30 45))

  (should
   (org-timeblock-check-intersection 0 35 30 45))

  (should
   (org-timeblock-check-intersection 0 100 30 45)))
