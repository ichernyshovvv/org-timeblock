;; -*- lexical-binding: t; -*-

(require 'org-timeblock-test)

(describe
 "org-timeblock-ts-date="
 (it
  "org-timeblock-ts-date="
  (expect
   (ot-ts-date= nil nil)
   :to-be
   t)
  (expect
   (ot-ts-date= (ts-parse "2023-10-10") nil)
   :to-be
   nil)
  (expect
   (ot-ts-date= nil (ts-parse "2023-10-10"))
   :to-be
   nil)
  (expect
   (ot-ts-date= (ts-parse "2023-10-10") (ts-parse "2023-10-10"))
   :to-be
   t)
  (expect
   (ot-ts-date= (ts-parse "2023-10-10") (ts-parse "2023-10-15"))
   :to-be
   nil)))

(describe
 "org-timeblock-ts-date<"
 (it
  "org-timeblock-ts-date<"
  (expect
   (ot-ts-date< (ts-parse "2023-10-10") nil)
   :to-be
   nil)
  (expect
   (ot-ts-date< nil (ts-parse "2023-10-10"))
   :to-be
   t)
  (expect
   (ot-ts-date< (ts-parse "2023-10-10") (ts-parse "2023-10-10"))
   :to-be
   nil)
  (expect
   (ot-ts-date< (ts-parse "2023-10-10") (ts-parse "2023-10-15"))
   :to-be
   t)

  (expect
   (ot-ts-date< (ts-parse "2023-07-31") (ts-parse "2023-08-01"))
   :to-be
   t)
  
  (expect
   (ot-ts-date< (ts-parse "2023-10-15") (ts-parse "2023-10-10"))
   :to-be
   nil)))

;; Local Variables:
;;   read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:
