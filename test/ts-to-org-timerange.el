;; -*- lexical-binding: t; -*-

(require 'org-timeblock-test)

(describe
    "org-timeblock-ts-to-org-timerange"
  (it "org-timeblock-ts-to-org-timerange"

    (expect
     (ot-ts-to-org-timerange
      (ts-parse "2022-10-15"))
     :to-equal
     "<2022-10-15 Sat>")

    (expect
     (ot-ts-to-org-timerange
      (ts-parse "2022-10-15")
      (ts-parse "2022-10-15"))
     :to-equal
     "<2022-10-15 Sat>")

    (expect
     (ot-ts-to-org-timerange
      (ts-parse "2022-10-15 20:15")
      (ts-parse "2022-10-15 20:15"))
     :to-equal
     "<2022-10-15 Sat 20:15>")
    
    (expect
     (ot-ts-to-org-timerange
      (ts-parse "2022-10-15")
      (ts-parse "2022-10-16"))
     :to-equal
     "<2022-10-15 Sat>--<2022-10-16 Sun>")
    
    (expect
     (ot-ts-to-org-timerange
      (ts-parse "2022-10-15 20:15"))
     :to-equal
     "<2022-10-15 Sat 20:15>")
    
    ;; TODO fix
    ;; (expect
    ;;  (ot-ts-to-org-timerange
    ;;     (ts-parse ""))
    ;;  :to-be
    ;;  nil)
    
    (expect
     (ot-ts-to-org-timerange
      (ts-parse "2022-10-15 20:15")
      (ts-parse "2022-10-15 23:15"))
     :to-equal
     "<2022-10-15 Sat 20:15-23:15>")
    (expect
     (ot-ts-to-org-timerange
      (ts-parse "2022-10-15 23:45")
      (ts-parse "2022-10-16 00:15"))
     :to-equal
     "<2022-10-15 Sat 23:45>--<2022-10-16 Sun 00:15>")
    ;; TODO this call should return other result
    (expect
     (ot-ts-to-org-timerange
      (ts-parse "2022-10-15 20:15")
      (ts-parse "2022-10-16 21:15")
      "+1d")
     :to-equal
     "<2022-10-15 Sat 20:15 +1d>--<2022-10-16 Sun 21:15 +1d>")))

;; Local Variables:
;;   read-symbol-shorthands: (("ot-" . "org-timeblock-"))
;; End:
