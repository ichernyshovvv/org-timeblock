(use-modules
 (guix gexp)
 (guix packages)
 (guix git-download)
 (guix build-system emacs)
 ((guix licenses) #:prefix license:)
 (gnu packages emacs)
 (gnu packages emacs-xyz))

(define %source-dir (dirname (current-filename)))

(define-public emacs-org-timeblock
  (package
   (name "emacs-org-timeblock")
   (version "git")
   (source (local-file %source-dir
                       #:recursive? #t
                       #:select? (git-predicate %source-dir)))
   (build-system emacs-build-system)
   (arguments (list #:emacs emacs))
   (propagated-inputs (list emacs-compat))
   (home-page "https://github.com/ichernyshovvv/org-timeblock")
   (synopsis "Timeblocking tool for orgmode inside Emacs")
   (description "Emacs package that provides interactive multiple-day timeblock
view for orgmode tasks.")
   (license license:gpl3+)))

emacs-org-timeblock
