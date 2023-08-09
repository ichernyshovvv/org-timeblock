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
   (propagated-inputs (list emacs-org-ql emacs-compat emacs-persist))
   (home-page "https://github.com/ichernyshovvv/org-timeblock")
   (synopsis "Schedule your day visually, using timeblocking technique inside Emacs")
   (description "The builtin orgmode package for viewing tasks or events
for a particular day, org-agenda, does not help you to quickly understand,
where, for example, you have free time in your day or where you have overlapping
tasks. Just a list of tasks is not sufficient. This package is created to
fix this problem and provide some of the functionality that modern calendars
provide.")
   (license license:gpl3+)))

emacs-org-timeblock
