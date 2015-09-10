(require '#:sb-concurrency)

(ql:quickload "trivial-backtrace" "sqlite")

(asdf:defsystem #:pjs-logging
  :serial t
  :depends-on (#:pjs-utils #:cl-fad #:sqlite)
  :components ((:file "package")
               (:file "pjs-logging")))
