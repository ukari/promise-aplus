(in-package :cl)

(defpackage promise-a+-test
  (:use :cl :fiveam)
  (:shadow :run-all-tests)
  (:export :run-all-tests
           :top))

(in-package :promise-a+-test)

(def-suite top
    :description "all tests for promise-a+")

(in-suite top)

(defun run-all-tests ()
  (run! 'top))
