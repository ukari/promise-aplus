(in-package :cl)

(defpackage promise-a+-test
  (:use :cl :fiveam)
  (:export :run-all-tests
           :top))

(in-package :promise-a+-test)

(def-suite top
    :description "all tests for promise-a+")

(in-suite top)
