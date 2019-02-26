(in-package :cl)

(defpackage promise-a+
  (:use :cl)
  (:import-from :promise-a+.promise
                :promise
                :promisep
                :then
                :finish)
  (:export :promise
           :promisep
           :then
           :finish))

(in-package promise-a+)
