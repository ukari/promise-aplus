(in-package :cl)

(defpackage promise-a+
  (:use :cl)
  (:import-from :promise-a+.promise
                :promise
                :then
                :finish)
  (:export :promise
           :then
           :finish))

(in-package promise-a+)
