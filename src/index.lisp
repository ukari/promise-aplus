(in-package :cl)

(defpackage promise-a+
  (:use :cl)
  (:import-from :promise-a+.promise
                :promise
                :then)
  (:export :promise
           :then))

(in-package promise-a+)
