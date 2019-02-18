(in-package :cl)

(defpackage promise-a+
  (:use :cl)
  (:import-from :promise-a+.promise
                :promise)
  (:export :promise))

(in-package promise-a+)
