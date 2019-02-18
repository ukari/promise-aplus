(in-package :cl)

(defpackage promise-a+-test.promise
  (:use :cl :fiveam)
  (:import-from :promise-a+-test
                :top))

(in-package :promise-a+-test.promise)

(def-suite promise :in top)

(in-suite promise)
