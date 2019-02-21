(in-package :cl)

(defpackage promise-a+-test.promise
  (:use :cl :fiveam)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread)
  (:import-from :promise-a+-test
                :top)
  (:import-from :promise-a+
                :promise
                :then))

(in-package :promise-a+-test.promise)

(def-suite promise :in top)

(in-suite promise)

(test test-make-promise
  (is (subtypep (class-of (promise (lambda (re rj) (declare (ignorable rj)) (funcall re 0)))) 'promise)))

(test test-promise-resolve
  (let ((p (promise (lambda (re rj) (declare (ignorable rj)) (funcall re 0)))))
    (then p (lambda (x) (is (eq x 0))) nil)))

(test test-promise-resolve-async
  (let* ((th)
         (value)
         (p (promise (lambda (re rj) (declare (ignorable rj)) (setf th (make-thread (lambda () (funcall re 0))))))))
    (then p (lambda (x) (setf value x)) nil)
    (join-thread th)
    (is (eq value 0))))


