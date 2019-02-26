(in-package :cl)

(defpackage promise-a+-test.promise
  (:use :cl :fiveam)
  (:import-from :bordeaux-threads
                :make-thread
                :join-thread
                :make-semaphore
                :wait-on-semaphore
                :signal-semaphore)
  (:import-from :promise-a+-test
                :top)
  (:import-from :promise-a+.promise
                :promise
                :then
                :finish
                ::format-warning
                ::format-illegal-mode))

(in-package :promise-a+-test.promise)

(def-suite promise :in top)

(in-suite promise)

(test make-promise
  (is (subtypep (class-of (promise (lambda (re rj) (declare (ignorable rj)) (funcall re 0)))) 'promise)))

(test promise-resolved
  (let ((p (promise (lambda (re rj) (declare (ignorable rj)) (funcall re 0))))
        (value))
    (then p (lambda (x) (setf value x)) nil)
    (is (eq value 0))))

(test promise-resolved-async
  (let* ((semaphore (make-semaphore))
         (th0 (make-thread (lambda () (wait-on-semaphore semaphore))))
         (value)
         (p (promise (lambda (re rj) (declare (ignorable rj)) (make-thread (lambda () (funcall re 0)))))))
    (then p (lambda (x) (setf value x) (signal-semaphore semaphore)) nil)
    (join-thread th0)
    (is (eq value 0))))

(test promise-rejected-async
  (let* ((semaphore (make-semaphore))
         (th0 (make-thread (lambda () (wait-on-semaphore semaphore))))
         (value)
         (p (promise (lambda (re rj) (declare (ignorable re)) (make-thread (lambda () (funcall rj 0)))))))
    (then p nil (lambda (x) (setf value x) (signal-semaphore semaphore)))
    (join-thread th0)
    (is (eq value 0))))

(test promise-resolver-rejected
  (let ((p (promise (lambda (re rj) (declare (ignorable re)) (error "err") (funcall rj 0))))
        (value))
    (then p nil (lambda (x) (setf value x)))
    (is (eq (simple-condition-format-control value) "err"))))

(test promise-rejected
  (let ((p (promise (lambda (re rj) (declare (ignorable re)) (funcall rj 0))))
        (value))
    (then p nil (lambda (x) (setf value x)))
    (is (eq value 0))))

(test promise-resolved-then
  (let ((p (promise (lambda (re rj) (declare (ignorable rj)) (funcall re 0))))
        (value1)
        (value2)
        (value3))
    (then p (lambda (x) (setf value1 (+ x 1))) nil)
    (then (then p (lambda (x) (setf value2 (+ x 10))) nil)
          (lambda (x) (setf value3 (+ x 100)))
          nil)
    (is (eq value1 1))
    (is (eq value2 10))
    (is (eq value3 110))))

(test promise-rejected-then
  (let ((p (promise (lambda (re rj) (declare (ignorable re)) (funcall rj 0))))
        (value1)
        (value2)
        (value3))
    (then p nil (lambda (x) (setf value1 (+ x 1))))
    (then (then p nil (lambda (x) (setf value2 (+ x 10))))
          (lambda (x) (setf value3 (+ x 100)))
          nil)
    (is (eq value1 1))
    (is (eq value2 10))
    (is (eq value3 110))))

(test promise-onfulfilled-nil
  (let ((p (promise (lambda (re rj) (declare (ignorable rj)) (funcall re 0))))
        (value))
    (then (then p nil nil) (lambda (x) (setf value x)) nil)
    (is (eq value 0))))

(test promise-onrejected-nil
  (let ((p (promise (lambda (re rj) (declare (ignorable re)) (funcall rj "err"))))
        (value))
    (then (then p nil nil) nil (lambda (x) (setf value x)))
    (is (eq value "err"))))

(test finish-mode-default
  (let ((p (promise (lambda (re rj) (declare (ignorable re)) (funcall rj "err"))))
         (value1)
         (value2))
    (handler-case (finish (then p nil (lambda (reason) (setf value1 reason) (error reason))))
      (error (reason) (setf value2 reason)))
    (is (equal value1 (simple-condition-format-control value2)))))

(test finish-mode-throw
  (let ((p (promise (lambda (re rj) (declare (ignorable re)) (funcall rj "err"))))
         (value1)
         (value2))
    (handler-case (finish (then p nil (lambda (reason) (setf value1 reason) (error reason))) :mode :throw)
      (error (reason) (setf value2 reason)))
    (is (equal value1 (simple-condition-format-control value2)))))

(test async-finish-mode-throw
  (let* ((semaphore (make-semaphore))
         (th (make-thread (lambda () (wait-on-semaphore semaphore))))
         (p (promise (lambda (re rj) (declare (ignorable re)) (make-thread (lambda () (funcall rj "err") (signal-semaphore semaphore))))))
         (value1)
         (value2))
    (join-thread th)
    (handler-case (finish (then p nil (lambda (reason) (setf value1 reason) (error reason))))
      (error (reason) (setf value2 reason)))
    (is (equal value1 (simple-condition-format-control value2)))))

(test finish-mode-warning
  (let* ((p1 (promise (lambda (re rj) (declare (ignorable re)) (funcall rj "err"))))
         (s1 (make-string-output-stream))
         (*standard-output* s1)
         (s2 (make-string-output-stream))
         (value)
        (p2))
    (finish (setf p2 (then p1 nil (lambda (reason) (setf value reason) (error reason)))) :mode :warning)
    (let ((*standard-output* s2))
      (print (format-warning p2 value)))
    (is (equal (get-output-stream-string s2) (get-output-stream-string s1)))))

(test finish-mode-silence
  (let* ((p (promise (lambda (re rj) (declare (ignorable re)) (funcall rj 0))))
        (s (make-string-output-stream))
        (*standard-output* s))
    (finish p :mode :silence)
    (is (equal "" (get-output-stream-string s)))))

(test finish-mode-illegal
  )
