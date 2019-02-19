(in-package :cl)

(defpackage promise-a+.promise
  (:use :cl :cl-generator)
  (:import-from :miso-queue
                :queue
                :make-queue
                :en
                :de
                :queue-empty-p)
  (:export :promise))

(in-package :promise-a+.promise)
#|
(defmethod* state ()
  "a simple state machine for promise status"
  (let ((action (yield :pending)))
    (cond ((eq action :onfulfilled) (yield :fulfilled))
          ((eq action :onrejected) (yield :rejected)))
    ))
|#

(defmethod init-state ()
  :pending)

(defmethod next-state (status action)
  (if (eq status :pending)
      (cond ((eq action :onfulfilled) :fulfilled)
            ((eq action :onrejected) :rejected)
            (t (error "promise states changing error")))))

(defclass promise ()
  ((status :initarg :status
           :accessor status
           :type keyword)
   (value :initform nil
          :accessor value
          :type any)
   (callbacks :initform (list)
              :accessor callbacks
              :type queue)))

(defmethod promise ((resolver function))
  (let ((promise (make-instance 'promise :status (init-state))))
    (funcall resolver (resolved promise) (rejected promise))
    promise))

(defmethod then ((self promise) (resolve function) (reject function))
  (en (callbacks self)
      (lambda ()
        (let ((status (status self)))
          (cond ((eq status :fulfilled) (funcall resolve))
                ((eq status :rejected) (funcall reject)))))))

(defmethod callback ((self promise))
  (let ((callbacks (callbacks self)))
    (loop until (queue-empty-p callbacks)
       do (funcall (dequeue callbacks)))))

(defmethod resolved ((self promise))
   (lambda (&optional value)
     (setf (status self) (next-state (status self) :onfulfilled))))
