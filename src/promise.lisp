(in-package :cl)

(defpackage promise-a+.promise
  (:use :cl :cl-generator)
  (:import-from :miso-queue
                :queue
                :make-queue
                :en
                :de
                :queue-empty-p)
  (:export :promise
           :then))

(in-package :promise-a+.promise)
#|
(defmethod* state ()
  "a simple state machine for promise status"
  (let ((action (yield :pending)))
    (cond ((eq action :onfulfilled) (yield :fulfilled))
          ((eq action :onrejected) (yield :rejected)))
    (error "promise states changing error")))
|#

(defmethod init-state ()
  :pending)

(defmethod next-state ((status symbol) (action symbol))
  (if (eq status :pending)
      (cond ((eq action :onfulfilled) (return-from next-state :fulfilled))
            ((eq action :onrejected) (return-from next-state  :rejected))))
  (error (format nil "promise states changing error: ~A -> ~A" (or status :init) action)))

(defclass promise ()
  ((status :initarg :status
           :accessor status
           :type keyword)
   (value :initform nil
          :accessor value
          :type any)
   (callbacks :initform (make-queue)
              :accessor callbacks
              :type queue)))

(defmethod promise ((resolver function))
  (let ((promise (make-instance 'promise :status (init-state))))
    (funcall resolver (resolved promise) (rejected promise))
    promise))

(defmethod then ((self promise) (resolve function) (reject function))
  (let ((promise (make-instance 'promise :status (init-state)))
        (status (status self))
        (value (value self)))
    (if (eq :pending status)
        (en (callbacks self)
            (lambda ()
              (let ((status (status self))
                    (value (value self)))
                (cond ((eq status :fulfilled) (error-handler promise resolve value))
                      ((eq status :rejected) (error-handler promise reject value))))))
        (cond ((eq status :fulfilled) (error-handler promise resolve value))
              ((eq status :rejected) (error-handler promise reject value))))
    promise))

(defmethod action ((self promise) (action symbol) value)
  (setf (status self) (next-state (status self) action))
  (setf (value self) value)
  (callback self))

(defmethod error-handler ((self promise) (closure function) value)
  (handler-case (funcall closure value)
    (error (reason) (funcall (rejected self) reason))
    (:no-error (value) (funcall (resolved self) value))))

(defmethod callback ((self promise))
  (let ((callbacks (callbacks self)))
    (loop until (queue-empty-p callbacks)
       do (funcall (de callbacks)))))

(defmethod resolved ((self promise))
  (lambda (&optional value)
    (print "resolved") (print self)
    (action self :onfulfilled value)))

(defmethod rejected ((self promise))
  (lambda (&optional value)
    (print "rejected") (print self)
    (action self :onrejected value)))

