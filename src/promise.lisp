(in-package :cl)

(defpackage promise-a+.promise
  (:use :cl)
  (:import-from :aria.structure.miso-queue
                :queue
                :make-queue
                :en
                :de
                :queue-empty-p)
  (:export :promise
           :then
           :finish))

(in-package :promise-a+.promise)

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
   (fulfilled-callbacks :initform (make-queue)
                        :accessor fulfilled-callbacks
                        :type queue)
   (rejected-callbacks :initform (make-queue)
                       :accessor rejected-callbacks
                       :type queue)))

(defmethod promise ((resolver function))
  (let ((promise (make-instance 'promise :status (init-state))))
    (funcall resolver (resolved promise) (rejected promise))
    promise))

(defmethod then ((self promise) resolve reject)
  (then-inner self resolve reject))

(defmethod then-inner ((self promise) resolve reject &optional danger)
  (let ((promise (make-instance 'promise :status (init-state)))
        (status (status self)))
    (cond ((eq status :pending)
           (progn (en (fulfilled-callbacks self)
                      (lambda () (then-run self promise resolve)))
                  (en (rejected-callbacks self)
                      (lambda () (then-run self promise reject danger)))))
          ((eq status :fulfilled) (then-run self promise resolve))
          ((eq status :rejected) (then-run self promise reject danger)))
    promise))

(defmethod then-run ((self promise) (next promise) callback &optional danger)
  (let ((status (status self))
        (value (value self)))
    (cond ((eq (type-of callback) 'function)
           (if danger
               (funcall callback value)
               (error-handler next callback value)))
          ((eq status :fulfilled) (funcall (resolved next) value))
          ((eq status :rejected) (funcall (rejected next) value)))))

(defmethod action ((self promise) (action symbol) value)
  (setf (status self) (next-state (status self) action))
  (setf (value self) value)
  (callback self))

(defmethod error-handler ((self promise) (closure function) value)
  (handler-case (funcall closure value)
    (error (reason) (funcall (rejected self) reason))
    (:no-error (value) (funcall (resolved self) value))))

(defmethod callback ((self promise))
  (let* ((status (status self))
         (callbacks (cond ((eq status :fulfilled) (fulfilled-callbacks self))
                          ((eq status :rejected) (rejected-callbacks self)))))
    (loop until (queue-empty-p callbacks)
       do (funcall (de callbacks)))))

(defmethod resolved ((self promise))
  (lambda (&optional value)
    (action self :onfulfilled value)))

(defmethod rejected ((self promise))
  (lambda (&optional value)
    (action self :onrejected value)))

(defmethod finish ((self promise) &key (mode :throw))
  (cond ((eq mode :throw) (then-inner self nil (lambda (reason) (error (format nil "~A" reason))) t))
        ((eq mode :warning) (then self nil (lambda (reason) (format t "Unhandled promise rejection (promise: ~A): ~A" self reason))))
        ((eq mode :silence) self)
        (t (error (format nil "unacceptable mode ~A in promise finish" mode))))
  nil)
