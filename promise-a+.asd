(defsystem promise-a+
  :depends-on (:cl-generator)
  :pathname "src/"
  :in-order-to ((test-op (test-op "promise-a+-test")))
  :components
  ((:file "index" :depends-on ("promise"))
   (:file "promise")))
