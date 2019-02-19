(defsystem promise-a+
  :depends-on (:cl-generator :miso-queue)
  :pathname "src/"
  :in-order-to ((test-op (test-op "promise-a+-test")))
  :components
  ((:file "index" :depends-on ("promise"))
   (:file "promise")))
