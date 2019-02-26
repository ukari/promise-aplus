(defsystem promise-a+
  :depends-on (:aria
               :atomics)
  :pathname "src/"
  :in-order-to ((test-op (test-op "promise-a+-test")))
  :components
  ((:file "index" :depends-on ("promise"))
   (:file "promise")))
