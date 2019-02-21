(defsystem promise-a+-test
  :depends-on (:fiveam
               :bordeaux-threads
               :promise-a+)
  :pathname "t/"
  :perform (test-op (o c)
                    (symbol-call :promise-a+-test '#:run-all-tests))
  :components
  ((:file "test")
   (:file "promise" :depends-on ("test"))))
