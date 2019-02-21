# promise-a+
promise A+ in common lisp

## referrence
[Promise/A+](https://promisesaplus.com/)

## warning
Due to the difference between lisp and javascript.

I think it seems not wise to use a thread in a promise library, so in the last `then` won't throw a error or report it. That means a promise rejected in the tail of `then` chain will silencely disappeared.

Instead, add a error catcher like `finish` to the tail of then chain would make it safe. For example
``` lisp
(defmethod finish ((self promise) &optional (mode :throw))
  (cond ((eq mode :throw) (then self nil (lambda (reason) (error reason))))
        ((eq mode :warning) (then self nil (lambda (reason) (print reason))))
        ((eq mode :silence) self)))

(finish (then (promise (lambda (re rj) (funcall rj "err")))) nil nil))
```

And the method `finish` has been provided by this library.
