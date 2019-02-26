# promise-a+
incomplete promise A+ in common lisp

## referrence
[Promise/A+](https://promisesaplus.com/)

## warning
Due to the difference between lisp and javascript.

I think it would be **not** wise to do the following things for the purpose of make a promise library automaticaly detect if there has a `then` after a promise.

 So the last `then` won't throw or report a error. That means a promise rejected in the tail of `then` chain will silencely disappeared.

- use a thread in a promise library
- use two task queue for difference between micro task and macro task
- only call related user code in the macro task queue
- only call a promise then's onfulfilled or onrejected in the micro task queue

Instead, add a error catcher like `finish` to the tail of then chain would make it safe. For example
``` lisp
(defmethod finish ((self promise) &optional (mode :throw))
  (cond ((eq mode :throw) (then self nil (lambda (reason) (error reason))))
        ((eq mode :warning) (then self nil (lambda (reason) (print reason))))
        ((eq mode :silence) self)))

(finish (then (promise (lambda (re rj) (funcall rj "err")))) nil nil))
```

And the method `finish` has been provided by this library.
