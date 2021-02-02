(defpackage :rv.ctfp.ch1.solutions
 (:use :cl))

(in-package :rv.ctfp.ch1.solutions)

(defun id (x)
 x)

(defun compose (&rest fns)
  (destructuring-bind (fn . rest-fns) (reverse fns)
    (lambda (&rest args)
      (reduce #'(lambda (v f) (funcall f v))
              rest-fns
              :initial-value (apply fn args)))))

(defun run-tests ()
  (let ((add2 (compose #'(lambda (x) (+ x 2)) #'id)))
    (assert (= (id 1) 1))
    (assert (= (funcall add2 1) 3))))
