(println "Hello world!")
(defun fact(x) 
  (let ((i 1) (x2 x))
    (loop (neq x2 0) 
       (set! i (* i x2))
       (set! x2 (- x2 1))
    )
    i
    ))
(println (* 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
(println (pow 2.0 0.25))
(println (* 1 (sqrt (big-rational 2))))
(println (sqrt 2.0))
(defvar x 100)
(println (cons x x))
(loop (< x 150000)
   (set! x (+ x 1)))