
(define size 500000)

(define-macro (m2 a b) `(+ ,a ,@b 1))
(define (f2)
  (let ((x 2)
	(y 0))
    (do ((j 0 (+ j 1)))
	((= j 1))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(set! y (m2 x (x x)))
	(if (not (= y (+ (* 3 x) 1)))
	    (format *stderr* "y: ~A~%" y))))))
(f2)


(define (f2d)
  (let ((x 2)
	(y 0)
	(size2 50000))
    (do ((j 0 (+ j 1)))
	((= j 1))
      (do ((i 0 (+ i 1)))
	  ((= i size2))
	(set! y ((macro (a b) `(+ ,a ,@b 1)) x (x x)))
	(if (not (= y (+ (* 3 x) 1)))
	    (format *stderr* "y: ~A~%" y))))))
(f2d)


(define-expansion (m3 a b) `(+ ,a ,@b 1))
(define (f3)
  (let ((x 2)
	(y 0))
    (do ((j 0 (+ j 1)))
	((= j 1))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(set! y (m3 x (x x)))
	(if (not (= y (+ (* 3 x) 1)))
	    (format *stderr* "y: ~A~%" y))))))
(f3)


(define (f4 m) (+ 2 (m 3)))
(define (f4-test mx)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f4 mx)))

(define-macro (m4 a) `(+ ,a 1))
(f4-test m4)

(define-macro (m5 a . b) `(+ ,a ,@b))
(f4-test m5)

(define-macro* (m6 (a 21)) `(+ ,a 1))
(f4-test m6)

(define (f5-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (m5 1 3 4 5)))
(f5-test)

(define-macro (m61 a b) (cons '+ (cons a b)))
(define (f61-test mx)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (mx 1 (3 4 5))))
(f61-test m61)

(define (f7-test mx)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (mx 1 ())))
(f7-test m61)

(define (f8-test mx)
  (let loop ((ctr size))
    (mx 1 3 4 5)
    (if (= ctr 0)
	0
	(loop (- ctr 1)))))
(f8-test m5)


(define-macro (trace f)
  (let ((old-f (gensym)))
    `(define ,f 
       (let ((,old-f ,f))
	 (apply lambda 'args 
		`((format #f "(~S ~{~S~^ ~}) -> " ',',f args)
		  (let ((val (apply ,,old-f args))) 
		    (format #f "~S~%" val) 
		    val)))))))

(define (trace-test)
  (let loop ((count 0))
    (if (< count 30000) ; not 'when for old snd timings
	(let ((f1 (lambda (x y z) (+ x y z))))
	  (trace f1) ; op_macro_d I think
	  (f1 count count count)
	  (loop (+ count 1))))))

(trace-test)


;;; recursive macros

(let ()
  (define-macro (m1 a b)
    `(if (> ,a 0)
	 (m1 (- ,a 1) (+ ,b 1))
	 (+ ,a ,b)))

  (define (test-m1)
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (m1 3 0)))
  
  (test-m1))


(let ((m2 (macro (a b)
	    `(if (> ,a 0)
		 (m2 (- ,a 1) (+ ,b 1)) ; m2 ok so named macro let is not needed
		 (+ ,a ,b)))))

  (define (test-m2)
    (do ((i 0 (+ i 1)))
	((= i 10000))
      (m2 3 0)))
  
  (test-m2))


#|
;; undefined m3
(let ((m3 (lambda (a b)
	    (if (> a 0)
		 (m3 (- a 1) (+ b 1)) ; m3 undefined so we need letrec/named let etc
		 (+ a b)))))

  ;(display (m3 3 0)) (newline)
  #f)
|#


(let ()
  (define-macro (tak x y z)           ; [1992], expansion is same (1980) even if declared globally, functional tak is [7]
    `(if (not (< ,y ,x))
	 ,z
	 (tak (tak (- ,x 1) ,y ,z)
              (tak (- ,y 1) ,z ,x)
              (tak (- ,z 1) ,x ,y))))
  
  (define (test-tak)
    (do ((i 0 (+ i 1)))
	((= i 200))
      (tak 6 4 2)))
  
  (test-tak))


(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)
