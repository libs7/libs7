(require reactive.scm)

(define (t)
  (let ((x 0))
    (do ((i 0 (+ i 1)))
	((= i 500))
      (let ((a 1))
	(reactive-set! x (* 2 a))
	(set! a 2)
	(if (not (= x 4))
	    (format *stderr* "x: ~D ~D~%" x a)))
      (let ((a 1))
	(reactive-set! a (* 2 x))
	(set! x 2)
	(if (not (= a 4))
	    (format *stderr* "a: ~D ~D~%" a x)))
      (let ((a 3))
	(set! a 2))
      (if (not (= x 2))
	  (format *stderr* "x: ~D~%" x))
      (let ((a 1))
	(do ((k 0 (+ k 1)))
	    ((= k 1))
	  (let ((b 2))
	    (do ((j 0 (+ j 1)))
		((= j 10))
	      (let ((c 3))
		(reactive-set! x (+ a b c))
		(set! c 2)
		(if (not (= x 5))
		    (format *stderr* "set: ~S ~S ~S ~S~%" x a b c))))
	    (set! b 3)
	    (if (not (= x 6))
		(format *stderr* "set: ~S ~S ~S~%" x a b))))
	(set! a 4)
	(if (not (= x 9))
	    (format *stderr* "set: ~S ~S~%" x a)))
      (reactive-let ((y (* x 2))
		     (z (+ (* x 3) 1)))
	(set! x 1)
	(if (or (not (= y 2))
		(not (= z 4)))
	    (format *stderr* "let: ~D ~D ~D~%" x y z)))
      (reactive-let* ((y (* x 2))
		      (z (+ (* x 3) y)))
	(set! x 1)
	(if (or (not (= y 2))
		(not (= z 5)))
	    (format *stderr* "let*: ~D ~D ~D~%" x y z)))
      
      (if (zero? (remainder i 9)) (gc)))))

(t)

;;; --------------------------------------------------------------------------------

(define size 1000000)

(define (f1)
  (let ((x 0))
    (set! (setter 'x) integer?)
    (do ((i 0 (+ i 1)))
	((= i size) x)
      (set! x (+ x 1)))))

(f1)


(define (f2)
  (let ((x 0))
    (set! (setter 'x) rational?)
    (do ((j 0 (+ j 1)))
	((= j 100000) x)
      (do ((i 1 (+ i 1)))
	  ((= i 10))
	(set! x (+ x (/ i))))))) ;if (/ i) and size (or > 100) -> 14.392725722864782 -- ratio overflows bignum

(f2)


(define (f3)
  (let ((x 128))
    (set! (setter 'x) (lambda (s v) 
			(if (and (integer? v)
				 (<= 0 v 128))
			    v
			    (error 'wrong-type-arg "(set! ~A ~S) but ~S should be an integer between 0 and 128" s v v))))
    (do ((i 0 (+ i 1)))
	((= i size) x)
      (set! x (modulo (+ x 1) 128)))))
  
(f3)


(define (f4 x) 
  (let ((y x)) 
    (set! (setter 'y) integer?)
    (set! y (+ y 1))))

(define (f4-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f4 i)))

(f4-test)


(require stuff.scm)

(define (f5 x)
  (typed-let ((y x integer?))
    (set! y (+ y 1))))

(define (f5-test)
  (do ((i 0 (+ i 1)))
      ((= i (/ size 10)))
    (f5 i)))

(f5-test)


(define (f6 x)
  (typed-let ((y x (lambda (s v) 
		     (if (integer? v) 
			 v 
			 (error 'wrong-type-arg "~S is not an integer" v)))))
    (set! y (+ y 1))))

(define (f6-test)
  (do ((i 0 (+ i 1)))
      ((= i (/ size 10)))
    (f6 i)))

(f6-test)



;;; --------------------------------------------------------------------------------
(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)


;; 3097 if initial heap=64k (3270 if 128k), 3036 if 32k
