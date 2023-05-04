(set! (*s7* 'heap-size) (* 4 1024000))

(if (not (defined? 'unless))
    (define-macro (unless test . body)
      `(if (not ,test) (begin ,@body))))

(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(let ((f32 (fib 32)))
  (unless (= f32 2178309) ;3524578)
    (format *stderr* "fib ~A~%" f32)))


(define (fibr n)
  (if (> n 2)
      (+ (fibr (- n 1))
         (fibr (- n 2)))
      1))

(let ((f32 (fibr 32)))
  (unless (= f32 2178309) ;3524578)
    (format *stderr* "fibr ~A~%" f32)))


(define (fibc n)
  (cond ((< n 1) n) ; dumb buts hits oprec_cond_a_a_a_a_opla_laq
	((< n 2) n)
	(else (+ (fibc (- n 1))
		 (fibc (- n 2))))))

(let ((f32 (fibc 32)))
  (unless (= f32 2178309) ;3524578)
    (format *stderr* "fibc ~A~%" f32)))


(define (tfib n a b) ; tail-call version, a=1 b=1 initially
   (if (= n 0)
       a
       (if (= n 1)
           b
           (tfib (- n 1) b (+ a b)))))

;;; overflow at (tfib 92): (tfib 92) 12200160415121877000.0, (tfib 91) 7540113804746346429 [this numbering is off-by-1]
;;;                              93: 12200160415121876738          92: 7540113804746346429 http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibtable.html

(define (dfib Z)             ; do-loop equivalent to tfib
  (do ((a 1 b)
       (b 1 (+ a b))
       (n Z (- n 1)))
      ((< n 2)
       (if (= n 0) a b))))

(define (dofib n)            ; another do-loop (twice as fast as tfib)
  (if (< n 2)
      1
      (do ((a 1)
	   (b 1)
	   (c 0)
	   (i 1 (+ i 1)))
	  ((= i n) b)
	(set! c b)
	(set! b (+ a b))
	(set! a c))))

(unless (= (dfib 91) 7540113804746346429)
  (format *stderr* "(dfib 91): ~D~%" (dfib 91)))
(unless (= (dofib 91) 7540113804746346429)
  (format *stderr* "(dofib 91): ~D~%" (dofib 91)))
(unless (= (tfib 91 1 1) 7540113804746346429)
  (format *stderr* "(tfib 91 1 1): ~D~%" (tfib 91)))

(define size 10000)

(define (test1)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (dfib 91)))

(define (test2)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (dofib 91)))

(define (test3)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (tfib 91 1 1)))

(test1)
(test2)
(test3)


(define* (tfib* n (a 1) (b 1)) ; tail-call version using lambda*
   (if (= n 0)
       a
       (if (= n 1)
           b
           (tfib* (- n 1) b (+ a b)))))

(define (test31)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (tfib* 91)))

(test31)


(define (trib n)
  (if (< n 3)
      1
      (+ (trib (- n 1))
         (trib (- n 2))
         (trib (- n 3)))))

(let ((f32 (trib 26)))
  (unless (= f32 3311233)
    (format *stderr* "trib ~A~%" f32)))

;; tc is much faster:
(define* (ttrib n (a 1) (b 1) (c 1))
  (if (= n 0)
      a
      (if (= n 1)
	  b
	  (if (= n 2)
	      c
	      (ttrib (- n 1) b c (+ a b c))))))

(let ((f32 (ttrib 26)))
  (unless (= f32 3311233)
    (format *stderr* "ttrib ~A~%" f32)))


(define* (tribc n (a 1) (b 1) (c 1))
  (case n
    ((0) a)
    ((1) b)
    ((2) c)
    (else (tribc (- n 1) b c (+ a b c)))))

(let ((f32 (tribc 26)))
  (unless (= f32 3311233)
    (format *stderr* "tribc ~A~%" f32)))


(define all-coins '(50 25 10 5 1)) 

(define (cc amount kinds-of-coins) 
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (null? kinds-of-coins)) 0) 
        (else (+ (cc amount (cdr kinds-of-coins)) 
                 (cc (- amount (car kinds-of-coins)) kinds-of-coins))))) 

(define (count-change amount) 
  (cc amount all-coins)) 

(let ((coins (count-change 400)))
  (unless (= coins 26517)
    (format *stderr* "cc ~A~%" coins)))

(define (add lst)
  (let loop ((p lst)
	     (sum 0))
    (if (null? p)
	sum
	(loop (cdr p) (+ sum (car p))))))

(define big-list (make-list 10000 1))

(define (more-add)
  (let ((lst big-list))
    (do ((i 0 (+ i 1)))
	((= i 1000))
      (add lst))))
(more-add)

(define (adder lst)
  (letrec ((add1 (lambda (L sum)
		   (if (pair? L)
		       (add1 (cdr L) (+ sum (car L)))
		       sum))))
    (add1 lst 0)))

(define (more-adder)
  (do ((lst big-list)
       (i 0 (+ i 1)))
      ((= i 1000))
    (adder big-list)))

(more-adder)

(define (got-symbol lst)
  (and (pair? lst)
       (or (symbol? (car lst))
	   (got-symbol (cdr lst)))))

(define (more-symbol)
  (let ((lst big-list))
    (do ((i 0 (+ i 1)))
	((= i 1000))
      (got-symbol lst))))
(more-symbol)

(set! big-list #f)

;;; add local-slot do cases to s7test

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(let ((n (ack 3 8)))
  (unless (= n 2045)
    (format *stderr* "ack ~A~%" n)))

(define (tree-eq? a b)
  (if (not (pair? a))
      (eq? a b)
      (and (pair? b)
	   (tree-eq? (car a) (car b))
	   (tree-eq? (cdr a) (cdr b)))))

(define tree '((a b) (c d e) (f) () (g h i j) (k (l m (n o)) p) (q ((r) s) (((t (u) v) w) x) y) z))
(define (more-eq)
  (do ((i 0 (+ i 1)))
      ((= i 100000))
    (tree-eq? tree tree)))
(more-eq)


(define (counts x)
  (cond ((= x 0) 0)
	(else (+ 1 (counts (- x 1))))))

(define (more-counts)
  (do ((i 0 (+ i 1)))
      ((= i 1000) (counts 1000))
    (counts 1000)))

(let ((result (more-counts)))
  (unless (= result 1000)
    (format *stderr* "counts ~A~%" result)))


(define (counts1 x)
  (if (= x 0) 0
      (+ 1 (counts1 (- x 1)))))

(define (more-counts1)
  (do ((i 0 (+ i 1)))
      ((= i 1000) (counts1 1000))
    (counts1 1000)))

(let ((result (more-counts1)))
  (unless (= result 1000)
    (format *stderr* "counts1 ~A~%" result)))


(define (counts2 x)
  (if (> x 0)
      (+ 1 (counts2 (- x 1)))
      0))

(define (more-counts2)
  (do ((i 0 (+ i 1)))
      ((= i 1000) (counts2 1000))
    (counts2 1000)))

(let ((result (more-counts2)))
  (unless (= result 1000)
    (format *stderr* "counts2 ~A~%" result)))


(define (counts3 x y)
  (cond ((= x 0) y)
	(else (+ 1 (counts3 (- x 1) (+ y 1))))))

(define (more-counts3)
  (do ((i 0 (+ i 1)))
      ((= i 1000) (counts3 1000 0))
    (counts3 1000 0)))

(let ((result (more-counts3)))
  (unless (= result 2000)
    (format *stderr* "counts3 ~A~%" result)))


(define (counts4 x y)
  (cond ((= x 0) y)
	((negative? x) 0)
	(else (+ 1 (counts4 (- x 1) (+ y 1))))))

(define (more-counts4)
  (do ((i 0 (+ i 1)))
      ((= i 500) (counts4 1000 0))
    (counts4 1000 0)))

(let ((result (more-counts4)))
  (unless (= result 2000)
    (format *stderr* "counts4 ~A~%" result)))


(define (counts5 x)
  (if (<= x 0)
      0
      (+ 1 (counts5 (- x 1)) (counts5 (- x 2)))))

(let ((result (counts5 30)))
  (unless (= result 2178308)
    (format *stderr* "counts5 ~A~%" result)))


(define (counts6 x)
  (if (> x 0)
      (+ 1 (counts6 (- x 1)) (counts6 (- x 2)))
      0))

(let ((result (counts6 30)))
  (unless (= result 2178308)
    (format *stderr* "counts6 ~A~%" result)))



(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)
