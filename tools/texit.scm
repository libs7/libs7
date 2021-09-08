(define size 100000)


(define (memb x ls)
  (call-with-exit
   (lambda (break)
     (do ((ls ls (cdr ls)))
	 ((null? ls) #f)
       (if (eq? x (car ls))
	   (break ls))))))

(define (membtest)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (memb 'd '(a b c))
    (memb 'b '(a b c))))

(membtest)


(define (find-if f sequence)
  (call-with-exit
   (lambda (return)
     (for-each (lambda (arg)
		 (if (f arg)
		     (return arg)))
	       sequence)
     #f)))

(define (find-if-test)
  (let ((lst '((#f 1) (#f 2) (3 3) (#f 4))))
    (do ((i 0 (+ i 1)))
	((= i size))
      (find-if car lst))))

(find-if-test)


(define-expansion (while test . body)      ; while loop with predefined break and continue.
  `(call-with-exit
    (lambda (break)
      (let loop ()
	(call-with-exit
	 (lambda (continue)
	   (do ()
	       ((not ,test) (break))
	     ,@body)))
	(loop)))))

(define (while-test)
  (do ((i 0)
       (k 0 (+ k 1)))
      ((= k size))
    (while (< i 100)
      (when (= i 3) (set! i 0) (break))
      (set! i (+ i 1))
      (if (> i 3) (display "oops:while\n")))))

(while-test)


(define (dwfib n)
  (dynamic-wind
      (lambda () 
	(if (negative? n) (display "oops")))
      (lambda ()
	(if (<= n 2)
	    n
	    (+ (dwfib (- n 1))
               (dwfib (- n 2)))))
      #f))

(dwfib 28)


(define lsize 100000)

(define (f x a) (x a))

(define (g1 lsize)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    (f (lambda (b) (+ b 1)) 10)))

(define (g2 lsize)
  (let loop ((i lsize))
    (when (>= i 0)
      (f (lambda (b) (+ b 1)) 10)
      (loop (- i 1)))))

(define (g3 lsize)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    (f (vector 11) 0)))

(g1 lsize)
(g2 lsize)
(g3 lsize)


(define (g31 g30 lsize)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    (g30 i)))

(g31 (lambda (x) (+ x 2)) lsize) ; fastest (more than 2x)


(define (f2 a x) (x a)) ; op_closure_af?

(define (g32 lsize)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    (f2 10 (lambda (b) (+ b 1)))))

(g32 lsize)


(define (f1 x1 x2 a) ; eval_args [unk(unk a) OP_S_opSq?]
  (x2 (x1 a)))

(define (f11 x1 x2 a) ; no eval_args [eval->op_safe_c_pp]
  (+ (x2 a) (x1 a)))

(display (f1 (lambda (b) (+ b 1)) (lambda (c) (* c 2)) 10)) (newline)

(define (g4 lsize)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    (f1 (lambda (b) (+ b 1)) (lambda (c) (* c 2)) 10)))

(define (g5 lsize)
  (let loop ((i lsize))
    (when (>= i 0)
      (f1 (lambda (b) (+ b 1)) (lambda (c) (* c 2)) 10)
      (loop (- i 1)))))

(define (g6 lsize)
  (let loop ((i lsize))
    (when (>= i 0)
      (f1 (lambda* (b (d 1)) (+ b d)) (macro (c) `(* ,c 2)) 10)
      (loop (- i 1)))))

(display (f1 (lambda* (b (d 1)) (+ b d)) (macro (c) `(* ,c 2)) 10)) (newline)
(display (f1 (lambda* (b (d 1)) (+ b d)) (lambda (c) (* c 2)) 10)) (newline)

(g4 lsize) ; (2x slower than g1)
(g5 lsize) 
(g6 lsize)  ; very slow, eval args (2x slower than g5)


(define (tc-1 x y) 
  (if (pair? x)
      (tc-1 (cdr x) y) 
      (begin (vector-set! y 0 32) y)))

(define (g7)
  (equal? (tc-1 '(1 2 3) #(1 2 3)) #(32 2 3)))

(display (g7)) (newline)

(define (g71)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    (g7)))

(g71)


(define (g80)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    ((lambda () (+ i 1)))))

(g80)

(define (g81)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    ((lambda (x) (+ x 1)) i)))

(g81)

(define (g82)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    ((lambda (x y) (+ x y)) i (+ i 1))))

(g82)


(define (g9)
  (do ((i 0 (+ i 1)))
      ((= i lsize))
    (g10 + (lambda (x y) (* x y)))))

(define (g10 f1 f2)
  (- (f1 1 2) (f2 3 4)))

(display (g10 + (lambda (x y) (* x y)))) (newline)

(g9)
