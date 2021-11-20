;;; c-object timing tests using s7test blocks

(define size 200000)

(load "s7test-block.so" (sublet (curlet) (cons 'init_func 'block_init)))


(define (f1 b)
  (b 0)) ; op_implicit_c_object_ref_a and g_block_ref -> s7_c_object_type|value, op_unknown_a

(define (ft1)
  (let ((b (block 1 2 3)))
    (f1 b)))

(unless (= (ft1) 1) (format *stderr* "(f1 b): ~S~%" (ft1)))

(define (ftest1)
  (let ((b (block 1 2 3)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (f1 b))))

(ftest1)


(define (f2 b)
  (block-ref b 0)) ; g_block_ref etc

(define (ft2)
  (let ((b (block 1 2 3)))
    (f2 b)))

(unless (= (ft2) 1) (format *stderr* "(f2 b): ~S~%" (ft2)))

(define (ftest2)
  (let ((b (block 1 2 3)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (f2 b))))

(ftest2)


(define (f3 b val)
  (set! (b 0) val)) ; g_block_set [set_pair + set_pair_p_3 etc]

(define (ft3)
  (let ((b (block 1 2 3)))
    (f3 b 32)
    (b 0)))

(unless (= (ft3) 32) (format *stderr* "(f3 b 32): ~S~%" (ft3)))

(define (ftest3)
  (let ((b (block 1 2 3)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (f3 b i))))

(ftest3)


(define (ftest4)
  (let ((b (make-block 100))
	(b1 #f)
	(b2 #f))
    (do ((i 0 (+ i 1)))
	((= i size))
      (fill! b 1.0)                     ; g_block_fill 
      (set! b (reverse! b))             ; g_block_reverse_in_place
      (unless (= (length b) 100)        ; c_object_length_to_int? -> g_block_length
	(format *stderr* "oops"))
      (set! b1 (copy b))                ; g_block_copy, g_make_block?
      (unless (and (equal? b b1)        ; g_blocks_are_equal
		   (equivalent? b b1))  ; g_blocks_are_equivalent, vector_equivalent?? + s7_make_float_vector_wrapper
	(format *stderr* "oops"))
      (set! b2 (append b b1))           ; g_block_append
      (unless (and (c-object? b2)
		   (= (length b2) 200))
	(format *stderr* "oops")))))

(ftest4)


(define (ftest5)
  (let ((b (make-block 10))
	(b1 (make-block 10)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (do ((j 0 (+ j 1)))
	  ((= j 10))
	(set! (b1 j) (b j)))))) ; (?) block_set_d_7pid block_ref_d_7pi

(ftest5)


(define (ftest6)
  (let ((b (make-block 10))
	(b1 (make-block 10)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (do ((j 0 (+ j 1)))
	  ((= j 10))
	(block-set! b1 j (block-ref b j))))))

(ftest6)


(define (ftest7)
  (let ((b (make-block 10)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (do ((j 0 (+ j 1)))
	  ((= j 10))
	(set! (b j) (* 2.0 (b j)))))))

(ftest7)

      
(define (f8 b x)
  (b x)) 

(define (ftest8)
  (let ((b (block 1 2 3)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (f8 b 2))))

(ftest8)


(define (ftest9)
  (let ((b (make-block 10)))
    (do ((i 0 (+ i 1)))
	((= i (/ size 10)))
      (map abs b))))

(ftest9)


(define (ftest10)
  (let ((b (make-block 10)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (let-temporarily (((b 0) (* i 2.0)))
	(unless (= (b 0) (* i 2.0))
	  (display 'oops *stderr*))))))

(ftest10)


(define (ftest11)
  (let* ((b (make-block 10))
	 (iter (make-iterator b)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (do ((j 0 (+ j 1)))
	  ((= j 10))
	(iterate iter)))))

(ftest11)


(define (ftest12)
  (let ((b (make-block 10)))
    (do ((i 0 (+ i 1)))
	((= i (/ size 10)))
      ;(sort! b >)             ; needs sort! method (s7.h does not try to export g_sort)
      (object->let b)
      (object->string b))))

(ftest12) 


(define f13
  (let ((L (list 0)))
    (lambda (b)
      (apply b L))))      ; apply_c_object

(define (ftest13)
  (let ((b (block 1 2 3)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (f13 b))))

(ftest13)


(define (randomize-block uv)
  (let* ((len (length uv))
	 (v (copy uv))
	 (nv (make-block len))
	 (min-i 0))
    (fill! nv +nan.0)
    (do ((i 0 (+ i 1))
	 (r (random len) (random len)))
	((= i len))
      (if (nan? (block-ref v r))
	  (do ((k min-i (+ k 1)))
	      ((or (= k len)
		   (not (nan? (block-ref v k))))
	       (if (= k len)
		   (format *stderr* "can't find a value for ~S!\n" r)
		   (begin
		     (set! min-i (+ k 1))
		     (block-set! nv i (block-ref v k))
		     (block-set! v k +nan.0)))))
	  (begin
	    (block-set! nv i (block-ref v r))
	    (block-set! v r +nan.0))))
    nv))

;(display (randomize-block (block 1 2 3 4 5 6))) (newline)

(define (rtest)
  (let ((v (make-block 100000)))
    (do ((i 0 (+ i 1)))
	((= i 100000))
      (block-set! v i i))
    (randomize-block v)))

(rtest)


(define (fload) ; check s7_make_c_type et al
  (do ((i 0 (+ i 1)))
      ((= i 2000))
    (load "s7test-block.so" (sublet (curlet) (cons 'init_func 'block_init)))))

(fload)



(exit)
