(set! (*s7* 'heap-size) 512000)

(let ()
  (define size 1000000)


  (define-constant (f1)
    (let-temporarily (((*s7* 'openlets) #f))
      (*s7* 'openlets)))
  (define (f11)
    (do ((i 0 (+ i 1)))
	((= i size))
      (f1)))
  
  (f11)
  

  (define-constant (f2 x)
    (set! (*s7* 'safety) 1)
    (+ x 1)
    (set! (*s7* 'safety) 0))
  
  (define (f21)
    (do ((i 0 (+ i 1)))
	((= i size))
      (f2 i)))
  
  (f21) ; 707 -> 386 -> 357
  

  (define-constant (f3 x)
    (+ (*s7* 'safety) (*s7* 'print-length)))
  
  (define (f31)
    (do ((i 0 (+ i 1)))
	((= i size))
      (f3 i)))

  (f31) ; 211 -> 180(print-length)


  (define (f6)
    (let ((obj (openlet (inlet 'value -1 
			       'abs (lambda (x)
				      (let-temporarily (((*s7* 'openlets) #f))
					(abs (x 'value))))))))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(abs obj))))

  (f6) ; 628! -> 425


  (define (f7)
    (let ((obj (openlet (inlet 'value -1 
			       'abs (lambda (x) 
				      (magnitude (x 'value)))))))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(abs obj)))) ; eval -> apply_c_function -> magnitude_p_p -> s7_let_ref + method_or_bust etc [50]
  (f7) ; 360 -> 342 [find_and_apply_method/apply_method_closure]
  
  
  (define (f12)
    (let ((obj -1))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(abs obj))))
  (f12) ; 17: 10-20 times faster than f1, opt_dotimes -> opt_i_i_s_abs

  
  (define (f13)
    (let ((obj (openlet (let ((value -1))
			  (define (abs x) (magnitude value))
			  (curlet)))))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(abs obj)))) ; eval -> apply_c_function -> magnitude_p_p -> method_or_bust etc g_abs find_method_with_let
  (f13) ; not much faster 314
  
  
  (define (f14)
    (let ((obj -1))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(magnitude obj))))
  (f14) ; 38
  
  ;;abs obj -> abs_method: still need find_method->eval and g_abs (saves 2+~15 of 360!), might preset method = 14 -> 10%?
  
  
  (define (f15)
    (let ((obj (openlet (sublet (inlet 'abs (lambda (x) (magnitude (x 'value))))
			  'value -1))))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(abs obj)))) ; eval -> apply_c_function -> magnitude_p_p -> s7_let_ref + method_or_bust etc
  (f15) ; 360
  
  
  (define-constant (f8 x)
    (if (let? x)
	((let-ref x 'f82) x)
	(+ x 1)))
  
  (define (f81)
    (let ((obj (openlet (inlet 'value -1 
			       'f82 (lambda (x)
				     (+ (x 'value) 1))))))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(f8 obj))))
  
  (f81) ; 505
  
  
  (define f9 (dilambda
	      (lambda (x)
		(if (let? x)
		    ((let-ref x 'f9) x)
		    (+ x 1)))
	      (lambda (x y)
		(if (let? x)
		    ((let-ref x 'set-f9) x y)
		    'oops))))
  
  (define (f91)
    (let ((obj (openlet (inlet 'value -1 
			       'f9 (lambda (x)
				     (+ (x 'value) 1))
			       'set-f9 (lambda (x y)
					 (set! (x 'value) y))))))
      (display (f9 obj)) (newline) ; 0
      (set! (f9 obj) 32)
      (display (f9 obj)) (newline) ; 32
      
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(set! (f9 obj) i))))
  
  (f91) ; 1142
  
  
  (define (f10)
    (let ((obj (openlet (inlet 'value -1 
			       'abs (lambda (x) 
				      (magnitude (x 'value)))
			       'set-abs (lambda (x y)
					  (set! (x 'value) y))))))
      (set! (setter abs)
	    (lambda (x y)
	      (if (let? x)
		  ((let-ref x 'set-abs) x y)
		  'oops)))
      (display (abs obj)) (newline) ; 1
      (set! (abs obj) 32)
      (display (abs obj)) (newline) ; 32
      
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(set! (abs obj) i))))
  
  (f10) ; 1142
)

(set! (setter abs) #f)

(exit)
