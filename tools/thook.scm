;;; simple hook timing tests

(let ((H (make-hook 'x)))
  
  (set! (hook-functions H)
	(list (lambda (h)
		(if (not (let? h))
		    (display h)))
	      (lambda (h)
		(unless (let? h) 
		  (newline)))))
  
  (define (thook)
    (do ((i 0 (+ i 1)))
	((= i 1000))
      (H i)))
  
  (H 32)
  (thook)
  
  (set! (car (hook-functions H))
	(lambda (h)
	  (call-with-exit
	   (lambda (return)
	     (return h)))))
  
  (H 32)
  (thook)
  
  (set! (hook-functions H) (cdr (hook-functions H)))
  
  (H 32)
  (thook))


;;; --------------------------------

(let* ((size 100000)
       (v (make-vector size)))
  (define (tmake)
    (do ((i 0 (+ i 1)))
	((= i size))
      (let ((j i))
	(set! (v i) (make-hook 'x 'y))
	(set! (hook-functions (v i)) 
	      (list (lambda (h) 
		      (set! (h 'result) j)))))))
  
  (define (trun)
    (do ((i 0 (+ i 1)))
	((= i size))
      (unless (= ((v i) 0) i)
	(format *stderr* "~D: ~D~%" i ((v i) 0)))
      (let ((j i))
	(set! (hook-functions (v i)) 
	      (list (car (hook-functions (v i)))
		    (lambda (h) 
		      (set! (h 'result) (+ (h 'result) j))))))))
  
  (define (trun1)
    (do ((i 0 (+ i 1)))
	((= i size))
      (unless (= ((v i) 0) (* 2 i))
	(format *stderr* "~D: ~D~%" i ((v i) 0)))))
  
  
  (tmake)
  (trun)
  (trun1))


;;; --------------------------------
(let ((hook-result #f))
  (let-temporarily (((hook-functions *unbound-variable-hook*) ; variable
		     (list (lambda (hook)
			     (set! hook-result (hook 'variable))))))
    (define (f)
      (do ((i 0 (+ i 1)))
	  ((= i 1000))
	(let ((val (catch #t (lambda () (+ 1 _an_undefined_variable_i_hope_)) (lambda (type info) type))))
	  (unless (eq? hook-result '_an_undefined_variable_i_hope_)
	    (format *stderr* "unbound variable hook: ~S~%" val)))))
  (f)
  (f)))

(let-temporarily (((hook-functions *missing-close-paren-hook*) ; no locals
		   (list (lambda (h) 
			   (set! (h 'result) 'incomplete-expr)))))
  (define (f)
    (do ((i 0 (+ i 1)))
	((= i 1000))
      (let ((val (catch #t (lambda () (eval-string "(+ 1 2")) (lambda args (car args)))))
	(unless (eq? val 'incomplete-expr)
	  (format *stderr* "missing close paren hook: ~S~%" val)))))
  (f)
  (f))

(let ((hook-result #f))
  (let-temporarily (((hook-functions *load-hook*) ; name
		     (list (lambda (hook)
			     (set! hook-result (hook 'name))))))
    (with-output-to-file "load-hook-test.scm"
      (lambda ()
	(format #t "(define (load-hook-test val) (+ val 1))")))
    (define (f)
      (do ((i 0 (+ i 1)))
	  ((= i 1000))
	(load "load-hook-test.scm")
	(unless (equal? hook-result "load-hook-test.scm")
	  (format *stderr* "load hook: ~S~%" hook-result))))
    (f)
    (f)))

(let ((hook-type #f)
      (hook-data #f))
    (define (f)
      (do ((i 0 (+ i 1)))
	  ((= i 1000))
	(catch #t (lambda () 
		    (let-temporarily (((hook-functions *error-hook*) ; type data -- why does this have to be in the catch?
				       (list (lambda (hook)
					       (set! hook-type (hook 'type))
					       (set! hook-data (apply format #f (hook 'data)))))))
		      (+ 1 #())))
	       (lambda (type info)
		 'error))
	(unless (and (eq? hook-type 'wrong-type-arg)
		     (equal? hook-data "+ second argument, #(), is a vector but should be a number"))
	  (format *stderr* "error-hook: ~S ~S~%" hook-type hook-data))))
    (f)
    (f))


(let ((hook-type #f)
      (hook-data #f))
  (define (f)
    (do ((i 0 (+ i 1)))
	((= i 1000))
      (catch #t ; why can't this be 'read-error?
	(lambda () 
	  (let-temporarily (((hook-functions *read-error-hook*)
			     (list (lambda (hook)
				     (set! hook-type (hook 'type)) 
				     ;; why isn't this 'read-error? #t=unknown_sharp_constant, #f=unknown_string_constant (read_string_constant)
				     (set! hook-data (hook 'data))))))
	    (eval-string "(+ 1 #T)")))
	(lambda (type info)
	  'error))
      (unless (and (eq? hook-type #t)
		   (string=? hook-data "T"))
	(format *stderr* "error-hook: ~S ~S~%" hook-type hook-data))))
  (f)
  (f))

;;; --------------------------------


(let ((H3 (make-hook 'x '(y 32))))
  
  (set! (hook-functions H3)
	(list (lambda (h)
		(set! (h 'result) ((h 'x) (h 'y))))))
  
  (unless (= (H3 (lambda (y) (+ y 1))) 33) 
    (format *stderr* "H3: ~S~%" (H3 (lambda (y) (+ y 1)))))
  
  (define H4 (make-hook 'z))
  
  (set! (hook-functions H4)
	(list (lambda (h)
		(set! (h 'result) (+ (h 'z) (H3 (lambda (y) (+ y 1))))))))
  
  (unless (= (H4 12) 45)
    (format *stderr* "H4: ~S~%" (H4 12)))
  
  (define H5 (make-hook))
  
  (set! (hook-functions H5)
	(list (lambda (h)
		(set! (h 'result) 100))
	      (lambda (h)
		(set! (h 'result)
		      (+ (sqrt (h 'result))
			 (H4 12))))))
  
  (define (f)
    (do ((i 0 (+ i 1)))
	((= i 100000))
      (unless (= (H5) 55)
	(format *stderr* "H5: ~S~%" (H5)))))
  (f)
  (f))




(exit)
