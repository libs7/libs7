(set! (*s7* 'heap-size) (* 3 1024000))
(set! (hook-functions *unbound-variable-hook*) ())
(set! (*s7* 'print-length) 6)
;(set! (*s7* 'gc-stats) #t)

(when (provided? 'snd)
  (format *stderr* "this won't work in Snd!~%")
  (exit))

(let ((max-args 3))
  (define-constant one 1)
  
  (define-constant auto-constants (list #f #t () #\a (/ (*s7* 'most-positive-fixnum)) (/ -1 (*s7* 'most-positive-fixnum)) 1.5+i
					"hi455" :key hi: 'hi (list 1) (list 1 2) (cons 1 2) (list (list 1 2)) (list (list 1)) (list ()) #() 
					1/0+i 0+0/0i 0+1/0i 1+0/0i 0/0+0i 0/0+0/0i 1+1/0i 0/0+i cons ''2 
					1+i 1+1e10i 1e15+1e15i 0+1e18i 1e18 #\xff (string #\xff) 1e308 
					;(*s7* 'most-positive-fixnum) (*s7* 'most-negative-fixnum) (- (*s7* 'most-positive-fixnum) 1) (+ (*s7* 'most-negative-fixnum) 1)
					;most-negative-fixnum hangs expt in gmp
					-1 0 0.0 1 1.5 1.0-1.0i 3/4 #\null -63 (make-hash-table) (hash-table '(a . 2) '(b . 3))
					'((1 2) (3 4)) '((1 (2)) (((3) 4))) "" (list #i(1) "1") '(1 2 . 3) (list (cons 'a 2) (cons 'b 3))
					#i(1 2) (vector 1 '(3)) (let ((x 3)) (lambda (y) (+ x y))) abs 'a 'b one
					(lambda args args) (lambda* ((a 3) (b 2)) (+ a b)) (lambda () 3)
					(sublet () 'a 1) ;(rootlet)
					*load-hook*  *error-hook* 
					(random-state 123)
					quasiquote macroexpand begin let letrec* if case cond (call-with-exit (lambda (goto) goto))
					(with-baffle (call/cc (lambda (cc) cc)))
					(string #\a #\null #\b) #2d((1 2) (3 4)) (inlet 'a 2 'b 3)
					#<undefined> #<unspecified> (make-int-vector 3) (make-float-vector 3 -1.4)
					(make-vector '(2 3) "hi") #("hiho" "hi" "hoho") (subvector (make-int-vector '(2 3) 1))
					(subvector (subvector (make-float-vector '(2 3) 1.0) 0 6) 0 4 '(2 2))
					(vector-ref #2d((#i(1 2 3)) (#i(3 4 5))) 0 0) (define-macro (m a) `(+ ,a 1))
					(c-pointer 0) (c-pointer -1) :readable :else (define-bacro* (m (a 1)) `(+ ,a 1))
					(byte-vector 0 1 2) (byte-vector) (byte-vector 255 0 127) (make-iterator #((a . 2)))
					(lambda (dir) 1.0) (float-vector) (make-float-vector '(2 32)) 
					'((a . 1)) #i(1) '((((A . B) C . D) (E . F) G . H) ((I . J) K . L) (M . N) O . P)
					#u(0 1 2) (openlet (inlet 'abs (lambda (x) (- x))))
					(make-iterator (list 1 2 3)) (make-iterator "1") #<eof> #r2d((.1 .2) (.3 .4))
					(dilambda (lambda () 1) (lambda (a) a))
					(gensym)))
  
  (define car-auto-constants (car auto-constants))
  (define-constant cdr-auto-constants (cdr auto-constants))
  
  (define low 0)
  (define-constant auto-arglists (vector (make-list 1) (make-list 2) (make-list 3) (make-list 4) (make-list 5) (make-list 6)))
  
  (define-constant (autotest func args args-now args-left sig)
    ;; args-left is at least 1, args-now starts at 0, args starts at ()
    ;; (format *stderr* "~A: ~D ~D (~D ~D): ~A~%" func (length args) args-now low args-left args)
    ;; (if (pair? args) (format *stderr* "~A " (car args)))
    
    (call-with-exit
     (lambda (quit)
       (if (>= args-now low)
	   (catch #t 
	     (lambda () 
					;(format *stderr* "args: ~A~%" args)
	       (apply func args))
	     (lambda (type info)
	       (if (and (positive? args-now)
			(memq type '(wrong-type-arg wrong-number-of-args out-of-range syntax-error io-error
				     division-by-zero format-error missing-method error invalid-escape-function)))
		   (quit)))))
       
       (let ((c-args (vector-ref auto-arglists args-now)))
	 (copy args c-args)
	 
	 (let ((p (list-tail c-args args-now))
	       (checker (and (pair? sig) (car sig)))) ; see map-values
	   
	   (if (= args-left 1)
	       (call-with-exit
		(lambda (quit)
		  (set-car! p car-auto-constants)
		  (catch #t
		    (lambda ()
					;(format *stderr* "c-args: ~A~%" c-args)
		      (apply func c-args))
		    (lambda (type info)
		      (if (or (memq type '(wrong-number-of-args out-of-range syntax-error io-error
					   division-by-zero format-error error missing-method invalid-escape-function))
			      (and (eq? type 'wrong-type-arg)
				   (pair? (cdr info))
				   (pair? (cddr info))
				   (integer? (caddr info)) ; if just 1 arg, arg num can be omitted
				   (< (caddr info) low)))
			  (quit))))
		  
		  (if checker
		      (for-each
		       (lambda (c)
			 (when (checker c)
			   (catch #t 
			     (lambda () 
			       (set-car! p c)
			       (apply func c-args))
			     (lambda any 'error))))
		       cdr-auto-constants)
		      (for-each
		       (lambda (c)
			 (catch #t 
			   (lambda () 
			     (set-car! p c)
			     (apply func c-args))
			   (lambda any 'error)))
		       cdr-auto-constants))))
	       
	       (let ((sig1 (if (pair? sig) (cdr sig) ()))
		     (c-args1 c-args)
		     (args-now1 (+ args-now 1))
		     (args-left1 (- args-left 1)))
		 (if checker
		     (for-each
		      (lambda (c)
			(when (checker c)
			  (set-car! p c)
			  (autotest func c-args1 args-now1 args-left1 sig1)))
		      auto-constants)
		     (for-each
		      (lambda (c)
			(set-car! p c)
			(autotest func c-args1 args-now1 args-left1 sig1))
		      auto-constants)))))))))
  
  (define (map-values lst)
    (do ((lst lst (cdr lst)))
	((or (not (pair? lst))
	     (not (car lst))
	     (procedure? (car lst))) 
	 lst)
      (set-car! lst
		(if (symbol? (car lst))
		    (symbol->value (car lst))
		    (and (pair? (car lst))
			 (apply lambda '(x) (list (list 'or (list (caar lst) 'x) (list (cadar lst) 'x)))))))))
  
  (define baddies '(exit emergency-exit abort autotest s7-optimize dynamic-unwind
			 all delete-file system set-cdr! stacktrace test-sym
			 cutlet varlet gc cond-expand reader-cond
			 openlet coverlet eval vector list cons values
			 symbol-table load throw error
			 make-rectangular macro macro* bacro bacro*
			 copy fill! hash-table-set! vector-set! let-set! list-values apply-values immutable!
			 *unbound-variable-hook* *load-hook* *rootlet-redefinition-hook* *missing-close-paren-hook* *read-error-hook*
			 tree-count ; signature is kinda silly here
			 c-define-1 apropos map-values trace-in profile-in
			 define-expansion
			 heap-scan heap-analyze heap-holders heap-holder))
  
  (define (test-sym sym)
    (when (and (not (memq sym baddies))
	       (defined? sym))
      (let* ((f (symbol->value sym))
	     (argn (and (or (procedure? f) (let? f)) (arity f))))
	(if argn
	    (let ((bottom (car argn))
		  (top (min (cdr argn) max-args))
		  (strname (symbol->string sym)))
	      (unless (memv (strname 0) '(#\{ #\[ #\())
		(if (< top bottom)
		    (format *stderr* ";~A (bottom: ~A, top: ~A)...~%" sym bottom top))
		(set! low bottom)
		(if (positive? (cdr argn))
		    (let ((sig (cond ((eq? sym 'append)
				      (let ((lst (list 'list?)))
					(set-cdr! lst lst)))
				     (else (copy (signature f))))))
		      (map-values sig)
		      (autotest f () 0 top (if (pair? sig) (cdr sig) ()))))))))))
  
  (define (all)
    (let ((st (symbol-table)))
      (for-each test-sym st)))
					;(do ((i 0 (+ i 1)) (len (length st))) ((= i 1000)) (test-sym (st (random len))))
					;(test-sym 'object->string)
					;(test-sym 'for-each)
					;(test-sym 'write)
  (all))


(let ((probes (vector #f #t () #\a #<undefined> #<unspecified> #<eof>
		      0 1 2/3 1.0 1+i
		      (list 0)
		      'a set!
		      "11" #(1+i 2+i) #i(312 1234) #r(1.5 2.5) #u(1 2) #2i((1 2) (3 4))
		      (hash-table 'a 1 'b 2)
		      (inlet 'a 1 'b 2)
		      (c-pointer 0)
		      (random-state 1234)
		      (lambda () 1) (lambda* ((a 21)) (+ a 1))
		      (macro (a) `(+ ,a 1)) (macro* ((a 32)) `(+ ,a 1)) quasiquote
		      (bacro (a) `(+ ,a 1)) (bacro* ((a 32)) `(+ ,a 1))
		      (open-output-string) (open-input-string "1234")
		      abs + make-hash-table map port-line-number
		      (call/cc (lambda (c) c)) (call-with-exit (lambda (c) c))
		      (make-iterator '(1 2 3))))
      (inputters '(read read-string read-line read-byte read-char))
      (outputters '(write display write-string write-char write-byte newline))
      (even-args '(hash-table weak-hash-table)))
  
  (define (type-ok probe types)
    (and (pair? types)
	 (or (memq (car types) '(#t values))
	     ((symbol->value (car types)) probe)
	     (type-ok probe (cdr types)))))
  
  (define (check sym)
    (let ((func (symbol->value sym)))
      (unless (or (not (procedure? func))
		  (memq sym baddies))
	(let ((ari (arity func))
	      (sig (signature func))
	      (doc (documentation func)))
	  (unless (or (string=? doc "")
		      (string-position (symbol->string sym) doc))
	      (format *stderr* "~A documentation does not mention it: ~S~%" sym doc))
	  (when (pair? sig)
	    (let ((res-types (if (pair? (car sig)) (car sig) (list (car sig)))))
	      
	      ;; 0 arg
	      (catch #t
		(lambda ()
		  (let ((res (if (memq sym inputters)
				 (let ((val #f)) (with-input-from-string "1234" (lambda () (set! val (func)))) val)
				 (if (memq sym outputters)
				     (let ((val #f)) (with-output-to-string (lambda () (set! val (func)))) val)
				     (func)))))
		    (unless (type-ok res res-types)
		      (format *stderr* "(~A) -> ~S, but ~A is not in ~A~%" sym res (type-of res) res-types))))
		(lambda (type info)
		  (let ((errstr (apply format #f info)))
		    (unless (string-position (symbol->string sym) errstr)
		      (format *stderr* "(~A) -> ~S~%" sym errstr)))
		  (when (and (eq? type 'wrong-number-of-args)
			     (aritable? func 0))
		    (format *stderr* "(~A) -> arg num error but arity: ~S~%" sym ari))))
	      
	      ;; 1 arg
	      (let ((par1-types (and (pair? (cdr sig)) (if (pair? (cadr sig)) (cadr sig) (list (cadr sig))))))
		(for-each 
		 (lambda (probe1)
		   (catch #t
		     (lambda ()
		       (let ((res (if (memq sym inputters)
				      (let ((val #f)) (with-input-from-string "1234" (lambda () (set! val (func probe1)))) val)
				      (if (memq sym outputters)
					  (let ((val #f)) (with-output-to-string (lambda () (set! val (func probe1)))) val)
					  (func probe1)))))
			 (unless (type-ok res res-types)
			   (format *stderr* "(~S ~S) -> ~S, but ~A is not in ~A~%" sym probe1 res (type-of res) res-types))
			 (unless (type-ok probe1 par1-types)
			   (format *stderr* "(~S ~S) ok, but ~A is not in ~S~%" sym probe1 (type-of probe1) par1-types)))
		       
		       ;; 2 args
		       (let ((par2-types (and (pair? (cddr sig)) (if (pair? (caddr sig)) (caddr sig) (list (caddr sig))))))
			 (for-each
			  (lambda (probe2)
			    (catch #t
			      (lambda ()
				(let ((res (if (memq sym inputters)
					       (let ((val #f)) (with-input-from-string "1234" (lambda () (set! val (func probe1 probe2)))) val)
					       (if (memq sym outputters)
						   (let ((val #f)) (with-output-to-string (lambda () (set! val (func probe1 probe2)))) val)
						   (func probe1 probe2)))))
				  (unless (type-ok res res-types)
				    (format *stderr* "(~S ~S ~S) -> ~S, but ~A is not in ~A~%" sym probe1 probe2 res (type-of res) res-types))
				  (unless (type-ok probe2 par2-types)
				    (format *stderr* "(~S ~S ~S) ok, but ~A is not in ~S~%" sym probe1 probe2 (type-of probe2) par2-types)))
				
				;; 3 args
				(let ((par3-types (and (pair? (cdddr sig)) (if (pair? (cadddr sig)) (cadddr sig) (list (cadddr sig))))))
				  (for-each
				   (lambda (probe3)
				     (catch #t
				       (lambda ()
					 (let ((res (if (memq sym inputters)
							(let ((val #f)) (with-input-from-string "1234" (lambda () (set! val (func probe1 probe2 probe3)))) val)
							(if (memq sym outputters)
							    (let ((val #f)) (with-output-to-string (lambda () (set! val (func probe1 probe2 probe3)))) val)
							    (func probe1 probe2 probe3)))))
					   (unless (type-ok res res-types)
					     (format *stderr* "(~S ~S ~S ~S) -> ~S, but ~A is not in ~A~%" sym probe1 probe2 probe3 res (type-of res) res-types))
					   (unless (type-ok probe3 par3-types)
					     (format *stderr* "(~S ~S ~S ~S) ok, but ~A is not in ~S~%" sym probe1 probe2 probe3 (type-of probe3) par3-types)))
					 )
				       (lambda (type info)
					 (let ((errstr (apply format #f info)))
					   (unless (string-position (symbol->string sym) errstr)
					     (format *stderr* "(~S ~S ~S ~S) -> ~S~%" sym probe1 probe2 probe3 errstr)))
					 (if (and (eq? type 'wrong-type-arg-error)
						  (type-ok probe3 par3-types))
					     (format *stderr* "(~S ~S ~S ~S) -> arg type error, but par-types: ~S~%" sym probe1 probe2 probe3 par3-types)
					     (when (and (eq? type 'wrong-number-of-args)
							(aritable? func 3))
					       (format *stderr* "(~S ~S ~S ~S) -> arg num error but arity: ~S~%" sym probe1 probe2 probe3 ari))))))
				   probes))
				)
			      (lambda (type info)
				(let ((errstr (apply format #f info)))
				  (unless (string-position (symbol->string sym) errstr)
				    (format *stderr* "(~S ~S ~S) -> ~S~%" sym probe1 probe2 errstr)))
				(if (and (eq? type 'wrong-type-arg-error)
					 (type-ok probe2 par2-types))
				    (format *stderr* "(~S ~S ~S) -> arg type error, but par-types: ~S~%" sym probe1 probe2 par2-types)
				    (when (and (eq? type 'wrong-number-of-args)
					       (aritable? func 2))
				      (format *stderr* "(~S ~S ~S) -> arg num error but arity: ~S~%" sym probe1 probe2 ari))))))
			  probes))
		       )
		     (lambda (type info)
		       (let ((errstr (apply format #f info)))
			 (unless (string-position (symbol->string sym) errstr)
			   (format *stderr* "(~S ~S) -> ~S~%" func probe1 errstr)))
		       (if (and (eq? type 'wrong-type-arg-error)
				(type-ok probe1 par1-types))
			   (format *stderr* "(~S ~S) -> arg type error, but par-types: ~S~%" sym probe1 par1-types)
			   (when (and (eq? type 'wrong-number-of-args)
				      (not (memq sym even-args))
				      (aritable? func 1)
				      (not (memq sym '(apply call-with-exit))))
			     (format *stderr* "(~S ~S) -> arg num error but arity: ~S~%" sym probe1 ari))))))
		 probes))))))))
  
  (define baddies '(exit emergency-exit abort s7-optimize dynamic-unwind 
		    delete-file system set-cdr! stacktrace check check-funcs type-ok
		    cutlet varlet gc cond-expand reader-cond
		    openlet coverlet eval ;vector list cons values hash-table
		    symbol-table load throw error
		    make-rectangular macro macro* bacro bacro*
		    copy fill! hash-table-set! vector-set! let-set! list-values apply-values immutable!
		    *unbound-variable-hook* *load-hook* *rootlet-redefinition-hook* *missing-close-paren-hook* *read-error-hook*
		    tree-count ; signature is kinda silly here
		    trace-in profile-in apply call-with-exit
		    define-expansion call-with-current-continuation vector-append append ; append gets uninteresting type conversion complaints
		    call/cc call-with-output-string open-input-function open-output-function
		    set-current-input-port set-current-output-port set-current-error-port
		    heap-scan heap-analyze heap-holders heap-holder
		    ))
  
  (define (check-funcs)
    (let ((syms (symbol-table)))
      (for-each check syms)))
  
  (check-funcs))


(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)
