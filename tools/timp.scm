;;; implicit ref/set -- tmisc.scm and tread.scm also have some

(load "s7test-block.so" (sublet (curlet) (cons 'init_func 'block_init)))

(define size 20000)

(define (s1 obj val)
  (set! (obj 'a) val))

(define (s11 obj val)
  (set! ((obj 'b) 'a) val))

(define (s111 obj val)
  (set! (obj 'b 'a) val))

(define (s1111 obj sym val)
  (set! (obj 'b sym) val))

(define (s11111 obj sym val)
  (set! (obj 'b (car sym)) val))


(define (s2 obj val)
  (set! (obj 'a) (+ (log 4 2) val)))

(define (s22 obj val)
  (set! ((obj 'b) 'a) (+ (log 4 2) val)))

(define (s222 obj val)
  (set! (obj 'b 'a) (+ (log 4 2) val)))

(define (s2222 obj sym val)
  (set! (obj 'b sym) (+ (log 4 2) val)))

(define (s22222 obj sym val)
  (set! (obj 'b (car sym)) (+ (log 4 2) val)))


(define (s3 obj val)
  (set! (obj 'b :a) val))


(define (s4 obj val)
  (set! (obj 'b 1) val))

(define (s44 obj val)
  (set! (obj 'b (- (log 4 2) 1)) val))

(define (s444 obj val)
  (set! (obj 'b 1) (car val)))

(define (s4444 obj ind val)
  (set! (obj 'b ind) val))


(define (s5 obj val)
  (set! (obj 0 1) val))

(define (s55 obj val)
  (set! (obj 0 (- (log 4 2) 1)) val))

(define (s555 obj val)
  (set! (obj 0 1) (car val)))

(define (s5555 obj ind val)
  (set! (obj 0 ind) val))


(define (s6 obj val)
  (set! (((obj 'b) 'b) 'a) val))

(define (s66 obj val)
  (set! (((obj 'b) 'b) 'a) val))

(define (s666 obj val)
  (set! (obj 'b 'b 'a) val))

(define (s6666 obj val)
  (set! ((obj 'b) 'b 'a) val))


(define (s7 obj val)
  (set! (list-ref (obj 0) 0) val))

(define (s77 obj i1 i2 val)
  (set! (list-ref (obj i1) i2) val))

(define (s777 obj val)
  (set! ((obj 0)) 32))


(define (stest)
  (let ((table (hash-table 'a 1 'b (hash-table 'a 3)))
	(env (inlet 'a 1 'b (inlet 'a 4)))
	(table1 (hash-table 'b "12345"))
	(table2 (vector (vector 1 2 3)))
	(table3 (hash-table 'b (block 1 2 3)))
	(table4 (hash-table 'b (let ((x (vector 1 2 3))) (dilambda (lambda (ind) (x ind)) (lambda (ind val) (set! (x ind) val))))))
	(table5 (hash-table 'a 1 'b (hash-table 'a 3 'b (hash-table 'a 4))))
	(table6 (vector (list 0 1) (list 2 3)))
	(lst (list 0 1))
	(lst1 (list dilambda_test))) ; from s7test-block
    (do ((i 0 (+ i 1)))
	((= i size))

      (s1 table 12)
      (unless (= (table 'a) 12) (format *stderr* "oops"))
      (s11 table 12)
      (unless (= ((table 'b) 'a) 12) (format *stderr* "gad"))
      (s111 table 12)
      (unless (= (table 'b 'a) 12) (format *stderr* "darn"))
      (s1111 table 'a 12)
      (unless (= (table 'b 'a) 12) (format *stderr* "darn"))
      (s11111 table '(a) 12)
      (unless (= (table 'b 'a) 12) (format *stderr* "darn"))

      (s1 env 12)
      (s11 env 12)
      (s111 env 12)
      (s1111 env 'a 12)
      (s11111 env '(a) 12)

      (s2 table 12)
      (unless (= (table 'a) 14) (format *stderr* "oops"))
      (s22 table 12)
      (unless (= ((table 'b) 'a) 14) (format *stderr* "gad"))
      (s222 table 12)
      (unless (= (table 'b 'a) 14) (format *stderr* "darn"))
      (s2222 table 'a 12)
      (unless (= (table 'b 'a) 14) (format *stderr* "darn"))
      (s22222 table '(a) 12)
      (unless (= (table 'b 'a) 14) (format *stderr* "darn"))

      (s2 env 12)
      (s22 env 12)
      (s222 env 12)
      (s2222 env 'a 12)
      (s22222 env '(a) 12)

      (s3 env 32)
      (unless (= (env 'b 'a) 32) (format *stderr* "darn"))

      (s4 table1 #\a) ; set_implicit_string
      (unless (char=? (table1 'b 1) #\a) (format *stderr* "darn"))
      (s44 table1 #\a)
      (unless (char=? (table1 'b 1) #\a) (format *stderr* "darn"))
      (s444 table1 '(#\a))
      (unless (char=? (table1 'b 1) #\a) (format *stderr* "darn"))
      (s4444 table1 1 #\a)
      (unless (char=? (table1 'b 1) #\a) (format *stderr* "darn"))

      (s4 table3 23.0) ; set_implicit_c_object
      (unless (= (table3 'b 1) 23.0) (format *stderr* "darn"))
      (s44 table3 23.0)
      (unless (= (table3 'b 1) 23.0) (format *stderr* "darn"))
      (s444 table3 '(23.0))
      (unless (= (table3 'b 1) 23.0) (format *stderr* "darn"))
      (s4444 table3 1 23.0)
      (unless (= (table3 'b 1) 23.0) (format *stderr* "darn"))

      (s4 table4 23.0) ; set_implicit_closure
      (unless (= (table4 'b 1) 23.0) (format *stderr* "darn"))
      (s44 table4 23.0)
      (unless (= (table4 'b 1) 23.0) (format *stderr* "darn"))
      (s444 table4 '(23.0))
      (unless (= (table4 'b 1) 23.0) (format *stderr* "darn"))
      (s4444 table4 1 23.0)
      (unless (= (table4 'b 1) 23.0) (format *stderr* "darn"))

      (s5 table2 #\a) ; set_implicit_vector
      (unless (char=? (table2 0 1) #\a) (format *stderr* "darn"))
      (s55 table2 #\a)
      (unless (char=? (table2 0 1) #\a) (format *stderr* "darn"))
      (s555 table2 '(#\a))
      (unless (char=? (table2 0 1) #\a) (format *stderr* "darn"))
      (s5555 table2 1 #\a)
      (unless (char=? (table2 0 1) #\a) (format *stderr* "darn"))

      (s6 table5 12)
      (unless (= (((table5 'b) 'b) 'a) 12) (format *stderr* "oops"))
      (s66 table5 12)
      (unless (= ((table5 'b) 'b 'a) 12) (format *stderr* "gad"))
      (s666 table5 12)
      (unless (= (table5 'b 'b 'a) 12) (format *stderr* "darn"))
      (s6666 table5 12)
      (unless (= (table5 'b 'b 'a) 12) (format *stderr* "darn"))

      (s7 table6 12)
      (unless (= (table6 0 0) 12) (format *stderr* "oops"))
      (s77 table6 0 1 12)
      (unless (= (table6 0 1) 12) (format *stderr* "oops"))
      (s777 lst1 12)
      )))

(stest)


;;; c-object[special cases, refs] iterator syntax macro function c_macro c_function (* cases too), else->no_setter_error
;;; why no implicit string ref if table1?
;;; for iterator see s7test.scm 26743 (set! (iter) 32)
;;; for syntax, this is (set! (with-let...) val) (set! (with-let (curlet) 'c) 32) etc
;;; for functions, it's setter

