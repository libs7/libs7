(set! (*s7* 'heap-size) 1024000)

(define size 1000000)

;;; -------- [416] --------
(define (f1 x) (x 123))           ; op_closure_a for (x 123)

;(display (f1 (lambda (y) (+ y 1)))) (newline)

(define (f1-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f1 (lambda (y) (+ y 1)))))   ; op_lambda_unchecked -> make_closure_unchecked, op_simple_do_step -> op_closure_p

(f1-test)


;;; -------- [467] --------
(define (f2 x)                    ; 465 if define-constant, 675 if both are define-constant
  (define (f2_1 y) (+ y 1))       ; define_funchecked + make_funclet, op_safe_closure_s
  (f2_1 x))

;(display (f2 123)) (newline)

(define (f2-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f2 123)))                    ; op_closure_a from op_safe_dotimes_step_o

(f2-test)


;;; -------- [110] --------
(define f3 (let ()                
	     (define (f3_1 y) (+ y 1))
	     (lambda (x)
	       (f3_1 x))))        ; fx_safe_closure_s_to_add1 [s_to_sc]

;(display (f3 123)) (newline)

(define (f3-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f3 123)))                    ; op_safe_closure_a_o from op_safe_dotimes_step_o

(f3-test)

;;; f1 could be faster, treat (lambda...) as a one-time evaluation (but where to save == lamlet? it's a do-loop: save locally)


;;; -------- [82] --------
(define-constant (f4_1 y) (+ y 1)) ; fx_safe_closure_s_to_add1
(define-constant (f4 x) (f4_1 x)) ; faster than f3 because of the define-constant (allows opt cases), 110 if define
                                  ;   has_fx f4, whereas not has_fx f3
;(display (f4 123)) (newline)

(define (f4-test)
  (do ((i 0 (+ i 1)))            ; opt_dotimes opt_p_fx_any
      ((= i size))
    (f4 123)))                   ; fx_safe_closure_a_a -> safe_closure_add1_c [y=c etc] -> add1_c or inner body using s->val

(f4-test)


;;; -------- [237] --------
(define (f5 x) ((lambda (y) (+ y 1)) x))

;(display (f5 123)) (newline)

(define (f5-test)
  (do ((i 0 (+ i 1)))            ; op_safe_dotimes_step_o -> op_f_a [op_safe_sc] -- this could be optimized [op_f_fx_a?]
      ((= i size))
    (f5 123)))                   ; op_closure_a_o

(f5-test)


;;; -------- [216] --------
(define f6 (let ((f6_1 (lambda (y) (+ y 1)))) ; use letrec (f7) or define (f3) 2x faster??
	     (lambda (x)
	       (f6_1 x))))       ; op_closure_s_o

;(display (f6 123)) (newline)

(define (f6-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f6 123)))                   ; op_closure_a_o

(f6-test)


;;; -------- [110] --------
(define f7 (letrec ((f7_1 (lambda (y) (+ y 1)))) ; 110?!? letrec* same,  fx_safe_closure_s_to_add1
	     (lambda (x)
	       (f7_1 x))))       ; op_safe_closure_a_o via letrec_setup_closures

;(display (f7 123)) (newline)

(define (f7-test)
  (do ((i 0 (+ i 1)))            ; op_safe_dotimes_step_o
      ((= i size))
    (f7 123)))

(f7-test)



;;; -------- [61] --------
(define-constant (f8 x) (+ x 1)) ; the obvious case... 97 if define rather than define-constant

;(display (f8 123)) (newline)

(define (f8-test)
  (do ((i 0 (+ i 1)))            ; opt_dotimes opt_p_fx_any
      ((= i size))
    (f8 123)))                   ; fx_safe_closure_a_to_sc

(f8-test)



;;; -------- [375] --------
(define (f9 x)
  (let ((f9_1 (lambda (y) (+ y 1)))) ; faster than define (f2)??  make_closure_unchecked
    (f9_1 x)))

;(display (f9 123)) (newline)

(define (f9-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f9 123)))

(f9-test)



;;; -------- [140] --------
(define (f10 x) (f10_1 x))  ; call before definition, 134 if define-constant
(define (f10_1 y) (+ y 1))  ; fx_safe_closure_s_to_add1
                           
;(display (f10 123)) (newline)

(define (f10-test)
  (do ((i 0 (+ i 1))) 
      ((= i size))
    (f10 123)))             ; op_closure_a_o [safe_closure_a_o if in the correct order, f3]

(f10-test)



;;; -------- [614] --------
(define-macro (f11_1 y) `(+ ,y 1)) ; 97 if expansion
(define (f11 x) (f11_1 x))
                           
;(display (f11 123)) (newline)

(define (f11-test)
  (do ((i 0 (+ i 1))) 
      ((= i size))
    (f11 123)))

(f11-test)



;;; -------- [43] --------
(define-macro (f12_1 y) `(+ ,y 1))
(define-macro (f12 x) `(f12_1 ,x))
                           
;(display (f12 123)) (newline)

(define (f12-test)
  (do ((i 0 (+ i 1))) 
      ((= i size))
    (f12 123)))

(f12-test)



;;; -------- [18] --------
(define-expansion (f13_1 y) `(+ ,y 1))
(define-expansion (f13 x) `(f13_1 ,x))   ; perhaps f4 expanded to fx of body?
                           
;(display (f13 123)) (newline)

(define (f13-test)
  (do ((i 0 (+ i 1))) 
      ((= i size))
    (f13 123)))         ; opt_dotimes opt_i_ii_cc add_i_ii, no eval

(f13-test)



;;; -------- [174] --------
(define* (f14_1 (y 0)) (+ y 1))
(define* (f14 (x 0)) (f14_1 x))
                           
;(display (f14 123)) (newline)

(define (f14-test)      ; why 2x slower than f4, f4 (define case) uses fx_safe_closure_s_to_add1 = less eval + g_add_x1 + fx_t
  (do ((i 0 (+ i 1))) 
      ((= i size))
    (f14 123)))         ; op_safe_closure_star_a1

(f14-test)



;;; -------- [209] --------
(define f15 (letrec ((f15_1 (lambda (y) (+ y 1)))
		     (f15_2 (lambda (y) (* y 2))))
	     (lambda (x)
	       (f15_2 (f15_1 x)))))

;(display (f15 61)) (newline)

(define (f15-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f15 61)))

(f15-test)



;;; -------- [715] --------
(define (f16 f1 f2) (f2 (f1 61)))

;(display (f16 (lambda (y) (+ y 1)) (lambda (y) (* y 2)))) (newline)

(define (f16-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f16 (lambda (y) (+ y 1)) (lambda (y) (* y 2)))))

(f16-test)



;;; -------- [342] --------
(define (f16_1 y) (+ y 1))
(define (f16_2 y) (* y 2))

;(display (f16 f16_1 f16_2)) (newline)

(define (f17-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f16 f16_1 f16_2)))

(f17-test)


	    

(exit)
