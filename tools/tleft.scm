(define tc-size 1500000)
(define rc-size 10000)
(define case-size 1000000)


(set! (*s7* 'heap-size) 512000)

;;; why no fx_tree? or tentative? sub_t1|s1, fx_lt|geq|leq_si, fb|fx_num_eq_s0, fx_s
;;; no other fb's
;;; cond/let cases


;;; --------------------------------------------------------------------------------
;OP_TC_WHEN_LA and unless cases

(define (f1 x)
  (when (> x 0)    ; fx_gt_ti
    (f1 (- x 1)))) ; fx_subtract_t1

(f1 tc-size)  ; safe_closure_a_o[checked] op_if_a_p

(define (f2 x)
  (unless (<= x 0) ; fx_leq_ti 
    (f2 (- x 1)))) ; fx_subtract_t1 

(f2 tc-size)  ; safe_closure_a_o[checked] op_if_a_r

;;; --------------------------------------------------------------------------------
;OP_TC_IF_A_Z_IF_A_Z_L3A and reverse

(define (f3 x y z)
  (if (<= x 0)                      ; fx_leq_ti
      y
      (if (= y 0)                   ; fx_num_eq_u0
	  x
	  (f3 (- x 1) (- y 1) z)))) ; fx_subtract_t|u1 fx_v

(f3 tc-size (* 2 tc-size) 0) ; safe_closure_3a[checked] if_a_a_p[leq] if_b_a_p[=] -- b_a_p leq case?

(define (f4 x y z)
  (if (<= x 0)
      y
      (if (> y 0)
	  (f4 (- x 1) (- y 1) z)
	  x)))

(f4 tc-size (* 2 tc-size) 0) ; safe_closure_3a[checked] if_a_a_p[leq] if_a_p_a[gt]

;;; --------------------------------------------------------------------------------
;OP_TC_IF_A_LA_IF_A_Z[_Z] -- already handled:

(define (f5 x) 
  (if (>= x 0)        ; opt_b_ii_sc_geq_0
      (f5 (- x 1))    ; opt_i_ii_sc_sub
      (if (< x 0)
	  x
	  'oops!)))

(f5 tc-size) ; op_tc_if_a_la_z

;;; --------------------------------------------------------------------------------
;OP_TC_AND_A_OR_A_L3A

(define (f6 x y z)
  (and (> x 0)                       ; fx_gt_ti
       (or (= y 0)                   ; fx_num_eq_u0
	   (f6 (- x 1) (- y 1) 0)))) ; fx_subtract_t|u1

(f6 tc-size (* 2 tc-size) 0) ; op_safe_closure_3a[checked] op_and_ap[fx_gt_ti] op_or_ap[fx_num_eq_u0]

;;; --------------------------------------------------------------------------------
;OP_TC_IF_A_Z_IF_A_Z_IF_A_Z_LA 

(define (f7 x)
  (if (= x 0)
      x
      (if (< x 0)
	  'oops
	  (if (= x 0)
	      'oops
	      (f7 (- x 1))))))

(f7 tc-size) ; op_safe_closure_3a[checked] if_a_a_p[fx_lt_si] if_b_a_p[fb_num_eq_s0]

;;; --------------------------------------------------------------------------------
;OP_TC_COND_A_Z_L3A

(define (f8 x y z)
  (cond ((= x 0) 0)                     ; fx_num_eq_t0
	(else (f8 (- x 1) (- y 1) z)))) ; fx_subtract_t|u1 fx_v

(f8 tc-size tc-size 0) ; op_safe_closure_3a

;;; --------------------------------------------------------------------------------
;OP_TC_CASE_LAA

(define (f9 x y)
  (case x
    ((0) (= y 0))
    ((1) (* x y))
    (else (f9 (- x 1) (+ y 1))))) ; fx_subtract_t1 fx_add_u1

(f9 tc-size 0)

;;; --------------------------------------------------------------------------------
;OP_TC_LET_IF_A_Z_L3A

(define (f10 x y z)
  (let ((a (+ x y z)))       ; fx_c_tuv -> g_add_3?[why not direct? no tuv_direct]  let_a_p_old
    (if (= a 0)              ; fx_num_eq_t0  if_a_a_p
	x                    ; fx_o?
	(f10 (- x 1) y z)))) ; op_safe_closure_agg  fx_subtract_s1 + fx_o

(f10 tc-size (/ tc-size 10) (/ tc-size 10))

;;; --------------------------------------------------------------------------------
;OP_TC_WHEN_L3A and unless cases

(define (f11 x y z)
  (when (> x 0)                     ; fx_gt_ti
    (f11 (- x 1) (- y 2) (+ z 1)))) ; op_safe_closure_3a fx_add_v1 [subtract]

(f11 tc-size 2 3) 

(define (f12 x y z)
  (unless (<= x 0)                  ; fx_leq_ti
    (f12 (- x 1) y z)))             ; op_safe_closure_agg fx_subtract_t1 fx_u fx_v

(f12 tc-size 1 2)


;;; --------------------------------------------------------------------------------
;OP_RECUR_IF_A_A_opLAA_LAAq -- if_a_oplaa_laaq_a etc

(define (rc1 x y)
  (if (<= y 0)
      x
      (+ (rc1 (+ x 1) (- y 1))
	 (rc1 (- x 1) (- y 1)))))

(define (trc1)
  (do ((i 0 (+ i 1)))
      ((= i rc-size))
    (rc1 0 6)))

(trc1)

;;; --------------------------------------------------------------------------------
;OP_RECUR_IF_A_A_opL3A_L3Aq

(define (rc2 x y z)
  (if (<= z 0)
      (+ x y)
      (+ (rc2 (+ x 1) (+ y 1) (- z 1))
	 (rc2 (- x 1) (- y 1) (- z 1)))))

(define (trc2)
  (do ((i 0 (+ i 1)))
      ((= i rc-size))
    (rc2 0 0 5)))

(trc2)


;;; OP_RECUR_IF_A_A_IF_A_A_opL3A_L3Aq, OP_RECUR_OR_A_AND_A_LA(AA)?


;;; --------------------------------------------------------------------------------

(define (tcase1)       ; opt_case, numbers_are_eqv
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case i
      ((0) 0)
      ((1) 1)
      ((2) 2)
      (else 3))))

(tcase1)

(define (case2 i) ; case_a_i_s_a, uses t_lookup
  (case i
    ((0) 0)
    ((1) 1)
    ((2) 2)
    (else 3)))

(define (tcase2)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case2 i)))

(tcase2)

(define (case3 x lst)        ; case_a_i_s_a
  (case x                    ;   t_lookup
    ((0) (pair? lst))        ; fx_is_pair_u
    ((1) (pair? (cdr lst)))  ; fx_is_pair_cdr_u
    (else (null? lst))))     ; fx_is_null_u

(define (tcase3)
  (do ((i 0 (+ i 1)))        ; op_simple_do_step -> safe_closure_aa_o
      ((= i case-size))
    (case3 i '(1 2))))       ; fx_t fx_q

(tcase3)

(define (tcase3-1)
  (do ((i 0 (+ i 1)))             ; op_simple_do_step -> safe_closure_aa_o, g_add_x1
      ((= i case-size))                ; g_num_eq_2
    (case3 (remainder i 3) '(1 2)))) ; modulo_p_pi via fx_c_ti_direct(80400 check_do), fx_q

(tcase3-1)

(define (case4 x) ; case a_e_s_a
  (case (remainder x 5)
    ((a) 0)
    ((b) 1)
    ((c) 2)
    ((d) 3)
    (else 4)))

(define (tcase4)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case4 i)))

(tcase4)

(define sym-selector
  (let ((syms '(a b c d e)))
    (lambda (x)
      (syms (remainder x 5))))) ; list-ref is slower

(define (case5 x) ; case p_e_s
  (case (sym-selector x)
    ((a) 0)
    ((b) 1)
    ((c) 2)
    ((d) 3)
    (else 4)))

(define (tcase5)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case5 i)))

(tcase5)

(define (int-selector x)
  (remainder x 5))

(define (case6 x) ; case p_i_s
  (case (int-selector x)
    ((1) 0)
    ((2) 1)
    ((3) 2)
    ((4) 3)
    (else 4)))

(define (tcase6)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case6 i)))

(tcase6)

(define (any-selector x)
  (if (zero? (remainder x 3)) #\a))

(define (case7 x) ; casae p_g_s
  (case (any-selector x)
    ((1/2) 0)
    ((#\a) 1)
    ((#<unspecified>) 2)
    ((#f) 3)
    (else 4)))

(define (tcase7)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case7 i)))

(tcase7)

(define (case8 x) ; casae a_g_s_a
  (case (even? x)
    ((1/2) 0)
    ((#\a) 1)
    ((#<unspecified>) 2)
    ((#f) 3)
    (else 4)))

(define (tcase8)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case8 i)))

(tcase8)

(define (case9 x) ; casae a_e_g
  (case (even? x)
    ((#t) (display x #f) (not x))
    ((#f) (write x #f) (integer? x))
    (else 'oops)))

(define (tcase9)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case9 i)))

(tcase9)

(define (case10 x) ; casae a_g_g
  (case (even? x)
    ((#t) (display x #f) (not x))
    ((1/2) (write x #f) (integer? x))
    ((1 2 3) 0)
    (else 'oops)))

(define (tcase10)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case10 i)))

(tcase10)

(define (case11 x) ; case a_s_g
  (case (remainder x 5)
    ((a) 0)
    ((b) (display x #f) 1)
    ((c) (+ 1 2))
    ((d) 3)
    (else 4)))

(define (tcase11)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case11 i)))

(tcase11)

(define (case12 x) ; casae p_g_g
  (case (any-selector x)
    ((#t) (display x #f) (not x))
    ((1/2) (write x #f) (integer? x))
    ((1 2 3) 0)
    (else 'oops)))

(define (tcase12)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case12 i)))

(tcase12)

(define (case13 x) ; case p_e_g
  (case (sym-selector x)
    ((a) 0)
    ((b) (display x #f) 32)
    ((c) 2)
    ((d) 3)
    (else 4)))

(define (tcase13)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case13 i)))

(tcase13)

(define (case14 x) ; a_g_s
  (case x
    ((1 2 3) 0)
    ((4 5) 1)
    ((6) 2)
    ((7 8 9) 3)
    ((10) 4)))

(define (tcase14)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case14 i)))

(tcase14)

(define (case15 x) ; case a_e_s
  (case (remainder x 5)
    ((a) 0)
    ((b) 1)
    ((c) 2)
    ((d) 3)))

(define (tcase15)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case15 i)))

(tcase15)

(define (case16 x) ; case a_s_g_a
  (case (remainder x 5)
    ((a) 0)
    ((b) (display x #f) 1)
    ((c) (+ 1 2))
    ((d) 3)
    (else 4)))

(define (tcase16)
  (do ((i 0 (+ i 1)))
      ((= i case-size))
    (case16 i)))

(tcase16)

(exit)
