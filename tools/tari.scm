;;; numeric function simple timing tests

(define size 250000)
(define int-limit 1000000)
(define float-limit 1000.0)


(define (make-ivals)
  (let ((v (make-int-vector size))
	(lim (* 2 int-limit)))
    (do ((i 0 (+ i 1)))
	((= i size) v)
      (int-vector-set! v i (- (random lim) int-limit)))))
(define ivals (make-ivals))

(define (make-ivals1)
  (let ((v (make-vector size))
	(lim (* 2 int-limit)))
    (do ((i 0 (+ i 1)))
	((= i size) v)
      (vector-set! v i (- (random lim) int-limit)))))
(define ivals1 (make-ivals1))


(define (make-fvals)
  (let ((v (make-float-vector size))
	(lim (* 2.0 float-limit)))
    (do ((i 0 (+ i 1)))
	((= i size) v)
      (float-vector-set! v i (- (random lim) float-limit)))))
(define fvals (make-fvals))

(define (make-fvals1)
  (let ((v (make-vector size))
	(lim (* 2.0 float-limit)))
    (do ((i 0 (+ i 1)))
	((= i size) v)
      (vector-set! v i (- (random lim) float-limit)))))
(define fvals1 (make-fvals1))


(define (make-ratvals)
  (let ((v (make-vector size))
	(lim (* 2 int-limit)))
    (do ((i 0 (+ i 1)))
	((= i size) v)
      (vector-set! v i (/ (- (random lim) int-limit) (+ 1 (random int-limit)))))))
(define ratvals (make-ratvals))


(define (make-cvals)
  (let ((v (make-vector size))
	(lim (* 2.0 float-limit)))
    (do ((i 0 (+ i 1)))
	((= i size) v)
      (vector-set! v i (complex (- (random lim) float-limit) (- (random lim) float-limit))))))
(define cvals (make-cvals))


;;; -------- min max --------
(define (minmax v)
  (let ((lo (v 0))
	(hi (v 0)))
    (do ((i 0 (+ i 1)))
	((= i size) (list lo hi))
      (set! lo (min lo (v i)))
      (set! hi (max hi (v i))))))

(define (minmax1 v)
  (let ((lo (v 0))
	(hi (v 0)))
    (do ((i 0 (+ i 1)))
	((= i size) (list lo hi))
      (set! lo (min lo (v i) hi))
      (set! hi (max hi lo (v i))))))

(format *stderr* "int-minmax ~S~%" (minmax ivals))     ; min/max_i_ii
(format *stderr* "int-minmax ~S~%" (minmax ivals1))    ; min/max_p_pp
(format *stderr* "int-minmax1 ~S~%" (minmax1 ivals))   ; min/max_i_iii
(format *stderr* "int-minmax1 ~S~%" (minmax1 ivals1))  ; g_min/max -> min/max_p_pp [perhaps min/max_3?]
(format *stderr* "float-minmax ~S~%" (minmax fvals))   
(format *stderr* "float-minmax ~S~%" (minmax fvals1))  
(format *stderr* "float-minmax1 ~S~%" (minmax1 fvals)) 
(format *stderr* "float-minmax1 ~S~%" (minmax1 fvals1))
(format *stderr* "ratio-minmax ~S~%" (minmax ratvals)) 


;;; -------- real-part imag-part --------

(define (complex-minmax v)
  (let ((rlo (real-part (v 0)))
	(rhi (real-part (v 0)))
	(ilo (imag-part (v 0)))
	(ihi (imag-part (v 0))))
    (do ((i 0 (+ i 1)))
	((= i size) (list rlo ilo rhi ihi))
      (set! rlo (min rlo (real-part (v i))))
      (set! rhi (max rhi (real-part (v i))))
      (set! ilo (min ilo (imag-part (v i))))
      (set! ihi (max ihi (imag-part (v i)))))))

(format *stderr* "complex-minmax ~S~%" (complex-minmax cvals))


;;; -------- numerator denominator --------

(define (numden-minmax v)
  (let ((numlo (numerator (v 0)))
	(numhi (numerator (v 0)))
	(denlo (denominator (v 0)))
	(denhi (denominator (v 0))))
    (do ((i 0 (+ i 1)))
	((= i size) (list numlo denlo numhi denhi))
      (set! numlo (min numlo (numerator (v i))))
      (set! numhi (max numhi (numerator (v i))))
      (set! denlo (min denlo (denominator (v i))))
      (set! denhi (max denhi (denominator (v i)))))))

(format *stderr* "numden-minmax ~S~%" (numden-minmax ratvals))


;;; -------- even? odd? --------

(define (count-evens v)
  (let ((even 0)
	(odd 0))
    (do ((i 0 (+ i 1)))
	((= i size) (list even odd size (+ even odd)))
      (if (even? (v i)) (set! even (+ even 1)))
      (if (odd? (v i)) (set! odd (+ odd 1))))))

(format *stderr* "evens: ~S~%" (count-evens ivals))
(format *stderr* "evens1: ~S~%" (count-evens ivals1))


;;; -------- zero? positive? negative? --------

(define (count-zeros v)
  (let ((zero 0)
	(pos 0)
	(neg 0))
    (do ((i 0 (+ i 1)))
	((= i size) (list zero pos neg size (+ zero pos neg)))
      (if (zero? (v i)) (set! zero (+ zero 1)))
      (if (positive? (v i)) (set! pos (+ pos 1)))
      (if (negative? (v i)) (set! neg (+ neg 1))))))

(format *stderr* "zeros: ~S~%" (count-zeros ivals))
(format *stderr* "zeros1: ~S~%" (count-zeros ivals1))
(format *stderr* "zerosf: ~S~%" (count-zeros fvals))
(format *stderr* "zerosrat: ~S~%" (count-zeros ratvals))


;;; -------- exact->inexact inexact->exact rationalize --------

(define (inex v1 v2)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (exact->inexact (v1 i))
    (inexact->exact (v2 i))
    (rationalize (v2 i))))

(inex ivals fvals)

(define (inex? v1 v2)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (if (inexact? (v1 i)) (display "oops: inexact?"))
    (if (exact? (v2 i)) (display "oops: exact"))))

(inex? ivals fvals)


;;; -------- integer? byte? number? real? float? complex? rational? infinite? nan? --------

(define (bools)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (if (infinite? (fvals i)) (display "oops inf"))
    (if (nan? (fvals i)) (display "oops nan"))
    (if (integer? (fvals i)) (display "oops int"))
    (if (byte? (cvals i)) (display "oops byte"))
    (if (and (real? (cvals i)) (not (zero? (imag-part (cvals i))))) (display "oops real"))
    (if (or (not (complex? (cvals i))) (not (number? (cvals i)))) (display "oops complex"))
    (if (rational? (cvals i)) (display "oops rational"))
    (if (float? (ivals1 i)) (display "oops float"))))

(bools)


;;; -------- ceiling truncate round floor --------

(define (ceil/floor)
  (let ((ints (make-int-vector 1)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (unless (integer? (ceiling (fvals i))) (display "oops: ceiling"))
      (unless (integer? (floor (fvals i))) (display "oops: floor"))
      (unless (integer? (truncate (fvals i))) (display "oops: truncate"))
      (unless (integer? (round (fvals i))) (display "oops: round"))
      (int-vector-set! ints 0 (ceiling (ratvals i)))
      (int-vector-set! ints 0 (ceiling (fvals i)))
      (int-vector-set! ints 0 (floor (fvals i)))
      (int-vector-set! ints 0 (floor (ratvals i)))
      (int-vector-set! ints 0 (round (fvals i)))
      (int-vector-set! ints 0 (truncate (fvals i))))))

(ceil/floor)


;;; -------- abs magnitude --------

(define (absmag)
  (let ((fv (make-float-vector 1))
	(iv (make-int-vector 1)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (if (not (= (abs (fvals i)) (magnitude (fvals i)))) (display "oops: abs"))
      (if (not (real? (magnitude (cvals i)))) (display "oops: magnitude"))
      (if (negative? (abs (ivals1 i))) (display "oops: abs neg"))
      (if (negative? (abs (ratvals i))) (display "oops: abs neg rat"))
      (int-vector-set! iv 0 (abs (ivals i)))
      (float-vector-set! fv 0 (abs (fvals i))))))

(absmag)
(newline)


;;; -------- exp sqrt --------

(define (expsq)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (sqrt (ivals i))
    (sqrt (fvals i))
    (sqrt (cvals i))
    (sqrt (ratvals i))
    (exp (fvals i))
    (exp (ivals i))
    (exp (cvals i))))

(expsq)


;;; -------- sin etc --------

(define (trigs)
  (let ((fv (make-float-vector 1)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (sin (fvals i))
      (sin (cvals i))
      (float-vector-set! fv 0 (sin (fvals i)))
      (cos (fvals i))
      (cos (cvals i))
      (float-vector-set! fv 0 (cos (fvals i)))
      (tan (fvals i))
      (tan (cvals i))
      (float-vector-set! fv 0 (tan (fvals i)))
      (asin (fvals i))
      (asin (cvals i))
      (acos (fvals i))
      (acos (cvals i))
      (atan (fvals i))
      (atan (cvals i))
      (sinh (fvals i))
      (sinh (cvals i))
      (cosh (fvals i))
      (cosh (cvals i))
      (tanh (fvals i))
      (tanh (cvals i))
      (asinh (fvals i))
      (asinh (cvals i))
      (acosh (fvals i))
      (acosh (cvals i))
      (atanh (fvals i))
      (atanh (cvals i))
      (angle (fvals i)))))

(trigs)


;;; -------- lognot etc --------

(define (logs)
  (let ((iv (make-int-vector 1)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (logand (ivals i) (ivals i))
      (logand (ivals1 i) (ivals1 i))
      (logior (ivals i) (ivals i))
      (logior (ivals1 i) (ivals1 i))
      (logxor (ivals i) (ivals i))
      (logxor (ivals1 i) (ivals1 i))
      (lognot (ivals i))
      (int-vector-set! iv 0 (lognot (ivals i)))
      (logbit? (ivals i) 12)
      (if (logbit? (ivals i) 12) (display "on" #f))
      (ash (ivals i) 2)
      (ash (ivals i) -2))))

(logs)


;;; -------- relops --------

(define (relops)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (< (ivals i) (ivals1 i))
    (<= (ivals i) (ivals1 i))
    (= (ivals i) (ivals1 i))
    (> (ivals i) (ivals1 i))
    (>= (ivals i) (ivals1 i))))

(relops) ; a million more choices here


;;; -------- quotient remainder modulo -------- 

(define (unzero v)
  (let ((v1 (copy v)))
    (do ((i 0 (+ i 1)))
	((= i (vector-length v1)) v1)
      (if (zero? (v1 i))
	  (set! (v1 i) 1)))))

(define unzeros (unzero ivals)) ; avoid divide by zero error
(define unzeros1 (unzero ivals1))
(define funzeros (unzero fvals))

(define (quorem)
  (let ((iv (make-int-vector 1))
	(fv (make-float-vector 1)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (int-vector-set! iv 0 (quotient (ivals i) (unzeros i)))
      (int-vector-set! iv 0 (remainder (ivals i) (unzeros i)))
      (int-vector-set! iv 0 (modulo (ivals i) (unzeros i)))
      (quotient (ivals i) (unzeros1 i))
      (remainder (ivals i) (unzeros1 i))
      (modulo (ivals i) (unzeros1 i))
      (float-vector-set! fv 0 (quotient (fvals i) (funzeros i)))
      (float-vector-set! fv 0 (remainder (fvals i) (funzeros i)))
      (float-vector-set! fv 0 (modulo (fvals i) (funzeros i))))))

(quorem)


;;; -------- gcd lcm --------

(define (gcdlcm)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (gcd (ivals i) (ivals1 i))
    (lcm (ivals i) (ivals1 i))))

(gcdlcm)


;;; -------- log --------

(define log-size (floor (/ size 8)))

(define (logtest)
  (do ((i 0 (+ i 1)))
      ((= i log-size))
    (log (fvals i) 2)
    (log (cvals i))
    (log (ivals i))
    (log (ratvals i))

    (log (ivals i) (ivals i))
    (log (ivals i) (fvals i))
    (log (ivals i) (ivals1 i))
    (log (ivals i) (cvals i))

    (log (fvals i) (ivals i))
    (log (fvals i) (fvals i))
    (log (fvals i) (ivals1 i))
    (log (fvals i) (cvals i))

    (log (ratvals i) (ivals i))
    (log (ratvals i) (fvals i))
    (log (ratvals i) (ivals1 i))
    (log (ratvals i) (cvals i))

    (log (cvals i) (ivals i))
    (log (cvals i) (fvals i))
    (log (cvals i) (ivals1 i))
    (log (cvals i) (cvals i))))

(logtest)


;;; -------- expt --------

(define (exptest)
  (do ((i 0 (+ i 1)))
      ((= i log-size))
    (expt (ivals i) (ivals i))
    (expt (ivals i) (fvals i))
    (expt (ivals i) (ivals1 i))
    (expt (ivals i) (ratvals i))
    (expt (ivals i) (cvals i))

    (expt (fvals i) (ivals i))
    (expt (fvals i) (fvals i))
    (expt (fvals i) (ivals1 i))
    (expt (fvals i) (ratvals i))
    (expt (fvals i) (cvals i))

    (expt (ratvals i) (ivals i))
    (expt (ratvals i) (fvals i))
    (expt (ratvals i) (ivals1 i))
    (expt (ratvals i) (ratvals i))
    (expt (ratvals i) (cvals i))

    (expt (cvals i) (ivals i))
    (expt (cvals i) (fvals i))
    (expt (cvals i) (ivals1 i))
    (expt (cvals i) (ratvals i))
    (expt (cvals i) (cvals i))))

(exptest)


(exit)
