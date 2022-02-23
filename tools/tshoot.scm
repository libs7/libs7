;;; various standard benchmarks 
;;; --------------------------------------------------------------------------------

(when (provided? 'pure-s7)
  (define (string-length str)
    (if (string? str)
	(length str)
	(if (openlet? str)
	    ((let-ref str 'string-length) str)
	    (error 'wrong-type-arg "string-length argument should be a string: ~A" str)))))

(set! (*s7* 'heap-size) 512000)

(define (fannkuch n)
  (call-with-exit
   (lambda (return)
     (let ((perm (make-int-vector n))
	   (perm1 (make-int-vector n))
	   (count (make-int-vector n))
	   (subs (make-vector n))
	   (subs1 (make-vector n))
	   (maxFlipsCount 0)
	   (checksum 0)
	   (r n)
	   (perm0 0))
       
       (do ((i 0 (+ i 1)))
	   ((= i n))
	 (int-vector-set! perm1 i i)
	 (vector-set! subs i (subvector perm 0 (+ i 1)))
	 (vector-set! subs1 i (subvector perm1 1 (+ i 1))))
       
       (do ((permCount #t (not permCount)))
	   (#f)
	 (do ()
	     ((= r 1))
	   (int-vector-set! count (- r 1) r)
	   (set! r (- r 1)))
	 (copy perm1 perm)
	 
	 (do ((flipsCount 0 (+ flipsCount 1))
	      (k (int-vector-ref perm 0) (int-vector-ref perm 0)))
	     ((= k 0)
	      (set! maxFlipsCount (max maxFlipsCount flipsCount))
	      (if permCount 
		  (set! checksum (+ checksum flipsCount)) 
		  (set! checksum (- checksum flipsCount))))
	   (reverse! (vector-ref subs k)))
	 
	 (do ((done #f))
	     (done)
	   (when (= r n)
	     (return (cons checksum maxFlipsCount)))
	   
	   (set! perm0 (int-vector-ref perm1 0))
	   (copy (vector-ref subs1 r) perm1)
	   (int-vector-set! perm1 r perm0)

	   (if (positive? (int-vector-set! count r (- (int-vector-ref count r) 1)))
	       (set! done #t)
	       (set! r (+ r 1)))))))))

;; (fannkuch 7): (228 . 16), 8: (1616 . 22), 9: (8629 . 30), 10: (73196 . 38), 11: (556355 . 51), 12: (3968050 . 65)
(fannkuch 8)
(display (fannkuch 8)) (newline)

;;; --------------------------------------------------------------------------------

(define (wc)
  (let ((nw 0)
	(nc 0)
	(port (open-input-file "/home/bil/test/scheme/bench/src/bib")))
    (do ((newk 0 0)
	 (str (read-line port) (read-line port))
	 (nl 0 (+ nl 1)))
	((eof-object? str)
	 (close-input-port port)
	 (list nl (+ nw nl) nc))
      (set! nc (+ nc 1 (string-length str)))
      (do ((k (char-position #\space str) (char-position #\space str (+ k 1))))
	  ((not k))
	(if (not (= k newk))
	    (set! nw (+ nw 1)))
	(set! newk (+ k 1))))))

(wc) (wc)
(display (wc)) (newline) ; '(31102 851820 4460056)

;;; --------------------------------------------------------------------------------

(define (cat)
  (let ((inport (open-input-file "/home/bil/cl/all-lg-results"))
	(outport (open-output-file "/dev/null"))) 
    (catch #t
      (lambda ()
	(do () (#f) 
	  (write-string (read-line inport #t) outport)))
      (lambda args
	(close-input-port inport)
	(close-output-port outport)))))

(cat)

;;; --------------------------------------------------------------------------------

(define (string-cat n)
  (let ((s "abcdef"))
    (do ((i 0 (+ i 1)))
	((= i 100) 
	 (string-length s))
      (set! s "abcdef")
      (do ()
	  ((> (string-length s) n))
	(set! s (string-append "123" s "456" s "789"))
	(set! s (string-append
		 (substring s (quotient (string-length s) 2))
		 (substring s 0 (+ 1 (quotient (string-length s) 2)))))))))

(display (string-cat 600000)) (newline) ; 524278

;;; --------------------------------------------------------------------------------

(define mbrot
  (let ()
    (define (count cr ci)
      (let ((max-count 64)
	    (radius^2 16.0)
	    (tzr 0.0)
	    (zr cr)
	    (zi ci))
	(do ((zr^2 (* zr zr) (* zr zr))
	     (zi^2 (* zi zi) (* zi zi))
	     (c 0 (+ c 1)))
	    ((or (= c max-count)
		 (> (+ zr^2 zi^2) radius^2))
	     c)
	  (set! tzr (+ (- zr^2 zi^2) cr))
	  (set! zi (+ (* 2.0 zr zi) ci))
	  (set! zr tzr))))

    (define (mb matrix r i step n)
      (do ((x 0 (+ x 1)))
	  ((= x n))
	(do ((y 0 (+ y 1)))
	    ((= y n))
	  (vector-set! matrix x y (count (+ r (* step x)) (+ i (* step y)))))))

    (lambda (n)
      (let ((matrix (make-vector (list n n))))
	(mb matrix -1.0 -0.5 0.005 n)
	(if (not (= (matrix 0 0) 5))
	    (format () ";mbrot: ~A~%" n))))))

(mbrot 140)

;;; --------------------------------------------------------------------------------

;;; this one is not very good

(define binary-tree
  (let ()
    (define (item-check tree)
      (if (not (car tree))
	  1
	  (+ 1 (item-check (car tree)) (item-check (cdr tree)))))
    
    (define (bottom-up-tree depth)
      (if (zero? depth)
	  (cons #f #f)
	  (cons (bottom-up-tree (- depth 1)) 
		(bottom-up-tree (- depth 1)))))
    
    (lambda (N)
      (let* ((min-depth 4)
	     (max-depth (max (+ min-depth 2) N))
	     (stretch-depth (+ max-depth 1))
	     (stretch-tree (bottom-up-tree stretch-depth)))
	(format *stderr* "stretch tree of depth ~D~30Tcheck: ~D~%" stretch-depth (item-check stretch-tree))
	(let ((long-lived-tree (bottom-up-tree max-depth)))
	  (do ((depth min-depth (+ depth 2)))
	      ((> depth max-depth))
	    (let ((iterations (expt 2 (- (+ max-depth min-depth) depth))))
	      (do ((i 0 (+ i 1))
		   (check 0 (+ check (item-check (bottom-up-tree depth)))))
		  ((= i iterations)
		   (format *stderr* "~D~9Ttrees of depth ~D~30Tcheck: ~D~%" iterations depth check)))))
	  (format *stderr* "long lived tree of depth ~D~30Tcheck: ~D~%" max-depth (item-check long-lived-tree)))))))

;(binary-tree 21)
(binary-tree 14)

;;; stretch      tree of  depth 22	 check: 8388607
;;; 2097152	 trees of depth 4	 check: 65011712
;;; 524288	 trees of depth 6	 check: 66584576
;;; 131072	 trees of depth 8	 check: 66977792
;;; 32768	 trees of depth 10	 check: 67076096
;;; 8192	 trees of depth 12	 check: 67100672
;;; 2048	 trees of depth 14	 check: 67106816
;;; 512	         trees of depth 16	 check: 67108352
;;; 128	         trees of depth 18	 check: 67108736
;;; 32	         trees of depth 20	 check: 67108832
;;; long lived   tree of  depth 21	 check: 4194303

;;; --------------------------------------------------------------------------------

(define collatz
  (let ()
    (define (collatz-count-until-1 n)
      (do ((count 0 (+ count 1)))
	  ((= n 1)
	   count)
	(if (logbit? n 0)
	    (set! n (+ (* 3 n) 1))
	    (set! n (ash n -1)))))
    (lambda (N)
      (let ((len 0)
	    (num 0)
	    (cur 0))
	(do ((i 1 (+ i 1)))
	    ((= i N)) ; 300000))
	  (set! cur (collatz-count-until-1 i))
	  (when (< len cur)
	    (set! len cur)
	    (set! num i)))
	(format *stderr* "Maximum stopping distance ~D, starting number ~D\n" len num)))))

;; (collatz 300000)
;; Maximum stopping distance 442, starting number 230631
;; .45 secs

(collatz 40000)

;;; --------------------------------------------------------------------------------

(define prime?                         ; from exobrain.se
  (let ((increments (list 4 2 4 2 4 6 2 6)))
    (set-cdr! (list-tail increments 7) increments)
    (lambda (z)
      (or (memq z '(2 3 5 7))          ; memv...
	  (and (odd? z)
	       (positive? (remainder z 3))	  
	       (positive? (remainder z 5))	  
	       (positive? (remainder z 7))
	       (do ((L increments)
		    (lim (sqrt z))
		    (divisor 11 (+ divisor (car L))))
		   ((or (zero? (remainder z divisor))
			(>= divisor lim))
		    (> divisor lim))
		 (set! L (cdr L))))))))

(let ()
  (define (count-primes limit)          ; for limit=10M 9.2 secs 664579, 1M .36 secs 78497 (+1 for 1?)
    (let ((primes 1))
      (when (even? limit) 
	(set! limit (+ limit 1)))
      (do ((i 3 (+ i 2)))
	  ((= i limit)
	   primes)
	(if (prime? i)
	    (set! primes (+ primes 1))))))
  (display (count-primes 300000)) (newline)) ; 100000: 9592, 200000: 17984, 30000: 25997

;;; --------------------------------------------------------------------------------
;;;
;;; spectral-norm, based on code by Anthony Borla (Computer Benchmarks Game)

(let ((weights #f))

  (define (mulAv n v av)
    (fill! av 0.0)
    (do ((i 0 (+ i 1)))
	((= i n))
      (do ((j 0 (+ j 1)))
          ((= j n))
	(float-vector-set! av i (+ (float-vector-ref av i) 
				   (* (/ 1.0 (+ i (float-vector-ref weights (+ i j))))
				      (float-vector-ref v j)))))))
  
  (define (mulAtV n v atv)
    (fill! atv 0.0)
    (do ((i 0 (+ i 1)))
	((= i n))
      (do ((j 0 (+ j 1)))
          ((= j n))
	(float-vector-set! atv i (+ (float-vector-ref atv i) 
				    (* (/ 1.0 (+ j (float-vector-ref weights (+ i j))))
				       (float-vector-ref v j)))))))
  
  (define (mulAtAv n v atav)
    (let ((u (make-float-vector n 0.0)))
      (mulAv n v u)
      (mulAtV n u atav)))
  
  (define (spectral-norm n)
    (let ((u (make-float-vector n 1.0))
          (v (make-float-vector n 0.0))
          (vBv 0.0) (vV 0.0))
      
      (set! weights (make-float-vector (* 2 n)))
      (do ((i 0 (+ i 1)))
	  ((= i (* 2 n)))
	(float-vector-set! weights i (+ (* 0.5 i (+ i 1)) 1.0)))
      
      (do ((i 0 (+ i 1)))
          ((= i 10))
	(mulAtAv n u v)
	(mulAtAv n v u))
      
      (do ((i 0 (+ i 1)))
          ((= i n))
	(set! vBv (+ vBv (* (float-vector-ref u i) (float-vector-ref v i))))
	(set! vV (+ vV (* (float-vector-ref v i) (float-vector-ref v i)))))
      
      (sqrt (/ vBv vV))))

  (display (spectral-norm 300)) ; (spectral-norm 5500) takes about 14.3 secs
  (newline))

;;; --------------------------------------------------------------------------------
;;;
;;; from Joe Marshall (not a "standard benchmark", but interesting)

(let ()
  (define (palindrome? str)
    (or (< (string-length str) 2)
	(and (char=? (string-ref str 0)
		     (string-ref str (- (string-length str) 1)))
	     (palindrome? (substring str 1 (- (string-length str) 1))))))

  (define (pal-test)
    (do ((i 0 (+ i 1)))
	((= i 50000))
      (palindrome? "abcdefgfedcba")
      (palindrome? "abcdefxfedcba")
      (palindrome? "")
      (palindrome? "abcdefghedcba")))

  (pal-test))

;;; this is 5-6 times faster:
;;; (define (palindrome? str) (string=? str (reverse str)))

;;; --------------------------------------------------------------------------------

;;; nbody (from nbody.gcc)

(define-constant solar_mass (* 4 pi pi))
(define-constant days_per_year 365.24)

(define planet-x    (dilambda (lambda (p) (float-vector-ref p 0)) (lambda (p val) (float-vector-set! p 0 val))))
(define planet-y    (dilambda (lambda (p) (float-vector-ref p 1)) (lambda (p val) (float-vector-set! p 1 val))))
(define planet-z    (dilambda (lambda (p) (float-vector-ref p 2)) (lambda (p val) (float-vector-set! p 2 val))))
(define planet-vx   (dilambda (lambda (p) (float-vector-ref p 3)) (lambda (p val) (float-vector-set! p 3 val))))
(define planet-vy   (dilambda (lambda (p) (float-vector-ref p 4)) (lambda (p val) (float-vector-set! p 4 val))))
(define planet-vz   (dilambda (lambda (p) (float-vector-ref p 5)) (lambda (p val) (float-vector-set! p 5 val))))
(define planet-mass (dilambda (lambda (p) (float-vector-ref p 6)) (lambda (p val) (float-vector-set! p 6 val))))

(define (advance nbodies bodies dt)
  (do ((i 0 (+ i 1)))
      ((= i nbodies))
    (let ((b (vector-ref bodies i)))
      (do ((j (+ i 1) (+ j 1)))
	  ((= j nbodies))
	(let* ((b2 (vector-ref bodies j))
	       (dx (- (planet-x b) (planet-x b2)))
	       (dy (- (planet-y b) (planet-y b2)))
	       (dz (- (planet-z b) (planet-z b2)))
	       (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
	       (mag (/ dt (* distance distance distance))))
	  (set! (planet-vx b) (- (planet-vx b) (* dx (planet-mass b2) mag)))
	  (set! (planet-vy b) (- (planet-vy b) (* dy (planet-mass b2) mag)))
	  (set! (planet-vz b) (- (planet-vz b) (* dz (planet-mass b2) mag)))
	  (set! (planet-vx b2) (+ (planet-vx b2) (* dx (planet-mass b) mag)))
	  (set! (planet-vy b2) (+ (planet-vy b2) (* dy (planet-mass b) mag)))
	  (set! (planet-vz b2) (+ (planet-vz b2) (* dz (planet-mass b) mag)))))))
  (do ((i 0 (+ i 1)))
      ((= i nbodies))
    (let ((b (vector-ref bodies i)))
      (set! (planet-x b) (+ (planet-x b) (* dt (planet-vx b))))
      (set! (planet-y b) (+ (planet-y b) (* dt (planet-vy b))))
      (set! (planet-z b) (+ (planet-z b) (* dt (planet-vz b)))))))

(define (energy nbodies bodies)
  (let ((e 0.0))
    (do ((i 0 (+ i 1)))
	((= i nbodies))
      (let ((b (vector-ref bodies i)))
	(set! e (+ e (* 0.5 (planet-mass b)
			(+ (* (planet-vx b) (planet-vx b))
			   (* (planet-vy b) (planet-vy b))
			   (* (planet-vz b) (planet-vz b))))))
	(do ((j (+ i 1) (+ j 1)))
	    ((= j nbodies))
	  (let* ((b2 (vector-ref bodies j))
		 (dx (- (planet-x b) (planet-x b2)))
		 (dy (- (planet-y b) (planet-y b2)))
		 (dz (- (planet-z b) (planet-z b2)))
		 (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
	    (set! e (- e (/ (* (planet-mass b) (planet-mass b2)) distance)))))))
    e))

(define (offset_momentum nbodies bodies)
  (let ((px 0.0)
	(py 0.0)
	(pz 0.0))
    (do ((i 0 (+ i 1)))
	((= i nbodies))
      (let ((b (vector-ref bodies i)))
	(set! px (+ px (* (planet-vx b) (planet-mass b))))
	(set! py (+ py (* (planet-vy b) (planet-mass b))))
	(set! pz (+ pz (* (planet-vz b) (planet-mass b))))))
    (let ((b (vector-ref bodies 0)))
      (set! (planet-vx b) (/ (- px) solar_mass))
      (set! (planet-vy b) (/ (- py) solar_mass))
      (set! (planet-vz b) (/ (- pz) solar_mass)))))

(define-constant NBODIES 5)

(define bodies (vector
		 (float-vector 0  0  0  0  0  0  solar_mass) ; sun

		 (float-vector 4.84143144246472090e+00 
			       -1.16032004402742839e+00 
			       -1.03622044471123109e-01 
			       (* 1.66007664274403694e-03 days_per_year)
			       (* 7.69901118419740425e-03 days_per_year)
			       (* -6.90460016972063023e-05 days_per_year)
			       (* 9.54791938424326609e-04 solar_mass)) ; jupiter

		 (float-vector 8.34336671824457987e+00 
			       4.12479856412430479e+00 
			       -4.03523417114321381e-01 
			       (* -2.76742510726862411e-03 days_per_year)
			       (* 4.99852801234917238e-03 days_per_year)
			       (* 2.30417297573763929e-05 days_per_year)
			       (* 2.85885980666130812e-04 solar_mass)) ; saturn
		 
		 (float-vector 1.28943695621391310e+01 
			       -1.51111514016986312e+01 
			       -2.23307578892655734e-01 
			       (* 2.96460137564761618e-03 days_per_year)
			       (* 2.37847173959480950e-03 days_per_year)
			       (* -2.96589568540237556e-05 days_per_year)
			       (* 4.36624404335156298e-05 solar_mass)) ; uranus

		 (float-vector 1.53796971148509165e+01 
			       -2.59193146099879641e+01 
			       1.79258772950371181e-01 
			       (* 2.68067772490389322e-03 days_per_year)
			       (* 1.62824170038242295e-03 days_per_year)
			       (* -9.51592254519715870e-05 days_per_year)
			       (* 5.15138902046611451e-05 solar_mass)))) ; neptune

(define (main n)
  (offset_momentum NBODIES bodies)
  (format () "~,9F~%" (energy NBODIES bodies))
  (do ((i 0 (+ i 1)))
      ((= i n))
    (advance NBODIES bodies 0.01))
  (format () "~,9F~%" (energy NBODIES bodies)))

(main 6000)

;;; 1000_out: -0.169075164 -0.169087605


;;; --------------------------------------------------------------------------------
(when (> (*s7* 'profile) 0)
  (show-profile 100))
(exit)
