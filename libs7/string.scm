;; https://srfi.schemers.org/srfi-152/srfi-152.html
;; String library (reduced)

;; see   ~/scheme/srfi-152

;; (load "srfi.scm")


;; derived from srfi-152
(define (%string-copy! to tstart from fstart fend)
  (if (> fstart tstart)
      (do ((i fstart (+ i 1))
	   (j tstart (+ j 1)))
	  ((>= i fend))
	(string-set! to j (string-ref from i)))

      (do ((i (- fend 1)                    (- i 1))
	   (j (+ -1 tstart (- fend fstart)) (- j 1)))
	  ((< i fstart))
	(string-set! to j (string-ref from i)))))

(define (string-concatenate strings)
  (let* ((total (do ((strings strings (cdr strings))
		     (i 0 (+ i (string-length (car strings)))))
		    ((not (pair? strings)) i)))
	 (ans (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
	  (let* ((s (car strings))
		 (slen (string-length s)))
	    (%string-copy! ans i s 0 slen)
	    (lp (+ i slen) (cdr strings)))))
    ans))

(define* (string-join strings (delim " "))
  (let ((buildit (lambda (lis final)
		     (let recur ((lis lis))
		       (if (pair? lis)
			   (cons delim (cons (car lis) (recur (cdr lis))))
			   final)))))

    (cond ((pair? strings)

           ;; infix
	   (string-concatenate
	    (cons (car strings) (buildit (cdr strings) '()))))

	  ((not (null? strings))
	   (error "STRINGS parameter not list." strings string-join))

	  ;; STRINGS is ()

	  (else ""))))

(define (string-parse-start+end proc s args)
  (if (not (string? s)) (error "Non-string value" proc s))
  (let ((slen (string-length s)))
    (if (pair? args)
	(let ((start (car args))
	      (args (cdr args)))
	  (if (and (integer? start) (exact? start) (>= start 0))
	      (receive (end args)
		  (if (pair? args)
		      (let ((end (car args))
			    (args (cdr args)))
			(if (and (integer? end) (exact? end) (<= end slen))
			    (values end args)
			    (error "Illegal substring END spec" proc end s)))
		      (values slen args))
		(if (<= start end) (values args start end)
		    (error "Illegal substring START/END spec"
			   proc start end s)))
	      (error "Illegal substring START spec" proc start s)))
	(values '() 0 slen))))

(define (string-parse-final-start+end proc s args)
  (receive (rest start end) (string-parse-start+end proc s args)
    (if (pair? rest) (error "Extra arguments to procedure" proc rest)
	(values start end))))

;; (define-syntax let-string-start+end
;; (define-macro
;;   (let-string-start+end start end) proc s-exp args-exp body ...)
;;   `(let-values (((start end)
;;                  ,(string-parse-final-start+end proc s-exp args-exp)))
;;   (format #t "start ~A end ~A" start ent)))

;; srfi 152
(define (string-null? s) (zero? (string-length s)))

(define (string-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0))))

;; srfi 152
;;; Returns starting-position in STRING or #f if not true.
;;; This implementation is slow & simple. It is useful as a "spec" or for
;;; comparison testing with fancier implementations.
;;; See below for fast KMP version.

;; _very_ inefficient
(define (string-contains string subs) ;; . maybe-starts+ends)
 ;; (let-string-start+end2 (start1 end1 start2 end2)
 ;;                        string-contains string substring maybe-starts+ends
  (let* ((len1 (length string))
         (len2 (length subs))
	 (i-bound (- len1 len2)))
     (let lp ((i 0))
       (if (> i i-bound)
           #f
           (let ((ss (substring string i)))
	     (if (string-prefix? subs ss)
	         i
	         (lp (+ i 1))))))))

;; srfi 152
(define (%substring s start end)
  (if (and (zero? start) (= end (string-length s))) s
      (substring s start end)))

(define string-take
  (let ((+documentation+
         "(string-take str n) returns first n characters of s")
        (+signature+ "(string-take str n)"))
    (lambda (s n)
      (check-arg string? s string-take)
      (check-arg (lambda (val) (and (integer? n) (exact? n)
				    (<= 0 n (string-length s))))
	         n string-take)
      (%substring s 0 n))))

(define string-take-right
  (let ((+documentation+ "Returns last n characters of str")
        (+signature+ "(string-take-right str n)"))
    (lambda (s n)
      (check-arg string? s string-take-right)
      (let ((len (string-length s)))
        (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	           n string-take-right)
        (%substring s (- len n) len)))))

(define string-drop
  (let ((+documentation+ "(string-drop str n) drop first n characters of str")
        (+signature+ "(string-drop str n)"))
    (lambda (s n)
      (check-arg string? s string-drop)
      (let ((len (string-length s)))
        (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	           n string-drop)
        (%substring s n len)))))

(define (string-drop-right s n)
  (check-arg string? s string-drop-right)
  (let ((len (string-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	       n string-drop-right)
    (%substring s 0 (- len n))))


;; srfi 152
(define string-index
  (let ((+documentation+
         "(string-index str pred) returns location of first char in str satisfying pred. Ex: (string-index \"abc\" (lambda (ch) (equal? ch #\b)))"))
    (lambda (str criterion) ;; . maybe-start+end)
      ;; (let-string-start+end (start end) string-index str maybe-start+end
      (let ((start 0) (end (length str)))
        (let lp ((i start))
          (and (< i end)
               (if (criterion (string-ref str i)) i
                   (lp (+ i 1)))))))))

(define (string-index-right str criterion) ;; . maybe-start+end)
  ;; (let-string-start+end (start end) string-index-right str maybe-start+end
  (let ((start 0) (end (length str)))
	   (let lp ((i (- end 1)))
	     (and (>= i start)
		  (if (criterion (string-ref str i)) i
		      (lp (- i 1)))))))

;; srfi 152
(define (%string-prefix-length s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))

    (if (and (eq? s1 s2) (= start1 start2))	; EQ fast path
	delta

	(let lp ((i start1) (j start2))		; Regular path
	  (if (or (>= i end1)
		  (not (char=? (string-ref s1 i)
			       (string-ref s2 j))))
	      (- i start1)
	      (lp (+ i 1) (+ j 1)))))))

;; (define (%string-prefix? s1 start1 end1 s2 start2 end2)
;;   (let ((len1 (- end1 start1)))
;;     (and (<= len1 (- end2 start2))	; Quick check
;; 	 (= (%string-prefix-length s1 start1 end1
;; 				   s2 start2 end2)
;; 	    len1))))

(define string-prefix?
  (let ((+documentation+ "(string-prefix? pfx s2)")
        (+signature+ "(string-prefix? pfx s2)"))
    (lambda (s1 s2)
      (let* ((len1 (length s1))
             (len2 (length s2))
             (delta (- len2 len1)))
        (and (<= len1 len2)	; Quick check
	     (= (%string-prefix-length s1 0 len1
				       s2 0 len2)
	        len1))))))

(define string-suffix?
  (let ((+documentation+ "(string-suffix? sfx s2)")
        (+signature+ "(string-suffix? sfx s2)"))
    (lambda (s1 s2)
      (let* ((len1 (length s1))
             (len2 (length s2))
             (delta (- len2 len1)))
        (and (<= len1 len2)
             (eq? delta
                  (string-position s1 s2 delta)))))))

;; srfi 13
(define (%string-prefix-length-ci s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))

    (if (and (eq? s1 s2) (= start1 start2))	; EQ fast path
	delta

	(let lp ((i start1) (j start2))		; Regular path
	  (if (or (>= i end1)
		  (not (char-ci=? (string-ref s1 i)
				  (string-ref s2 j))))
	      (- i start1)
	      (lp (+ i 1) (+ j 1)))))))

(define (%string-prefix-ci? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))	; Quick check
	 (= len1 (%string-prefix-length-ci s1 start1 end1
					   s2 start2 end2)))))

(define (string-prefix-ci? s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (and (<= len1 len2)
	 (= len1 (%string-prefix-length-ci s1 0 len1
                                           s2 0 len2)))))

;; srfi 152
(define (string-drop-right s n)
  (check-arg string? s string-drop-right)
  (let ((len (string-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	       n string-drop-right)
    (%substring s 0 (- len n))))

;; string-capitalize from s7test.scm
(define (cl-string x)
  (if (string? x) x
      (if (char? x)
	  (string x)
	  (if (symbol? x)
	      (symbol->string x)
	      (error 'wrong-type-arg "string ~A?" x)))))

(define* (nstring-capitalize str-1 (start 0) end)
  (define (alpha? c)
    (or (char-alphabetic? c)
	(char-numeric? c)))
  (let ((str (cl-string str-1)))
    (let ((nd (if (number? end) end (length str))))
      (do ((i start (+ i 1)))
	  ((= i nd) str)
	(if (alpha? (str i))
	    (if (or (= i 0)
		    (not (alpha? (str (- i 1)))))
		(set! (str i) (char-upcase (str i)))
		(set! (str i) (char-downcase (str i)))))))))

;; upcase char 0, downcase the rest
(define* (string-capitalize str-1 (start 0) end)
  (let ((str (cl-string str-1)))
    (nstring-capitalize (copy str) start end)))

;; s7test.scm
(define* (subseq sequence start end)
  (let* ((len (length sequence))
	 (nd (or (and (number? end) end) len))
	 (size (- nd start))
	 (obj (make sequence size)))
    (do ((i start (+ i 1))
	 (j 0 (+ j 1)))
	((= i nd) obj)
      (set! (obj j) (sequence i)))))

(define (string-left-trim bag str-1)
  (let ((str (cl-string str-1)))
    (if (string? bag) (set! bag (string->list bag)))
    (let ((len (length str)))
      (do ((i 0 (+ i 1)))
	  ((or (= i len)
	       (not (member (str i) bag)))
	   (if (= i 0)
	       str
	       (subseq str i)))))))

(define (string-right-trim bag str-1)
  (let ((str (cl-string str-1)))
    (if (string? bag) (set! bag (string->list bag)))
    (let ((len (length str)))
      (do ((i (- len 1) (- i 1)))
	  ((or (< i 0)
	       (not (member (str i) bag)))
	   (if (= i (- len 1))
	       str
	       (subseq str 0 (+ i 1))))))))

(define (string-trim bag str)
  (string-right-trim bag (string-left-trim bag str)))


;; srfi-152
;; the segment of characters in string1 from start1 to end1 is
;; replaced by the segment of characters in string2 from start2 to
;; end2
(define string-replace
  (let ((+documentation+ "SRFI-152. The segment of characters in string1 from start1 to end1 is replaced by the segment of characters in string2 from start2 to end2.")
        (+signature+ "(string-replace s1 s2 start end1 . maybe-start+end"))
    (lambda (s1 s2 start1 end1 . maybe-start+end)
      ;; (check-substring-spec string-replace s1 start1 end1)
      ;;(let-string-start+end (start2 end2) string-replace s2 maybe-start+end
      (let* ((slen1 (string-length s1))
	     (sublen2 (string-length s2)) ;; (- end2 start2))
	     (alen (+ (- slen1 (- end1 start1)) sublen2))
	     (ans (make-string alen)))
        (%string-copy! ans 0 s1 0 start1)
        (%string-copy! ans start1 s2 0 sublen2) ;; start2 end2)
        (%string-copy! ans (+ start1 sublen2) s1 end1 slen1)
        ans))))

(define string-replace-all
  (let ((+documentation+ "Replaces all occurrences of substring a by substring b")
        (+signature+ "(string-replace-all s a b"))
    (lambda (s a b)
      (let ((alen (string-length a))
            (posn (string-position a s)))
        ;; (format #t "posn: ~A,  ~A: ~A\n" a s posn)
        (while posn
               ;; (format #t "posn: ~A\n" posn)
               ;; (format #t "s: ~A\n" s)
	       (set! s (string-replace s b posn (+ posn alen)))
               (if-let ((pos (string-position a s)))
                       (set! posn pos)
                       (set! posn #f)))
        s))))

;; (string-replace-all "a/../b/../c" ".." "x")

(define (string-tr s char-from char-to)
  (let ((segs (string-split s char-from)))
    (string-join segs (string char-to))))

