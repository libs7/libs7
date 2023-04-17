;; (display "loading libs7/string.scm") (newline)

;; https://srfi.schemers.org/srfi-152/srfi-152.html
;; String library (reduced)

;; see   ~/scheme/srfi-152

(load "s7/r7rs.scm")
(load "srfi-14.scm")

(define CCRED (format #f "~C[0;31m" #\escape))
(define CCGRN (format #f "~C[0;32m" #\escape))
(define CCYEL (format #f "~C[0;33m" #\escape))
(define CCBLU (format #f "~C[0;34m" #\escape))
(define CCMAG (format #f "~C[0;35m" #\escape))
(define CCCYN (format #f "~C[0;36m" #\escape))
(define CCWHT (format #f "~C[0;37m" #\escape))
(define CCRESET (format #f "~C[0m" #\escape))

(define (red text) (format #f "~C[31m~A~C[0m" #\escape text #\escape))
(define (green text) (format #f "~C[32m~A~C[0m" #\escape text #\escape))
(define (yellow text) (format #f "~C[33m~A~C[0m" #\escape text #\escape))
(define (blue text) (format #f "~C[34m~A~C[0m" #\escape text #\escape))
(define (magenta text) (format #f "~C[35m~A~C[0m" #\escape text #\escape))
(define (cyan text) (format #f "~C[36m~A~C[0m" #\escape text #\escape))
(define (white text) (format #f "~C[37m~A~C[0m" #\escape text #\escape))

;; background
(define (bgred text) (format #f "~C[41m~A~C[0m" #\escape text #\escape))
(define (bggreen text) (format #f "~C[42m~A~C[0m" #\escape text #\escape))
(define (bgyellow text) (format #f "~C[43m~A~C[0m" #\escape text #\escape))
(define (bgblue text) (format #f "~C[44m~A~C[0m" #\escape text #\escape))
(define (bgmagenta text) (format #f "~C[45m~A~C[0m" #\escape text #\escape))
(define (bgcyan text) (format #f "~C[46m~A~C[0m" #\escape text #\escape))
(define (bgwhite text) (format #f "~C[47m~A~C[0m" #\escape text #\escape))

;; underlined
(define (ured text) (format #f "~C[4;31m~A~C[0m" #\escape text #\escape))
(define (ugreen text) (format #f "~C[4;32m~A~C[0m" #\escape text #\escape))
(define (uyellow text) (format #f "~C[4;33m~A~C[0m" #\escape text #\escape))
(define (ublue text) (format #f "~C[4;34m~A~C[0m" #\escape text #\escape))
(define (umagenta text) (format #f "~C[4;35m~A~C[0m" #\escape text #\escape))
(define (ucyan text) (format #f "~C[4;36m~A~C[0m" #\escape text #\escape))
(define (uwhite text) (format #f "~C[4;37m~A~C[0m" #\escape text #\escape))


(define (stringify x)
  (if (symbol? x) (symbol->string x)
      (if (string? x) x
          (if (number? x)
              (number->string x)))))

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

;;; Returns three values: rest start end
;; (define (string-parse-start+end proc s args)
;;   (if (not (string? s)) (error "Non-string value" proc s))
;;   (let ((slen (string-length s)))
;;     (if (pair? args)
;; 	(let ((start (car args))
;; 	      (args (cdr args)))
;; 	  (if (and (integer? start) (exact? start) (>= start 0))
;; 	      (receive (end args)
;; 		  (if (pair? args)
;; 		      (let ((end (car args))
;; 			    (args (cdr args)))
;; 			(if (and (integer? end) (exact? end) (<= end slen))
;; 			    (values end args)
;; 			    (error "Illegal substring END spec" proc end s)))
;; 		      (values slen args))
;; 		(if (<= start end) (values args start end)
;; 		    (error "Illegal substring START/END spec"
;; 			   proc start end s)))
;; 	      (error "Illegal substring START spec" proc start s)))
;; 	(values '() 0 slen))))

;; (define (string-parse-final-start+end proc s args)
;;   (receive (rest start end) (string-parse-start+end proc s args)
;;     (if (pair? rest) (error "Extra arguments to procedure" proc rest)
;; 	(values start end))))

;; (define-syntax let-string-start+end
;; (define-macro
;;   (let-string-start+end start end) proc s-exp args-exp body ...)
;;   `(let-values (((start end)
;;                  ,(string-parse-final-start+end proc s-exp args-exp)))
;;   (format #t "start ~A end ~A" start ent)))

;; srfi 152
(define (string-null? s) (zero? (string-length s)))

(define string-split
  (let ((+documentation+
         "(string-split str ch) splits str on char ch")
        (+signature+ "(string-split str ch)"))
    (lambda (str ch)
      (let ((ch (if (char? ch) ch
                    (if (string? ch)
                        (if (= 1 (string-length ch))
                            (car (string->list ch))
                        (error 'wrong-type-arg "second arg must be a char")))))
            (len (string-length str)))
        (letrec
            ((split
              (lambda (a b)
                (cond
                 ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
                 ((char=? ch (string-ref str b)) (if (= a b)
                                                     (split (+ 1 a) (+ 1 b))
                                                     (cons (substring str a b) (split b b))))
                 (else (split a (+ 1 b)))))))
          (split 0 0))))))

(define string-join
  (let ((+documentation+ "split str on delim string, default: space ")
        (+signature+ "(string-join strings (delim str))"))
    (lambda* (strings (delim " "))
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

	             (else ""))))))

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

(define string-index-right
  (let ((+documentation+ "index string from right. criterion: char-set or char or predicate")
        (+signature+ "(string-index-right s (criterion fn) (start 0) (end (string-length str)))"))
    (lambda*
     (str criterion (start 0) (end (string-length str)))
     ;; (format #t "~A: ~A~%" (blue "string-index-right") str)
     ;; (let ((end (string-length str)))
     (cond ((char? criterion)
	    (let lp ((i (- end 1)))
	      (and (>= i start)
		   (if (char=? criterion (string-ref str i)) i
		       (lp (- i 1))))))
	   ((char-set? criterion)
	    (let lp ((i (- end 1)))
	      (and (>= i start)
		   (if (char-set-contains? criterion (string-ref str i)) i
		       (lp (- i 1))))))
	   ((procedure? criterion)
	    (let lp ((i (- end 1)))
	      (and (>= i start)
		   (if (criterion (string-ref str i)) i
		       (lp (- i 1))))))
	   (else (error "Second param is neither char-set, char, or predicate procedure."
		        string-index-right criterion))))))

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

(define string-trim
  (let ((+documentation+
         "(string-trim bag str) trims chars in bag from ends of str")
        (+signature+ "(string-trim bag str)"))
    (lambda (bag str)
      (string-right-trim bag (string-left-trim bag str)))))

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

(define string-skip-right
  (let ((+documentation+ "complement of string-index-right")
        (+signature+ "(string-skip-right s (criterion fn) (start 0) (end #f))"))
    (lambda*
     (str criterion (start 0) (end (string-length str)))
     ;; (format #t "~A: ~A~%" (blue "string-skip-right") str)
     ;; (format #t "~A: ~A, ~A~%" (blue "start, end") start end)
     ;; (format #t "~A: ~A~%\n" (blue "criterion") criterion)
     ;; (format #t "~A: ~A~%" (blue "criterion t") (type-of criterion))
     ;; (let ((start 0) (end (length str)))
     (cond ((char? criterion)
	    (let lp ((i (- end 1)))
              ;; (format #t "~A: ~A~%" (red "char lp") i)
	      (and (>= i start)
		   (if (char=? criterion (string-ref str i))
		       (lp (- i 1))
		       i))))

	   ((char-set? criterion)
            ;; (format #t "~A~%" (red "Char-set lp"))
	    (let lp ((i (- end 1)))
              ;; (format #t "~A: ~A~%" (red "sr") (string-ref str i))
              ;; (format #t "~A: ~A~%" (red "sr t") (type-of (string-ref str i)))
              ;; (format #t "~A: ~A~%" (red "cscon?")
              ;; (char-set-contains? criterion (string-ref str i)))
	    (and (>= i start)
	         (if (char-set-contains? criterion (string-ref str i))
	             (lp (- i 1))
	             i))))

           ((procedure? criterion)
            (let lp ((i (- end 1)))
	      (and (>= i start)
	           (if (criterion (string-ref str i)) (lp (- i 1))
		       i))))
           (else (error 'fixme
                        "CRITERION param is neither char-set or char."
		        #|string-skip-right criterion|#))))))

;;; string-tokenize s [token-set start end] -> list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Break S up into a list of token strings, where a token is a maximal
;;; non-empty contiguous sequence of chars belonging to TOKEN-SET.
;;; (string-tokenize "hello, world") => ("hello," "world")

(define string-tokenize
(let ((+documentation+ "tokenizes string (by whitespace only, for now)")
      (+signature+ "(string-tokenize s (token-chars #f) (start 0) (end #f))"))
  (lambda*
   (s (token-chars (lambda (ch) (member ch '(#\space #\newline))))
      (start 0) (end (string-length s)))
   ;; (format #t "~A: ~A~%" (blue "string-tokenize") s)
   ;; (let ((end (if end end )))
   (let* lp ((i end) (ans '()))
         ;; (format #t "~A: ~A~%" (magenta "loop ans") ans)
         ;; (format #t "~A: ~A, ~A, ~A~%" (magenta "start, end, i") start end i)
         ;; (format #t "~A: ~A~%" (magenta "string") s)
	 (cond ((and (< start i) (string-index-right s token-chars start i)) =>
	             (lambda (tend-1)
                       ;; (format #t "~A: ~A~%" (cyan "tend-1") tend-1)
		       (let ((tend (+ 1 tend-1)))
		         (cond ((string-skip-right s token-chars start tend-1) =>
			        (lambda (tstart-1)
                                  ;; (format #t "~A: ~A~%" (cyan "tstart-1") tstart-1)
			          (lp tstart-1
				      (cons (substring s (+ 1 tstart-1) tend)
				            ans))))
			       (else (cons (substring s start tend) ans))))))
	       (else ans))))))

;; (let ((s "a b
;; c

;; x"))
;;   (string-tokenize s char-set:graphic))
