;;; string and char timing

(define (concord)
  (call-with-output-file "test.cx"
    (lambda (op)
      (call-with-input-file "s7.c"
	(lambda (ip)
	  (let ((words (make-hash-table))
		(cur-word "")
		(cur-line 1)
		(last-c #\null))
	    (do ((c (read-char ip) (read-char ip)))
		((eof-object? c)
		 (for-each (lambda (w)
			     (format op "~A: ~S~%" (car w) (reverse (cdr w))))
			   words))
	      (if (or (char-alphabetic? c)
		      (char=? c #\_)
		      (and (char-numeric? c)
			   (positive? (length cur-word))))
		  (set! cur-word (string-append cur-word (string c)))
		  (begin
		    (if (char=? c #\newline)
			(set! cur-line (+ cur-line 1))
			(if (and (char=? c #\*)
				 (char=? last-c #\/))
			    (let ((last-c1 #\null))
			      (do ((c1 (read-char ip) (read-char ip)))
				  ((and (char=? c1 #\/)
					(char=? last-c1 #\*)))
				(if (char=? c1 #\newline)
				    (set! cur-line (+ cur-line 1)))
				(set! last-c1 c1)))
			    (if (and (char=? c #\")
				     (not (char=? last-c #\')))               ; '"' 
				(let ((last-c1 #\null)
				      (last-c2 #\null))
				  (do ((c1 (read-char ip) (read-char ip)))
				      ((and (char=? c1 #\")
					    (or (not (char=? last-c1 #\\))    ; \"
						(char=? last-c2 #\\))))       ; \\"
				    (if (char=? c1 #\newline)
					(set! cur-line (+ cur-line 1)))
				    (set! last-c2 last-c1)
				    (set! last-c1 c1))))))
		    (set! last-c c)
		    (when (positive? (length cur-word))
		      (hash-table-set! words cur-word 
				       (cons cur-line (or (hash-table-ref words cur-word) ()))))
		    (set! cur-word ""))))))))))


;;; --------------------------------
;;; various simple cases

(define (strcop str) ; opt_dotimes
  (let* ((len (length str))
	 (new-str (make-string len)))
    (do ((i 0 (+ i 1)))
	((= i len) new-str)
      (string-set! new-str i (string-ref str i)))))

(define (strup str)
  (let ((len (length str))
	(new-str (copy str)))
    (do ((i 0 (+ i 1)))
	((= i len) new-str)
      (string-set! new-str i (char-upcase (string-ref str i))))))


(define tc-cpos ; op_tc_if_a_z_if_a_z_la [opt]
  (let ((len 0)
	(c #f)
	(str #f))
    (define (cpos-1 pos)
      (if (= pos len)
	  #f
	  (if (char=? c (string-ref str pos))
	       pos
	       (cpos-1 (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (cpos-1 0))))

(define rev-cpos  ; op_tc_if_a_z_if_a_la_z [opt, same as direct case]
  (let ((len 0)
	(c #f)
	(str #f))
    (define (cpos-rev pos)
      (if (= pos len)
	  #f
	  (if (not (char=? c (string-ref str pos)))
	      (cpos-rev (+ pos 1))
	      pos)))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (cpos-rev 0))))

(define tc2-cpos ; op_tc_if_a_z_if_a_z_laa
  (let ((len 0)
	(str #f))
    (define (cpos-2 c pos)
      (if (= pos len)
	  #f
	  (if (char=? c (string-ref str pos))
	       pos
	       (cpos-2 c (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! str str1)
      (cpos-2 c1 0))))

(define tc3-cpos ; eval? (there is no op_tc_if_a_z_if_a_z_l3a but the 2 case is slower -- 2 case is not s7_optimized)
  (let ((len 0))
    (define (cpos-3 c str pos)
      (if (= pos len)
	  #f
	  (if (char=? c (string-ref str pos))
	       pos
	       (cpos-3 c str (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (cpos-3 c1 str1 0))))

(define and-cpos 
  (let ((len 0)
	(c #f)
	(str #f))
    (define (and-cpos-1 pos)
      (and (< pos len)
	   (if (char=? c (string-ref str pos))
	       pos
	       (and-cpos-1 (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (and-cpos-1 0))))

(define andrev-cpos 
  (let ((len 0)
	(c #f)
	(str #f))
    (define (andrev-cpos-1 pos)
      (and (< pos len)
	   (if (not (char=? c (string-ref str pos)))
	       (andrev-cpos-1 (+ pos 1))
	       pos)))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (andrev-cpos-1 0))))

(define cond-cpos ; op_tc_if_a_z_if_a_z_la [opt]
  (let ((len 0)
	(c #f)
	(str #f))
    (define (cond-cpos-1 pos)
      (cond ((= pos len)
	     #f)
	    ((char=? c (string-ref str pos))
	     pos)
	    (else (cond-cpos-1 (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (cond-cpos-1 0))))

(define condrev-cpos 
  (let ((len 0)
	(c #f)
	(str #f))
    (define (condrev-cpos-1 pos)
      (cond ((= pos len)
	     #f)
	    ((not (char=? c (string-ref str pos)))
	     (condrev-cpos-1 (+ pos 1)))
	    (else pos)))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (condrev-cpos-1 0))))

(define (do-cpos c str) ; op_dox
  (do ((len (length str))
       (i 0 (+ i 1)))
      ((or (= i len)
	   (char=? c (string-ref str i)))
       (and (< i len)
	    i))))

(define (call-cpos c str)
  (call-with-exit
   (lambda (return)
     (do ((len (length str))
	  (i 0 (+ i 1)))
	 ((= i len) #f)
       (if (char=? c (string-ref str i))
	   (return i))))))


(define tc-spos ; op_tc_if_a_z_if_a_z_la, substr+start&end [opt]
  (let ((len 0)
	(flen 0)
	(slen 0)
	(find #f)
	(str #f))
    (define (spos-1 pos)
      (if (= pos slen)
	  #f
	   (if (string=? find (substring str pos (+ pos flen)))
	       pos
	       (spos-1 (+ pos 1)))))
    (lambda (find1 str1)
      (set! len (length str1))
      (set! flen (length find1))
      (set! slen (- len flen -1))
      (set! str str1)
      (set! find find1)
      (spos-1 0))))

(define and-spos
  (let ((len 0)
	(flen 0)
	(slen 0)
	(find #f)
	(str #f))
    (define (and-spos-1 pos)
      (and (< pos slen)
	   (if (string=? find (substring str pos (+ pos flen)))
	       pos
	       (and-spos-1 (+ pos 1)))))
    (lambda (find1 str1)
      (set! len (length str1))
      (set! flen (length find1))
      (set! slen (- len flen -1))
      (set! str str1)
      (set! find find1)
      (and-spos-1 0))))

(define andrev-spos
  (let ((len 0)
	(flen 0)
	(slen 0)
	(find #f)
	(str #f))
    (define (andrev-spos-1 pos)
      (and (< pos slen)
	   (if (not (string=? find (substring str pos (+ pos flen))))
	       (andrev-spos-1 (+ pos 1))
	       pos)))
    (lambda (find1 str1)
      (set! len (length str1))
      (set! flen (length find1))
      (set! slen (- len flen -1))
      (set! str str1)
      (set! find find1)
      (andrev-spos-1 0))))

(define cond-spos ; op_tc_if_a_z_if_a_z_la [opt]
  (let ((len 0)
	(flen 0)
	(slen 0)
	(find #f)
	(str #f))
    (define (cond-spos-1 pos)
      (cond ((= pos slen)
	     #f)
	    ((string=? find (substring str pos (+ pos flen)))
	     pos)
	    (else (cond-spos-1 (+ pos 1)))))
    (lambda (find1 str1)
      (set! len (length str1))
      (set! flen (length find1))
      (set! slen (- len flen -1))
      (set! str str1)
      (set! find find1)
      (cond-spos-1 0))))

(define (do-spos find str)
  (let ((len (length str))
	(flen (length find)))
    (do ((slen (- len flen -1))
	 (i 0 (+ i 1)))
	((or (= i slen)
	     (string=? find (substring str i (+ i flen))))
	 (and (< i slen)
	      i)))))

(define (call-spos find str)
  (let* ((len (length str))
	 (flen (length find))
	 (slen (- len flen -1)))
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i slen) #f)
	 (if (string=? find (substring str i (+ i flen)))
	     (return i)))))))


(format *stderr* "strcop ~S: ~S~%" "asdfghjkl" (strcop "asdfghjkl"))
(format *stderr* "strup ~S: ~S~%" "abcdefghij" (strup "abcdefghij"))

(format *stderr* "tc-cpos ~C ~S: ~S~%" #\a "123456789a12343" (tc-cpos #\a "123456789a12343"))
(format *stderr* "tc2-cpos ~C ~S: ~S~%" #\a "123456789a12343" (tc2-cpos #\a "123456789a12343"))
(format *stderr* "tc3-cpos ~C ~S: ~S~%" #\a "123456789a12343" (tc3-cpos #\a "123456789a12343"))
(format *stderr* "do-cpos ~C ~S: ~S~%" #\a "123456789a12343" (do-cpos #\a "123456789a12343"))
(format *stderr* "and-cpos ~C ~S: ~S~%" #\a "123456789a12343" (and-cpos #\a "123456789a12343"))
(format *stderr* "andrev-cpos ~C ~S: ~S~%" #\a "123456789a12343" (andrev-cpos #\a "123456789a12343"))
(format *stderr* "call-cpos ~C ~S: ~S~%" #\a "123456789a12343" (call-cpos #\a "123456789a12343"))
(format *stderr* "cond-cpos ~C ~S: ~S~%" #\a "123456789a12343" (cond-cpos #\a "123456789a12343"))
(format *stderr* "condrev-cpos ~C ~S: ~S~%" #\a "123456789a12343" (condrev-cpos #\a "123456789a12343"))
(format *stderr* "rev-cpos ~C ~S: ~S~%" #\a "123456789a12343" (rev-cpos #\a "123456789a12343"))

(format *stderr* "tc-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdfhjgfrkl" (tc-spos "asdf" "fdsghjkasdfhjgfrkl"))
(format *stderr* "do-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdfhjgfrkl" (do-spos "asdf" "fdsghjkasdfhjgfrkl"))
(format *stderr* "and-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdfhjgfrkl" (and-spos "asdf" "fdsghjkasdfhjgfrkl"))
(format *stderr* "tc-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdf" (tc-spos "asdf" "fdsghjkasdf"))
(format *stderr* "do-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdf" (do-spos "asdf" "fdsghjkasdf"))
(format *stderr* "and-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdf" (and-spos "asdf" "fdsghjkasdf"))
(format *stderr* "andrev-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdf" (andrev-spos "asdf" "fdsghjkasdf"))
(format *stderr* "tc-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" (tc-spos "asdf" "fdsghjkasd"))
(format *stderr* "do-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" (do-spos "asdf" "fdsghjkasd"))
(format *stderr* "and-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" (and-spos "asdf" "fdsghjkasd"))
(format *stderr* "andrev-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" (andrev-spos "asdf" "fdsghjkasd"))
(format *stderr* "call-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" (call-spos "asdf" "fdsghjkasd"))
(format *stderr* "cond-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" (cond-spos "asdf" "fdsghjkasd"))


(define-macro (time . expr) 
  `(let ((start (*s7* 'cpu-time)))
     (do ((k 0 (+ k 1))) ((= k 4)) ,@expr)
     (- (*s7* 'cpu-time) start)))

(newline *stderr*)

(define (simple-tests size)
  (let ((bigstr (make-string size)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (string-set! bigstr i (integer->char (+ 33 (random 94)))))
    (string-set! bigstr (- size 10) #\space)
    (string-set! bigstr (- size 9) #\a)

    (let ((t1 (time (strup bigstr)))
	  (t2 (time (string-upcase bigstr))))
      (format *stderr* "strup: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (strcop bigstr)))
      (set! t2 (time (copy bigstr)))
      (format *stderr* "strcop: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t2 (* 0.5 (time (char-position #\space bigstr) (char-position #\space bigstr))))
      (set! t1 (time (do-cpos #\space bigstr)))
      (format *stderr* "do-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))


      (set! t1 (time (tc-cpos #\space bigstr)))
      (format *stderr* "tc-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (tc2-cpos #\space bigstr)))
      (format *stderr* "tc2-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (tc3-cpos #\space bigstr)))
      (format *stderr* "tc3-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (and-cpos #\space bigstr)))
      (format *stderr* "and-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (andrev-cpos #\space bigstr)))
      (format *stderr* "andrev-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (call-cpos #\space bigstr)))
      (format *stderr* "call-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (cond-cpos #\space bigstr)))
      (format *stderr* "cond-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (condrev-cpos #\space bigstr)))
      (format *stderr* "condrev-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (rev-cpos #\space bigstr)))
      (format *stderr* "rev-cpos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))


      (set! t2 (* 0.5 (time (string-position " a" bigstr) (string-position " a" bigstr))))
      (set! t1 (time (tc-spos " a" bigstr)))
      (format *stderr* "tc-spos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (do-spos " a" bigstr)))
      (format *stderr* "do-spos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (and-spos " a" bigstr)))
      (format *stderr* "and-spos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (andrev-spos " a" bigstr)))
      (format *stderr* "andrev-spos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (call-spos " a" bigstr)))
      (format *stderr* "call-spos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2)))

      (set! t1 (time (cond-spos " a" bigstr)))
      (format *stderr* "cond-spos: ~G ~G: ~D~%" t1 t2 (round (/ t1 t2))))

    (do ((i 0 (+ i 1)))
	((= i 20))
      (strup bigstr)
      (string-upcase bigstr)
      (tc-cpos #\space bigstr)
      (do-cpos #\space bigstr)
      (char-position #\space bigstr)
      (tc-spos " a" bigstr)
      (do-spos " a" bigstr)
      (string-position " a" bigstr)
      (strcop bigstr)
      (copy bigstr))))


;;; --------------------------------

(define (searcher)
  ;; }\n}
  (call-with-input-file "s7.c"
    (lambda (p)
      (let ((last1 ""))
	(do ((this (read-line p) (read-line p))
	     (line 0 (+ line 1)))
	    ((eq? this #<eof>))
	  (let ((len (length this)))
	    (unless (or (= len 0)
			(char=? (string-ref this 0) #\}))
	      (do ((i 0 (+ i 1)))
		  ((or (>= i len)
		       (not (char-whitespace? (string-ref this i))))
		   (set! this (substring this i))))
	      (when (and (> (length this) 0) (char=? (string-ref this 0) #\})
			 (> (length last1) 0) (char=? (string-ref last1 0) #\}))
		(format #f "~D ~S~%" line last1)))
	    (set! last1 this)))))))


;;; --------------------------------

(concord)
(simple-tests 100000)
(searcher)

(#_exit)
