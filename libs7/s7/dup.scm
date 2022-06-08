;;; dup.scm
;;; (dups size file alloc-lines): 
;;;    find all matches of "size" successive lines in "file" ignoring empty lines and leading/trailing whitespace
;;;    "alloc-lines" is any number bigger than the number of lines in "file"
;;;    (dups 16 "s7.c" 91000) finds all 16-line matches in s7.c which (we wish) has less than 91000 lines in all

(define dup
  (let ((unique #f))

    (define-constant (all-positive? start len)
      (do ((j (+ start len) (- j 1)))
	  ((or (vector-ref unique j)
	       (= j start))
	   j)))

    (lambda (size file alloc-lines)
      (let ((lines (make-vector alloc-lines ""))
	    (original-lines (make-vector alloc-lines ""))
	    (lens (make-int-vector alloc-lines))
	    (linenums (make-int-vector alloc-lines))
	    (size-1 0)
	    (reported-already (make-vector alloc-lines #f)))
	(set! unique (make-vector alloc-lines #f))
	
	(call-with-input-file file
	  (lambda (p)
	    ;; get lines, original and trimmed
	    (let ((total-lines 
		   (do ((i 0 (+ i 1))
			(j 0)
			(line (read-line p) (read-line p)))
		       ((eq? line #<eof>) j)
		     ;; save original lines
		     (vector-set! original-lines i line)
		     (let ((len (length line)))
		       (when (> len 0)
			 ;; trim leading whitespace
			 (do ((k 0 (+ k 1)))
			     ((or (= k len)
				  (not (char-whitespace? (string-ref line k))))
			      (when (> k 0)
				(set! line (substring line k))
				(set! len (- len k)))))
			 ;; trim trailing whitespace
			 (when (> len 0)
			   (do ((j (- len 1) (- j 1)))
			       ((or (< j 0)
				    (not (char-whitespace? (string-ref line j))))
				(unless (= j (- len 1))
				  (set! line (substring line 0 (+ j 1)))
				  (set! len (+ j 1)))))
			   (when (> len 0)
			     (int-vector-set! linenums j i)
			     (vector-set! lines j line)
			     (int-vector-set! lens j (length line))
			     (set! j (+ j 1)))))))))
	      
	      (set! size (min size total-lines))
	      (set! size-1 (- size 1))
	      ;; (format *stderr* "lines: ~S~%" total-lines)         ; 84201 2-jul-19, 89690 29-Aug-20

	      ;; mark unmatchable strings
	      (let ((sortv (make-vector total-lines)))
		(do ((i 0 (+ i 1)))
		    ((= i total-lines))
		  (vector-set! sortv i (cons (vector-ref lines i) i)))
		(set! sortv (sort! sortv (lambda (a b)
					   (string<? (car a) (car b)))))
		(let ((unctr -1)
		      (matches #f)
		      (current (vector-ref sortv 0)))
		  (for-each (lambda (srt)
			      (if (string=? (car current) (car srt))
				  (set! matches #t)
				  (begin
				    (unless matches
				      (int-vector-set! lens (cdr current) unctr)
				      (vector-set! unique (cdr current) #t) ; unique = (negative? (lens...))
				      (set! unctr (- unctr 1)))
				    (set! matches #f)
				    (set! current srt))))
			    sortv)
		  ;; (format *stderr* "unmatched: ~D~%" (abs unctr)) ; 33796
		  ))

	      ;; look for matches
	      (do ((first #t #t)
		   (last-line (- total-lines size))
		   (i 0 (+ i 1)))
		  ((>= i last-line)) ; >= because i is set below
		(unless (vector-ref reported-already i) ; this match was reported earlier
		  (let ((j (all-positive? i size-1)))   ; is a match possible?
		    (if (not (= j i))
			(set! i j)
			(let ((lenseq (subvector lens i (+ i size)))
			      (lineseq (subvector lines i (+ size i))))
			  (do ((k (+ i 1) (+ k 1)))
			      ((or (>= i k)   ; i incremented by full-size when local matches end
				   (>= k last-line)))
			    (let ((jk (all-positive? k size-1)))
			      (if (not (= jk k))
				  (set! k jk)
				  (when (and (equal? lenseq (subvector lens k (+ size k)))
					     (equal? lineseq (subvector lines k (+ size k))))
				    (vector-set! reported-already k #t)
				    (let ((full-size size))
				      (do ((nk (+ k size) (+ nk 1))
					   (ni (+ i size) (+ ni 1)))  ; here if i==k we run to the end and quit
					  ((or (= nk total-lines)
					       (not (= (int-vector-ref lens ni) (int-vector-ref lens nk)))
					       (not (string=? (vector-ref lines ni) (vector-ref lines nk))))
					   (set! full-size (- nk k))))
				      (if first
					  (let ((first-line (int-vector-ref linenums i)))
					    (vector-set! reported-already i #t)
					    (format *stderr* "~NC~%~{~A~%~}~%  lines ~D ~D" 8 #\- ; lineseq 
						    (subvector original-lines first-line (int-vector-ref linenums (+ i size)))
						    first-line
						    (int-vector-ref linenums k))
					    (set! first #f))
					  (format *stderr* " ~D" (int-vector-ref linenums k)))
				      (set! i (+ i full-size))
				      (when (< size full-size)
					(format *stderr* "[~D]" full-size)))))))
			  (unless first
			    (format *stderr* "~%")))))))))))))
  )

(dup 16 "s7.c" 110000)
;(dup 12 "s7.c" 110000)
;(dup 12 "ffitest.c" 10000)
;(dup 8 "ffitest.c" 10000)
;(dup 1 "s7test.scm" 110000)

(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)
