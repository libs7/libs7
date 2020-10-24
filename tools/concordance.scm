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

(concord)
(#_exit)


    
    
