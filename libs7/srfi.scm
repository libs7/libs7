
;; srfi 1
;;(define (remove pred l) (filter (lambda (x) (not (pred x))) l))
;; see 'remove' in utils.scm

;; srfi 1
(define (last-pair lis)
  (check-arg pair? lis last-pair)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))

;; srfi 1
(define (append! . lists)
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists) (prev '()))
    (if (not (pair? lists)) prev
	(let ((first (car lists))
	      (rest (cdr lists)))
	  (if (not (pair? first)) (lp rest first)

	      ;; Now, do the splicing.
	      (let lp2 ((tail-cons (last-pair first))
			(rest rest))
		(if (pair? rest)
		    (let ((next (car rest))
			  (rest (cdr rest)))
		      (set-cdr! tail-cons next)
		      (lp2 (if (pair? next) (last-pair next) tail-cons)
			   rest))
		    first)))))))

;; srfi 1
(define (find-tail pred list)
  (check-arg procedure? pred find-tail)
  (let lp ((list list))
    (and (not (null-list? list))
	 (if (pred (car list)) list
	     (lp (cdr list))))))

(define (find pred list)
  (cond ((find-tail pred list) => car)
	(else #f)))

;; r7rs.scm
(define-macro (let-values vars . body)
  (if (and (pair? vars)
	   (pair? (car vars))
	   (null? (cdar vars)))
      `((lambda ,(caar vars)
	  ,@body)
	,(cadar vars))
      `(with-let (apply sublet (curlet)
			(list ,@(map (lambda (v)
				       `((lambda ,(car v)
					   (values ,@(map (lambda (name)
							    (values (symbol->keyword name) name))
							  (let args->proper-list ((args (car v)))
							    (cond ((symbol? args)	(list args))
								  ((not (pair? args))	args)
								  ((pair? (car args))	(cons (caar args) (args->proper-list (cdr args))))
								  (else                 (cons (car args) (args->proper-list (cdr args)))))))))
					 ,(cadr v)))
				     vars)))
	 ,@body)))


;; srfi 8: receive
;; https://srfi.schemers.org/srfi-71/srfi-71.html#SRFI8
;; guile doc: "(receive formals expr body) Evaluate the expression expr, and bind the result values (zero or more) to the formal arguments in the formal argument list formals. formals must have the same syntax like the formal argument list used in lambda (see Lambda). After binding the variables, the expressions in body ... are evaluated in order."

;; srfi 11: let-values etc.

;; srfi 71: extends plain let to handle multiple values


;; srfi 1
;; https://github.com/scheme-requests-for-implementation/srfi-1/blob/master/srfi-1-reference.scm
(define (check-arg pred val caller)
  (let lp ((val val))
       (if (pred val) val (lp (error "Bad argument" val pred caller)))))

(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error "null-list?: argument out of domain" l))))

(define (filter pred lis)			; Sleazing with EQ? makes this
  (check-arg procedure? pred filter)		; one faster.
  (let recur ((lis lis))
    (if (null-list? lis) lis			; Use NOT-PAIR? to handle dotted lists.
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))	; Replicate the RECUR call so
		(if (eq? tail new-tail) lis
		    (cons head new-tail)))
	      (recur tail))))))			; this one can be a tail call.

(define (lset-difference = lis1 . lists)
  (check-arg procedure? = lset-difference)
  (let ((lists (filter pair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (not (member x lis =)))
				 lists))
			lis1)))))

(define (car+cdr pair) (values (car pair) (cdr pair)))

(define (%cars+cdrs lists)
  ;; (call-with-current-continuation
  (call/cc
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
	    ;; (receive (list other-lists) (car+cdr lists)
	    (let-values (((list other-lists) (car+cdr lists)))
	      (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
		  ;; (receive (a d) (car+cdr list)
		  (let-values (((a d) (car+cdr list)))
		    ;; (receive (cars cdrs) (recur other-lists)
		    (let-values (((cars cdrs) (recur other-lists)))
		      (values (cons a cars) (cons d cdrs))))))
	    (values '() '()))))))

;; FIXME: only works for one list
(define any
  (let ((+documentation+ "(any pred lst1 . lists)"))
    (lambda (pred lis1 . lists)
      (check-arg procedure? pred any)
      (if (pair? lists)

          ;; N-ary case
          (let-values (((heads tails) (values (%cars+cdrs (cons lis1 lists)))))
            ;; (receive (heads tails) (%cars+cdrs (cons lis1 lists))
            (display (format #f "hd ~A tl ~A" heads tails)) (newline)
	    (and (pair? heads)
	         (let lp ((heads heads) (tails tails))
	           ;; (receive (next-heads next-tails) (%cars+cdrs tails)
	           (let-values (((next-heads next-tails) (values (%cars+cdrs tails))))
		     (if (pair? next-heads)
		         (or (apply pred heads) (lp next-heads next-tails))
		         (apply pred heads)))))) ; Last PRED app is tail call.

          ;; Fast path
          (and (not (null-list? lis1))
	       (let lp ((head (car lis1)) (tail (cdr lis1)))
	         (if (null-list? tail)
		     (pred head)		; Last PRED app is tail call.
		     (or (pred head) (lp (car tail) (cdr tail))))))))))

(define (every pred lis1 . lists)
  (check-arg procedure? pred every)
  (if (pair? lists)

      ;; N-ary case
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
	(or (not (pair? heads))
	    (let lp ((heads heads) (tails tails))
	      (receive (next-heads next-tails) (%cars+cdrs tails)
		(if (pair? next-heads)
		    (and (apply pred heads) (lp next-heads next-tails))
		    (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (or (null-list? lis1)
	  (let lp ((head (car lis1))  (tail (cdr lis1)))
	    (if (null-list? tail)
		(pred head)	; Last PRED app is tail call.
		(and (pred head) (lp (car tail) (cdr tail))))))))

