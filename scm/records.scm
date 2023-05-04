;; from s7/r7rs.scm

;; records
(define-macro (define-record-type type make ? . fields)
  (let ((obj (gensym))
	(typ (gensym))
	(args (map (lambda (field)
		     (values (list 'quote (car field))
			     (let ((par (memq (car field) (cdr make))))
			       (and (pair? par) (car par)))))
		   fields)))
    `(begin
       (define (,? ,obj)
	 (and (let? ,obj)
	      (eq? (let-ref ,obj ',typ) ',type)))
       
       (define ,make 
         (inlet ',typ ',type ,@args))

       ,@(map
	  (lambda (field)
	    (when (pair? field)
	      (if (null? (cdr field))
		  (values)
		  (if (null? (cddr field))
		      `(define (,(cadr field) ,obj)
			 (let-ref ,obj ',(car field)))
		      `(begin
			 (define (,(cadr field) ,obj)
			   (let-ref ,obj ',(car field)))
			 (define (,(caddr field) ,obj val)
			   (let-set! ,obj ',(car field) val)))))))
	  fields)
       ',type)))

;;; srfi 111:
(define-record-type box-type (box value) box? (value unbox set-box!))

