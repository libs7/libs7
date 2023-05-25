;; (display "loading libs7/utils.scm") (newline)

;; s7test.scm
(define-macro (defstruct struct-name . fields)
	  (let* ((name (if (list? struct-name) (car struct-name) struct-name))
		 (sname (if (string? name) name (symbol->string name)))

		 (fsname (if (list? struct-name)
			     (let ((cname (assoc :conc-name (cdr struct-name))))
			       (if cname
				   (symbol->string (cadr cname))
				   sname))
			     sname))

		 (make-name (if (list? struct-name)
				(let ((cname (assoc :constructor (cdr struct-name))))
				  (if cname
				      (cadr cname)
				      (symbol "make-" sname)))
				(symbol "make-" sname)))

		 (copy-name (if (list? struct-name)
				(let ((cname (assoc :copier (cdr struct-name))))
				  (if cname
				      (cadr cname)
				      (symbol "copy-" sname)))
				(symbol "copy-" sname)))

		 (field-names (map (lambda (n)
				     (symbol->string (if (list? n) (car n) n)))
				   fields))

		 (field-types (map (lambda (field)
				     (if (list? field)
					 (apply (lambda* (val type read-only) type) (cdr field))
					 #f))
				   fields))

		 (field-read-onlys (map (lambda (field)
					  (if (list? field)
					      (apply (lambda* (val type read-only) read-only) (cdr field))
					      #f))
					fields)))
	    `(begin

	       (define ,(symbol sname "?")
		 (lambda (obj)
		   (and (vector? obj)
			(eq? (obj 0) ',(string->symbol sname)))))

	       (define* (,make-name
			 ,@(map (lambda (n)
				  (if (and (list? n)
					   (>= (length n) 2))
				      (list (car n) (cadr n))
				      (list n #f)))
				fields))
		 (vector ',(string->symbol sname) ,@(map string->symbol field-names)))

	       (define ,copy-name copy)

	       ,@(map (let ((ctr 1))
			(lambda (n type read-only)
			  (let ((val (if read-only
					 `(define ,(symbol fsname "-" n)
					    (lambda (arg) (arg ,ctr)))
					 `(define ,(symbol fsname "-" n)
					    (dilambda
					     (lambda (arg) (arg ,ctr))
					     (lambda (arg val) (set! (arg ,ctr) val)))))))
			    (set! ctr (+ 1 ctr))
			    val)))
		      field-names field-types field-read-onlys))))

;;; ----------------
;;; stuff.scm
(define concatenate
  (let ((+documentation+ "(concatenate type . sequences) concatenates sequences returning an object of type:\n\
    (concatenate vector '(1 2) #(3 4)) -> #(1 2 3 4)"))
    (lambda (type . sequences)
      (apply type (apply sequences->list sequences)))))

(define collect-if
  (let ((+documentation+ "(collect-if type func sequence) gathers the elements of sequence that satisfy func, and returns them via type:\n\
    (collect-if list integer? #(1.4 2/3 1 1+i 2)) -> '(1 2)"))
    (lambda (type f sequence)
      (unless (sequence? sequence)
	(error 'wrong-type-arg "collect-if: sequence arg is ~A" sequence))
      (if (eq? type hash-table)
	  (apply hash-table (map (lambda (arg) (if (f arg) (values (car arg) (cdr arg)) (values))) sequence))
	  (apply type (map (lambda (arg) (if (f arg) arg (values))) sequence))))))

(define remove-if
  (let ((+documentation+ "(remove-if type f sequence) returns via type the elements of sequence that do not satisfy func:\n\
    (remove-if list integer? #(1.4 2/3 1 1+i 2)) -> '(1.4 2/3 1+1i)"))
    (lambda (type f sequence)
      (unless (sequence? sequence)
	(error 'wrong-type-arg "remove-if: sequence arg is ~A" sequence))
      (collect-if type (lambda (obj) (not (f obj))) sequence))))

;;; ----------------
;; s7test.scm
(define* (find-if predicate sequence from-end (start 0) end (key identity))
  (let* ((len (length sequence))
	 (nd (or (and (number? end) end) len))) ; up to but not including end
    (if (< nd start)
	(error 'out-of-range "~A :start ~A is greater than ~A ~A" (*function* (curlet)) start (if end ":end" "length") nd))
    (call-with-exit
     (lambda (return)
       (if (not from-end)
	   (do ((i start (+ i 1)))
	       ((= i nd) #f)
	     (if (predicate (key (sequence i)))
		 (return (sequence i))))
	   (do ((i (- nd 1) (- i 1)))
	       ((< i start) #f)
	     (if (predicate (key (sequence i)))
		 (return (sequence i)))))))))

(define* (find item sequence from-end (test eql) (start 0) end (key identity))
  ;; (format #t "~A: ~A in ~A~%" (bgred "find") item sequence)
  (find-if (lambda (arg) (test item arg)) sequence from-end start end key))

(define (null obj) (or (not obj) (null? obj)))

(define* (remove-duplicates sequence from-end (test eql) (start 0) end (key identity))
  ;; (format #t "~A: ~A~%" (ured "remove-duplicates") sequence)
  (let* ((result ())
	 (start-seq (+ start 1))
	 (len (length sequence))
	 (nd (if (number? end) end len)))
    (do ((i start (+ i 1)))
	((= i nd))
      (let* ((orig-obj (sequence i))
	     (obj (key orig-obj)))
	(if (null from-end)
	    (begin
	      (if (not (find obj sequence :start start-seq :end nd :test test :key key))
		  (set! result (cons orig-obj result)))
	      (set! start-seq (+ start-seq 1)))
	    (if (not (find obj result :test test :key key))
		(set! result (cons orig-obj result))))))
    (let* ((res (reverse result))
	   (new-len (+ (length result) start (- len nd)))
	   (new-seq (make sequence new-len)))
      (let ((n 0))
	(do ((i 0 (+ i 1)))
	    ((= i len) new-seq)
	  (if (or (< i start)
		  (>= i nd))
	      (begin
		(set! (new-seq n) (sequence i))
		(set! n (+ n 1)))
	      (if (not (null? res))
		  (begin
		    (set! (new-seq n) (car res))
		    (set! res (cdr res))
		    (set! n (+ n 1))))))))))


(define find-then
  (let ((+documentation+ "(find-then fn sequence) applies fn to each member of sequence.\n\
If func approves of one, find-then returns the result of applying fn to it."))
    (lambda (f sequence)
      (call-with-exit
       (lambda (return)
	 (for-each (lambda (arg)
		     (if (f arg)
			 (return (f arg))))
		   sequence)
	 #f)))))

(define (subset? l1 l2)
  (or (null? l1)
      (and (member  (car l1) l2)
           (subset? (cdr l1) l2))))

(define (set-equal? l1 l2)
  (and (subset? l1 l2)
       (subset? l2 l1)))

(define (remove-ifx func lst)
  (map (lambda (x) (if (func x) (values) x)) lst))

(define (last list)
   (if (zero? (length (cdr list)))
      (car list)
      (last (cdr list))))

(define (but-last list)
  (reverse (cdr (reverse list))))

;; (load "s7/stuff.scm")

(set! *#readers*
      (cons (cons #\h (lambda (str)
			(and (string=? str "h") ; #h(...)
			     (apply hash-table (read)))))
	    *#readers*))
*#readers*

(define (sym<? s1 s2)
  (let ((x1 (symbol->string s1)) (x2 (symbol->string s2)))
    (string<? x1 x2)))

;;FIXME: this is dune stuff that should be moved to mibl
(define (modules<? s1 s2)
  (let ((x1 (if (symbol? s1) (symbol->string s1) s1))
        (x2 (if (symbol? s2) (symbol->string s2) s2)))
    (string<? x1 x2)))

;; (modules Registerer), (modules (:standard \ legacy_store_builder))
;; (modules)
;; (modules (:standard) \ Plugin_registerer)
;; (modules (:standard (symbol "\\") delegate_commands delegate_commands_registration))
;; (modules (:standard (symbol "\\") legacy_store_builder))
;; (modules :standard (symbol "\\") gen)
;; NB: modules may be generated rather than srcfile modules!
;; ex: src/lib_protocol_environment/sigs:Tezos_protocol_environment_sigs
;; depends on "V0", "V1"... which are generated by rule
;; a module must go in one of :direct or :indirect
(define (indirect-module-dep? module srcfiles)
  ;; (format #t "indirect-module-dep? ~A : ~A\n" module srcfiles)
  (let recur ((srcfiles srcfiles))
    (if (null? srcfiles)
        #t
        (let* ((m (if (symbol? module) (symbol->string module)
                     (copy module)))
               (bn (bname (car srcfiles))))

          (if (string=? m bn)
              #f
              (begin
                (string-set! m 0 (char-downcase (string-ref m 0)))
                (if (string=? m bn)
                    #f
                    (recur (cdr srcfiles)))))))))

(define (libdep->module-name libdep)
  (let ((mname (copy libdep)))
    libdep))

;; bname (=principal-name) basename with extension removed
(define (bname path)
  (let* ((bn (basename path))
         (last-dot (string-index-right bn (lambda (c) (eq? c #\.))))
         )
    ;; (format #t "last-dot: ~A~%" last-dot)
    (if last-dot
        (string-take bn last-dot)
        bn)))

(define principal-name
  (let ((+documentation+ "Returns path segment with extension removed. Usually for filenames, but works for directory names too, if they contain a dot.")
        (+signature+ "(principal-name path)"))
    (lambda (path)
      (bname path))))

(define filename-extension
  (let ((+documentation+ "Returns extension of filename (path), EXcluding dot.")
        (+signature+ "(filename-extension path)"))
    (lambda (path)
      (let* ((path (format #f "~A" path))
             (fname (basename path))
             (len (string-length fname))
             (last-dot (string-index-right fname (lambda (c) (eq? c #\.)))))
        (if last-dot
            (string-take-right fname (- len last-dot))
            #f)))))

;; s7test.scm
(define (identity x) x)
(define eq eq?)
(define eql eqv?)
(define equal equal?)

;; s7test.scm
(define (make obj size)
	  (cond ((vector? obj)     (make-vector size))
		((list? obj)       (make-list size))
		((string? obj)     (make-string size))
		((hash-table? obj) (make-hash-table size))))

;; s7test.scm
;; (define* (remove-if predicate sequence from-end (start 0) end count (key identity))
;;   (let* ((len (length sequence))
;; 	 (nd (or (and (number? end) end) len))
;; 	 (num (if (number? count) count len))
;; 	 (changed 0))
;;     (if (not (positive? num))
;; 	sequence
;; 	(let ((result ()))
;; 	  (if (null? from-end)
;; 	      (do ((i 0 (+ i 1)))
;; 		  ((= i len))
;; 		(if (or (< i start)
;; 			(>= i nd)
;; 			(>= changed num)
;; 			(not (predicate (key (sequence i)))))
;; 		    (set! result (cons (sequence i) result))
;; 		    (set! changed (+ changed 1))))
;; 	      (do ((i (- len 1) (- i 1)))
;; 		  ((< i 0))
;; 		(if (or (< i start)
;; 			(>= i nd)
;; 			(>= changed num)
;; 			(not (predicate (key (sequence i)))))
;; 		    (set! result (cons (sequence i) result))
;; 		    (set! changed (+ changed 1)))))
;; 	  (let* ((len (length result))
;; 		 (obj (make sequence len))
;; 		 (vals (if (null? from-end) (reverse result) result)))
;; 	    (do ((i 0 (+ i 1)))
;; 		((= i len))
;; 	      (set! (obj i) (vals i)))
;; 	    obj)))))

;; s7test.scm
(define (remove item sequence) ;; (test eql))
  ;; (start 0) end count (key identity))
  ;; (format #t "~A: ~A~%" (red "REMOVE") item)
  (remove-if list (lambda (arg) (equal? item arg)) sequence))

;; s7test.scm
(define-macro* (delete item sequence from-end (test eql) (start 0) end count (key identity))
	    `(let ((obj (remove ,item ,sequence ,from-end ,test ,start ,end ,count ,key)))
	       (if (symbol? ',sequence)
		   (set! ,sequence obj))
	       obj))

;; convert a_b_c to a-b-c
(define (endash str)
  (apply string (map (lambda (ch)
                       (if (char=? ch #\_)
                           #\-
                           ch))
                     str)))

;; convert a-b-c to a_b_c
(define (undash str)
  (apply string (map (lambda (ch)
                       (if (char=? ch #\-)
                           #\_
                           ch))
                     str)))

;; convert a.b.c to a/b/c
(define (enslash str)
  (apply string (map (lambda (ch)
                       (if (char=? ch #\.)
                           #\/
                           ch))
                     str)))

(define filename-cache (make-hash-table))

;; (define (file-name->module-name path)
;;   (if-let ((modname (filename-cache path)))
;;           modname
;;           (let* ((last-slash (string-index-right path
;;                                                  (lambda (c) (eq? c #\/))))
;;                  (fname (if last-slash
;;                             (string-drop path (+ last-slash 1))
;;                             path))
;;                  (mraw (if (string-suffix? ".ml" fname)
;;                            (string-drop-right fname 3)
;;                            (if (string-suffix? ".mli" fname)
;;                                (string-drop-right fname 4)
;;                                (error 'bad-filename
;;                                       (string-append "extension should be .ml or .mli: "
;;                                                      fname)))))
;;                  (modname (normalize-module-name mraw)))
;;             (hash-table-set! filename-cache path modname)
;;             modname)))

;; s7test.scm
(define (flatten lst)
    (map values (list (let flatten-1 ((lst lst))
                        (cond ((null? lst) (values))
                               ((not (pair? lst)) lst)
                               (else (values (flatten-1 (car lst))
                                             (flatten-1 (cdr lst)))))))))

(define (concatenate . args)
  (apply append (map (lambda (arg) (map values arg)) args)))

(define (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))

(define (hash-table-keys ht)
  (map car ht))

(define-macro* (if-let bindings true false)
  (let* ((binding-list (if (and (pair? bindings) (symbol? (car bindings)))
			   (list bindings)
			   bindings))
	 (variables (map car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
	   ,true
	   ,false))))

(define (dirname path)
  (let* ((path (format #f "~A" path))
         (last-slash (string-index-right path (lambda (c) (eq? c #\/)))))
    (if last-slash
        (string-take path last-slash)
        "./")))

(define (basename _path)
  (let* ((path (if (string? _path) _path
                   (if (symbol? _path) (symbol->string _path)
                       (error 'wrong-arg-type "Arg to basename must be string or symbol: ~A" _path))))
         (last-slash (string-index-right path (lambda (c) (eq? c #\/)))))
    (if last-slash
        (string-drop path (+ last-slash 1))
        path)))

(define (fs-glob->list pattern)
  ;;(with-let (sublet *libc* :pattern pattern)
  (let ((g (glob.make)))
    (glob pattern 0 g)
    (let ((res (glob.gl_pathv g)))
      (globfree g)
      res)))

(define (pwd)
  (getcwd (make-string 1024) 1024))

(define (mkdir-recursive path flags)
  (let ((path (format #f "~A" path)))
    (if (directory? path)
        #t
        (let ((dir (dirname path)))
          (if (mkdir-recursive dir flags)
              (mkdir path flags))))))

(define (truthy? x)
  (if (list? x)
      (not (null? x))
      (if x #t #f)))

;; (display "loaded libs7/utils.scm") (newline)
