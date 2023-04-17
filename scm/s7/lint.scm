;;; lint for s7 scheme
;;;
;;; (lint "file.scm") checks file.scm for infelicities
;;; to control the kinds of checks, set the variables below.
;;; for tests and examples, see lint-test in s7test.scm

(provide 'lint.scm)

(define *report-unused-parameters* #f)                    ; many of these are reported anyway if they are passed some non-#f value
(define *report-unused-top-level-functions* #f)           ; very common in Scheme, but #t makes the ghastly leakage of names obvious
(define *report-shadowed-variables* #f)                   ; shadowed parameters, etc
(define *report-undefined-identifiers* #f)                ; names we can't account for
(define *report-multiply-defined-top-level-functions* #f) ; top-level funcs defined in more than one file
(define *report-nested-if* 4)                             ; 3 is lowest, this sets the nesting level that triggers an if->cond suggestion
(define *report-short-branch* 12)                         ; controls when a lop-sided if triggers a reordering suggestion
(define *report-one-armed-if* 90)                         ; if -> when/unless, can be #f/#t; if an integer, sets tree length which triggers revision (80 is too small)
(define *report-loaded-files* #f)                         ; if load is encountered, include that file in the lint process
(define *report-any-!-as-setter* #t)                      ; unknown funcs/macros ending in ! are treated as setters
(define *report-doc-strings* #f)                          ; old-style (CL) doc strings (definstrument ignores this switch -- see ws.scm)
(define *report-func-as-arg-arity-mismatch* #f)           ; as it says...
(define *report-combinable-lets* #t)                      ; report lets that can be combined
(define *report-splittable-lets* #f)                      ; report let*'s that can be split into a few nested lets

(define *report-ridiculous-variable-names* 50)            ; max length of var name
(define *report-bad-variable-names* '(l ll .. O ~ else))  ; bad names -- a list to check such as:
;;;             '(l ll .. ~ else data datum new item info temp tmp temporary val vals value foo bar baz aux dummy O var res retval result count str)
;;;   else is evaluated in cond
;;;   => is evaluated in both cond and case, so both are better left alone (see s7test.scm for examples)
;;;     at the top-level, r7rs deems it an error to change their value
;;;   also '+signature+ and '+documentation+ as local vars could cause confusion

(define *report-built-in-functions-used-as-variables* #f) ; string and length are the most common cases
(define *report-forward-functions* #f)                    ; functions used before being defined
(define *report-sloppy-assoc* #f)                         ; i.e. (cdr (assoc x y)) and the like
(define *report-bloated-arg* 24)                          ; min arg expr tree size that can trigger a rewrite-as-let suggestion (32 is too high I think)
(define *report-clobbered-function-return-value* #f)      ; function returns constant sequence, which is then stomped on -- very rare!
(define *report-boolean-functions-misbehaving* #t)        ; function name ends in #\? but function returns a non-boolean value -- dubious.
(define *report-quasiquote-rewrites* #t)                  ; simple quasiquote stuff rewritten as a normal list expression
(define *report-||-rewrites* #t)                          ; | has no special meaning in s7, |...| does not represent the symbol ...
(define *report-constant-expressions-in-do* #t)           ; a first stab at this
(define *report-recursion->iteration* #t)                 ; named-let -> do and the like

;;; these turn out to be less useful than I expected
(define *report-repeated-code-fragments* 200)             ; #t, #f, or an int = min reported fragment size * uses * uses, #t=130.
(define *fragment-max-size* 128)  ; biggest seen if 512: 180 -- appears to be in a test suite, if 128 max at 125
(define *fragment-min-size* 5)    ; smallest seen - 1 -- maybe 8 would be better

(define *report-laconically* #f)                          ; leave out introductory verbiage

(define *lint* #f)                                        ; the lint let
;; this gives other programs a way to extend or edit lint's tables: for example, the
;;   table of functions that are simple (no side effects) is (*lint* 'no-side-effect-functions)
;;   see snd-lint.scm.

;;; --------------------------------------------------------------------------------

(when (provided? 'pure-s7)
  (define (make-polar mag ang) (complex (* mag (cos ang)) (* mag (sin ang))))

  (define (char-ci=? . chars) (apply char=? (map char-upcase chars)))
  (define (char-ci<=? . chars) (apply char<=? (map char-upcase chars)))
  (define (char-ci>=? . chars) (apply char>=? (map char-upcase chars)))
  (define (char-ci<? . chars) (apply char<? (map char-upcase chars)))
  (define (char-ci>? . chars) (apply char>? (map char-upcase chars)))

  (define (string-ci=? . strs) (apply string=? (map string-upcase strs)))
  (define (string-ci<=? . strs) (apply string<=? (map string-upcase strs)))
  (define (string-ci>=? . strs) (apply string>=? (map string-upcase strs)))
  (define (string-ci<? . strs) (apply string<? (map string-upcase strs)))
  (define (string-ci>? . strs) (apply string>? (map string-upcase strs)))

  (define (let->list e)
    (if (let? e)
	(reverse! (map values e))
	(error 'wrong-type-arg "let->list argument should be an environment: ~A" str))))


(format *stderr* "loading lint.scm~%")

;(set! reader-cond #f)
;(define-macro (reader-cond . clauses) `(values))          ; clobber reader-cond to avoid (incorrect) unbound-variable errors


(unless (defined? 'need-lint-line-numbers)
  (define need-lint-line-numbers #t))

(when (and (provided? 'debugging) need-lint-line-numbers)
  (with-let (rootlet) ; force this to be top-level even in repl
    (define-expansion (lint-format str caller . args)
      `(begin
	 (format outport "lint.scm line ~A~%" ,(port-line-number))
	 (lint-format-1 ,str ,caller ,@args)))))

(if (and (provided? 'debugging) need-lint-line-numbers)
    (with-let (rootlet)
      (define-expansion (out-format port . args)
	`(begin
	   (format ,port "lint.scm line ~A~%" ,(port-line-number))
	   (format ,port ,@args))))
    (with-let (rootlet)
      (define-expansion (out-format port . args)
	`(format ,port ,@args))))


(define var-name car)
(define var-member assq)

;;; --------------------------------------------------------------------------------
(define lint

  (let ((no-side-effect-functions
	 (let ((ht (make-hash-table)))
	   (for-each
	    (lambda (op)
	      (set! (ht op) #t))
	    '(* + - / < <= = > >=
	      abs acos acosh and angle append aritable? arity ash asin asinh assoc assq assv atan atanh
	      begin boolean? byte-vector byte-vector-ref byte-vector?
	      caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
	      call-with-input-string call-with-input-file
	      c-pointer c-pointer? c-object? c-object-type call-with-exit car case catch
	      cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr
	      cddar cdddar cddddr cdddr cddr cdr ceiling char->integer char-alphabetic? char-ci<=?
	      char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric?
	      char-position char-ready? char-upcase char-upper-case? char-whitespace? char<=? char<?
	      char=? char>=? char>? char? complex complex? cond cons continuation? cos constant?
	      cosh curlet current-error-port current-input-port current-output-port cyclic-sequences
	      defined? denominator dilambda? do dynamic-wind
	      eof-object? eq? equal? eqv? even? exact->inexact exact? exp expt
	      float? float-vector float-vector-ref float-vector? floor for-each funclet
	      gcd gensym gensym?
	      hash-table  hash-table-entries hash-table-ref hash-table-key-typer hash-table-value-typer hash-table? help hook-functions
	      if imag-part immutable? inexact->exact inexact? infinite? inlet input-port?
	      int-vector int-vector-ref int-vector? iterator-at-end? iterator-sequence integer->char
	      integer-decode-float integer-length integer? iterator?
	      keyword->symbol keyword?
	      lambda lambda* lcm let->list length let let* let-ref let? letrec letrec* list list->string list->vector list-ref
	      list-tail list? log logand logbit? logior lognot logxor
	      macro? magnitude make-byte-vector make-float-vector make-int-vector make-hash-table make-hook make-iterator make-list make-polar
	      make-rectangular subvector make-string make-vector map max member memq memv min modulo equivalent?
	      nan? negative? not null? number->string number? numerator
	      object->let object->string odd? openlet? or outlet output-port? owlet
	      pair-line-number pair-filename pair? port-closed? port-filename port-line-number positive? documentation
	      setter signature procedure-source procedure? proper-list? provided?
	      quasiquote quote quotient
	      random-state random-state->list random-state? rational? rationalize real-part real? remainder reverse rootlet round
	      sequence? sin sinh square sqrt stacktrace string string->list string->number string->symbol string->keyword string-append
	      string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? string-downcase string-length
	      string-position string-ref string-upcase string<=? string<? string=? string>=? string>? string?
	      sublet substring subvector? subvector-position subvector-vector
	      symbol symbol->dynamic-value symbol->keyword symbol->string symbol->value symbol? syntax?
	      tan tanh tree-leaves tree-memq tree-set-memq tree-count tree-cyclic? truncate type-of
	      unless unspecified? undefined?
	      values vector vector-append vector->list vector-dimensions vector-dimension vector-rank vector-length vector-ref vector-typer vector?
	      when with-baffle with-let with-input-from-file with-input-from-string with-output-to-string
	      zero?

	      c-pointer-weak2 c-pointer-type bignum port-position byte-vector->string c-pointer-info c-pointer->list subvector-vector c-pointer-weak1
	      funclet? bignum? weak-hash-table? goto? port-file byte? hash-code
	      dilambda ; these 3 lines added 5-May-22, not getenv directory->list or file-mtime because bools are evaluated if constant args!

	      list-values apply-values unquote))
	   ;; do not include file-exists? or directory? (also not peek-char because these are checked via eval)
	   ht))

	(built-in-functions (let ((ht (make-hash-table)))
			      (for-each
			       (lambda (op)
				 (set! (ht op) #t))
			       '(symbol? gensym? keyword? let? openlet? iterator? macro? c-pointer? c-object? c-object-type constant? subvector?
			         input-port? output-port? eof-object? integer? number? real? complex? rational? random-state?
			         char? string? list? pair? vector? float-vector? int-vector? byte-vector? hash-table?
			         continuation? procedure? dilambda? boolean? float? proper-list? sequence? null? gensym
			         symbol->string string->symbol symbol symbol->value symbol->dynamic-value
			         string->keyword symbol->keyword keyword->symbol outlet rootlet curlet unlet sublet varlet
			         cutlet inlet owlet coverlet openlet let-ref let-set! make-iterator iterate iterator-sequence
			         iterator-at-end? provided? provide defined? c-pointer port-line-number port-filename
			         pair-line-number pair-filename port-closed? current-input-port current-output-port
			         current-error-port let->list char-ready? close-input-port close-output-port flush-output-port
			         open-input-file open-output-file open-input-string open-output-string get-output-string
			         newline write display read-char peek-char write-char write-string read-byte write-byte
			         read-line read-string read call-with-input-string call-with-input-file with-input-from-string
			         with-input-from-file call-with-output-string call-with-output-file with-output-to-string
			         with-output-to-file real-part imag-part numerator denominator even? odd? zero? positive?
			         negative? infinite? nan? complex magnitude angle rationalize abs exp log sin cos tan asin
			         acos atan sinh cosh tanh asinh acosh atanh sqrt expt floor ceiling truncate round lcm gcd
			         + - * / max min quotient remainder modulo = < > <= >= logior logxor logand lognot ash
			         random-state random inexact->exact exact->inexact integer-length make-polar make-rectangular
			         logbit? integer-decode-float exact? inexact? random-state->list number->string string->number
			         char-upcase char-downcase char->integer integer->char char-upper-case? char-lower-case?
			         char-alphabetic? char-numeric? char-whitespace? char=? char<? char>? char<=? char>=?
			         char-position string-position make-string string-ref string-set! string=? string<? string>?
			         string<=? string>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? string-ci=? string-ci<?
			         string-ci>? string-ci<=? string-ci>=? string-copy string-fill! list->string string-length
			         string->list string-downcase string-upcase string-append substring string object->string
			         format cons car cdr set-car! set-cdr! caar cadr cdar cddr caaar caadr cadar cdaar caddr
			         cdddr cdadr cddar caaaar caaadr caadar cadaar caaddr cadddr cadadr caddar cdaaar cdaadr
			         cdadar cddaar cdaddr cddddr cddadr cdddar assoc member list list-ref list-set! list-tail
			         make-list length copy fill! reverse reverse! sort! append assq assv memq memv vector-append
			         list->vector vector-fill! vector-length vector->list vector-ref vector-set! vector-dimensions vector-dimension
			         make-vector subvector vector float-vector make-float-vector float-vector-set! vector-rank vector-typer
			         float-vector-ref int-vector make-int-vector int-vector-set! int-vector-ref string->byte-vector
			         byte-vector make-byte-vector hash-table  make-hash-table hash-table-ref hash-table-key-typer hash-table-value-typer
			         hash-table-set! hash-table-entries cyclic-sequences call/cc call-with-current-continuation
			         call-with-exit load autoload eval eval-string apply for-each map dynamic-wind values
			         catch throw error documentation signature help procedure-source funclet
			         setter arity aritable? not eq? eqv? equal? equivalent? gc emergency-exit
			         exit dilambda make-hook hook-functions stacktrace tree-leaves tree-memq object->let
				 getenv directory? file-exists? type-of immutable! immutable? byte-vector-set! syntax?
				 list-values apply-values unquote set-current-output-port unspecified? undefined? byte-vector-ref
				 set-current-input-port set-current-error-port directory->list system subvector-position
				 tree-count tree-set-memq tree-cyclic?))
			      ht))

	(side-effect-functions '(provide list-set! fill! reverse! sort! vector-fill! vector-set! float-vector-set! int-vector-set! byte-vector-set!
				 string->byte-vector string-set! hash-table-set! call/cc call-with-current-continuation load autoload eval eval-string
				 apply set-current-error-port display set-current-input-port set-current-output-port write close-input-port
				 close-output-port flush-output-port open-input-file open-output-file open-input-string open-output-string
				 get-output-string newline read-char peek-char write-char write-string read-byte write-byte read-line read-string
				 read call-with-output-string call-with-output-file string-fill! with-output-to-file system format set-car! set-cdr!
				 immutable! varlet cutlet coverlet openlet random let-set! iterate
				 emergency-exit exit error throw))

	(makers (let ((h (make-hash-table)))
		  (for-each
		   (lambda (op)
		     (set! (h op) #t))
		   '(gensym sublet inlet make-iterator let->list random-state random-state->list number->string object->let
		     make-string string string-copy copy list->string string->list string-append substring object->string
		     format cons list make-list reverse append vector-append list->vector vector->list make-vector
		     subvector vector make-float-vector float-vector make-int-vector int-vector byte-vector
		     hash-table  make-hash-table make-hook list-values append gentemp)) ; gentemp for other schemes
		  h))

	(non-negative-ops (let ((h (make-hash-table)))
			    (for-each
			     (lambda (op)
			       (set! (h op) #t))
			     '(string-length vector-length abs magnitude denominator gcd lcm tree-leaves
			       char->integer hash-table-entries write-byte
			       char-position string-position pair-line-number port-line-number))
			    h))

	(numeric-ops (let ((h (make-hash-table)))
		       (for-each
			(lambda (op)
			  (set! (h op) #t))
			'(+ * - /
			  sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh
			  log exp expt sqrt make-polar complex make-rectangular
			  imag-part real-part abs magnitude angle max min exact->inexact
			  modulo remainder quotient lcm gcd
			  rationalize inexact->exact random
			  logior lognot logxor logand integer-length numerator denominator
			  floor round truncate ceiling ash

			  ;; r7rs
			  exact inexact

			  ;; hooboy...
			  fix:+ fx+ flo:+ fl+ fix:* fx* flo:* fl* fix:- fx- flo:- fl- fix:/ fx/ flo:/ fl/
			  ))
		       h))

	(bools (let ((h (make-hash-table)))
		 (for-each
		  (lambda (op)
		    (set! (h op) #t))
		  '(symbol? byte? integer? rational? real? number? complex? float? keyword? gensym? byte-vector? string? list? sequence?
		    char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? null? pair? proper-list?
		    output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?
		    unspecified? immutable? constant? syntax? undefined? tree-cyclic? openlet? subvector?))
		 h))

	(booleans (let ((h (make-hash-table)))
		  (for-each
		   (lambda (op)
		     (set! (h op) #t))
		   '(symbol? byte? integer? rational? real? number? complex? float? keyword? gensym? byte-vector? string? list? sequence?
		     char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? null? pair? proper-list?
		     output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?
		     unspecified? exact? inexact? defined? provided? even? odd? char-whitespace? char-numeric? char-alphabetic?
		     negative? positive? zero? syntax? undefined? tree-cyclic? not openlet? ; immutable? constant?
		     infinite? nan? char-upper-case? char-lower-case? directory? file-exists? iterator-at-end? subvector?))
		  h))

	(setters '(symbol? syntax? gensym? keyword? let? openlet? iterator? macro? c-pointer? input-port? output-port? eof-object?
		   integer? byte? number? real? float? complex? rational? random-state? char? string? list? pair? vector? float-vector?
		   int-vector? byte-vector? hash-table? continuation? procedure? dilambda? boolean? properd-list? sequence? null?
		   undefined? unspecified? c-object? subvector? weak-hash-table?))

	(notables (let ((h (make-hash-table)))
		    (for-each
		     (lambda (op)
		       (set! (h (car op)) (cadr op)))
		     '((< >=) (> <=) (<= >) (>= <)
		       (char<? char>=?) (char>? char<=?) (char<=? char>?) (char>=? char<?)
		       (string<? string>=?) (string>? string<=?) (string<=? string>?) (string>=? string<?)
		       (char-ci<? char-ci>=?) (char-ci>? char-ci<=?) (char-ci<=? char-ci>?) (char-ci>=? char-ci<?)
		       (string-ci<? string-ci>=?) (string-ci>? string-ci<=?) (string-ci<=? string-ci>?) (string-ci>=? string-ci<?)
		       (odd? even?) (even? odd?) (exact? inexact?) (inexact? exact?)))
		    h))

	(reversibles (let ((h (make-hash-table)))
		       (for-each
			(lambda (op)
			  (set! (h (car op)) (cadr op)))
			'((< >) (> <) (<= >=) (>= <=)
			  (* *) (+ +) (= =) (char=? char=?) (string=? string=?)
			  (eq? eq?) (eqv? eqv?) (equal? equal?) (equivalent? equivalent?)
			  (logand logand) (logxor logxor) (logior logior)
			  (max max) (min min) (lcm lcm) (gcd gcd)
			  (char<? char>?) (char>? char<?) (char<=? char>=?) (char>=? char<=?)
			  (string<? string>?) (string>? string<?) (string<=? string>=?) (string>=? string<=?)
			  (char-ci<? char-ci>?) (char-ci>? char-ci<?) (char-ci<=? char-ci>=?) (char-ci>=? char-ci<=?)
			  (string-ci<? string-ci>?) (string-ci>? string-ci<?) (string-ci<=? string-ci>=?) (string-ci>=? string-ci<=?)))
		       h))

	(syntaces (let ((h (make-hash-table)))
		    (for-each
		     (lambda (op)
		       (set! (h op) #t))
		     '(quote if begin let let* letrec letrec* cond case or and do set! unless when
		       with-let with-baffle
		       lambda lambda* define define*
		       define-macro define-macro* define-bacro define-bacro*
		       define-constant define-expansion))
		    h))

	(definers '(define define* define-constant lambda lambda* curlet require load eval eval-string
		    define-macro define-macro* define-bacro define-bacro* define-expansion
		    definstrument define-animal define-envelope
		    define-values define-module define-method
		    define-syntax define-public define-inlinable define-integrable define^))

	(open-definers '(define define* define-constant require load eval eval-string
			 define-macro define-macro* define-bacro define-bacro* define-expansion
			 definstrument define-animal define-envelope defgenerator
			 define-values define-module define-method
			 define-syntax define-public define-inlinable define-integrable define^))

	(cxars (hash-table 'car ()  'caar 'car  'cdar 'cdr
			   'caaar 'caar  'cdaar 'cdar  'cddar 'cddr  'cadar 'cadr
			   'caaaar 'caaar  'caadar 'caadr  'cadaar 'cadar  'caddar 'caddr
			   'cdaaar 'cdaar  'cdadar 'cdadr  'cddaar 'cddar  'cdddar 'cdddr))

	(outport #t)
	(linted-files ())
	(big-constants (make-hash-table))
	(other-names-counts (make-hash-table))
	(*e* #f)
	(other-identifiers (make-hash-table))
	(quote-warnings 0)
	;; these line numbers are trying to reduce redundant output, but they sometimes squelch useful feedback
	(last-simplify-boolean-line-number -1)
	(last-simplify-numeric-line-number -1)
	(last-simplify-cxr-line-number -1)
	(last-if-line-number -1)
	(last-if->case-line-number -1)
	(last-let->case-line-number -1)
	(last-checker-line-number -1)
	(last-cons-line-number -1)
	(last-rewritten-internal-define #f)
	(last-lambda-let #f)
	(last-lambda-let-funcs #f)
	(local-function-context 40)
	(line-number -1)
	(lint-in-with-let #f)
	(pp-left-margin 4)
	(lint-left-margin 1)
	(*current-file* "")
	(*top-level-objects* (make-hash-table))
	(*output-port* *stderr*)
	(fragments (make-vector *fragment-max-size* #f))
	(fragmin *fragment-max-size*)
	(fragmax 0)
	;(recent-names (make-list 8 #f))
	(*max-cdr-len* 16)) ; 40 is too high, 24 questionable, if #f the let+do rewrite is turned off

    ;; (let ((st (symbol-table))) (for-each (lambda (x) (if (and (procedure? (symbol->value x)) (not (hash-table-ref built-in-functions x))) (format *stderr* "~A~%" x))) st))

    (set! *e* (curlet))
    (set! *lint* (curlet))                ; external access to (for example) the built-in-functions hash-table via (*lint* 'built-in-functions)
    ;(set-cdr! (list-tail recent-names 7) recent-names)

    (define denote define-constant)

    (define definers-table
      (let ((h (make-hash-table)))
	(for-each (lambda (d)
		    (set! (h d) #t))
		  definers)
	h))

    (define open-definers-table
      (let ((h (make-hash-table)))
	(for-each (lambda (d)
		    (set! (h d) #t))
		  open-definers)
	h))

    ;; -------- lint-format --------
    (define target-line-length 80) ; also 120 via let-temporarily

    (denote (lint-truncate-string str)
      (if (< (length str) target-line-length)
	  str
	  (do ((i (- target-line-length 6) (- i 1)))
		((or (= i 40)
		     (char-whitespace? (string-ref str i)))
		 (string-append (substring str 0 (if (<= i 40)
						     (- target-line-length 6)
						     i))
				"...")))))

    (denote (truncated-list->string form)
      ;; return form -> string with limits on its length
      (lint-truncate-string (object->string form #t target-line-length)))

    (define lint-pp #f) ; avoid crosstalk with other schemes' definitions of pp and pretty-print (make-lint-var also collides)
    (define lint-pp-funclet #f)
    (let ()
      (require write.scm)
      (set! lint-pp pp)
      (set! lint-pp-funclet (funclet pretty-print)))

    (denote (lists->string f1 f2)
      (let ((str1 (lint-truncate-string (object->string f1 #t (+ target-line-length 2)))))
	(if (> (tree-leaves f2) 10)
	    (begin
	      (set! (lint-pp-funclet '*pretty-print-left-margin*) pp-left-margin)
	      (set! (lint-pp-funclet '*pretty-print-length*) (- 114 pp-left-margin))
	      (format #f "~%~NC~A ->~%~NC~A" pp-left-margin #\space str1 pp-left-margin #\space (lint-pp f2)))
	    (let ((str2 (lint-truncate-string (object->string f2 #t (+ target-line-length 2)))))
	      (if (< (+ (length str1) (length str2)) target-line-length)
		  (format #f "~A -> ~A" str1 str2)
		  (format #f "~%~NC~A ->~%~NC~A" pp-left-margin #\space str1 pp-left-margin #\space str2))))))

    (define (truncated-lists->string f1 f2)
      ;; same but 2 strings that may need to be lined up vertically and both are truncated
      (let ((str1 (lint-truncate-string (object->string f1 #t (+ target-line-length 2))))
	    (str2 (lint-truncate-string (object->string f2 #t (+ target-line-length 2)))))
	(if (< (+ (length str1) (length str2)) target-line-length)
	    (format #f "~A -> ~A" str1 str2)
	    (format #f "~%~NC~A ->~%~NC~A" pp-left-margin #\space str1 pp-left-margin #\space str2))))

    (define made-suggestion 0)

    (denote (lint-format-1 str caller . args)
      (let ((outstr (if *report-laconically*
 			(apply format #f str args)
			(apply format #f
			   (string-append (if (and (integer? line-number) (> line-number 0))
					      "~NC~A (line ~D): "
					      "~NC~A: ")
					  str "~%")
			   lint-left-margin #\space
			   (truncated-list->string caller)
			   (if (and (integer? line-number) (> line-number 0))
			       (values line-number args)
			       args)))))
	(set! made-suggestion (+ made-suggestion 1))
	(display outstr outport)
	(if (or *report-laconically*
		(> (length outstr) (+ target-line-length 40)))
	    (newline outport))))

    (unless (and (provided? 'debugging) need-lint-line-numbers)
      (define lint-format lint-format-1))

    (denote (local-line-number tree)
      (let ((tree-line (and (pair? tree) (pair-line-number tree))))
	(if (and (integer? tree-line)
		 (> tree-line 0)
		 (not (= tree-line line-number)))
	    (format #f " (line ~D)" tree-line)
	    "")))


    ;; -------- vars --------
    (denote (var? v) (and (pair? v) (let? (cdr v))))
    (denote var-ref     (dilambda (lambda (v) (let-ref (cdr v) 'ref))     (lambda (v x) (let-set! (cdr v) 'ref x))))
    (denote var-set     (dilambda (lambda (v) (let-ref (cdr v) 'set))     (lambda (v x) (let-set! (cdr v) 'set x))))
    (denote var-history (dilambda (lambda (v) (let-ref (cdr v) 'history)) (lambda (v x) (let-set! (cdr v) 'history x))))
    (denote var-ftype   (dilambda (lambda (v) (let-ref (cdr v) 'ftype))   (lambda (v x) (if (defined? 'ftype (cdr v) #t) (let-set! (cdr v) 'ftype x)))))
    (denote var-retcons (dilambda (lambda (v) (let-ref (cdr v) 'retcons)) (lambda (v x) (let-set! (cdr v) 'retcons x))))
    (denote var-arglist (dilambda (lambda (v) (let-ref (cdr v) 'arglist)) (lambda (v x) (let-set! (cdr v) 'arglist x))))
    (denote var-definer (dilambda (lambda (v) (let-ref (cdr v) 'definer)) (lambda (v x) (let-set! (cdr v) 'definer x))))
    (denote var-scope   (dilambda (lambda (v) (let-ref (cdr v) 'scope))   (lambda (v x) (let-set! (cdr v) 'scope x))))
    (denote var-setters (dilambda (lambda (v) (let-ref (cdr v) 'setters)) (lambda (v x) (let-set! (cdr v) 'setters x))))
    (denote var-env     (dilambda (lambda (v) (let-ref (cdr v) 'env))     (lambda (v x) (let-set! (cdr v) 'env x))))
    (denote (var-arity v)
      (let ((val (let-ref (cdr v) 'arit)))
	(and (not (eq? val #<undefined>))
	     val)))
    (denote (var-initial-value v) (let-ref (cdr v) 'initial-value)) ; not (easily) settable

    (denote var-refenv
      (dilambda (lambda (v)
		  (let-ref (cdr v) 'refenv))
		(lambda (v e)
		  (let ((old-e (let-ref (cdr v) 'refenv)))
		    (if (null? old-e)                         ; nil if unset
			(let-set! (cdr v) 'refenv e)          ;   so set to current
			(if (not (eq? old-e e))               ; not eq, so (for now) set to #f
			    (let-set! (cdr v) 'refenv #f))))
		  e)))

    (denote var-side-effect
      (dilambda (lambda (v)
		  (case (let-ref (cdr v) 'side-effect)
		    ((()) (let-set! (cdr v) 'side-effect (get-side-effect v)))
		    (else)))
		(lambda (v x)
		  (let-set! (cdr v) 'side-effect x))))

    (denote var-signature
      (dilambda (lambda (v)
		  (case (let-ref (cdr v) 'sig)
		    ((()) (let-set! (cdr v) 'sig (get-signature v)))
		    (else)))
		(lambda (v x)
		  (if (defined? 'sig (cdr v) #t)
		      (let-set! (cdr v) 'sig x))))) ; perhaps fallback on varlet here and in var-ftype above?

    (denote (make-lint-var name initial-value definer)
      (let ((old (or (hash-table-ref other-identifiers name) ())))
	(if (pair? old)
	    (hash-table-set! other-identifiers name #f) ; remove name
	    )
#|
	    (begin
	      ;(format *stderr* "~S ~S ~S~%" name initial-value definer)
	      (set-car! recent-names (list name definer initial-value))
	      (set! recent-names (cdr recent-names))))
|#
	(cons name (inlet 'history (if initial-value
				       (cons initial-value old)
				       old)
			  'ref (length old)
			  'scope ()
			  'initial-value initial-value
			  'refenv ()
			  'set 0
			  'definer definer
			  'env ()
			  'setters ()
			  ))))


    ;; -------- the usual list functions --------

    (denote (len=1? x)
      (and (pair? x)
	   (null? (cdr x))))

    (denote (len=2? x)
      (and (pair? x)
	   (pair? (cdr x))
	   (null? (cddr x))))

    (denote (len=3? x)
      (and (pair? x)
	   (pair? (cdr x))
	   (pair? (cddr x))
	   (null? (cdddr x))))

    (denote (len>1? x)
      (and (pair? x)
	   (pair? (cdr x))))

    (denote (len>2? x)
      (and (pair? x)
	   (pair? (cdr x))
	   (pair? (cddr x))))

    (denote (last-ref x)
      (let ((len (length x)))
	(and (integer? len)
	     (positive? len)
	     (list-ref x (- len 1)))))

    (denote (proper-pair? x)
      (and (pair? x)
	   (proper-list? (cdr x))))

    (denote (unquoted-pair? x)
      (and (pair? x)
	   (not (eq? (car x) 'quote))))

    (define (remove-one item sequence)
      (cond ((not (pair? sequence)) sequence)
	    ((equal? item (car sequence)) (cdr sequence))
	    (else (cons (car sequence)
			(remove-one item (cdr sequence))))))

    (define (remq-set items sequence)
      (cond ((not (pair? sequence))
	     sequence)
	    ((memq (car sequence) items)
	     (remq-set items (cdr sequence)))
	    (else
	     (cons (car sequence)
		   (remq-set items (cdr sequence))))))

    (define (remove-all item sequence)
      (map (lambda (x)
	     (if (equal? x item)
		 (values)
		 x))
	   sequence))

    (define (remove-if p lst)
      (cond ((not (pair? lst)) lst)
	    ((p (car lst)) (remove-if p (cdr lst)))
	    (else (cons (car lst)
			(remove-if p (cdr lst))))))

    (denote (lint-remove-duplicates lst env)
      (reverse (let rem-dup ((lst lst)
			     (nlst ()))
		 (cond ((null? lst) nlst)
		       ((and (member (car lst) nlst)
			     (not (and (pair? (car lst))
				       (side-effect? (car lst) env))))
			(rem-dup (cdr lst) nlst))
		       (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))

    (define applicable? arity)

    (denote (code-constant? x)
      (and (constant? x)
	   (or (not (pair? x))
	       (eq? (car x) 'quote))))

    (denote lint-every?
      (let ((+documentation+ "(lint-every? func sequence) returns #t if func approves of every member of the list sequence")
	    (+signature+ '(boolean? procedure? list?)))
	(lambda (f sequence)
	  (if (pair? sequence)
	      (and (f (car sequence))
		   (lint-every? f (cdr sequence)))
	      (null? sequence)))))

    (denote (just-pairs? sequence)
      (or (null? sequence)
	  (and (pair? (car sequence))
	       (just-pairs? (cdr sequence)))))

    (denote (just-symbols? sequence)
      (or (null? sequence)
	  (and (symbol? (car sequence))
	       (just-symbols? (cdr sequence)))))

    (denote (just-keywords? sequence)
      (or (null? sequence)
	  (and (keyword? (car sequence))
	       (just-keywords? (cdr sequence)))))

    (denote (just-integers? sequence)
      (or (null? sequence)
	  (and (integer? (car sequence))
	       (just-integers? (cdr sequence)))))

    (denote (just-rationals? sequence)
      (or (null? sequence)
	  (and (rational? (car sequence))
	       (just-rationals? (cdr sequence)))))

    (denote (just-reals? sequence)
      (or (null? sequence)
	  (and (real? (car sequence))
	       (just-reals? (cdr sequence)))))

    (denote (just-len>1? sequence)
      (or (null? sequence)
	  (and (len>1? (car sequence))
	       (just-len>1? (cdr sequence)))))

    (denote (just-code-constants? sequence)
      (or (null? sequence)
	  (and (code-constant? (car sequence))
	       (just-code-constants? (cdr sequence)))))

    (denote lint-any?
      (let ((+documentation+ "(lint-any? func sequence) returns #t if func approves of any member of the list sequence")
	    (+signature+ '(boolean? procedure? list?)))
	(lambda (f sequence)
	  (and (pair? sequence)
	       (or (f (car sequence))
		   (lint-any? f (cdr sequence)))))))

    (denote (any-side-effect? form env vars)
      (and (pair? form)
	   (or (side-effect-with-vars? (car form) env vars)
	       (any-side-effect? (cdr form) env vars))))

    (denote (any-pairs? sequence)
      (and (pair? sequence)
	   (or (pair? (car sequence))
	       (any-pairs? (cdr sequence)))))

    (denote (any-keywords? sequence)
      (and (pair? sequence)
	   (or (keyword? (car sequence))
	       (any-keywords? (cdr sequence)))))

    (denote (any-numbers? sequence)
      (and (pair? sequence)
	   (or (number? (car sequence))
	       (any-numbers? (cdr sequence)))))

    (denote lint-find-if
      (let ((+documentation+ "(lint-find-if func lst) applies func to each member of the list lst.\n\
              If func approves of one, find-if returns that member of the sequence")
	    (+signature+ '(#t procedure? list?)))
	(lambda (f lst)
	  (and (pair? lst)
	       (if (f (car lst))
		   (car lst)
		   (lint-find-if f (cdr lst)))))))

    (define (collect-if f lst)
      (map (lambda (arg) (if (f arg) arg (values))) lst))

    (define (collect-if-rational lst)
      (map (lambda (x) (if (rational? x) x (values))) lst))

    (define (collect-if-integer lst)
      (map (lambda (x) (if (integer? x) x (values))) lst))

    (define (collect-if-real lst)
      (map (lambda (x) (if (real? x) x (values))) lst))

    (define (collect-if-not-number lst)
      (map (lambda (x) (if (number? x) (values) x)) lst))


    ;; -------- trees --------

    (define (proper-tree? tree)
      (or (not (pair? tree))
	  (lint-every? proper-tree? tree)))

    (define (shadowed? sym tree1)
      (let shadow? ((tree tree1))
	(and (pair? tree)
	     (or (and (null? (cdr tree))
		      (shadow? (car tree)))
		 (and (pair? (cdr tree))
		      (null? (cddr tree))
		      (or (shadow? (car tree))
			  (shadow? (cdr tree))))
		 (and (pair? (cdr tree))
		      (pair? (cddr tree))
		 (case (car tree)
		   ((let let*)
		    (and (len>1? (cdr tree))
			 (or (eq? sym (cadr tree))
			     (let ((vars ((if (symbol? (cadr tree)) caddr cadr) tree)))
			       (or (and (pair? vars)
					(assq sym vars))
				   (shadow? ((if (symbol? (cadr tree)) cdddr cddr) tree)))))))

		   ((letrec letrec* do)
		    (or (and (pair? (cadr tree))
			     (assq sym (cadr tree)))
			(shadow? (cddr tree))))

		   ((define lambda define-macro define-constant)
		    (or (eq? sym (cadr tree))
			(and (pair? (cadr tree))
			     (memq sym (cadr tree)))
			(shadow? (cddr tree))))

		   ((lambda* define* define-macro*)
		    (or (eq? sym (cadr tree))
			(and (pair? (cadr tree))
			     (member sym (cadr tree) (lambda (a b)
						       (or (eq? a b)
							   (and (pair? b)
								(eq? a (car b)))))))
			(shadow? (cddr tree))))
		   (else
		    (or (shadow? (car tree))
			(shadow? (cdr tree))))))))))

    (define (gather-symbols tree)
      (let ((syms ()))
	(let walk ((p tree))
	  (if (pair? p)
	      (if (symbol? (car p))
		  (if (not (eq? (car p) 'quote))
		      (for-each (lambda (a)
				  (if (symbol? a)
				      (if (not (memq a syms))
					  (set! syms (cons a syms)))
				      (if (pair? a) (walk a))))
				(cdr p)))
		  (when (pair? (car p))
		    (walk (car p))
		    (walk (cdr p))))
	      (if (and (symbol? tree)
		       (not (memq tree syms)))
		  (set! syms (cons tree syms)))))
	syms))

    (define (tree-arg-member sym tree)
      (and (proper-list? tree)
	   (or (and (memq sym (cdr tree))
		    tree)
	       (and (pair? (car tree))
		    (tree-arg-member sym (car tree)))
	       (and (pair? (cdr tree))
		    (call-with-exit
		     (lambda (return)
		       (for-each
			(lambda (b)
			  (cond ((and (pair? b)
				      (tree-arg-member sym b))
				 => return)))
			(cdr tree))
		       #f))))))

    (define (tree-member sym tree1) ; tree-memq ignoring quote and no match if tree1 == bare sym -- nearly all of these "members" should be "memqs"
      (let tm ((tree tree1))
	(and (pair? tree)
	     (or (eq? (car tree) sym)
		 (tm (car tree))
		 (tm (cdr tree))))))

    (define (tree-equal-member sym tree)
      (and (pair? tree)
	   (or (equal? (car tree) sym)
	       (tree-equal-member sym (car tree))
	       (tree-equal-member sym (cdr tree)))))

    (define (tree-unquoted-member sym tree)
      (and (pair? tree)
	   (tree-memq sym tree)))

    (define (tree-car-member sym tree)
      (and (pair? tree)
	   (or (eq? (car tree) sym)
	       (and (pair? (car tree))
		    (tree-car-member sym (car tree)))
	       (and (pair? (cdr tree))
		    (member sym (cdr tree) tree-car-member)))))

    (define (tree-sym-set-member sym set tree) ; sym as arg, set as car
      (and (pair? tree)
	   (or (memq (car tree) set)
	       (and (pair? (car tree))
		    (tree-sym-set-member sym set (car tree)))
	       (and (pair? (cdr tree))
		    (or (member sym (cdr tree))
			(member #f (cdr tree) (lambda (a b) (tree-sym-set-member sym set b))))))))

    (define (tree-set-member set tree1)
      (case (length set)
	((0) #f)
	((1) (tree-memq (car set) tree1))
	((2) (or (tree-memq (car set) tree1)
		 (tree-memq (cadr set) tree1)))
	(else
	 (let ts ((tree tree1))
	   (if (pair? tree)
	       (and (not (eq? (car tree) 'quote))
		    (or (ts (car tree))
			(ts (cdr tree))))
	       (memq tree set))))))

    (define (tree-set-car-member set tree) ; set as car
      (and (pair? tree)
	   (or (and (memq (car tree) set)
		    tree)
	       (and (pair? (car tree))
		    (tree-set-car-member set (car tree)))
	       (and (pair? (cdr tree))
		    (do ((p (cdr tree) (cdr p)))
			((or (not (pair? p))
			     (tree-set-car-member set (car p)))
			 (pair? p)))))))

    (define (tree-table-car-member set tree) ; hash-table as car
      (and (pair? tree)
	   (or (and (hash-table-ref set (car tree))
		    tree)
	       (and (pair? (car tree))
		    (tree-table-car-member set (car tree)))
	       (and (pair? (cdr tree))
		    (do ((p (cdr tree) (cdr p)))
			((or (not (pair? p))
			     (tree-table-car-member set (car p)))
			 (pair? p)))))))

    (define (maker? tree)
      (tree-table-car-member makers tree))

    (define (tree-symbol-walk tree syms)
      (if (pair? tree)
	  (case (car tree)
	    ((quote)
	     (if (and (pair? (cdr tree))
		      (symbol? (cadr tree))
		      (not (memq (cadr tree) (car syms))))
		 (tree-symbol-walk (cddr tree) (begin (set-car! syms (cons (cadr tree) (car syms))) syms))))
	    ((list-values)
	     (if (and (pair? (cdr tree))
		      (pair? (cadr tree))
		      (eq? (caadr tree) 'quote)
		      (symbol? (cadadr tree))
		      (not (memq (cadadr tree) (cadr syms))))
		 (tree-symbol-walk (cddr tree) (begin (list-set! syms 1 (cons (cadadr tree) (cadr syms))) syms))))
	    (else
	     (tree-symbol-walk (car tree) syms)
	     (tree-symbol-walk (cdr tree) syms)))))

    (denote (unbegin x)
      ((if (and (pair? x)
		(list? (cdr x))
		(eq? (car x) 'begin))
	  cdr list)
       x))


    ;; -------- types --------

    (denote (quoted-pair? x)
      (and (pair? x)
	   (eq? (car x) 'quote)
	   (pair? (cdr x))
	   (pair? (cadr x))))

    (denote (quoted-undotted-pair? x)
      (and (quoted-pair? x)
	   (positive? (length (cadr x)))))

    (denote (quoted-null? x)
      (and (len=2? x)
	   (eq? (car x) 'quote)
	   (null? (cadr x))))

    (denote (any-null? x)
      (or (null? x)
	  (and (pair? x)
	       (case (car x)
		 ((quote)
		  (and (pair? (cdr x))
		       (null? (cadr x))))
		 ((list)
		  (null? (cdr x)))
		 ;; no hits (in this context): (make-list 0 ...) (string->list "") (vector->list #()) (reverse ()) (copy ()) (append ()) (append)
		 (else #f)))))

    (denote (quoted-not? x)
      (and (len=2? x)
	   (eq? (car x) 'quote)
	   (not (cadr x))))

    (denote (quoted-symbol? x)
      (and (pair? x)
	   (eq? (car x) 'quote)
	   (pair? (cdr x))
	   (symbol? (cadr x))))

    (define constant-expression?
      (let ((constant-functions (let ((ht (make-hash-table)))
				  (for-each
				   (lambda (op)
				     (set! (ht op) #t))
				   '(* + - / < <= = > >=
				     abs acos acosh and angle append aritable? arity ash asin asinh assoc assq assv atan atanh
				     begin boolean? byte? byte-vector byte-vector?
				     caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
				     c-pointer c-pointer? c-object? c-object-type car case cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr
				     cddar cdddar cddddr cdddr cddr cdr ceiling char->integer char-alphabetic? char-ci<=?
				     char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric?
				     char-position char-upcase char-upper-case? char-whitespace? char<=? char<?
				     char=? char>=? char>? char? complex complex? cons continuation? cos constant?
				     cosh cyclic-sequences
				     denominator dilambda?
				     eof-object? eq? equal? eqv? even? exact->inexact exact? exp expt
				     float? float-vector-ref float-vector? floor
				     gcd gensym?
				     hash-table-entries hash-table-ref hash-table?
				     imag-part immutable? inexact->exact inexact? infinite? inlet input-port?
				     int-vector-ref int-vector? iterator-at-end? iterator-sequence integer->char
				     integer-decode-float integer-length integer? iterator?
				     keyword->symbol keyword?
				     lcm let->list length let-ref let? list-ref
				     list-tail list? log logand logbit? logior lognot logxor
				     macro? magnitude make-polar
				     make-rectangular max member memq memv min modulo equivalent?
				     nan? negative? not null? number->string number? numerator
				     object->string odd? openlet? or output-port?
				     pair? port-closed? positive?
				     setter signature procedure? proper-list? provided?
				     quote quotient
				     random-state? rational? rationalize real-part real? remainder reverse round
				     sequence? sin sinh square sqrt string->number string->symbol
				     string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? string-downcase string-length
				     string-position string-ref string-upcase string<=? string<? string=? string>=? string>? string?
				     sublet substring  subvector? symbol symbol->keyword symbol->string symbol? syntax?
				     tan tanh tree-leaves tree-memq truncate
				     unspecified? undefined?
				     vector-dimensions vector-dimension vector-length vector-ref vector?
				     zero?))
				  ht)))
	(lambda (val env)
	  (or (code-constant? val)
	      (and (pair? val)
		   (hash-table-ref constant-functions (car val))
		   (not (var-member (car val) env))
		   (just-code-constants? (cdr val)))))))


    ;; -------- func info --------
    (define (arg-signature fnc e)
      (and (symbol? fnc)
	   (let ((fd (var-member fnc e)))
	     (if fd
		 (and (symbol? (var-ftype fd))
		      (var-signature fd))
		 (signature (symbol->value fnc))))))

    (define (arg-arity fnc env)
      (and (symbol? fnc)
	   (cond ((var-member fnc env) => var-arity)
		 (else
		  (let ((f (symbol->value fnc (rootlet))))
		    (and (procedure? f)
			 (arity f)))))))

    (define (dummy-func caller form f)
      (catch #t
	(lambda ()
	  (eval f))
	(lambda args
	  (lint-format "in ~A, ~A" caller
		       (truncated-list->string form)
		       (apply format #f (cadr args))))))

    (define (count-values body)
      (let ((mn #f)
	    (mx #f))
	(if (pair? body)
	    (let counter ((ignored #f) ; 'ignored is for member's benefit
			  (tree (last-ref body)))
	      (if (pair? tree)
		  (if (eq? (car tree) 'values)
		      (if (pair? (cdr tree))
			  (let ((args (- (length tree) 1)))
			    (for-each (lambda (p)
					(if (and (pair? p)
						 (eq? (car p) 'values))
					    (set! args (+ args (length p) -2))))
				      (cdr tree))
			    (set! mn (min (or mn args) args))
			    (set! mx (max (or mx args) args))))
		      (begin
			(if (pair? (car tree))
			    (counter 'values (car tree)))
			(if (pair? (cdr tree))
			    (member #f (cdr tree) counter)))))
	      #f)) ; return #f so member doesn't quit early
	(and mn (list mn mx))))


    (define (get-signature v)
      (let ((ftype (var-ftype v))
	    (initial-value (var-initial-value v))
	    (arglist (var-arglist v))
	    (env (var-env v)))
	(let ((body (and (memq ftype '(define define* lambda lambda* let))
			 (cddr initial-value))))
	  (and (pair? body)
	       (let ((sig (let signer ((endb (last-ref body)))
			    (cond ((not (pair? endb))
				   (and (not (symbol? endb))
					(list (->lint-type endb))))

				  ((side-effect? endb env)
				   (list (case (car endb)
					   ((display write)
					    (->lint-type (cadr endb)))

					   ((write-char write-byte string-copy openlet float-vector-set! read-char read-byte
					    open-output-string open-input-file int-vector-set! dilambda open-input-string
					    get-output-string read-line newline close-output-port close-input-port coverlet
					    peek-char open-output-file string->byte-vector read-string file-exists? directory?)
					    => ->lint-type)

					   (else #t))))

				  ((arg-signature (car endb) env)
				   => (lambda (a)
					(and (pair? a)
					     (list (car a)))))
				  (else
				   (let ((len (length endb)))
				     (case (car endb)
				       ((if)
					(and (= len 4)
					     (let ((a1 (signer (caddr endb)))
						   (a2 (signer (cadddr endb))))
					       (and (equal? a1 a2) a1))))

				       ((let let* letrec letrec* unless when with-let let-temporarily with-baffle)
					(and (> len 2)
					     (signer (list-ref endb (- len 1)))))

				       ((begin)
					(and (> len 1)
					     (signer (list-ref endb (- len 1)))))

				       ((do)
					(and (> len 2)
					     (pair? (caddr endb))
					     (pair? (cdaddr endb)) ; if nil -> unspecified?
					     (signer (last-ref (cdaddr endb)))))

				       (else #f))))))))
		 (if (not (pair? sig))
		     (set! sig (list #t)))

		 (when (and (proper-list? arglist)
			    (not (any-keywords? arglist)))
		   (for-each
		    (lambda (arg)       ; new function's parameter
		      (set! sig (cons #t sig))
		      ;; (if (pair? arg) (set! arg (car arg)))
		      ;; causes trouble when tree-count sees keyword args in s7test.scm
		      (if (= (tree-count arg body 2) 1)
			  (let ((p (tree-arg-member arg body)))
			    (when (pair? p)
			      (let ((f (car p))
				    (m (memq arg (cdr p))))
				(if (pair? m)
				    (let ((fsig (arg-signature f env)))
				      (if (pair? fsig)
					  (let ((chk (catch #t
						       (lambda ()
							 (fsig (- (length p) (length m))))
						       (lambda args #f))))
					    (if (and (symbol? chk) ; it defaults to #t
						     (not (memq chk '(integer:any? integer:real?))))
						(set-car! sig chk)))))))))))
		    arglist))
		 (and (lint-any? (lambda (a) (not (eq? a #t))) sig)
		      (reverse sig)))))))

    (denote (args->proper-list args)
      (cond ((symbol? args)
	     (list args))
	    ((not (pair? args))
	     ())
	    ((pair? (car args))
	     (cons (caar args) (args->proper-list (cdr args))))
	    ((keyword? (car args))              ; omit :rest et al
	     (args->proper-list (cdr args)))
	    (else
	     (cons (car args) (args->proper-list (cdr args))))))

    (define out-vars
      (let ((ref ())
	    (set ()))

	(define (var-walk-body tree e)
	  (when (pair? tree)
	    (for-each (lambda (p) (set! e (var-walk p e))) tree)))

	(define (shadowed v e)
	  (if (and (or (memq v e)
		       (memq v ref))
		   (not (memq v set)))
	      (set! set (cons v set)))
	  v)

	(define (var-walk tree e)
	  (if (symbol? tree)
	      (if (not (or (memq tree e)
			   (memq tree ref)
			   (defined? tree (rootlet))))
		  (set! ref (cons tree ref)))
	      (when (pair? tree)
		(if (not (pair? (cdr tree)))
		    (var-walk (car tree) e)
		    (if (not (pair? (cddr tree)))
			(begin
			  (var-walk (car tree) e)
			  (var-walk (cadr tree) e))
			(case (car tree)
			  ((set! vector-set! list-set! hash-table-set! float-vector-set! int-vector-set!
				 string-set! let-set! fill! string-fill! list-fill! vector-fill!
				 reverse! sort! set-car! set-cdr!)
			   (let ((sym (if (symbol? (cadr tree))
					  (cadr tree)
					  (if (pair? (cadr tree)) (caadr tree)))))
			     (if (not (or (memq sym e) (memq sym set)))
				 (set! set (cons sym set)))
			     (var-walk (cddr tree) e)))

			  ((let letrec)
			   (let* ((named (symbol? (cadr tree)))
				  (vars (if named
					    (list (shadowed (cadr tree) e))
					    ()))
				  (varlist ((if named caddr cadr) tree)))
			     (when (pair? varlist)
			       (for-each (lambda (v)
					   (when (len>1? v)
					     (var-walk (cadr v) e)
					     (set! vars (cons (shadowed (car v) e) vars))))
					 ((if named caddr cadr) tree)))
			     (var-walk-body ((if named cdddr cddr) tree) (append vars e))))

			  ((let* letrec*)
			   (let* ((named (symbol? (cadr tree)))
				  (vars (if named (list (cadr tree)) ()))
				  (varlist ((if named caddr cadr) tree)))
			     (when (pair? varlist)
			       (for-each (lambda (v)
					   (when (len>1? v)
					     (var-walk (cadr v) (append vars e))
					     (set! vars (cons (shadowed (car v) e) vars))))
					 varlist))
			     (var-walk-body ((if named cdddr cddr) tree) (append vars e))))

			  ((case)
			   (var-walk (cadr tree) e)
			   (for-each (lambda (c)
				       (when (pair? c)
					 (var-walk (cdr c) e)))
				     (cddr tree)))

			  ((quote) #f)

			  ((do)
			   (let ((vars ()))
			     (when (pair? (cadr tree))
			       (for-each (lambda (v)
					   (when (len>1? v)
					     (var-walk (cadr v) e)
					     (set! vars (cons (shadowed (car v) e) vars))))
					 (cadr tree))
			       (for-each (lambda (v)
					   (if (len>2? v)
					       (var-walk (caddr v) (append vars e))))
					 (cadr tree)))
			     (var-walk (caddr tree) (append vars e))
			     (var-walk-body (cdddr tree) (append vars e))))

			  ((lambda lambda*)
			   (var-walk-body (cddr tree) (append (args->proper-list (cadr tree)) e)))

			  ((define* define-macro define-macro* define-bacro define-bacro*)
			   (when (pair? (cadr tree))
			     (set! e (cons (caadr tree) e))
			     (var-walk-body (cddr tree) (append (args->proper-list (cdadr tree)) e))))

			  ((define define-constant)
			   (cond ((symbol? (cadr tree))
				  (var-walk (caddr tree) e)
				  (set! e (cons (cadr tree) e)))
				 ((pair? (cadr tree))
				  (set! e (cons (caadr tree) e))
				  (var-walk-body (cddr tree) (append (args->proper-list (cdadr tree)) e)))))

			  (else
			   (var-walk (car tree) e)
			   (var-walk (cdr tree) e)))))))
	  e)
	(lambda (func-name arglist body)
	  (set! ref ())
	  (set! set ())
	  (var-walk body (cons func-name arglist))
	  (list ref set))))

    (define (get-side-effect v)
      (let ((ftype (var-ftype v)))
	(or (not (memq ftype '(define define* lambda lambda* define-public)))
	    (let ((body (cddr (var-initial-value v)))
		  (env (var-env v))
		  (args (cons (var-name v) (args->proper-list (var-arglist v)))))
	      (let ((outvars (append (cadr (out-vars (var-name v) args body)) args)))
		;; this should omit lambda and lambda* since reference to the name is a recursive call but
		;;   then we get an inifinite loop remaking var-side-effect (a bug).
		(let ((fv (copy v)))
		  (let-set! (cdr fv) 'side-effect #f)
		  (set! env (cons fv env)))
		(any-side-effect? body env outvars))))))

    (denote (tree-subst new old tree)
      (cond ((equal? old tree)
	     new)

	    ((not (pair? tree))
	     tree)

	    ((eq? (car tree) 'quote)
	     (copy tree))

	    (else (cons (tree-subst new old (car tree))
			(tree-subst new old (cdr tree))))))


    (define (do->make-list caller form original-form var1 var2) ; (var1: (... (+/-)) var2: (... (cons)))
      (when (and (len=2? (cdr var2))
		 (len=3? (caddr var1))
		 (len=3? (caddr var2)))
	(let ((name1 (car var1))
	      (name2 (car var2))
	      (init1 (cadr var1))
	      (init2 (cadr var2))
	      (step1 (caddr var1))
	      (step2 (caddr var2))
	      ;; (do ((i 0 (+ i 1)) (lst () (cons 1 lst))) ((= i 10) lst)) -> (make-list 10 1)
	      ;;      name1: i, init1: 0 :step1: (+ i 1), name2: lst, init2: (), step2: (cons 1 lst)
	      ;;                                          end: (= i 10), result: (lst)
	      (end (and (pair? (caddr form)) (caaddr form)))
	      (result (and (pair? (caddr form)) (cdaddr form))))

	  ;; the equivalent named let:
	  ;; (let loop ((i 0) (lst ())) (if (= i 10) lst (loop (+ i 1) (cons 1 lst))))

	  (when (eq? (car end) 'negative?)
	    (set! end `(< ,(cadr end) 0)))

	  (when (and (len=1? result)
		     (eq? name2 (caddr step2))
		     (eq? name1 (cadr end))
		     (eq? (car result) name2)
		     (len=3? end)
		     (any-null? init2)
		     (if (eq? (car step1) '+)          ; (+ i 1) or (+ 1 i)
			 (and (memv 1 step1)
			      (memq name1 step1)
			      (memq (car end) '(= >= >)))
			 (and (eq? name1 (cadr step1)) ; (- i 1)
			      (eqv? 1 (caddr step1))
			      (memq (car end) '(= <= <)))))

	    (let ((fill (cadr step2)))
	      (cond ((or (not (pair? fill))
			 (eq? (car fill) 'quote)
			 (not (tree-memq name1 fill))) ; perhaps if (pair? fill) check somehow for changing fill values
		     (unless (eq? name1 fill) ; "iota" in this case
		       (let ((len (cond
				   ((and (integer? init1)
					 (integer? (caddr end)))
				    (if (memq (car end) '(> <))
					(+ (abs (- init1 (caddr end))) 1)
					(abs (- init1 (caddr end)))))

				   ((eq? (car step1) '+)
				    (if (eqv? init1 0)
					(if (eq? (car end) '>)
					    `(+ ,(caddr end) 1)
					    (caddr end))
					(if (eq? (car end) '>)
					    `(+ (- ,(caddr end) ,init1) 1)
					    `(- ,(caddr end) ,init1))))

				   ;; else (car step1) is '-
				   ((eqv? (caddr end) 0)
				    (if (eq? (car end) '<)
					`(+ ,init1 1)
					init1))

				   ((eq? (car end) '<)
				    `(+ (- ,init1 ,(caddr end)) 1))
				   (else `(- ,init1 ,(caddr end))))))

			 (lint-format "perhaps ~A~A" caller
				      (if (and (pair? fill)
					       (not (eq? (car fill) 'quote)))
					  (format #f ", (assuming ~S is not problematic), " fill)
					  "")
				      (lists->string original-form
						     `(make-list ,len ,fill))))))

		    ((and (memq (car fill) '(string-ref vector-ref))
			  (len=3? fill)
			  (or (eq? (caddr fill) name1)
			      (equal? (caddr fill) `(- ,name1 1))))
		     (lint-format "perhaps ~A" caller
				  (format #f "~A -> ~A" original-form
					  (if (eq? (car fill) 'string-ref) 'string->list 'vector->list))))

		    ((and (len=2? fill)
			  (len=3? (cadr fill))
			  (memq (caadr fill) '(vector-ref string-ref byte-vector-ref float-vector-ref int-vector-ref  list-ref))
			  (eq? name1 (caddr (cadr fill))))
		     (lint-format "perhaps ~A" caller
				  (format #f "~A -> ~A" original-form
					  `(map ,(car fill) ,(cadadr fill)))))))))))

    (define recursion->iteration
      (let ((rewrite-map
	     (lambda (map? name iter sequence form outer-form)
	       (let* ((new-form (cons 'lambda
				      (cons (list '<1>)
					    (let rem ((tree form))
					      (cond ((not (unquoted-pair? tree))
						     tree)
						    ((and (pair? (cdr tree))
							  (eq? (cadr tree) iter)
							  (hash-table-ref cxars (car tree)))
						     => (lambda (cxar)
							  (if (null? cxar)
							      '<1>
							      (list cxar '<1>))))
						    (else (cons (rem (car tree))
								(rem (cdr tree)))))))))
		      (leftovers (tree-memq iter new-form))) ; did we catch every reference to the iter?
		 (unless leftovers
		   (if (and map?
			    (eq? '<1> (caddr new-form)))
		       (lint-format "perhaps ~A" name (lists->string outer-form (list 'copy sequence)))
		       (begin
			 (if (and (len=1? (cddr new-form))
				  (len=2? (caddr new-form))
				  (eq? '<1> (cadr (caddr new-form))))
			     (set! new-form (caaddr new-form)))
			 (lint-format "perhaps ~A" name
				      (lists->string outer-form
						     (if (eq? (car outer-form) 'let) ; named-let, not define
							 (list (if map? 'map 'for-each) new-form sequence)
							 (list (car outer-form) (cadr outer-form)
							       (list (if map? 'map 'for-each) new-form sequence))))))))
		 (not leftovers)))))

	(lambda (name ftype arglist initial-value env)
	  ;; look for recursion -> simpler iteration possibilities

	  (when (and (pair? initial-value)
		     (proper-pair? arglist)
		     (= (tree-count name (cddr initial-value) 2) 1))

	    (let ((body ((if (memq ftype '(let let*)) cdddr cddr) initial-value))
		  (for-each-case #f)) ; avoid rewriting twice
	      (when (and (len=1? body)
			 (len>1? (car body))
			 (let ((exprs (cdar body)))
			   (case (caar body)        ; change body to use if
			     ((if)                  ; only 1 hit for 2 reversal branches, say 20 hits for 2 ifs + repeated return vals (collapsible) -- see tmp
			      (len>1? exprs))
			     ((when)
			      (and (len>1? exprs)
				   (set! body `((if ,(car exprs)
						    ,@(if (null? (cddr exprs))
							  (cdr exprs)
							  (list (cons 'begin (cdr exprs)))))))))
			     ((unless)
			      (and (len>1? exprs)
				   (set! body `((if (not ,(car exprs))
						    ,@(if (null? (cddr exprs))
							  (cdr exprs)
							  (list (cons 'begin (cdr exprs)))))))))
			     ((cond)
			      (and (<= 1 (length exprs) 2)
				   (not (tree-memq '=> exprs))
				   (or (null? (cdr exprs))
				       (and (pair? (cadr exprs))
					    (memq (caadr exprs) '(else #t))))
				   (let ((arg1 (car exprs)))
				     (and (proper-list? arg1)
					  (set! body `((if ,(car arg1)
							   ,@(if (null? (cdr arg1)) '(#t)
								 (if (null? (cddr arg1)) (cdr arg1)
								     (list (cons 'begin (cdr arg1)))))
							   ,@(if (not (= (length exprs) 2))
								 ()
								 (let ((arg2 (cdadr exprs)))
								   (if (null? (cdr arg2))
								       arg2
								       `((begin ,@arg2))))))))))))
			     ((case)
			      (and (<= 2 (length exprs) 3)
				   (not (tree-memq '=> exprs))
				   (let ((selector (car exprs))
					 (arg1 (cadr exprs))
					 (rest (cddr exprs)))
				     (and (or (null? rest)
					      (and (pair? (car rest))
						   (eq? 'else (caar rest))))
					  (set! body `((if (memv ,selector ',(car arg1))
							   ,@(if (null? (cddr arg1)) (cdr arg1)
								 (list (cons 'begin (cdr arg1))))
							   ,@(if (not (pair? rest))
								 ()
								 (let ((arg2 (cdar rest)))
								   (if (null? (cdr arg2))
								       arg2
								       `((begin ,@arg2))))))))))))

			     ;; ((and or) (let ((last (last-ref exprs))) (if (and (pair? last) (eq? (car last) name)) (format *stderr* "~S~%" body)) #f))
			     ;;   -> ((or done? (loop (process-stderr server)))) etc
			     ;; zillions of or/and (well, 200) -- the problem is the result, and do is not shorter
			     ;; (if (and (memq (caar body) '(or and)) (pair? (cadar body)) (memq (caadar body) '(= < > <= >=)) (format *stderr* "~A~%~%" (lint-pp body)))
			     ;; (or (>= i len) (and (x i) (rec (+ i 1))))
			     ;;   (do ((i 0 (+ i 1))) ((or (>= i len) (not (x i))) (>= i len)))
			     ;; (and (< i len) (or (x i) (rec (+ i 1))))
			     ;;   (do ((i 0 (+ i 1))) ((or (= i len) (x i)) (< i len)))
			     ;; memx here might be shorter if var is a list

			     (else #f))))

		;; (caar body) is 'if

		;; 2 arg map case
		(when (and (= (length arglist) 2)
			   (eq? ftype 'let))
		  (let ((args (caddr initial-value)))
		    (let ((iter (car args))
			  (res (cadr args)))

		      (when (and (len>1? iter)
				 (any-null? (cadr iter)))
			(set! iter (cadr args))
			(set! res (car args)))

		      (when (and (len=2? res)
				 (any-null? (cadr res))
				 (<= 2 (tree-count (car res) body 4) 3))

			(let ((nf (cdar body)))       ;((null? lst) (reverse...) (loop ...))
			  (when (and (len>2? nf)
				     (pair? (car nf))
				     (memq (caar nf) '(null? not pair?))) ; no hits for list? or eq?+any-null?
			    (let ((cdrf #f)
				  (recur #f)
				  (iter-name (car iter))
				  (res-name (car res)))

			      ;; the wrapper case [(cons (reverse res) ...) at the end] in non-problematic code gets about 10 hits
			      (when (and (or (and (len=2? (cadr nf))
						  (memq (caadr nf) '(reverse reverse!))
						  (eq? (cadadr nf) res-name)
						  (or (equal? (car nf) (list 'null? iter-name))
						      (equal? (car nf) (list 'not (list 'pair? iter-name))))
						  (set! cdrf (caddr nf)))
					     (and (len=2? (caddr nf))
						  (memq (caaddr nf) '(reverse reverse!))
						  (eq? (cadr (caddr nf)) res-name)
						  (or (equal? (car nf) (list 'not (list 'null? iter-name)))
						      (equal? (car nf) (list 'pair? iter-name)))
						  (set! cdrf (cadr nf))))
					 (pair? cdrf)
					 ;; here the end test looks ok, and its branch reverses the apparent result
					 ;;   cdrf is the other branch
					 (or (and (eq? name (car cdrf))
						  (set! recur cdrf))
					     (and (memq (car cdrf) '(begin let let*))
						  (let ((ret (last-ref cdrf)))
						    (and (pair? ret)
							 (eq? name (car ret))
							 (set! recur ret)))))   ; recur is the looper
					 (len=3? recur)
					 (or (and (eq? iter-name (caar args)) ; order should be (cdr iter) res
						  (equal? (cadr recur) (list 'cdr iter-name))
						  (set! recur (caddr recur)))
					     (and (eq? res-name (caar args)) ; order should be res (cdr iter)
						  (equal? (caddr recur) (list 'cdr iter-name))
						  (set! recur (cadr recur)))))
				;; now recur is the portion that conses up the growing list
				;;   it can be (cons ... res) or some variant thereof
				;;   (append ... res) -> (values ...), res by itself -> (values)
				;; so whatever recur has, its only return exprs are (cons|append ... res) or res
				;;   but what about the body above? -- we have (body ... recur) and need to edit just recur
				;; if (eq? name (car cdrf)) return edited recur else (body[-1]+edited recur)

				(let ((nmap (call-with-exit
					     (lambda (quit)
					       (let subst ((tree recur))
						 (cond ((eq? tree res-name)
							(quit #f))

						       ((not (unquoted-pair? tree))
							tree)

						       ((and (eq? (car tree) 'cons)
							     (eq? res-name (caddr tree)))
							(cadr tree))

						       ((eq? (car tree) 'append)
							(if (and (len=3? tree)
								 (eq? (caddr tree) res-name))
							    (list 'apply 'values (cadr tree))
							    (quit #f)))

						       ((and (eq? (car tree) 'if)
							     (len=3? (cdr tree))
							     (not (tree-memq res-name (cadr tree)))
							     (tree-memq res-name (cddr tree)))
							(list 'if (cadr tree)
							      (if (eq? (caddr tree) res-name)
								  '(values)
								  (subst (caddr tree)))
							      (if (eq? (cadddr tree) res-name)
								  '(values)
								  (subst (cadddr tree)))))

						       (else (cons (subst (car tree))
								   (subst (cdr tree))))))))))
				  (if (pair? nmap)
				      (set! for-each-case (rewrite-map #t name iter-name (cadr iter)
								       (if (eq? (car cdrf) name)
									   (copy (unbegin nmap))
									   (tree-subst nmap recur cdrf))
								       initial-value))))))))))))
		;; for two arg for-each gets about a dozen hits

		;; one arg...
		(when (null? (cdr arglist))
		  ;; recursion -> for-each and map

		  (let ((nf (cdar body))
			(sequence (case (car initial-value)
				    ((let let*) ; named-let, not define
				     (let ((args (cdar (caddr initial-value))))
				       (and (pair? args)
					    (car args))))
				    ((lambda lambda*)
				     (caadr initial-value))
				    (else (cadadr initial-value)))))

		    (when (len>1? nf)
		      (when (and (pair? (cddr nf))
				 (pair? (car nf))
				 (memq (caar nf) '(null? not pair?)))
			;; recursion -> map
			(let ((cdrf #f)
			      (iter (car arglist)))
			  (when (and (or (and (or (any-null? (cadr nf))
						  (eq? (cadr nf) iter)) ; actually, if (not (pair? iter)) this will not be null -- no hits
					      (or (equal? (car nf) (list 'null? iter))
						  (equal? (car nf) (list 'not (list 'pair? iter))))
					      (set! cdrf (caddr nf)))
					 (and (or (any-null? (caddr nf))
						  (eq? (caddr nf) iter))
					      (or (equal? (car nf) (list 'not (list 'null? iter)))
						  (equal? (car nf) (list 'pair? iter)))
					      (set! cdrf (cadr nf))))
				     (pair? cdrf)
				     (case (car cdrf)
				       ((cons)
					(and (len>1? (cdr cdrf))
					     (equal? (caddr cdrf) (list name (list 'cdr iter)))))

				       ((let) ; this only get 5 hits
					(and (len=1? (cadr cdrf))
					     (len=1? (cddr cdrf))
					     (len=3? (caddr cdrf))
					     (eq? (caaddr cdrf) 'cons)
					     (len>1? (cdaddr cdrf))
					     (equal? (cadr (caadr cdrf)) (list 'car iter))
					     (equal? (caddr (caddr cdrf)) (list name (list 'cdr iter)))
					     (set! cdrf (list 'cons
							      (tree-subst (list 'car iter) (caaadr cdrf) (cadr (caddr cdrf)))
							      (caddr (caddr cdrf))))))

				       (else #f)))
			    (set! for-each-case (rewrite-map #t name iter sequence
							     (copy (unbegin (cadr cdrf)))
							     initial-value)))))

		      (let ((iters ()))
			;; recursion -> for-each
			(when (and (null? (cddr nf))
				   (let ((lst (cadr nf))
					 (find-iter (lambda (args)
						      (lint-any? (lambda (p)
								   (and (pair? p)
									(eq? (car p) 'cdr)
									(let ((sym (cadr p)))
									  (and (symbol? sym)
									       (set! iters (cons sym iters))))))
								 args))))
				     (and (pair? lst)
					  (or (and (eq? name (car lst))
						   (find-iter (cdr lst)))
					      (and (memq (car lst) '(begin when unless let let*))
						   (let ((ret (last-ref lst)))
						     (and (pair? ret)
							  (eq? name (car ret))
							  (find-iter (cdr ret))))))))
				   (len=1? iters)
				   (eq? (car iters) (car arglist))
				   (or (equal? (car nf) (list 'pair? (car iters)))
				       (equal? (car nf) (list 'not (list 'null? (car iters))))))
			  (set! for-each-case (rewrite-map #f name
							   (car iters) sequence
							   (unbegin (copy (cadr nf) (make-list (- (length (cadr nf)) 1))))
							   initial-value)))))))

		;; any number of args here, still if-based as above
		;;
		;; recursion->do
		;;   (n ((a b) (c d)) (if .1. (begin .2. (n (+ a 1) (cdr c))) .3.)) -> (do ((a b (+ a 1)) (c d (cdr c))) ((not .1.) .3.) .2.)
		;;   (n ((a b)) (if .1. .2. (begin .3. (n (f a))))) -> (do ((a b (f a))) (.1. .2.) .3.)
		;;
		;; if (define loop (lambda ...)), then initial-value is (lambda ...)
		;;   but name is :lambda -- define-walker around 12440 catches this case and gives true name, so it calls this function

		(unless for-each-case                   ; one rewrite is enough
		  (let ((f (cdar body)))
		    (let ((end-test (car f))            ; assume (if test result recur)
			  (result (cadr f))
			  (do-body (if (pair? (cddr f))
				       (caddr f)
				       (list 'begin)))) ; assume end in rewrite below is ,@(unbegin...)

		      (when (tree-memq name result)     ; flip above assumption: (if test recur result)
			(set! end-test (simplify-boolean (list 'not end-test) () () env))
			(let ((old-res result))
			  (set! result do-body)
			  (set! do-body old-res)))

		      (when (and (pair? do-body)
				 (< (tree-leaves result) 50))
			(let ((call (if (eq? (car do-body) name)
					do-body
					(and (eq? (car do-body) 'begin)
					     (let ((lp (last-ref do-body)))
					       (and (pair? lp)
						    (eq? (car lp) name)
						    lp))))))
			  (when (and (pair? call)
				     (pair? (cdr call))
				     ;; now check arglist/call -- each arg just current name or outer vars [leaving aside car]
				     (or (and (null? (cddr call))
					      (null? (cdr arglist)))
					 (let check-iters ((pars arglist) (args (cdr call)))
					   (if (null? pars)
					       (null? args)
					       (and (pair? args)
						    (let ((par ((if (pair? (car pars)) caar car) pars)))
						      (and (not (memq (car args) (remove-one par arglist)))
							   (not (tree-set-memq (remove-one par arglist) (car args)))
							   (check-iters (cdr pars) (cdr args)))))))))
			    (let ((do-loop `(do ,(map (lambda (par init arg)
							(let ((var (if (pair? par) (car par) par)))
							  (if (eq? var arg)
							      (list var
								    (if (len>1? init) (cadr init) init))
							      (list var
								    (if (len>1? init) (cadr init) init)
								    arg))))
						      arglist
						      (if (memq ftype '(let let*))
							  (caddr initial-value)
							  (map (lambda (p)
								 (if (pair? p)
								     (car p)
								     p))
							       arglist))
						      (cdr call))
						(,end-test ,@(unbegin result))
					      ,@(if (eq? (car do-body) name)
						    ()
						    (unbegin (copy do-body (make-list (- (length do-body) 1))))))))
			      (lint-format "perhaps ~A" name
					   (lists->string initial-value
							  (if (memq ftype '(let let*))
							      do-loop
							      (list (car initial-value) (cadr initial-value) do-loop))))

			      (when (and (len=2? arglist)
					 (null? (cdddr do-loop))) ; no body
				(let ((var1 (caadr do-loop)))
				  (if (and (len=2? (cdr var1))
					   (pair? (caddr var1))
					   (eq? (caaddr var1) 'cons))
				      (do->make-list name do-loop initial-value (cadadr do-loop) var1)
				      (let ((var2 (cadadr do-loop)))
					(if (and (len=2? (cdr var2))
						 (pair? (caddr var2))
						 (eq? (caaddr var2) 'cons))
					    (do->make-list name do-loop initial-value var1 var2))))))
			      )))))))))))))

    (define (improper-arglist->define* name ftype arglist initial-value)
      ;; look for define/lambda -> define*/lambda*

      (when (and (or (symbol? arglist)
		     (and (pair? arglist)
			  (not (proper-list? arglist)))) ; can't be named-let
		 (memq ftype '(define lambda))
		 (len=1? (cddr initial-value)))
	(let ((body (caddr initial-value)))
	  (when (and (len>1? body)
		     (memq (car body) '(let let*))
		     (pair? (cadr body)))
	    (let* ((rest-name (if (symbol? arglist)
				  arglist
				  (list-tail arglist (abs (length arglist)))))
		   (rest-refs (tree-count rest-name body 3)))

	      (when (= rest-refs 2) ; more refs for added optargs got few hits (and they were riddled with mistakes)
		(let ((var1 (caadr body))
		      (default-value :unset))
		  (when (and (len>1? var1)
			     (pair? (cadr var1)))
		    (let ((value (cadr var1)))
		      (when (and (eq? (car value) 'if) ; cond (in ((null? rest) defval) (else (car rest))) gets 3 hits
				 (len=3? (cdr value)))
			(let ((test (cadr value))
			      (access (list 'car rest-name)))
			  (if (and (equal? (cadddr value) access)
				   (or (equal? test (list 'null? rest-name))
				       (equal? test (list 'not (list 'pair? rest-name)))))
			      (set! default-value (caddr value))
			      (if (and (equal? (caddr value) access)
				       (or (equal? test (list 'pair? rest-name))
					   (equal? test (list 'not (list 'null? rest-name)))
					   (eq? test rest-name)   ; actually incorrect but it happens
					   (equal? test access))) ; same
				  (set! default-value (cadddr value))))
			  (unless (eq? default-value :unset)
			    (let ((new-arglist (if (symbol? arglist)
						   (list (if default-value
							     (list (car var1) default-value)
							     (car var1)))
						   (append (copy arglist (make-list (abs (length arglist))))
							   (list (if default-value ; #f is default
								     (list (car var1) default-value)
								     (car var1))))))
				  (new-body (if (null? (cdadr body))               ; 0 other vars
						'...
						(list (if (or (eq? (car body) 'let)
							      (null? (cddadr body)))  ; 1 other var
							  'let 'let*)
						      (cdadr body)
						      '...))))
			      ;; we're assuming that trailing args are a mistake -- we could add :rest or something to allow them
			      (lint-format "perhaps ~A" name
					   (lists->string initial-value
							  (if (eq? (car initial-value) 'define)
							      (list 'define* (cons (caadr initial-value) new-arglist)
								    new-body)
							      (list 'lambda* new-arglist
								    new-body)))))))))))))))))

    (define form->arity
      (let ((max-arity 536870912))
	(lambda (form)
	  ;; this ignores non-s7 keywords and the like (#!optional :: etc), treating them as parameter names
	  (and (pair? form)
	       (case (car form)
		 ((lambda)
		  (cond ((list? (cadr form))
			 (let ((len (length (cadr form))))
			   (if (negative? len)
			       (cons (abs len) max-arity)
			       (cons len len))))
			((symbol? (cadr form))
			 (cons 0 max-arity))
			(else #f)))

		 ((define define-constant define-macro define-bacro)
		  (cond ((list? (cdadr form))
			 (let ((len (length (cdadr form))))
			   (if (negative? len)
			       (cons (abs len) 536870912)
			       (cons len len))))
			((symbol? (cdadr form))
			 (cons 0 max-arity))
			(else #f)))

		 ((let)                              ; let = named let
		  (let ((len (length (caddr form))))
		    (cons len len)))

		 ((let*)
		  (cons 0 (length (caddr form))))

		 ((lambda*)
		  (let ((args (cadr form)))
		    (cond ((list? args)
			   (let ((len (length args))
				 (rest (or (memq :rest args)
					   (memq :allow-other-keys args))))
			     (cons 0 (if (or rest (negative? len))
					 max-arity
					 len))))
			  ((symbol? args)
			   (cons 0 max-arity))
			  (else #f))))

		 ((define* define-macro* define-bacro*)
		  (let ((args (cdadr form)))
		    (cond ((list? args)
			   (let ((len (length args))
				 (rest (or (memq :rest args)
					   (memq :allow-other-keys args))))
			     (cons 0 (if (or rest (negative? len))
					 max-arity
					 len))))
			  ((symbol? args)
			   (cons 0 max-arity))
			  (else #f))))

		 ((defmacro)
		  (cond ((list? (caddr form))
			 (let ((len (length (caddr form))))
			   (if (negative? len)
			       (cons (abs len) max-arity)
			       (cons len len))))
			((symbol? (caddr form))
			 (cons 0 max-arity))
			(else #f)))

		 (else #f))))))

    (define (report-shadower caller head vtype v expr env)
      (when (symbol? v)
	(if (var-member v env)
	    (lint-format "~A ~A ~A in ~S shadows an earlier declaration" caller head vtype v expr)
	    (if (defined? v (rootlet))
		(lint-format "~A ~A ~A shadows built-in ~A" caller head vtype v v)))))

    (define (make-fvar name ftype arglist initial-value env)
      (when (and *report-recursion->iteration* (not (keyword? name)))
	(recursion->iteration name ftype arglist initial-value env))
      (improper-arglist->define* name ftype arglist initial-value)

      (when *report-shadowed-variables*
	(for-each (lambda (v)
		    (report-shadower ftype name 'parameter v arglist env))
		  (args->proper-list arglist)))

      (let ((new (let ((old (hash-table-ref other-identifiers name))
		       (allow-keys (and (pair? arglist)
					(memq ftype '(define* define-macro* define-bacro* defmacro*))
					(eq? (last-ref arglist) :allow-other-keys)))
		       (nv (and (pair? initial-value)
				(tree-memq 'values initial-value)
				(count-values (cddr initial-value)))))
		   (let ((hist (if old
				   (begin
				     (hash-table-set! other-identifiers name #f) ; remove name
				     (if initial-value (cons initial-value old) old))
				   (if initial-value (list initial-value) ())))
			 (rf (if old (length old) 0))
			 (ar (form->arity initial-value)))
		     (cons name
			   (inlet 'history hist
				  'ref rf
				  'ftype ftype
				  'scope ()
				  'initial-value initial-value
				  'set 0
				  'refenv ()
				  'env env
				  'sig ()
				  'arglist arglist
				  'side-effect ()
				  'setters ()
				  'arit ar
				  'retcons #f
				  'nvalues nv
				  'allow-other-keys allow-keys))))))
	(reduce-function-tree new env)
	new))

    (define (return-type sym e)
      (let ((sig (arg-signature sym e)))
	(and (pair? sig)
	     (or (eq? (car sig) 'values) ; turn it into #t for now
		 (car sig)))))           ; this might be undefined in the current context (eg oscil? outside clm)

    (define any-macro?
      (let ((macros (let ((h (make-hash-table)))
		      (for-each
		       (lambda (m)
			 (set! (h m) #t))
		       '(call-with-values let-values define-values let*-values cond-expand require quasiquote
			 multiple-value-bind reader-cond match while))
		      h)))
	(lambda (f env)
	  (or (hash-table-ref macros f)
	      (let ((fd (var-member f env)))
		(and fd
		     (memq (var-ftype fd) '(define-macro define-macro* define-expansion
					    define-bacro define-bacro* defmacro defmacro* define-syntax))))))))

    (define (any-procedure? f v)
      (or (hash-table-ref built-in-functions f)
	  (and v (memq (var-ftype v) '(define define* lambda lambda*))))) ; this order seems a bit faster

    (define ->simple-type
      (let ((markers (list (cons :call/exit 'continuation?)
			   (cons :call/cc 'continuation?)
			   (cons :dilambda 'dilambda?)
			   (cons :lambda 'procedure?))))
	(lambda (c)
	  (case (type-of c)
	    ((symbol?)
	     (or (not (keyword? c))
		 (cond ((assq c markers) => cdr)
		       (else 'keyword?))))
	    ((procedure?)
	     (if (dilambda? c) 'dilambda? 'procedure?))
	    (else)))))

    (define (define->type c)
      (and (pair? c)
	   (case (car c)
	     ((define)
	      (or (not (pair? (cdr c)))
		  (if (pair? (cadr c))
		      'procedure?
		      (and (pair? (cddr c))
			   (->lint-type (caddr c))))))
	     ((define* lambda lambda* case-lambda) 'procedure?)
	     ((dilambda) 'dilambda?)
	     ((define-macro define-macro* define-bacro define-bacro* defmacro defmacro* define-expansion) 'macro?)
	     ((:call/cc :call/exit) 'continuation?)
	     (else #t))))

    (define (->lint-type c)
      (cond ((not (pair? c))	        (->simple-type c))
	    ((procedure? (car c))       (return-type (car c) ()))     ; (#_abs ...)
	    ((not (symbol? (car c)))    (or (sequence? (car c)) 'pair?))
	    ((not (eq? (car c) 'quote)) (or (return-type (car c) ()) (define->type c)))
	    ((not (pair? (cdr c)))      (->simple-type c)) ; ??
	    ((symbol? (cadr c))         'symbol?)
	    (else                       (->simple-type (cadr c)))))   ; don't look for return type!

    (define (compatible? type1 type2) ; we want type1, we have type2 -- is type2 ok?
      (or (eq? type1 type2)
	  (not (and (symbol? type1)
		    (symbol? type2)
		    (hash-table-ref booleans type1)
		    (hash-table-ref booleans type2)))
	  (case type1
	    ((number? complex?)  (memq type2 '(float? real? rational? integer? number? complex? exact? inexact? zero? negative? positive? even? odd? infinite? nan? byte?)))
	    ((real?)             (memq type2 '(float? rational? integer? complex? number? exact? inexact? zero? negative? positive? even? odd? infinite? nan? byte?)))
	    ((zero?)             (memq type2 '(float? real? rational? integer? number? complex? exact? inexact? even? byte?)))
	    ((negative? positive?) (memq type2 '(float? real? rational? integer? complex? number? exact? inexact? even? odd? infinite? nan?)))
	    ((float?)            (memq type2 '(real? complex? number? inexact? zero? negative? positive? infinite? nan?)))
	    ((rational?)         (memq type2 '(integer? real? complex? number? exact? zero? negative? positive? even? odd? byte?)))
	    ((integer?)          (memq type2 '(real? rational? complex? number? exact? even? odd? zero? negative? positive? byte?)))
	    ((odd? even?)        (memq type2 '(real? rational? complex? number? exact? integer? zero? negative? positive? byte?)))
	    ((exact?)            (memq type2 '(real? rational? complex? number? integer? zero? negative? positive? odd? even? byte?)))
	    ((inexact?)          (memq type2 '(real? number? complex? float? zero? negative? positive? infinite? nan?)))
	    ((infinite? nan?)    (memq type2 '(real? number? complex? positive? negative? inexact? float?)))
	    ((vector?)           (memq type2 '(float-vector? int-vector? byte-vector? sequence? subvector?)))
	    ((float-vector? int-vector? byte-vector? subvector?) (memq type2 '(vector? sequence? subvector?)))
	    ((sequence?)         (memq type2 '(list? pair? null? proper-list? vector? float-vector? int-vector? byte-vector? tree-cyclic? openlet? subvector?
					       string? let? hash-table? iterator? procedure? directory? file-exists?))) ; procedure? for extended iterator
	    ((symbol?)           (memq type2 '(gensym? keyword? defined? provided?)))
	    ((string?)           (memq type2 '(sequence? directory? file-exists?)))
	    ((not)               (eq? type2 'boolean?))
	    ((output-port? boolean?) (eq? type2 'not))
	    ((keyword? gensym?)  (memq type2 '(symbol? defined? provided?)))
	    ((defined?)          (memq type2 '(symbol? provided? keyword?)))
	    ((list?)             (memq type2 '(null? pair? proper-list? sequence? tree-cyclic?)))
	    ((proper-list?)      (memq type2 '(null? pair? list? sequence? tree-cyclic?)))
	    ((pair?)             (memq type2 '(list? proper-list? sequence? tree-cyclic?)))
	    ((null?)             (memq type2 '(list? proper-list? sequence?)))
	    ((dilambda?)         (memq type2 '(procedure? macro? iterator?)))
	    ((procedure?)        (memq type2 '(dilambda? iterator? macro? sequence? continuation?)))
	    ((macro?)            (memq type2 '(dilambda? iterator? procedure?)))
	    ((continuation?)     (eq? type2 'procedure?))
	    ((iterator?)         (memq type2 '(dilambda? procedure? sequence?))); iterator-at-end?)))
	    ((let?)              (memq type2 '(defined? sequence? openlet?)))
	    ((openlet?)          (memq type2 '(let? c-pointer? macro? procedure? sequence? defined? undefined?)))
	    ((hash-table?)       (eq? type2 'sequence?))
	    ((char? char-whitespace? char-numeric? char-alphabetic? char-upper-case? char-lower-case?)
	     (memq type2 '(char? char-whitespace? char-numeric? char-alphabetic? char-upper-case? char-lower-case?)))
	    ((tree-cyclic?)      (memq type2 '(list? pair? sequence?)))
	    ((provided?)         (memq type2 '(defined? symbol?)))
	    ((directory?)        (memq type2 '(string? sequence? file-exists?)))
	    ((file-exists?)      (memq type2 '(string? sequence? directory?)))
	    ;;((iterator-at-end?)  (memq type2 '(iterator? sequence?)))
	    (else #f))))

    (define (any-compatible? type1 type2)
      ;; type1 and type2 can be either a list of types or a type
      (if (symbol? type1)
	  (if (symbol? type2)
	      (compatible? type1 type2)
	      (and (pair? type2)
		   (or (compatible? type1 (car type2))
		       (any-compatible? type1 (cdr type2)))))
	  (and (pair? type1)
	       (or (compatible? (car type1) type2)
		   (any-compatible? (cdr type1) type2)))))

    (define (subsumes? type1 type2)
      (or (eq? type1 type2)
	  (case type1
	    ((integer?)         (memq type2 '(even? odd?)))
	    ((rational?)        (memq type2 '(integer? byte? exact? odd? even?)))
	    ((exact?)           (memq type2 '(integer? byte? rational?)))
	    ((real?)            (memq type2 '(integer? byte? rational? float? negative? positive? zero? odd? even?)))
	    ((complex? number?) (memq type2 '(integer? byte? rational? float? real? complex? number? negative? positive? zero?
					      even? odd? exact? inexact? nan? infinite?)))
	    ((list?)            (memq type2 '(pair? null? proper-list?)))
	    ((proper-list?)     (eq? type2 'null?))
	    ((vector?)          (memq type2 '(float-vector? int-vector? byte-vector?)))
	    ((symbol?)          (memq type2 '(keyword? gensym?))) ; defined? provided? ??
	    ((sequence?)        (memq type2 '(list? pair? null? proper-list? vector?  subvector? float-vector? int-vector? byte-vector?
					      string? let? hash-table? directory? file-exists?)))
	    ((char?)            (memq type2 '(char-whitespace? char-numeric? char-alphabetic? char-upper-case? char-lower-case?)))
	    ((iterator)         (eq? type2 'iterator-at-end?))
	    (else #f))))

    (define (never-false expr)
      (or (eq? expr #t)
	  (let ((type (if (pair? expr)
			  (return-type (car expr) ())
			  (->lint-type expr))))
	    (or (and (symbol? type)
		     (not (symbol? expr))
		     (not (memq type '(not boolean? values))))
		(not (or (not (pair? type))
			 (memq #t type)
			 (memq 'boolean? type)
			 (memq 'not type)
			 (memq 'values type)))))))

    (define (never-true expr)
      (or (not expr)
	  (and (len>1? expr)
	       (eq? (car expr) 'not)
	       (never-false (cadr expr)))))

    (define (prettify-checker-unq op)
      (if (pair? op)
	  (string-append (prettify-checker-unq (car op)) " or " (prettify-checker-unq (cadr op)))
	  (case op
	    ((rational?) "rational")
	    ((real?)     "real")
	    ((complex?)  "complex")
	    ((null?)     "null")
	    ((length)    "a sequence")
	    ((unspecified?) "untyped")
	    ((undefined?) "not defined")
	    ((not)        "#f")
	    (else
	     (let ((op-name (symbol->string op)))
	       (string-append (if (memv (op-name 0) '(#\a #\e #\i #\o #\u)) "an " "a ")
			      (substring op-name 0 (- (length op-name) 1))))))))

    (define (prettify-checker op)
      (if (pair? op)
	  (string-append (prettify-checker-unq (car op)) " or " (prettify-checker (cadr op)))
	  (case op
	    ((rational?) "rational?")
	    ((real?)     "real?")
	    ((complex?)  "complex?")
	    ((null?)     "null?")
	    ((unspecified?) "untyped")
	    ((undefined?) "not defined")
	    ((not)        "#f")
	    (else
	     (let ((op-name (symbol->string op)))
	       (string-append (if (memv (op-name 0) '(#\a #\e #\i #\o #\u)) "an " "a ") op-name))))))

    (define (side-effect-with-vars? form env vars)
      ;; could evaluation of form have any side effects (like IO etc)
      ;;   vars is not null only in get-side-effect which is checking a function (in var-side-effect)
      (if (or (not (proper-list? form))                   ; we don't want dotted lists or () here
	      (null? form))

	  (and (symbol? form)
	       (or (eq? form '=>)                         ; (cond ((x => y))...) -- someday check y...
		   (let ((e (var-member form env)))
		     (if e
			 (and (symbol? (var-ftype e))
			      (var-side-effect e))
			 (and (not (hash-table-ref no-side-effect-functions form))
			      (procedure? (symbol->value form *e*))))))) ; i.e. function passed as argument

	  ;; can't optimize ((...)...) because the car might eval to a function
	  (or (and (not (hash-table-ref no-side-effect-functions (car form))) ; includes quote, let, etc
		   ;; if it's not in the no-side-effect table and ...

		   (let ((e (var-member (car form) env)))
		     (or (not e)
			 (not (symbol? (var-ftype e)))
			 (var-side-effect e)))

		   (or (not (eq? (car form) 'format))              ; (format #f ...)
		       (not (pair? (cdr form)))                    ; (format)!
		       (cadr form))

		   (or (null? vars)
		       (not (memq (car form) '(set! define define* define-macro define-macro* define-bacro define-bacro*)))))

	      ;; it's not the common (format #f ...) special case, then...(goto case below)
	      ;; else return #t: side-effects are possible -- this is too hard to read
	      (case (car form)

		((define-constant define-expansion) #t)

		((define define* define-macro define-macro* define-bacro define-bacro*
		  quote)
		 #f) ;; was (null? vars) which (leaving aside quote) is never the case (see above)

		((set!)
		 (or (not (pair? (cdr form)))
		     (not (symbol? (cadr form)))
		     (memq (cadr form) vars)))

		((case)
		 (or (not (pair? (cdr form)))
		     (side-effect-with-vars? (cadr form) env vars) ; the selector
		     (let case-effect? ((f (cddr form)))
		       (and (pair? f)
			    (or (not (pair? (car f)))
				(any-side-effect? (cdar f) env vars)
				(case-effect? (cdr f)))))))

		((cond)
		 (or (not (pair? (cdr form)))
		     (not (pair? (cadr form)))
		     (let cond-effect? ((f (cdr form))
					(e env))
		       (and (pair? f)
			    (or (and (pair? (car f))
				     (any-side-effect? (car f) e vars))
				(cond-effect? (cdr f) e))))))

		((let let* letrec letrec*)
		 ;; here if the var value involves a member of vars, we have to add it to vars
		 (or (< (length form) 3)
		     (let ((syms (cadr form))
			   (body (cddr form))
			   (e env))
		       (when (symbol? (cadr form))
			 (set! syms (caddr form))
			 (let ((fv (make-lint-var (cadr form) #f 'let)))
			   (varlet (cdr fv) 'side-effect #f)
			   (varlet (cdr fv) 'ftype 'let)
			   (set! e (cons fv env)))
			 (set! body (cdddr form)))
		       (if (and (pair? vars)
				(pair? syms))
			   (for-each (lambda (sym)
				       (when (and (len>1? sym)
						  (tree-set-memq vars (cdr sym)))
					 (set! vars (cons (car sym) vars))))
				     syms))
		       (or (let let-effect? ((f syms))
			     (and (pair? f)
				  (or (not (pair? (car f)))
				      (not (pair? (cdar f))) ; an error, reported elsewhere: (let ((x)) x)
				      (side-effect-with-vars? (cadar f) e vars)
				      (let-effect? (cdr f)))))
			   (any-side-effect? body e vars)))))

		((do)
		 (or (< (length form) 3)
		     (not (list? (cadr form)))
		     (not (list? (caddr form)))
		     (let do-effect? ((f (cadr form))	(e env))
		       (and (pair? f)
			    (or (not (pair? (car f)))
				(not (pair? (cdar f)))
				(side-effect-with-vars? (cadar f) e vars)
				(and (pair? (cddar f))
				     (side-effect-with-vars? (caddar f) e vars))
				(do-effect? (cdr f) e))))
		     (any-side-effect? (caddr form) env vars)
		     (any-side-effect? (cdddr form) env vars)))

		;; ((lambda lambda*) (lint-any? (lambda (ff) (side-effect-with-vars? ff env vars)) (cddr form))) ; this is trickier than it looks

		(else
		 (or (lint-any? (lambda (f)                              ; any subform has a side-effect
				  (and (not (null? f))
				       (side-effect-with-vars? f env vars)))
			   (cdr form))
		     (let ((sig (signature (symbol->value (car form))))) ; sig has func arg and it is not known safe
		       (and (pair? sig)
			    (memq 'procedure? (cdr sig))
			    (call-with-exit
			     (lambda (return)
			       (for-each
				(lambda (sg arg)
				  (when (and (eq? sg 'procedure?)
					     (not (and (symbol? arg)
						       (hash-table-ref no-side-effect-functions arg))))
				    (return #t)))
				(cdr sig) (cdr form))
			       #f))))))))))

    (define (side-effect? form env)
      (side-effect-with-vars? form env ()))

    (define (just-constants? form env)
      ;; can we probably evaluate form given just built-in stuff?
      ;;   watch out here -- this is used later by 'if, so (defined 'hiho) should not be evalled to #f!
      (if (not (pair? form))
	  (constant? form)
	  (and (symbol? (car form))
	       (hash-table-ref no-side-effect-functions (car form))
	       (hash-table-ref built-in-functions (car form)) ; and not hook-functions
	       (not (var-member (car form) env))              ; e.g. exp declared locally as a list
	       (lint-every? (lambda (p) (just-constants? p env)) (cdr form)))))

    (define (repeated-member? lst env)
      (and (pair? lst)
	   (or (and (pair? (cdr lst))
		    (member (car lst) (cdr lst))
		    (not (and (pair? (car lst))
			      (side-effect? (car lst) env))))
	       (repeated-member? (cdr lst) env))))

    (define (update-scope v caller env)
      (unless (or (memq caller (var-scope v))
		  (assq caller (var-scope v)))
	(let ((cv (var-member caller env)))
	  (set! (var-scope v)
		(cons (if (and cv
			       (memq (var-ftype cv) '(define lambda define* lambda*))) ; named-let does not define ftype
			  caller
			  (cons caller env))
		      (var-scope v))))))

    (define check-for-bad-variable-name
      (let ((bad-var-names ())
	    (sname #f) (slen #f) (s0 #f))
	(define (initialize-bad-var-names vars)
	  (set! bad-var-names ())
	  (for-each (lambda (n)
		      (let ((name (symbol->string n)))
			(cond ((assq (string-ref name 0) bad-var-names) =>
			       (lambda (cur)
				 (set! (cdr cur) (cons (list n name (length name)) (cdr cur)))))
			      (else
			       (set! bad-var-names (cons (list (string-ref name 0) (list n name (length name))) bad-var-names))))))
		    vars))
	(initialize-bad-var-names *report-bad-variable-names*)

	(set! (setter '*report-bad-variable-names*) ; update these local variables if the global variable changes
	      (lambda (sym val)
		(when (just-symbols? val)
		  (initialize-bad-var-names val))
		val))

	(lambda (caller vname)
	  (set! sname (symbol->string vname)) ;(if (keyword? vname) (keyword->symbol vname) vname)))
	  (set! slen (length sname))
	  (set! s0 (string-ref sname 0))

	  (cond ((assq s0 bad-var-names) =>
		 (lambda (baddies)
		   (if (or (assq vname (cdr baddies))
			   (lint-any? (lambda (b)
					(and (eqv? (string-position (cadr b) sname) 0)
					     (string->number (substring sname (caddr b)))))
				      (cdr baddies)))
		       (lint-format "surely there's a better name for this variable than ~A" caller vname)))))

	  (if (> slen *report-ridiculous-variable-names*)
	      (lint-format "the name ~A (~A chars!) is unreadable" caller vname slen)

	      (case s0
		((#\i)
		 (if (eqv? (string-position "is-" sname) 0)                   ; is-x? -> x?
		     (if (char=? (sname (- slen 1)) #\?)
			 (lint-format "'~A is redundant: perhaps use '~A" caller vname (string->symbol (substring sname 3)))
			 (lint-format "perhaps use '~A?, not '~A" caller (string->symbol (substring sname 3)) vname))))

		((#\c)
		 (if (and (> slen 8)
			  (or (string=? "compute" (substring sname 0 7))      ; compute-* is as bad as get-*
			      (string=? "calculate" (substring sname 0 9))))  ;   perhaps one exception: computed-goto*
		     (lint-format "surely there's a better name for this variable than ~A" caller vname)))

		((#\@)
		 (lint-format "the name ~A will be problematic in quasiquote" caller vname))
		;; a check for other malformed numbers got no hits

		((#\+)
		 (if (memq vname '(+i +2i +0.i +1.0i +2.0i +2.i +3.141592653589793i))
		     (lint-format "~A is not a number" caller vname)))

		((#\-)
		 (if (memq vname '(-i -0.i -1.0i -2.0i -2i -3.141592653589793i -8.i -8i))
		     (lint-format "~A is not a number" caller vname)))

		((#\|)
		 (if (and *report-||-rewrites*
			  (> slen 2)
			  (eqv? (char-position #\| (substring sname 1)) (- slen 2))) ; starting at 1, so ends -2
		     (lint-format "| is not a special character, so ~A is not the symbol ~A" caller
				  vname (substring sname 1 (- slen 1))))))))))

    (denote (set-ref name caller form env)
      ;; if name is in env, set its "I've been referenced" flag
      (let ((data (var-member name env)))
	(if data
	    (begin
	      (set! (var-ref data) (+ (var-ref data) 1))
	      (update-scope data caller env)
	      (when (and form (not (memq form (var-history data))))
		(set! (var-history data) (cons form (var-history data)))
		(set! (var-refenv data) env)))
	    (if (and (symbol? name)
		     (not (defined? name (rootlet))))
		(let ((old (hash-table-ref other-identifiers name)))
		  (check-for-bad-variable-name caller name)
		  (hash-table-set! other-identifiers name (cons form (or old ())))))))
      env)


    (denote (set-set name caller form env)
      (let ((data (var-member name env)))
	(when data
	  (set! (var-set data) (+ (var-set data) 1))
	  (update-scope data caller env)
	  (if (not (memq caller (var-setters data)))
	      (set! (var-setters data) (cons caller (var-setters data))))
	  (unless (memq form (var-history data))
	    (set! (var-history data) (cons form (var-history data)))
	    (set! (var-refenv data) env))
	  (set! (var-signature data) #f)
	  (set! (var-ftype data) #f))))


    (denote (proper-list lst)
      ;; return lst as a proper list
      (if (not (pair? lst))
	  lst
	  (cons (car lst)
		(if (pair? (cdr lst))
		    (proper-list (cdr lst))
		    (case (cdr lst) ((())) (else => list))))))

    (define (keywords lst count)
      (if (pair? lst)
	  (keywords (cdr lst) (if (keyword? (car lst)) (+ count 1) count))
	  count))

    (define (eqv-selector clause)
      (if (not (pair? clause))
	  (memq clause '(else #t))
	  (case (car clause)
	    ((memq memv member)
	     (and (= (length clause) 3)
		  (cadr clause)))
	    ((eq? eqv? = equal? char=? char-ci=? string=? string-ci=?)
	     (and (= (length clause) 3)
		  ((if (code-constant? (cadr clause)) caddr cadr) clause)))
	    ((or)
	     (and (pair? (cdr clause))
		  (eqv-selector (cadr clause))))
	    ((not null? eof-object? zero? boolean?)
	     (and (pair? (cdr clause))
		  (cadr clause)))
 	    (else #f))))

    (define (->eqf x)
      (case x
	((char?) '(eqv? char=?))
	((integer? rational? real? number? complex? float? infinite? nan?) '(eqv? =))
	((symbol? keyword? boolean? not null? procedure? syntax? macro? unspecified?) '(eq? eq?))
	((string?) '(equal? string=?))
	((pair? vector? float-vector? int-vector? byte-vector? subvector? hash-table?) '(equal? equal?))
	((eof-object?) '(eq? eof-object?))
	(else
	 (if (and (len>1? x)
		  (or (and (or (memq 'boolean? x)
			       (memq 'not x))
			   (or (memq 'real? x) (memq 'number? x) (memq 'integer? x)))
		      (and (memq 'eof-object? x)
			   (or (memq 'char? x) (memq 'integer? x)))))
	     '(eqv? eqv?)
	     '(#t #t)))))

    (define (eqf selector env)
      (cond ((symbol? selector)
	     (if (and (or (hash-table-ref built-in-functions selector)
			  (hash-table-ref syntaces selector))
		      (not (var-member selector env)))
		 '(eq? eq?)
		 '(#t #t)))

	    ((not (pair? selector))
	     (->eqf (->lint-type selector)))

	    ((and (eq? (car selector) 'quote)
		  (pair? (cdr selector)))
	     (cond ((or (symbol? (cadr selector))
			(memq (cadr selector) '(#f #t #<unspecified> #<undefined> #<eof> ())))
		    '(eq? eq?))
		   ((char? (cadr selector))   '(eqv? char=?))
		   ((string? (cadr selector)) '(equal? string=?))
		   ((number? (cadr selector)) '(eqv? =))
		   (else                      '(equal? equal?))))

	    ((and (eq? (car selector) 'list)
		  (null? (cdr selector)))
	     '(eq? eq?))

	    ((symbol? (car selector))
	     (let ((sig (arg-signature (car selector) env)))
	       (if (pair? sig)
		   (->eqf (car sig))
		   '(#t #t))))

	    (else '(#t #t))))

    (define (unquoted x)
      (if (and (len=2? x)
	       (eq? (car x) 'quote))
	  (cadr x)
	  x))

    (define (distribute-quote x)
      (map (lambda (item)
	     (if (or (symbol? item)
		     (pair? item))
		 (list 'quote item)
		 item))
	   x))

    (define (focus-str str focus)
      (let ((len (length str)))
	(if (< len 40)
	    str
	    (let ((pos (string-position focus str))
		  (focus-len (length focus)))
	      (if (not pos)
		  str
		  (if (<= pos 20)
		      (string-append (substring str 0 (min 60 (- len 1) (+ focus-len pos 20))) " ...")
		      (string-append "... " (substring str (- pos 20) (min (- len 1) (+ focus-len pos 20))) " ...")))))))

    (define check-star-parameters
      (let ((pi-arg (lambda (a b) (or (eq? b 'pi) (and (pair? b) (eq? (car b) 'pi))))))
	(lambda (f args env)
	  (if (lint-any? (lambda (k) (memq k '(:key :optional))) args)
	      (let ((kw (if (memq :key args) :key :optional)))
		(out-format outport "~NC~A: ~A is no longer accepted: ~A~%" lint-left-margin #\space f kw
			(focus-str (object->string args) (symbol->string kw)))))

	  (if (member 'pi args pi-arg)
	      (out-format outport "~NC~A: parameter can't be a constant: ~A~%" lint-left-margin #\space f
		      (focus-str (object->string args) "pi")))

	  (let ((r (memq :rest args)))
	    (when (pair? r)
	      (if (not (pair? (cdr r)))
		  (out-format outport "~NC~A: :rest parameter needs a name: ~A~%" lint-left-margin #\space f args)
		  (if (pair? (cadr r))
		      (out-format outport "~NC~A: :rest parameter can't specify a default value: ~A~%" lint-left-margin #\space f args)))))

	  (let ((a (memq :allow-other-keys args)))
	    (when (pair? a)
	      (if (pair? (cdr a))
		  (out-format outport "~NC~A: :allow-other-keys should be at the end of the parameter list: ~A~%" lint-left-margin #\space f
			  (focus-str (object->string args) ":allow-other-keys")))
	      (if (len=1? args)
		  (out-format outport "~NC~A: :allow-other-keys can't be the only parameter: ~A~%" lint-left-margin #\space f args))))

	  (for-each (lambda (p)
		      (if (len>1? p)
			  (lint-walk f (cadr p) env)))
		    args))))

    (define (checked-eval form)
      (and (proper-list? form) ;(not (infinite? (length form))) but when would a dotted list work?
	   (catch #t
	     (lambda ()
	       (eval (copy form) (rootlet)))  ; :readable))) -- use rootlet else "form" is defined (in curlet)
	     (lambda args
	       :checked-eval-error))))

    (define (eval/error caller form)
      (catch #t
	(lambda ()
	  (eval form))
	(lambda args
	  (lint-format "~A: ~A" caller (car args) (apply format #f (cadr args)))
	  :error)))

    (define (return-type-ok? type ret)
      (or (eq? type ret)
	  (and (pair? ret)
	       (memq type ret))))


    (define last-and-incomplete-arg2 #f)

    (define (and-incomplete form head arg1 arg2 env)           ; head: 'and | 'or (not ...) | 'if | 'if2 -- symbol arg1 in any case
      (unless (or (memq (car arg2) '(and or not list cons vector)) ; these don't tell us anything about arg1's type
		  (eq? arg2 last-and-incomplete-arg2))
	(let ((v (var-member arg1 env)))                       ; try to avoid the member->cdr trope
	  (unless (and v
		       (pair? (var-history v))
		       (member #f (var-history v)
			       (lambda (a b)
				 (and (pair? b)
				      (memq (car b) '(char-position string-position format string->number assoc assq assv memq memv member))))))
	    (let* ((pos (do ((i 0 (+ i 1))                     ; get arg number of arg1 in arg2
			     (p arg2 (cdr p))) ; 0th=car -> (and x (x))
			    ((or (null? p)
				 (eq? (car p) arg1))
			     i)))
		   (arg-type (let ((sig (and (positive? pos)   ; signature for arg2
					     (arg-signature (car arg2) env))))
			       (if (zero? pos)                 ; its type indication for arg1's position
				   'procedure? ; or sequence? -- how to distinguish? use 'applicable?
				   (and (pair? sig)
					(< pos (length sig))
					(list-ref sig pos))))))
	      (set! last-and-incomplete-arg2 arg2)             ; ignore unwanted repetitions due to recursive simplifications
	      (if (and (symbol? arg-type)
		       (not (eq? arg-type 'unused-parameter?)))
		  (let ((ln (and (< 0 line-number 100000) line-number))
			(comment (if (and (eq? arg-type 'procedure?)
					  (= pos 0)
					  (pair? (cdr arg2)))
				     " ; or maybe sequence? " ""))
			(old-arg (case head
				   ((and if cond when) arg1)
				   ((or if2)           (list 'not arg1))
				   (else 'error)))
			(new-arg (case head
				   ((and if cond when) (list arg-type arg1))
				   ((or if2)           (list 'not (list arg-type arg1)))
				   (else 'error))))
		    (out-format outport "~NCin ~A~A,~%~NCperhaps change ~S to ~S~A~%"
			    lint-left-margin #\space
			    (truncated-list->string form)
			    (if (and (integer? ln) (> ln 0)) (format #f " (line ~D)" ln) "")
			    (+ lint-left-margin 4) #\space
			    old-arg new-arg comment))))))))

    (define (and-redundant? arg1 arg2)
      ;(format *stderr* "ar: ~A ~A~%" arg1 arg2)
      (let ((type1 (car arg1))
	    (type2 (car arg2)))
	(and (symbol? type1)
	     (symbol? type2)
	     (hash-table-ref booleans type1)
	     (or (hash-table-ref booleans type2)     ; return #f if not (obviously) redundant, else return which of the two to keep
		 (eq? type2 'not)
		 (and (memq type2 '(= char=? string=? eq?))
		      (pair? (cddr arg2))))
	     (if (eq? type1 type2)
		 type1
		 (case type1
		   ((number? complex?)
		    (case type2
		      ((float? real? rational? integer?) type2)
		      ((number? complex?) type1)
		      ((=) (let ((x (and (len=3? arg2)
					 ((if (number? (caddr arg2)) caddr cadr) arg2))))
			     (and (number? x)
				  (if (= x (floor x)) 'memv 'eqv?))))
		      (else #f)))

		   ((real?)
		    (case type2
		      ((float? rational? integer?) type2)
		      ((number? complex?) type1)
		      ((=) (let ((x ((if (real? (caddr arg2)) caddr cadr) arg2)))
			     (and (real? x)
				  (if (= x (floor x)) 'memv 'eqv?))))
		      (else #f)))

		   ((float?)
		    (and (memq type2 '(real? complex? number? inexact?)) type1))

		   ((rational?)
		    (case type2
		      ((integer?) type2)
		      ((real? complex? number? exact?) type1)
		      ((=)
		       (and (or (rational? (caddr arg2))
				(rational? (cadr arg2)))
			    'eqv?))
		      (else #f)))

		   ((integer?)
		    (case type2
		      ((real? rational? complex? number? exact?) type1)
		      ((=)
		       (and (pair? (cddr arg2))
			    (or (integer? (caddr arg2))
				(integer? (cadr arg2)))
			    'eqv?))
		      (else #f)))

		   ((exact?)
		    (and (memq type2 '(rational? integer?)) type2))

		   ((even? odd?)
		    (and (memq type2 '(integer? rational? real? complex? number?)) type1)) ; not zero? -> 0.0

		   ((zero?)
		    (and (memq type2 '(complex? number? real?)) type1))

		   ((negative? positive?)
		    (and (eq? type2 'real?) type1))

		   ((inexact?)
		    (and (eq? type2 'float?) type2))

		   ((infinite? nan?)
		    (and (memq type2 '(number? complex? inexact?)) type1))

		   ((vector?)
		    (and (memq type2 '(float-vector? int-vector? byte-vector?)) type2))

		   ((float-vector? int-vector? byte-vector?)
		    (and (eq? type2 'vector?) type1))

		   ((symbol?)
		    (case type2
		      ((keyword? gensym?) type2)
		      ((eq?)
		       (and (or (quoted-symbol? (cadr arg2))
				(quoted-symbol? (caddr arg2)))
			    'eq?))
		      (else #f)))

		   ((keyword?)
		    (case type2
		      ((symbol? constant?) type1)
		      ((eq?)
		       (and (or (keyword? (cadr arg2))
				(keyword? (caddr arg2)))
			    'eq?))
		      (else #f)))

		   ((gensym? defined? provided?)
		    (and (eq? type2 'symbol?) type1))

		   ((boolean?)
		    (and (or (eq? type2 'not)
			     (and (eq? type2 'eq?)
				  (len=2? (cdr arg2))
				  (or (boolean? (cadr arg2))
				      (boolean? (caddr arg2)))))
			 type2))

		   ((list?)
		    (and (memq type2 '(null? pair? proper-list?)) type2))

		   ((null?)
		    (and (memq type2 '(list? proper-list?)) type1))

		   ((pair?)
		    (and (eq? type2 'list?) type1))

		   ((proper-list?)
		    (and (eq? type2 'null?) type2))

		   ((string?)
		    (and (eq? type2 'string=?)
			 (or (eq? (->lint-type (cadr arg2)) 'string?)
			     (eq? (->lint-type (caddr arg2)) 'string?))
			 'equal?))

		   ((char?)
		    (and (eq? type2 'char=?)
			 (or (eq? (->lint-type (cadr arg2)) 'char?)
			     (eq? (->lint-type (caddr arg2)) 'char?))
			 'eqv?))

		   ((char-numeric? char-whitespace? char-alphabetic? char-upper-case? char-lower-case?)
		    (and (eq? type2 'char?) type1))

		   ((directory?)
		    (and (memq type2 '(string? file-exists?)) type1))

		   ((file-exists?)
		    (if (eq? type2 'string?)
			type1
			(and (eq? type2 'directory?)
			     type2)))

		   (else #f))))))


    (define (and-forgetful form head arg1 arg2 env)
      (unless (or (memq (car arg2) '(and or not list cons vector)) ; these don't tell us anything about arg1's type
		  (eq? arg2 last-and-incomplete-arg2))
	(let* ((pos (do ((i 0 (+ i 1))                         ; get arg number of arg1 in arg2
			 (p arg2 (cdr p))) ; 0th=car -> (and x (x))
			((or (null? p)
			     (equal? (car p) (cadr arg1)))
			 (if (null? p) -1 i))))
	       (arg-type (let ((sig (and (positive? pos)       ; signature for arg2
					 (arg-signature (car arg2) env))))
			   (if (zero? pos)                     ; its type indication for arg1's position
			       'procedure?                     ; or sequence? -- how to distinguish? use 'applicable?
			       (and (pair? sig)
				    (< pos (length sig))
				    (list-ref sig pos))))))
	  (when (symbol? arg-type)
	    (let ((new-type (and-redundant? arg1 (cons arg-type (cdr arg1)))))
	      (when (and new-type
			 (not (eq? new-type (car arg1))))
		(let ((old-arg (case head
				 ((and if cond when) arg1)
				 ((or if2)           (list 'not arg1))
				 (else 'error)))
		      (new-arg (case head
				 ((and if cond when) (list new-type (cadr arg1)))
				 ((or if2)           (list 'not (list new-type (cadr arg1))))
				 (else 'error)))
		      (ln (and (< 0 line-number 100000) line-number))
		      (comment (if (and (eq? arg-type 'procedure?)
					(= pos 0)
					(pair? (cdr arg2)))
				   " ; or maybe sequence? " "")))
		  (set! last-and-incomplete-arg2 arg2)
		  (out-format outport "~NCin ~A~A,~%~NCperhaps change ~S to ~S~A~%"
			  lint-left-margin #\space
			  (truncated-list->string form)
			  (if (and (integer? ln) (> ln 0)) (format #f " (line ~D)" ln) "")
			  (+ lint-left-margin 4) #\space
			  old-arg new-arg comment)))))))

      ;; perhaps change pair? -> eq? or ignore it?
      (when (and (pair? (cdr arg2))
		 (not (eq? (car arg1) 'pair?)))
	(let ((a2 (if (eq? (car arg2) 'not)
		      (cadr arg2)
		      arg2)))
	  (when (and (pair? a2)
		     (memq (car a2) '(memq memv member assq assv assoc eq? eqv? equal?))
		     (equal? (cadr arg1) (cadr a2)))
	    (let ((new-e (case (car (->eqf (car arg1)))
			   ((eq?)
			    (case (car a2)
			      ((memq assq eq?))
			      ((memv member) 'memq)
			      ((assv assoc) 'assq)
			      ((eqv? equal?) 'eq?)))
			   ((eqv?)
			    (case (car a2)
			      ((memv assv eqv?))
			      ((memq member) 'memv)
			      ((assq assoc) 'assv)
			      ((eq? equal?) 'eqv?)))
			   ((equal?)
			    (case (car a2)
			      ((member assoc equal?))
			      ((memq memv) 'member)
			      ((assq assv) 'assoc)
			      ((eq? eqv?) 'equal?)))
			   (else (car a2)))))
	      (when (and (not (eq? (car a2) new-e))
			 (symbol? new-e))
		(let ((ln (and (< 0 line-number 100000) line-number)))
		  (out-format outport "~NCin ~A~A,~%~NCperhaps change ~A to ~A~%"
			  lint-left-margin #\space
			  (truncated-list->string form)
			  (if (and (integer? ln) (> ln 0)) (format #f " (line ~D)" ln) "")
			  (+ lint-left-margin 4) #\space
			  (truncated-list->string a2)
			  (list new-e '...)))))))))


    ;; --------------------------------
    (define simplify-boolean

      (let ((relsub
	     (let ((relops '((< <= > number?) (<= < >= number?) (> >= < number?) (>= > <= number?)
			     (char<? char<=? char>? char?) (char<=? char<? char>=? char?)  ; these never happen
			     (char>? char>=? char<? char?) (char>=? char>? char<=? char?)
			     (string<? string<=? string>? string?) (string<=? string<? string>=? string?)
			     (string>? string>=? string<? string?) (string>=? string>? string<=? string?))))
	       (lambda (A B rel-op env)
		 (call-with-exit
		  (lambda (return)
		    (when (and (pair? A)
			       (pair? B)
			       (= (length A) (length B) 3))
		      (let ((Adata (assq (car A) relops))
			    (Bdata (assq (car B) relops)))
			(when (and Adata Bdata)
			  (let ((op1 (car A))
				(op2 (car B))
				(A1 (cadr A))
				(A2 (caddr A))
				(B1 (cadr B))
				(B2 (caddr B)))
			    (let ((x (if (and (not (number? A1))
					      (member A1 B))
					 A1
					 (and (not (number? A2))
					      (member A2 B)
					      A2))))
			      (when x
				(let ((c1 (if (equal? x A1) A2 A1))
				      (c2 (if (equal? x B1) B2 B1))
				      (type (cadddr Adata)))
				  (if (or (side-effect? c1 env)
					  (side-effect? c2 env)
					  (side-effect? x env))
				      (return 'ok))
				  (if (equal? x A2) (set! op1 (caddr Adata)))
				  (if (equal? x B2) (set! op2 (caddr Bdata)))

				  (let ((typer #f)
					(gtes #f)
					(gts #f)
					(eqop #f))
				    (case type
				      ((number?)
				       (set! typer number?)
				       (set! gtes '(>= <=))
				       (set! gts  '(< >))
				       (set! eqop '=))
				      ((char?)
				       (set! typer char?)
				       (set! gtes '(char>=? char<=?))
				       (set! gts  '(char<? char>?))
				       (set! eqop 'char=?))
				      ((string?)
				       (set! typer string?)
				       (set! gtes '(string>=? string<=?))
				       (set! gts  '(string<? string>?))
				       (set! eqop 'string=?)))

				    (case rel-op
				      ((and)
				       (cond ((equal? c1 c2)
					      (return (cond ((eq? op1 op2)
							     (list op1 x c1))

							    ((eq? op2 (cadr (assq op1 relops)))
							     (list (if (memq op2 gtes) op1 op2) x c1))

							    ((and (memq op1 gtes)
								  (memq op2 gtes))
							     (list eqop x c1))

							    (else #f))))

					     ((and (typer c1)
						   (typer c2))
					      (cond ((or (eq? op1 op2)
							 (eq? op2 (cadr (assq op1 relops))))
						     (return (if ((symbol->value op1 (rootlet)) c1 c2)
								 (list op1 x c1)
								 (list op2 x c2))))

						    ((eq? op1 (caddr (assq op2 relops)))
						     (if ((symbol->value op1 (rootlet)) c2 c1)
							 (return (list op1 c2 x c1))
							 (if (memq op1 gts)
							     (return #f))))

						    ((and (eq? op2 (hash-table-ref reversibles (cadr (assq op1 relops))))
							  ((symbol->value op1 (rootlet)) c1 c2))
						     (return #f))))

					     ((eq? op2 (caddr (assq op1 relops)))
					      (return (list op1 c2 x c1)))))

				      ((or)
				       (cond ((equal? c1 c2)
					      (return (cond ((eq? op1 op2)
							     (list op1 x c1))

							    ((eq? op2 (cadr (assq op1 relops)))
							     (list (if (memq op2 gtes) op2 op1) x c1))

							    ((and (memq op1 gts)
								  (memq op2 gts))
							     (list 'not (list eqop x c1)))

							    (else #t))))

					     ((and (typer c1)
						   (typer c2))
					      (cond ((or (eq? op1 op2)
							 (eq? op2 (cadr (assq op1 relops))))
						     (return (if ((symbol->value op1 (rootlet)) c1 c2)
								 (list op2 x c2)
								 (list op1 x c1))))

						    ((eq? op1 (caddr (assq op2 relops)))
						     (if ((symbol->value op1 (rootlet)) c2 c1)
							 (return #t))
						     (return (list 'not (list (cadr (assq op1 relops)) c1 x c2))))

						    ((and (eq? op2 (hash-table-ref reversibles (cadr (assq op1 relops))))
							  ((symbol->value op1 (rootlet)) c2 c1))
						     (return #t))))

					     ((eq? op2 (caddr (assq op1 relops)))
					      (return (list 'not (list (cadr (assq op1 relops)) c1 x c2)))))))))))))))
		    'ok))))))

	(define (contradictory? ands)
	  (let ((vars ()))
	    (call-with-exit
	     (lambda (return)
	       (do ((b ands (cdr b)))
		   ((null? b) #f)
		 (if (and (pair? b)
			  (len>1? (car b)))
		     (let ((func (caar b))
			   (args (cdar b)))
		       (if (and (memq func '(eq? eqv? equal?))
				(len>1? args))
			   (if (and (symbol? (car args))
				    (code-constant? (cadr args)))
			       (set! func (->lint-type (cadr args)))
			       (if (and (symbol? (cadr args))
					(code-constant? (car args)))
				   (set! func (->lint-type (car args))))))

		       (if (symbol? func)
			   (for-each
			    (lambda (arg)
			      (if (symbol? arg)
				  (let ((type (assq arg vars)))
				    (if (not type)
					(set! vars (cons (cons arg func) vars))
					(unless (compatible? (cdr type) func)
					  (return #t))))))
			    args)))))))))

	(define (and-redundants env . args)
	  (do ((locals ())
	       (diffs #f)
	       (p args (cdr p)))
	      ((or (null? p)
		   (not (and (len>1? (car p))
			     (hash-table-ref booleans (caar p)))))
	       (and (null? p)
		    (pair? locals)
		    (or diffs
			(lint-any? (lambda (a) (pair? (cddr a))) locals))
		    (let ((keepers ()))
		      (for-each (lambda (a)
				  (let ((next-a (cdr a)))
				    (cond ((null? (cdr next-a))
					   (set! keepers (cons (car next-a) keepers)))

					  ((null? (cddr next-a))
					   (let ((res (apply and-redundant? (reverse next-a))))
					     (if res
						 (begin
						   (set! keepers (cons ((if (eq? res (caar next-a)) car cadr) next-a) keepers))
						   (set! diffs #t))
						 (set! keepers (cons (car next-a) (cons (cadr next-a) keepers))))))

					  (else
					   (let ((ar (reverse next-a)))
					     (let ((ar1 (car ar))
						   (ar2 (cadr ar))
						   (ar3 (caddr ar)))
					       (let ((res1 (and-redundant? ar1 ar2))     ; if res1 either 1 or 2 is out
						     (res2 (and-redundant? ar2 ar3))     ; if res2 either 2 or 3 is out
						     (res3 (and-redundant? ar1 ar3)))    ; if res3 either 1 or 3 is out
						 ;; only in numbers can 3 actually be reducible
						 (if (not (or res1 res2 res3))
						     (set! keepers (append (cdr a) keepers))
						     (begin
						       (set! diffs #t)
						       (if (and (or (not res1)
								    (eq? res1 (car ar1)))
								(or (not res3)
								    (eq? res3 (car ar1))))
							   (set! keepers (cons ar1 keepers)))
						       (if (and (or (not res1)
								    (eq? res1 (car ar2)))
								(or (not res2)
								    (eq? res2 (car ar2))))
							   (set! keepers (cons ar2 keepers)))
						       (if (and (or (not res2)
								    (eq? res2 (car ar3)))
								(or (not res3)
								    (eq? res3 (car ar3))))
							   (set! keepers (cons ar3 keepers)))
						       (if (pair? (cdddr ar))
							   (set! keepers (append (reverse (cdddr ar)) keepers))))))))))))
				(reverse locals))
		      (and diffs (reverse keepers)))))
	    (let* ((bool (car p))
		   (local (assoc (cadr bool) locals)))
	      (if (pair? local)
		  (if (member bool (cdr local))
		      (set! diffs #t)
		      (set-cdr! local (cons bool (cdr local))))
		  (set! locals (cons (list (cadr bool) bool) locals))))))

	(define (and-not-redundant arg1 arg2)
	  (let ((type1 (car arg1))    ; (? ...)
		(type2 (caadr arg2))) ; (not (? ...))
	    (and (symbol? type1)
		 (symbol? type2)
		 (hash-table-ref booleans type1)
		 (if (eq? type1 type2)     ; (and (?) (not (?))) -> #f
		     'contradictory
		     (and (hash-table-ref booleans type2)
			  (case type1
			    ((pair?)
			     (case type2
			       ((list? sequence?) 'contradictory)
			       ((proper-list? tree-cyclic?) #f)
			       (else arg1)))

			    ((null?)
			     (if (memq type2 '(list? sequence? proper-list?))
				 'contradictory
				 arg1))

			    ((list?)
			     (case type2
			       ((pair?) 'null?)
			       ((null?) 'pair?)
			       ((proper-list? tree-cyclic?) #f)
			       ((sequence?) 'contradictory)
			       (else arg1)))

			    ((proper-list?)
			     (case type2
			       ((list? sequence?) 'contradictory)
			       ((null?) #f)
			       ((pair?) 'null?)
			       (else arg1)))

			    ((symbol?)
			     (and (not (memq type2 '(keyword? gensym? defined? provided?)))
				  arg1))

			    ((keyword? gensym?)
			     (and (eq? type2 'symbol?)
				  'contradictory))

			    ((char=?)
			     (if (eq? type2 'char?)
				 'contradictory
				 (and (or (char? (cadr arg1))
					  (char? (caddr arg1)))
				      (cons 'eqv? (cdr arg1))))) ; arg2 might be (not (eof-object?...))

			    ((real?)
			     (case type2
			       ((rational? exact?)  (cons float? (cdr arg1)))
			       ((inexact?)          (cons 'rational? (cdr arg1)))
			       ((complex? number?)  'contradictory)
			       ((negative? positive? even? odd? zero? integer? float? infinite? nan?) #f)
			       (else arg1)))

			    ((integer?)
			     (case type2
			       ((real? complex? number? rational? exact?) 'contradictory)
			       ((float? inexact? infinite? nan?) arg1)
			       (else #f)))

			    ((rational?)
			     (case type2
			       ((real? complex? number? exact?) 'contradictory)
			       ((float? inexact? infinite? nan?) arg1)
			       (else #f)))

			    ((complex? number?)
			     (and (memq type2 '(complex? number?))
				  'contradictory))

			    ((float?)
			     (case type2
			       ((real? complex? number? inexact?) 'contradictory)
			       ((rational? integer? exact?) arg1)
			       (else #f)))

			    ((exact?)
			     (case type2
			       ((rational?) 'contradictory)
			       ((inexact? infinite? nan?) arg1)
			       (else #f)))

			    ((even? odd?)
			     (case type2
			       ((integer? exact? rational? real? number? complex?) 'contradictory)
			       ((infinite? nan?) arg1)
			       (else #f)))

			    ((zero? negative? positive?)
			     (and (memq type2 '(complex? number? real?))
				  'contradictory))

			    ((infinite? nan?)
			     (case type2
			       ((number? complex? inexact? real?) 'contradictory)
			       ((integer? rational? exact? even? odd?) arg1)
			       (else #f)))

			    ((float-vector? int-vector? byte-vector?)
			     (and (memq type2 '(vector? sequence?))
				  'contradictory))

			    ((string? let? hash-table? openlet? vector?)
			     (and (eq? type2 'sequence?)
				  'contradictory))

			    ((tree-cyclic?)
			     (and (memq type2 '(sequence? list? pair?))
				  'contradictory))

			    ((char-whitespace? char-numeric? char-alphabetic? char-upper-case? char-lower-case?)
			     (and (eq? type2 'char?)
				  'contradictory))

			    ((directory? file-exists?)
			     (and (memq type2 '(string? sequence?))
				  'contradictory))

			    ((continuation? dilambda?)
			     (and (eq? type2 'procedure?)
				  'contradictory))

			    (else
			     ;; none of the rest happen
			     #f)))))))

	(define (or-not-redundant arg1 arg2)
	  (let ((type1 (car arg1))    ; (? ...)
		(type2 (caadr arg2))) ; (not (? ...))
	    (and (symbol? type1)
		 (symbol? type2)
		 (hash-table-ref bools type1)
		 (if (eq? type1 type2)     ; (or (?) (not (?))) -> #t
		     'true
		     (and (hash-table-ref bools type2)
			  (case type1
			    ((null?)
			     (case type2
			       ((list?) ; not proper-list? here
				(list 'not (list 'pair? (cadr arg1))))
			       ((proper-list? sequence? constant? immutable?) #f)
			       (else arg2)))
			    ((list?)
			     (and (memq type2 '(pair? null? proper-list? tree-cyclic?))
				  'true))
			    ((proper-list?)
			     (and (eq? type2 'null?)
				  'true))
			    ((pair?)
			     (and (eq? type2 'tree-cyclic?)
				  'true))
			    ((eof-object?)
			     (and (not (memq type2 '(constant? immutable?)))
				  arg2))
			    ((string?)
			     (and (not (memq type2 '(constant? immutable? sequence?)))
				  arg2))
			    ((sequence?)
			     (and (memq type2 '(string? vector? proper-list? null? let? list? pair? float-vector? int-vector? byte-vector? hash-table? tree-cyclic?))
				  'true))
			    ((symbol?)
			     (and (memq type2 '(keyword? gensym?))
				  'true))
			    ((complex? number?)
			     (and (memq type2 '(float? real? integer? number? complex? rational?))
				  'true))
			    ((real?)
			     (and (memq type2 '(float? integer? rational?))
				  'true))
			    ((rational?)
			     (and (eq? type2 'integer?)
				  'true))
			    ((procedure?)
			     (and (memq type2 '(continuation? dilambda?))
				  'true))
			    ((vector?)
			     (and (memq type2 '(float-vector? int-vector? byte-vector?))
				  'true))
			    ;; next two are not currently in bools
			    ;;((exact?) (and (memq type2 '(integer? rational?)) 'true))
			    ;;((inexact?) (and (eq? type2 'float?) 'true))
			    ((iterator-at-end?)
			     (and (eq? type2 'iterator?)
				  'true))
			    (else #f)))))))

	(define (gather-or-eqf-elements eqfnc sym vals env)
	  (let* ((func (case eqfnc
			 ((eq?) 'memq)
			 ((eqv? char=?) 'memv)
			 (else 'member)))
		 (equals (if (and (eq? func 'member)
				  (not (eq? eqfnc 'equal?)))
			     (list eqfnc)
			     ()))
		 (elements (lint-remove-duplicates (map unquoted vals) env)))
	    (cond ((null? (cdr elements))
		   (cons eqfnc (cons sym elements)))

		  ((and (eq? eqfnc 'char=?)
			(= (length elements) 2)
			(char-ci=? (car elements) (cadr elements)))
		   (list 'char-ci=? sym (car elements)))

		  ((and (eq? eqfnc 'string=?)
			(= (length elements) 2)
			(string-ci=? (car elements) (cadr elements)))
		   (list 'string-ci=? sym (car elements)))

		  ((member elements '((#t #f) (#f #t)))
		   (list 'boolean? sym))		; zero? doesn't happen

		  ((and (eq? eqfnc '=)                  ; (or (= <expr> 20) (= <expr> 21)) -> (<= 20 <expr> 21)
			(pair? sym)
			(eq? (car sym) 'length)
			(= (length elements) 2)         ;    this used to be -> (member '(20 21) =)
			(integer? (car elements))
			(integer? (cadr elements))
			(= (abs (- (car elements) (cadr elements))) 1))
		   `(,(if (< (cadr elements) (car elements)) '<= '>=) ,(cadr elements) ,sym ,(car elements)))

		  (else
		   `(,func ,sym ',(reverse elements) ,@equals)))))

	(define (reversible-member expr lst)
	  (and (pair? lst)
	       (or (member expr lst)
		   (and (eqv? (length expr) 3)
			(let ((rev-op (hash-table-ref reversibles (car expr))))
			  (and rev-op
			       (member (list rev-op (caddr expr) (cadr expr)) lst)))))))

	(define and-rel-ops (let ((h (make-hash-table)))
			      (for-each (lambda (op)
					  (hash-table-set! h op #t))
					'(< = > <= >= char-ci>=? char-ci<? char-ready? char<? char-ci=? char>?
					    char<=? char-ci>? char-ci<=? char>=? char=? string-ci<=? string=?
					    string-ci>=? string<? string-ci<? string-ci=? string-ci>? string>=? string<=? string>?
					    eqv? equal? eq? equivalent?))
			      h))

	(define (booleans-with-not? arg1 arg2)
	  (and (eq? (car arg2) 'not)
	       (len>1? (cadr arg2))
	       (equal? (cdr arg1) (cdadr arg2))))

	(define (collect-nots start end)
	  (if (eq? (cdr start) end) ; just one not
	      (car start)
	      (do ((nf ())
		   (np start (cdr np)))
		  ((eq? np end)
		   (reverse nf))
		(set! nf (cons (cadar np) nf)))))

	;; -------- invert-successive-nots --------
	(define (invert-successive-nots return form len env)
	  (let ((nots 0)
		(ctr 0)
		(max-ctr 0)
		(revers 0)
		(arglen (- len 1))
		(new-head (if (eq? (car form) 'or) 'and 'or)))
	    (for-each (lambda (a)
			(if (len>1? a)
			    (if (eq? (car a) 'not)
				(begin
				  (set! nots (+ nots 1))
				  (set! ctr (+ ctr 1)))
				(begin
				  (set! max-ctr (max max-ctr ctr))
				  (set! ctr 0)
				  (if (hash-table-ref notables (car a))
				      (set! revers (+ revers 1)))))))
		      (cdr form))

	    (cond ((= nots arglen)                               ; every arg is `(not ...)
		   (let ((nf (simplify-boolean (cons new-head (map cadr (cdr form))) () () env)))
		     (return (simplify-boolean (list 'not nf) () () env))))

		  ((and (> nots 1)                           ; if nots+revers=arglen, entire thing can be inverted
			(= (+ nots revers) arglen))          ;   revers>0 because we checked for nots=arglen above
		   (return (simplify-boolean
			    (list 'not (cons new-head (map (lambda (p)
							     (if (eq? (car p) 'not)
								 (cadr p)
								 (cons (hash-table-ref notables (car p)) (cdr p))))
							   (cdr form))))
			    () () env)))

		  ((and (> arglen 2)
			(or (>= nots (/ (* 3 arglen) 4))      ; > 2/3 seems to get some ugly rewrites
			    (and (>= nots (/ (* 2 arglen) 3)) ; was > 1/2 here
				 (> revers 0))))
		   (let ((nf (cons new-head (map (lambda (p)
						   (cond ((not (pair? p))
							  (list 'not p))
							 ((eq? (car p) 'not)
							  (cadr p))
							 ((hash-table-ref notables (car p)) =>
							  (lambda (op)
							    (cons op (cdr p))))
							 (else (list 'not p))))
						 (cdr form)))))
		     (return (simplify-boolean (list 'not nf) () () env))))

		  ((> max-ctr 2)
		   (return (simplify-boolean
			    (cons (car form)
				  (do ((start ())
				       (new-form ())
				       (p (cdr form) (cdr p)))
				      ((null? p)
				       (reverse
					(if (pair? start)
					    (cons (list 'not (cons new-head (collect-nots start p))) new-form)
					    new-form)))
				    (let ((c (car p)))
				      (if (and (pair? c)
					       (eq? (car c) 'not))
					  (if (null? start)
					      (set! start p))
					  (begin
					    (when (pair? start)
					      (set! new-form (cons (list 'not (cons new-head (collect-nots start p))) new-form))
					      (set! start ()))
					    (set! new-form (cons c new-form)))))))
			    () () env))))))

	;; -------- or->memx --------
	(define (or->memx return form env)
	  (do ((sym #f)
	       (eqfnc #f)
	       (vals ())
	       (start #f)
	       (fp (cdr form) (cdr fp)))
	      ((null? fp))
	    (let ((p (car fp)))
	      (if (and (pair? p)
		       (if (not sym)
			   (set! sym (eqv-selector p))
			   (equal? sym (eqv-selector p)))
		       (or (not (memq eqfnc '(char-ci=? string-ci=? =)))
			   (memq (car p) '(char-ci=? string-ci=? =)))

		       ;; = can't share: (equal? 1 1.0) -> #f, so (or (not x) (= x 1)) can't be simplified
		       ;;   except via member+equivalent? but that brings in float-epsilon and NaN differences.
		       ;;   We could add both: 1 1.0 as in cond?
		       ;;
		       ;; another problem: using memx below means the returned value of the expression
		       ;;   may not match the original (#t -> '(...)), so perhaps we should add a one-time
		       ;;   warning about this, and wrap it in (pair? (mem...)) as an example.
		       ;;
		       ;; and another thing... the original might be broken: (eq? x #(1)) where equal?
		       ;;   is more sensible, but that also changes the behavior of the expression:
		       ;;   (memq x '(#(1))) may be #f (or #t!) when (member x '(#(1))) is '(#(1)).
		       ;;
		       ;; I think I'll try to turn out a more-or-less working expression, but warn about it.

		       (case (car p)
			 ((string=? equal?)
			  (set! eqfnc (if (or (not eqfnc)
					      (eq? eqfnc (car p)))
					  (car p)
					  'equal?))
			  (and (= (length p) 3)
			       (if (code-constant? (cadr p))
				   (set! vals (cons (cadr p) vals))
				   (and (code-constant? (caddr p))
					(set! vals (cons (caddr p) vals))))))
			 ((char=?)
			  (if (memq eqfnc '(#f char=?))
			      (set! eqfnc 'char=?)
			      (if (not (eq? eqfnc 'equal?))
				  (set! eqfnc 'eqv?)))
			  (and (= (length p) 3)
			       (if (code-constant? (cadr p))
				   (set! vals (cons (cadr p) vals))
				   (and (code-constant? (caddr p))
					(set! vals (cons (caddr p) vals))))))

			 ((eq? eqv?)
			  (let ((leqf (car (->eqf (->lint-type ((if (code-constant? (cadr p)) cadr caddr) p))))))
			    (cond ((not eqfnc)
				   (set! eqfnc leqf))

				  ((or (memq leqf '(#t equal?))
				       (not (eq? eqfnc leqf)))
				   (set! eqfnc 'equal?))

				  ((memq eqfnc '(#f eq?))
				   (set! eqfnc leqf))))
			  (and (= (length p) 3)
			       (if (code-constant? (cadr p))
				   (set! vals (cons (cadr p) vals))
				   (and (code-constant? (caddr p))
					(set! vals (cons (caddr p) vals))))))

			 ((char-ci=? string-ci=? =)
			  (and (or (not eqfnc)
				   (eq? eqfnc (car p)))
			       (set! eqfnc (car p))
			       (= (length p) 3)
			       (if (code-constant? (cadr p))
				   (set! vals (cons (cadr p) vals))
				   (and (code-constant? (caddr p))
					(set! vals (cons (caddr p) vals))))))

			 ((eof-object?)
			  (set! eqfnc (case eqfnc ((string=? string-ci=? = equal?) 'equal?) ((#f eq?) 'eq?) (else 'eqv?)))
			  (set! vals (cons #<eof> vals)))

			 ((not)
			  (set! eqfnc (case eqfnc ((string=? string-ci=? = equal?) 'equal?) ((#f eq?) 'eq?) (else 'eqv?)))
			  (set! vals (cons #f vals)))

			 ((boolean?)
			  (set! eqfnc (case eqfnc ((string=? string-ci=? = equal?) 'equal?) ((#f eq?) 'eq?) (else 'eqv?)))
			  (set! vals (cons #f (cons #t vals))))

			 ((zero?)
			  (if (memq eqfnc '(#f eq?)) (set! eqfnc 'eqv?))
			  (set! vals (cons 0 (cons 0.0 vals))))

			 ((null?)
			  (set! eqfnc (case eqfnc ((string=? string-ci=? = equal?) 'equal?) ((#f eq?) 'eq?) (else 'eqv?)))
			  (set! vals (cons () vals)))

			 ((memq memv member)
			  (cond ((eq? (car p) 'member)
				 (set! eqfnc 'equal?))

				((eq? (car p) 'memv)
				 (set! eqfnc (if (eq? eqfnc 'string=?) 'equal? 'eqv?)))

				((not eqfnc)
				 (set! eqfnc 'eq?)))
			  (and (= (length p) 3)
			       (quoted-pair? (caddr p))
			       (proper-list? (cadr (caddr p)))
			       (set! vals (append (cadr (caddr p)) vals))))

			 (else #f)))

		  (if (not start)
		      (set! start fp) ; we're in a loop above...
		      (if (and (proper-list? form)
			       (len=1? fp))
			  (return (if (eq? start (cdr form))
				      (gather-or-eqf-elements eqfnc sym vals env)
				      `(or ,@(copy (cdr form) (make-list (do ((g (cdr form) (cdr g))
									      (len 0 (+ len 1)))
									     ((eq? g start)
									      len))))
					   ,(gather-or-eqf-elements eqfnc sym vals env))))))

		  ;; false branch of if above -- not consequent on previous
		  (when (pair? start)
		    (if (eq? fp (cdr start))
			(begin
			  (set! sym #f)
			  (set! eqfnc #f)
			  (set! vals ())
			  (set! start #f))
			;; here we have possible header stuff + more than one match + trailing stuff
			(let ((trailer (if (not (len>1? fp))
					   fp
					   (let ((nfp (simplify-boolean (cons 'or fp) () () env)))
					     ((if (and (pair? nfp)
						       (eq? (car nfp) 'or))
						  cdr list)
					      nfp)))))
			  (return (if (eq? start (cdr form))
				      (cons 'or (cons (gather-or-eqf-elements eqfnc sym vals env) trailer))
				      `(or ,@(copy (cdr form) (make-list (do ((g (cdr form) (cdr g))
									      (len 0 (+ len 1)))
									     ((eq? g start)
									      len))))
					   ,(gather-or-eqf-elements eqfnc sym vals env)
					     ,@trailer))))))))))

	;; -------- or->case --------
	(define (or->case return form)
	  (do ((selector #f)              ; (or (and (eq?...)...)....) -> (case ....)
	       (keys ())
	       (fp (cdr form) (cdr fp)))
	      ((or (null? fp)
		   (let ((p (and (pair? fp)
				 (car fp))))
		     (not (and (len>1? p)
			       (eq? (car p) 'and)
			       (len>1? (cadr p))
			       (or selector
				   (set! selector (cadadr p)))
			       (let ((expr (cadr p))
				     (arg1 (cadadr p)))
				 (case (car expr)
				   ((null?)
				    (and (equal? selector arg1)
					 (not (memq () keys))
					 (set! keys (cons () keys))))
				   ;; we have to make sure no keys are repeated:
				   ;;   (or (and (eq? x 'a) (< y 1)) (and (eq? x 'a) (< y 2)))
				   ;;   this rewrite has become much trickier than expected...

				   ((boolean?)
				    (and (equal? selector arg1)
					 (not (memq #f keys))
					 (not (memq #t keys))
					 (set! keys (cons #f (cons #t keys)))))

				   ((eof-object?)
				    (and (equal? selector arg1)
					 (not (memq #<eof> keys))
					 (set! keys (cons #<eof> keys))))

				   ((zero?)
				    (and (equal? selector arg1)
					 (not (memv 0 keys))
					 (not (memv 0.0 keys))
					 (set! keys (cons 0.0 (cons 0 keys)))))

				   ((memq memv)
				    (and (equal? selector arg1)
					 (pair? (cddr expr))
					 (quoted-pair? (caddr expr))
					 (not (lint-any? (lambda (g)
							   (memv g keys))
							 (cadr (caddr expr))))
					 (set! keys (append (cadr (caddr expr)) keys))))

				   ((eq? eqv? char=?)
				    (and (len=1? (cddr expr))
					 (or (and (equal? selector arg1)
						  (code-constant? (caddr expr))
						  (not (memv (unquoted (caddr expr)) keys))
						  (set! keys (cons (unquoted (caddr expr)) keys)))
					     (and (equal? selector (caddr expr))
						  (code-constant? arg1)
						  (not (memv (unquoted arg1) keys))
						  (set! keys (cons (unquoted arg1) keys))))))

				   ((not)
				    ;; no hits here for last+not eq(etc)+no collision in keys
				    (and (equal? selector arg1)
					 (not (memq #f keys))
					 (set! keys (cons #f keys))))

				   (else #f)))))))
	       (if (null? fp)
		   (return `(case ,selector
			      ,@(map (lambda (p)
				       (let ((result (if (null? (cdddr p))
							 (caddr p)
							 (cons 'and (cddr p))))
					     (key (let ((expr (cadr p)))
						    (case (car expr)
						      ((eq? eqv? char=?)
						       (let ((repeats (equal? selector (cadr expr))))
							 (list (unquoted ((if repeats caddr cadr) expr)))))
						      ((memq memv)   (unquoted (caddr expr)))
						      ((null?)       (list ()))
						      ((eof-object?) (list #<eof>))
						      ((zero?)       (list 0 0.0))
						      ((not)         (list #f))
						      ((boolean?)    (list #t #f))))))
					 (list key result)))
				     (cdr form))
			      (else #f)))))))


	(define (classify e env)
	  (if (not (just-constants? e env))
	      e
	      (catch #t
		(lambda ()
		  (let ((val (eval e)))
		    (if (boolean? val)
			val
			e)))
		(lambda ignore e))))

	;; -------- reduce-or --------
	(define (reduce-or return form len true false env)
	  (do ((new-form ())
	       (retry #f)
	       (exprs (cdr form) (cdr exprs)))
	      ((not (pair? exprs))
	       (return (and (pair? new-form)
			    (if (null? (cdr new-form))
				(car new-form)
				(if retry
				    (simplify-boolean (cons 'or (reverse new-form)) () () env)
				    (cons 'or (reverse new-form)))))))
	    (let ((val (classify (car exprs) env))
		  (old-form new-form))

	      (when (and (pair? val)
			 (memq (car val) '(and or not)))
		(set! val (classify (simplify-boolean val true false env) env))
		(when (and (> len 3)
			   (len=2? val) ; pair? val needs to precede car val
			   (eq? (car val) 'not)
			   (pair? (cdr exprs)))
		  (if (symbol? (cadr val))
		      (if (and (pair? (cadr exprs))
			       (memq (cadr val) (cadr exprs)))
			  (and-incomplete form 'or (cadr val) (cadr exprs) env)
			  (do ((ip (cdr exprs) (cdr ip))
			       (found-it #f))
			      ((or found-it
				   (not (pair? ip))))
			    (do ((p (car ip) (cdr p)))
				((or (not (pair? p))
				     (and (memq (cadr val) p)
					  (set! found-it p)))
				 (if (pair? found-it)
				     (and-incomplete form 'or (cadr val) found-it env))))))
		      (when (and (pair? (cadr val))
				 (pair? (cadr exprs))
				 (hash-table-ref bools (caadr val)))
			(if (member (cadadr val) (cadr exprs))
			    (and-forgetful form 'or (cadr val) (cadr exprs) env)
			    (do ((p (cadr exprs) (cdr p)))
				((or (not (pair? p))
				     (and (pair? (car p))
					  (member (cadadr val) (car p))))
				 (if (pair? p)
				     (and-forgetful form 'or (cadr val) (car p) env)))))))))
	      (if (not (or retry
			   (equivalent? val (car exprs)))) ; morally because val might be NaN!
		  (set! retry #t))

	      (cond ((not val))                     ; #f in or is ignored

		    ((or (eq? val #t)               ; #t or any non-#f constant in or ends the expression
			 (code-constant? val))
		     (set! new-form (cons val       ; (or x1 123) -> value of x1 first
					  (if (null? new-form)
					      ()
					      new-form)))
		     ;; reversed when returned
		     (set! exprs '(#t)))

		    ((and (pair? val)                       ; (or ...) -> splice into current
			  (proper-list? val)
			  (eq? (car val) 'or))
		     (set! exprs (append val (cdr exprs)))) ; we'll skip the 'or in do step

		    ((not (or (memq val new-form)
			      (and (len>1? val)         ;   and redundant tests
				   (hash-table-ref booleans (car val))
				   (lint-any? (lambda (p)
						(and (len>1? p)
						     (subsumes? (car p) (car val))
						     (equal? (cadr val) (cadr p))))
					      new-form))))
		     (set! new-form (cons val new-form))))

	      (if (and (not (eq? new-form old-form))
		       (pair? (cdr new-form)))
		  (let ((rel (relsub (cadr new-form) (car new-form) 'or env))) ; new-form is reversed
		    (if (or (boolean? rel)
			    (pair? rel))
			(set! new-form (cons rel (cddr new-form)))))))))

	;; -------- reduce-and --------
	(define reduce-and
	  (let ((last-reduce-and-form #f)) ; try to reduce repetitive output
	    (lambda (return form len false env)
	      (do ((new-form ())
		   (retry #f)
		   (exprs (cdr form) (cdr exprs)))
		  ((null? exprs)
		   (or (null? new-form)      ; (and) -> #t
		       (let ((newer-form (let ((nform (reverse new-form)))
					   (map (lambda (x cdr-x)
						  (if (and x (code-constant? x))
						      (values)
						      x))
						nform (cdr nform)))))
			 (return
			  (cond ((null? newer-form)
				 (car new-form))

				((and (eq? (car new-form) #t) ; trailing #t is dumb if next-to-last is boolean func
				      (pair? (cdr new-form))
				      (pair? (cadr new-form))
				      (symbol? (caadr new-form))
				      (eq? (return-type (caadr new-form) env) 'boolean?))
				 (if (null? (cdr newer-form))
				     (car newer-form)
				     (cons 'and newer-form)))

				(retry
				 (simplify-boolean `(and ,@newer-form ,(car new-form)) () () env))

				(else `(and ,@newer-form ,(car new-form))))))))

		(let* ((e (car exprs))
		       (val (classify e env))
		       (old-form new-form))

		  (if (and (pair? val)
			   (memq (car val) '(and or not)))
		      (set! val (classify (set! e (simplify-boolean val () false env)) env))

		      (when (and (> len 3)
				 (pair? (cdr exprs)))
			(if (symbol? val)
			    (if (and (pair? (cadr exprs))
				     (memq val (cadr exprs)))
				(let ((nval (simplify-boolean (list 'and val (cadr exprs)) () false env)))
				  (if (and (pair? nval)
					   (eq? (car nval) 'and))
				      (and-incomplete form 'and val (cadr exprs) env)
				      (begin
					(set! val nval)
					(set! exprs (cdr exprs)))))
				(do ((ip (cdr exprs) (cdr ip))
				     (found-it #f))
				    ((or found-it
					 (not (pair? ip))))
				  (do ((p (car ip) (cdr p)))
				      ((or (not (pair? p))
					   (and (memq val p)
						(let ((nval (simplify-boolean (list 'and val p) () false env)))
						  (if (and (pair? nval)
							   (eq? (car nval) 'and))
						      (set! found-it p)
						      (let ((ln (and (< 0 line-number 100000) line-number)))
							(out-format outport "~NCin ~A~A,~%~NCperhaps change ~S to ~S~%"
								lint-left-margin #\space
								(truncated-list->string form)
								(if (and (integer? ln) (> ln 0)) (format #f " (line ~D)" ln) "")
								(+ lint-left-margin 4) #\space
								(list 'and '... val '... p)
								nval)
							(set! found-it #t)))))
					   (and (pair? (car p))
						(memq val (car p))
						(set! found-it (car p))))
				       (if (pair? found-it)
					   (and-incomplete form 'and val found-it env))))))
			    (when (and (pair? val)
				       (pair? (cadr exprs))
				       (hash-table-ref bools (car val)))
			      (if (member (cadr val) (cadr exprs))
				  (and-forgetful form 'and val (cadr exprs) env)
				  (do ((p (cadr exprs) (cdr p)))
				      ((or (not (pair? p))
					   (and (pair? (car p))
						(member (cadr val) (car p))))
				       (if (pair? p)
					   (and-forgetful form 'and val (car p) env)))))))))

		  (if (not (or retry
			       (equivalent? e (car exprs)))) ; NaN again
		      (set! retry #t))

		  ;; (and x1 x2 x1) is not reducible
		  ;;   the final thing has to remain at the end, but can be deleted earlier if it can't short-circuit the evaluation,
		  ;;   but if there are expressions following the first x1, we can't be sure that it is not
		  ;;   protecting them:
		  ;;       (and false-or-0 (display (list-ref lst false-or-0)) false-or-0)
		  ;;   so I'll not try to optimize that case.  But (and x x) is optimizable.

					;(format *stderr* "  new-form: ~S\n  val: ~S\n  e: ~S\n  exprs: ~S~%" new-form val e exprs)
		  (cond ((eq? val #t)
			 (if (null? (cdr exprs))     ; (and x y #t) should not remove the #t
			     (if (or (and (pair? e)
					  (eq? (return-type (car e) env) 'boolean?))
				     (eq? e #t))
				 (set! new-form (cons val new-form))
				 (if (or (null? new-form)
					 (not (equal? e (car new-form))))
				     (set! new-form (cons e new-form))))
			     (if (and (not (eq? e #t))
				      (or (null? new-form)
					  (not (member e new-form))))
				 (set! new-form (cons e new-form)))))

			((not val)             ; #f in 'and' ends the expression
			 (set! new-form (if (or (null? new-form)
						(just-symbols? new-form))
					    '(#f)
					    (cons #f new-form)))
			 (set! exprs '(#f)))

			((and (pair? e)       ; if (and ...) splice into current
			      (eq? (car e) 'and))
			 (set! exprs (append e (cdr exprs))))

			((and (len>1? e)       ; (and (list? p) (pair? p) ...) -> (and (pair? p) ...)
			      (pair? (cdr exprs))
			      (len>1? (cadr exprs))
			      (eq? (and-redundant? e (cadr exprs)) (caadr exprs))
			      (equal? (cadr e) (cadadr exprs))))

			((and (len>1? e)       ; (and (list? p) (not (null? p)) ...) -> (and (pair? p) ...)
			      (memq (car e) '(list? pair?))
			      (pair? (cdr exprs))
			      (let ((p (cadr exprs)))
				(and (pair? p)
				     (eq? (car p) 'not)
				     (len>1? (cadr p))
				     (eq? (caadr p) 'null?)
				     (equal? (cadr e) (cadadr p)))))
			 (set! new-form (cons (list 'pair? (cadr e)) new-form))
			 (set! exprs (cdr exprs)))

			;; redundant type check after something like list-ref never happens

			(else

			 (when (and (not (eq? form last-reduce-and-form))
				    (pair? (cdr form))
				    (pair? (cddr form))
				    (pair? (cadr form))
				    (hash-table-ref booleans (caadr form))
				    (pair? (caddr form))
				    (memq (caaddr form) '(or memq memv member assq assv assoc =)))
			   (let* ((ande (cadr form))
				  (othere (caddr form))
				  (andf (car ande))
				  (otherf (car othere)))
			     (set! last-reduce-and-form form)
			     (cond ((memq andf '(integer? rational?))
				    (cond ((and (eq? otherf '=)                ; (and (integer? x) (= x 0)) -> (eqv? x 0), ok because we explicitly want 0, not 0.0
						(len=3? othere)
						(memq (cadr ande) othere))
					   (let ((num ((if (eq? (cadr ande) (caddr othere)) cadr caddr) othere)))
					     (if (and (number? num) ; might be an expression
						      (not ((symbol->value andf) num)))
						 (lint-format "~S, but ~S is not ~S" 'and form num andf)
						 (lint-format "perhaps ~S -> ~S" 'and form (list 'eqv? (cadr ande) num)))))

					  ((eq? otherf 'or)                    ; (and (integer? x) (or (= x 0) (= x 1))) -> (memv x '(0 1))
					   (let ((vals (call-with-exit
							(lambda (return)
							  (map (lambda (x)
								 (if (and (len=3? x)
									  (eq? (car x) '=)
									  (memq (cadr ande) x))
								     (if (number? (cadr x))
									 (cadr x)
									 (if (number? (caddr x))
									     (caddr x)
									     (return #f)))))
							       (cdr othere))))))
					     (if (and (pair? vals)
						      (not (memq #<unspecified> vals)))
						 (lint-format "perhaps ~S -> ~S" 'and form
							      (list 'memv (cadr ande) (list 'quote vals))))))))
				   ;; there are also cases with zero? etc

				   ((eq? andf 'exact?)
				    (let ((num ((if (eq? (cadr ande) (caddr othere)) cadr caddr) othere)))
				      (if (and (eq? otherf '=)
					       (len=3? othere)
					       (memq (cadr ande) othere)
					       (or (not (number? num))
						   ((symbol->value andf) num)))
					  (lint-format "perhaps ~S -> ~S" 'and form
						       (list 'eqv? (cadr ande) num)))))

				   ((and (eq? andf 'symbol?)
					 (eq? otherf 'memq)
					 (eq? (cadr ande) (cadr othere)))
				    (lint-format "perhaps ~S -> ~S" 'and form othere))

				   ((and (eq? andf 'not)
					 (eq? otherf 'or)
					 (pair? (cadr ande))
					 (eq? (caadr ande) 'eof-object?))
				    (let ((vals (call-with-exit
						 (lambda (return)
						   (map (lambda (x)
							  (if (and (len=3? x)
								   (eq? (car x) 'char=?)
								   (memq (cadadr ande) x))
							      (if (char? (cadr x))
								  (cadr x)
								  (if (char? (caddr x))
								      (caddr x)
								      (return #f)))))
							(cdr othere))))))
				      (if (and (pair? vals)
					       (not (memq #<unspecified> vals)))
					  (lint-format "perhaps ~S -> ~S" 'and form
						       (list 'memv (cadadr ande) (list 'quote vals)))))))))

			 (unless (or (and (len>2? e)                   ; (and ... (or ... 123) ...) -> splice out or
					  (pair? (cdr exprs))
					  (eq? (car e) 'or)
					  (cond ((last-ref e) => code-constant?) ; (or ... #f)
						(else #f)))
				     (and (pair? new-form)
					  (or (eq? val (car new-form)) ; omit repeated tests
					      (and (len>1? val)        ;   and redundant tests
						   (hash-table-ref booleans (car val))
						   (lint-any? (lambda (p)
								(and (len>1? p)
								     (subsumes? (car val) (car p))
								     (equal? (cadr val) (cadr p))))
							      new-form)))))
			   (set! new-form (cons val new-form)))))

		  (if (and (not (eq? new-form old-form))
			   (pair? (cdr new-form)))
		      (let ((rel (relsub (car new-form) (cadr new-form) 'and env)))
			;; rel #f should halt everything as above, and it looks ugly in the output,
			;;   but it never happens in real code
			(if (or (pair? rel)
				(boolean? rel))
			    (set! new-form (cons rel (cddr new-form)))))))))))

	;; -------- bool-simp-1 --------
	(define (bool-simp-1 form true false len env)
	  (let ((op (case (car form)
		      ((or) 'and)
		      ((and) 'or)
		      (else #f))))
	    (if (and op
		     (>= len 3)
		     (lint-every? (lambda (p)
				    (and (len>2? p)
					 (eq? (car p) op)))
				  (cdr form)))
		(let ((first (cadadr form)))
		  (if (lint-every? (lambda (p)
				     (equal? (cadr p) first))
				   (cddr form))
		      (set! form `(,op ,first (,(car form) ,@(map (lambda (p)
								    (if (null? (cdddr p))
									(caddr p)
									(cons op (cddr p))))
								  (cdr form)))))
		      (if (null? (cdddr (cadr form)))
			  (let ((last (caddr (cadr form))))
			    (if (lint-every? (lambda (p)
					       (and (null? (cdddr p))
						    (equal? (caddr p) last)))
					     (cddr form))
				(set! form (list op
						 (cons (car form)
						       (map cadr (cdr form)))
						 last)))))))))
	  ;; (or (and A B) (and A C)) -> (and A (or B C))
	  ;; (or (and A B) (and C B)) -> (and (or A C) B)
	  ;; (and (or A B) (or A C)) -> (or A (and B C))
	  ;; (and (or A B) (or C B)) -> (or (and A C) B)

	  (case (car form)
	    ;; --------------------------------
	    ((not)
	     ;(format *stderr* "not: ~S~%" form)
	     (if (not (= len 2))
		 form
		 (let* ((arg (cadr form))
			(val (classify (if (and (pair? arg)
						(memq (car arg) '(and or not)))
					   (simplify-boolean arg true false env)
					   arg)
				       env))
			(arg-op (and (pair? arg)
				     (car arg))))

		   (cond ((boolean? val)
			  (not val))

			 ((or (code-constant? arg)
			      (and (pair? arg)
				   (symbol? arg-op)
				   (hash-table-ref no-side-effect-functions arg-op)
				   (let ((ret (return-type arg-op env)))
				     (and (or (symbol? ret)
					      (pair? ret))
					  (not (return-type-ok? 'boolean? ret))
					  (not (return-type-ok? 'not ret))))
				   (not (var-member arg-op env))))
			  #f)

			 ((and (pair? val)
			       (> (length val) 1)                           ; (not (not ...)) -> ...  this is usually internally generated,
			       (memq (car val) '(not if cond case begin)))  ;   so the message about (and x #t) is in special-case-functions below
			  (case (car val)
			    ((not)
			     (cadr val))

			    ((if)
			     (if (not (pair? (cddr val)))
				 form
				 (let ((if-true (simplify-boolean (list 'not (caddr val)) () () env))
				       (if-false (or (not (pair? (cdddr val)))  ; (not #<unspecified>) -> #t
						     (simplify-boolean (list 'not (cadddr val)) () () env))))
				   ;; ideally we'd call if-walker on this to simplify further
				   (list 'if (cadr val) if-true if-false))))

			    ((cond case)
			     `(,(car val)
			       ,@(if (eq? (car val) 'cond) () (list (cadr val)))
			       ,@(map (lambda (c)
					(if (not (and (pair? c)
						      (> (length c) 1)))
					    c
					    (let* ((len (length (cdr c)))
						   (new-last (let ((last (list-ref c len)))
							       (if (and (pair? last)
									(eq? (car last) 'error))
								   last
								   (simplify-boolean (list 'not last) () () env)))))
					      `(,(car c) ,@(copy (cdr c) (make-list (- len 1))) ,new-last))))
				      ((if (eq? (car val) 'cond) cdr cddr) val))))

			    ((begin)
			     (let* ((len-1 (- (length val) 1))
				    (new-last (simplify-boolean (list 'not (list-ref val len-1)) () () env)))
			       (append (copy val (make-list len-1)) (list new-last))))))

			 ((not (equal? val arg))
			  (list 'not val))

			 ((not (pair? arg))
			  form)

			 ((and (memq arg-op '(and or))        ; (not (or|and x (not y))) -> (and|or (not x) y)
			       (= (length arg) 3)
			       (or (and (pair? (cadr arg))
					(eq? (caadr arg) 'not))
				   (and (pair? (caddr arg))
					(eq? (caaddr arg) 'not))))
			  (let ((rel (if (eq? arg-op 'or) 'and 'or)))
			    (cons rel (map (lambda (p)
					     (if (and (len=2? p)
						      (eq? (car p) 'not))
						 (cadr p)
						 (simplify-boolean (list 'not p) () () env)))
					   (cdr arg)))))

			 ((<= (length arg) 3)           ; avoid (<= 0 i 12) and such
			  (case arg-op
			    ((< > <= >= odd? even? exact? inexact?char<? char>? char<=? char>=? string<? string>? string<=? string>=?
				char-ci<? char-ci>? char-ci<=? char-ci>=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
			     (cons (hash-table-ref notables arg-op) (cdr arg)))

			    ;; null? is not quite right because (not (null? 3)) -> #t
			    ;; char-upper-case? and lower are not switchable here

			    ((zero?)       ; (not (zero? (logand p 2^n | (ash 1 i)))) -> (logbit? p i)
			     (if (not (pair? (cdr arg)))
				 form
				 (let ((zarg (cadr arg)))  ; (logand...)
				   (if (not (and (len=3? zarg)
						 (eq? (car zarg) 'logand)))
				       form
				       (let ((arg1 (cadr zarg))
					     (arg2 (caddr zarg))) ; these are never reversed
					 (or (and (len>1? arg2)
						  (eq? (car arg2) 'ash)
						  (eqv? (cadr arg2) 1)
						  (list 'logbit? arg1 (caddr arg2)))
					     (and (integer? arg2)
						  (positive? arg2)
						  (zero? (logand arg2 (- arg2 1))) ; it's a power of 2
						  (list 'logbit? arg1 (floor (log arg2 2)))) ; floor for freeBSD?
					     form))))))

			    (else form)))
			 (else form)))))

	    ;; --------------------------------
	    ((or)
	     (case len
	       ((1) #f)
	       ((2) (if (code-constant? (cadr form)) (cadr form) (classify (cadr form) env)))
	       (else
		(call-with-exit
		 (lambda (return)
		   (when (= len 3)
		     (let ((arg1 (cadr form))
			   (arg2 (caddr form)))

		       (if (and (len>1? arg2)     ; (or A (and ... A ...)) -> A
				(eq? (car arg2) 'and)
				(member arg1 (cdr arg2))
				(not (side-effect? arg2 env)))
			   (return arg1))
		       (if (and (pair? arg1)     ; (or (and ... A) A) -> A
				(eq? (car arg1) 'and)
				(equal? arg2 (last-ref arg1))
				(not (side-effect? arg1 env)))
			   (return arg2))

		       (when (pair? arg2)
			 (if (and (eq? (car arg2) 'and) ; (or A (and (not A) B)) -> (or A B)
				  (pair? (cdr arg2))
				  (len>1? (cadr arg2)))
			     (if (and (eq? (caadr arg2) 'not)
				      (equal? arg1 (cadadr arg2)))
				 (return (cons 'or (cons arg1 (cddr arg2))))
				 (if (or (and (pair? (cadr arg2))
					      (eq? (caadr arg2) 'not)
					      (equal? arg1 (cadadr arg2)))
					 (and (pair? arg1)
					      (hash-table-ref reversibles (car arg1))
					      (equal? arg1 ;(simplify-boolean arg1 () () env)
						      (simplify-boolean (list 'not (cadr arg2)) () () env))))
				     (lint-format "perhaps ~A" 'or  ; (or (< x y) (and (>= x y) (= x 2))) -> (or (< x y) (= x 2))
						  ;; this could be much fancier, but it's not hit much (the 'and case below is not hit at all I think)
						  (lists->string form
								 (or (null? (cddr arg2))
								     (list 'or arg1
									   (if (pair? (cdddr arg2))
									       (cons 'and (cddr arg2))
									       (caddr arg2)))))))))
			 (when (pair? arg1)
			   (when (and (eq? (car arg1) 'not)
				      (len=1? (cdr arg1)))
			     (if (symbol? (cadr arg1))
				 (if (memq (cadr arg1) arg2)
				     (begin
				       (if (eq? (car arg2) 'boolean?)
					   (return arg2))
				       (and-incomplete form 'or (cadr arg1) arg2 env))
				     (do ((p arg2 (cdr p)))
					 ((or (not (pair? p))
					      (and (pair? (car p))
						   (memq (cadr arg1) (car p))))
					  (if (pair? p)
					      (and-incomplete form 'or (cadr arg1) (car p) env)))))
				 (if (and (len=2? (cadr arg1))   ; (or (not (number? x)) (> x 2)) -> (or (not (real? x)) (> x 2))
					  (hash-table-ref bools (caadr arg1)))
				     (if (member (cadadr arg1) arg2)
					 (and-forgetful form 'or (cadr arg1) arg2 env)
					 (do ((p arg2 (cdr p)))
					     ((or (not (pair? p))
						  (and (pair? (car p))
						       (member (cadadr arg1) (car p))))
					      (if (pair? p)
						  (and-forgetful form 'or (cadr arg1) (car p) env)))))))

			     (if (and (eq? (car arg2) 'and) ; (or (not A) (and A B)) -> (or (not A) B) -- this stuff actually happens!
				      (len>1? (cdr arg2))
				      (equal? (cadr arg1) (cadr arg2)))
				 (return (cons 'or (cons arg1 (cddr arg2))))))

			   (when (and (eq? (car arg1) 'and)
				      (eq? (car arg2) 'and)
				      (= 3 (length arg1) (length arg2))
				      ;; (not (side-effect? arg1 env)) ; maybe??
				      (or (equal? (cadr arg1) (list 'not (cadr arg2)))
					  (equal? (list 'not (cadr arg1)) (cadr arg2)))
				      (not (equal? (caddr arg1) (list 'not (caddr arg2))))
				      (not (equal? (list 'not (caddr arg1)) (caddr arg2))))
			     ;; kinda dumb, but common: (or (and A B) (and (not A) C)) -> (if A B C)
			     ;;    the other side: (and (or A B) (or (not A) C)) -> (if A C (and B #t)), but it never happens
			     (lint-format "perhaps ~A" 'or
					  (lists->string form
							 (if (and (pair? (cadr arg1))
								  (eq? (caadr arg1) 'not))
							     (list 'if (cadr arg2) (caddr arg2) (caddr arg1))
							     (list 'if (cadr arg1) (caddr arg1) (caddr arg2))))))
			   (let ((t1 (and (pair? (cdr arg1))
					  (pair? (cdr arg2))
					  (or (equal? (cadr arg1) (cadr arg2))
					      (and (len=1? (cddr arg2))
						   (equal? (cadr arg1) (caddr arg2))))
					  (not (side-effect? arg1 env))
					  (and-redundant? arg1 arg2))))
			     (if t1
				 (return (if (eq? t1 (car arg1)) arg2 arg1))))

			   ;; if all clauses are (eq-func x y) where one of x/y is a symbol|simple-expr repeated throughout
			   ;;   and the y is a code-constant, or -> memq and friends.
			   ;;   This could also handle cadr|caddr reversed, but it apparently never happens.
			   (if (and (or (and (eq? (car arg2) '=)
					     (memq (car arg1) '(< > <= >=)))
					(and (eq? (car arg1) '=)
					     (memq (car arg2) '(< > <= >=))))
				    (= (length arg1) 3)
				    (equal? (cdr arg1) (cdr arg2)))
			       (return (cons (if (or (memq (car arg1) '(< <=))
						     (memq (car arg2) '(< <=)))
						 '<= '>=)
					     (cdr arg1))))

			   ;; this makes some of the code above redundant
			   (let ((rel (relsub arg1 arg2 'or env)))
			     (if (or (boolean? rel)
				     (pair? rel))
				 (return rel)))

			   ;; (or (pair? x) (null? x)) -> (list? x)
			   (when (and (pair? (cdr arg1))
				      (pair? (cdr arg2))
				      (equal? (cadr arg1) (cadr arg2)))
			     (if (and (memq (car arg1) '(null? pair?))
				      (memq (car arg2) '(null? pair?))
				      (not (eq? (car arg1) (car arg2))))
				 (return (list 'list? (cadr arg1))))

			     (if (and (eq? (car arg1) 'zero?)  ; (or (zero? x) (positive? x)) -> (not (negative? x)) -- other cases don't happen
				      (memq (car arg2) '(positive? negative?)))  ; but +nan.0 messes this up -- perhaps add that to the suggestion?
				 (return (list 'not (list (if (eq? (car arg2) 'positive?) 'negative? 'positive?)
							  (cadr arg1))))))

			   ;; (or (and A B) (and (not A) (not B))) -> (eq? (not A) (not B))
			   ;; more accurately (if A B (not B)), but every case I've seen is just boolean
			   ;; perhaps also (or (not (or A B)) (not (or (not A) (not B)))), but it never happens
			   (let ((a1 (cadr form))
				 (a2 (caddr form)))
			     (when (and (len=3? a1)
					(len=3? a2)
					(eq? (car a1) 'and)
					(eq? (car a2) 'and))
			       (let ((A ((if (and (pair? (cadr a1)) (eq? (caadr a1) 'not)) cadadr cadr) a1))
				     (B (if (and (pair? (caddr a1)) (eq? (caaddr a1) 'not)) (cadr (caddr a1)) (caddr a1))))
				 (if (or (equal? form `(or (and ,A ,B) (and (not ,A) (not ,B))))
					 (equal? form `(or (and (not ,A) (not ,B)) (and ,A ,B))))
				     (return `(eq? (not ,A) (not ,B))))
				 (if (or (equal? form `(or (and ,A (not ,B)) (and (not ,A) ,B)))
					 (equal? form `(or (and (not ,A) ,B) (and ,A (not ,B)))))
				     (return `(not (eq? (not ,A) (not ,B))))))))

			   (when (and (pair? (cdr arg1))
				      (pair? (cdr arg2))
				      (not (eq? (car arg1) (car arg2))))
			     (when (subsumes? (car arg1) (car arg2))
			       (return arg1))

			     (let ((t2 (if (eq? (car arg1) 'not)
					   (and (booleans-with-not? arg2 arg1)
						(or-not-redundant arg2 arg1))
					   (and (booleans-with-not? arg1 arg2)
						(or-not-redundant arg1 arg2)))))
			       (when t2
				 (if (eq? t2 'true)
				     (return #t)
				     (if (pair? t2)
					 (return t2))))))

			   ;; (or (if a c d) (if b c d)) -> (if (or a b) c d) never happens, sad to say
			   ;;   or + if + if does happen but not in this easily optimized form
			   )))) ; len = 3

		   ;; len > 3 or nothing was caught above
		   (invert-successive-nots return form len env)
		   (or->memx return form env)
		   (or->case return form)
		   (reduce-or return form len true false env))))))

	    ;; --------------------------------
	    ((and)
	     (case len
	       ((1) #t)
	       ((2) (classify (cadr form) env))
	       (else
		(and (not (contradictory? (cdr form)))
		     (call-with-exit
		      (lambda (return)
			(when (= len 3)
			  (let ((arg1 (cadr form))
				(arg2 (caddr form)))
			    (if (and (len>1? arg2)                ; (and A (or A ...)) -> A
				     (eq? (car arg2) 'or))
				(if (and (equal? arg1 (cadr arg2))
					 (not (side-effect? arg2 env)))
				    (return arg1)
				    (if (or (and (pair? (cadr arg2))
						 (eq? (caadr arg2) 'not)
						 (equal? arg1 (cadadr arg2)))
					    (and (pair? arg1)
						 (hash-table-ref reversibles (car arg1))
						 (equal? arg1 ;(simplify-boolean arg1 () () env)
							 (simplify-boolean (list 'not (cadr arg2)) () () env))))
					(lint-format "perhaps ~A" 'and  ; (and (< x y) (or (>= x y) (= x 2))) -> (and (< x y) (= x 2)) ??
						     (lists->string form
								    (and (not (null? (cddr arg2)))
									 (list 'and arg1
									       (if (pair? (cdddr arg2))
										   (cons 'or (cddr arg2))
										   (caddr arg2)))))))))
			    (if (and (pair? arg1)                ; (and (or ... A ...) A) -> A
				     (eq? (car arg1) 'or)
				     (member arg2 (cdr arg1))
				     (not (side-effect? arg1 env)))
				(return arg2))
			    ;; the and equivalent of (or (not A) (and A B)) never happens

			    (when (pair? arg2)
			      (if (symbol? arg1)                 ; (and x (pair? x)) -> (pair? x)
				  (if (memq arg1 arg2)
				      (begin
					(case (car arg2)
					  ((not)      (return #f))
					  ((boolean?) (return (list 'eq? arg1 #t))))
					(and-incomplete form 'and arg1 arg2 env)
					(if (hash-table-ref booleans (car arg2))
					    (return arg2)))
				      (do ((p arg2 (cdr p)))   ; (and x (+ (log x) 1)) -> (and (number? x)...)
					  ((or (not (pair? p))
					       (and (pair? (car p))
						    (memq arg1 (car p))))
					   (if (pair? p)
					       (and-incomplete form 'and arg1 (car p) env)))))
				  (if (and (len>1? arg1)               ; (and (number? x) (> x 2)) -> (and (real? x) (> x 2))
					   (hash-table-ref bools (car arg1)))
				      (if (member (cadr arg1) arg2)
					  (and-forgetful form 'and arg1 arg2 env)
					  (do ((p arg2 (cdr p)))
					      ((or (not (pair? p))
						   (and (pair? (car p))
							(member (cadr arg1) (car p))))
					       (if (pair? p)
						   (and-forgetful form 'and arg1 (car p) env))))))))

			    (if (and (not (side-effect? arg1 env))
				     (equal? arg1 arg2))                  ; (and x x) -> x
				(return arg1))

			    (when (and (len>1? arg1)
				       (len>1? arg2))
			      (let ((t1 (and (or (equal? (cadr arg1) (cadr arg2))
						 (and (len=1? (cddr arg2))
						      (equal? (cadr arg1) (caddr arg2))))
					     (not (side-effect? arg1 env))
					     (and-redundant? arg1 arg2)))) ; (and (integer? x) (number? x)) -> (integer? x)
				(when t1
				  (return (cond
					   ((memq t1 '(eq? eqv? equal?))
					    (cons t1 (cdr arg2)))

					   ((eq? t1 'memv)
					    (let ((x ((if (equal? (cadr arg1) (cadr arg2)) caddr cadr) arg2)))
					      (if (rational? x)
						  `(memv ,(cadr arg1) '(,x ,(* 1.0 x)))
						  `(memv ,(cadr arg1) '(,(floor x) ,x)))))

					   ((eq? t1 (car arg1)) arg1)
					   (else arg2)))))

			      (when (and (hash-table-ref reversibles (car arg1))
					 (len=1? (cddr arg1))
					 (len=1? (cddr arg2))
					 (not (side-effect? arg2 env))             ; arg1 is hit in any case
					 (or (eq? (car arg1) (car arg2))           ; either ops are equal or
					     (let ((rf (hash-table-ref reversibles (car arg2))))  ;    try reversed op for arg2
					       (and (eq? (car arg1) rf)
						    (set! arg2 (cons rf (reverse (cdr arg2))))))))
				(when (and (memq (car arg1) '(< <= >= >))          ; (and (op x y) (op x z)) -> (op x (min|max y z))
					   (equal? (cadr arg1) (cadr arg2)))
				  (if (and (rational? (caddr arg1))
					   (rational? (caddr arg2)))
				      (return (list (car arg1)
						    (cadr arg1)
						    ((if (memq (car arg1) '(< <=)) min max) (caddr arg1) (caddr arg2)))))
				  (if (equal? (caddr arg1) (caddr arg2))             ; (and (< 0 x) (> x 0)) -> (< 0 x)
				      (return arg1))
				  (return (list (car arg1)
						(cadr arg1)
						(list (if (memq (car arg1) '(< <=)) 'min 'max)
						      (caddr arg1)
						      (caddr arg2)))))

				(when (and (or (equal? (caddr arg1) (cadr arg2))     ; (and (op x y) (op y z))
					       (equal? (cadr arg1) (caddr arg2))     ; (and (op x y) (op z x))
					       (and (memq (car arg1) '(= char=? string=? char-ci=? string-ci=?))
						    (or (equal? (cadr arg1) (cadr arg2))
							(equal? (caddr arg1) (caddr arg2)))))
					   (let ((ary (arg-arity (car arg1) env)))   ; don't try to combine (and (eq? x y) (eq? y z))!
					     (and (pair? ary)
						  (> (cdr ary) 2))))
				  (let ((op1 (car arg1))
					(arg1-1 (cadr arg1))
					(arg1-2 (caddr arg1))
					(arg2-1 (cadr arg2))
					(arg2-2 (caddr arg2)))
				    (return
				     (cond ((equal? arg1-2 arg2-1)       ; (and (op x y) (op y z)) -> (op x y z)
					    (if (equal? arg1-1 arg2-2)
						(if (memq op1 '(= char=? string=? char-ci=? string-ci=?))
						    arg1
						    (and (memq op1 '(<= >= char<=? char>=? string<=? string>=?
									char-ci<=? char-ci>=? string-ci<=? string-ci>=?))
							 (cons (case op1
								 ((>= <=) '=)
								 ((char<= char>=) 'char=?)
								 ((char-ci<= char-ci>=) 'char-ci=?)
								 ((string<= string>=) 'string=?)
								 ((string-ci<= string-ci>=) 'string-ci=?)
								 (else 'error))
							       (cdr arg1))))
						(and (or (not (code-constant? arg1-1))
							 (not (code-constant? arg2-2))
							 ((symbol->value op1 (rootlet)) arg1-1 arg2-2))
						     (list op1 arg1-1 arg2-1 arg2-2))))

					   ((equal? arg1-1 arg2-2)       ; (and (op x y) (op z x)) -> (op z x y)
					    (if (equal? arg1-2 arg2-1)
						(and (memq op1 '(= char=? string=? char-ci=? string-ci=?))
						     arg1)
						(and (or (not (code-constant? arg2-1))
							 (not (code-constant? arg1-2))
							 ((symbol->value op1 (rootlet)) arg2-1 arg1-2))
						     (list op1 arg2-1 arg1-1 arg1-2))))

					   ;; here we're restricted to equalities and we know arg1 != arg2
					   ((equal? arg1-1 arg2-1)        ; (and (op x y) (op x z)) -> (op x y z)
					    (if (and (code-constant? arg1-2)
						     (code-constant? arg2-2))
						(and ((symbol->value op1 (rootlet)) arg1-2 arg2-2)
						     arg1)
						(list op1 arg1-1 arg1-2 arg2-2)))

					   ;; equalities again
					   ((and (code-constant? arg1-1)
						 (code-constant? arg2-1))
					    (and ((symbol->value op1 (rootlet)) arg1-1 arg2-1)
						 arg1))

					   (else (list op1 arg1-1 arg1-2 arg2-1)))))))

			      ;; check some special cases
			      (when (and (or (equal? (cadr arg1) (cadr arg2))
					     (and (len=1? (cddr arg2))
						  (equal? (cadr arg1) (caddr arg2))))
					 (hash-table-ref booleans (car arg1)))
				(when (or (eq? (car arg1) 'zero?)  ; perhaps rational? and integer? here -- not many hits
					  (eq? (car arg2) 'zero?))
				  (if (or (memq (car arg1) '(integer? rational? exact?))
					  (memq (car arg2) '(integer? rational? exact?)))
				      (return (list 'eqv? (cadr arg1) 0)))
				  (if (or (eq? (car arg1) 'inexact?)
					  (eq? (car arg2) 'inexact?))
				      (return (list 'eqv? (cadr arg1) 0.0))))

				(when (hash-table-ref and-rel-ops (car arg2))
				  (when (and (eq? (car arg1) 'symbol?)
					     (memq (car arg2) '(eq? eqv? equal?))
					     (or (quoted-symbol? (cadr arg2))
						 (quoted-symbol? (caddr arg2))))
				    (return (cons 'eq? (cdr arg2))))

				  (when (and (eq? (car arg1) 'positive?)
					     (eq? (car arg2) '<)
					     (eq? (cadr arg1) (cadr arg2)))
				    (return (list '< 0 (cadr arg1) (caddr arg2))))))

			      (when (and (member (cadr arg1) arg2)
					 (memq (car arg2) '(string=? char=? eq? eqv? equal?))
					 (len=2? (cdr arg2))
					 (hash-table-ref bools (car arg1))
					 (or (and (code-constant? (cadr arg2))
						  (compatible? (car arg1) (->lint-type (cadr arg2))))
					     (and (code-constant? (caddr arg2))
						  (compatible? (car arg1) (->lint-type (caddr arg2))))))
				(return (cons (if (eq? (car arg1) 'char?) 'eqv? 'equal?)
					      (cdr arg2))))

			      (when (and (equal? (cadr arg1) (cadr arg2))
					 (eq? (car arg1) 'inexact?)
					 (eq? (car arg2) 'real?))
				(return (list 'and arg2 arg1)))

			      ;; this makes some of the code above redundant
			      (let ((rel (relsub arg1 arg2 'and env)))
				(if (or (boolean? rel)
					(pair? rel))
				    (return rel)))

			      ;; (and ... (not...))
			      (unless (eq? (car arg1) (car arg2))
				(let ((t2 (if (eq? (car arg1) 'not)
					      (and (booleans-with-not? arg2 arg1)
						   (and-not-redundant arg2 arg1))
					      (and (booleans-with-not? arg1 arg2)
						   (and-not-redundant arg1 arg2)))))
				  (cond
				   ((eq? t2 'contradictory) (return #f))
				   ((symbol? t2)            (return (cons t2 (cdr (if (eq? (car arg1) 'not) arg2 arg1)))))
				   ((pair? t2)	            (return t2))))
				(when (and (eq? (car arg1) 'not)        ; (and (not (eof-object x)) (char=? x #\a)) -> (eqv? x #\a)
					   (eq? (car arg2) 'char=?)
					   (pair? (cadr arg1))
					   (eq? (caadr arg1) 'eof-object?)
					   (eq? (cadadr arg1) (cadr arg2)))
				  (return (cons 'eqv? (cdr arg2)))))

			      (when (hash-table-ref bools (car arg1))
				(let ((p (member (cadr arg1) (cdr arg2))))
				  (when p
				    (let ((sig (arg-signature (car arg2) env))
					  (pos (- (length arg2) (length p))))

				      (when (pair? sig)
					(let ((arg-type (and (> (length sig) pos)
							     (list-ref sig pos))))
					  (unless (compatible? (car arg1) arg-type)
					    (let ((ln (and (< 0 line-number 100000) line-number)))
					      (out-format outport "~NCin ~A~A, ~A is ~A, but ~A wants ~A"
						      lint-left-margin #\space
						      (truncated-list->string form)
						      (if (and (integer? ln) (> ln 0)) (format #f " (line ~D)" ln) "")
						      (cadr arg1)
						      (prettify-checker-unq (car arg1))
						      (car arg2)
						      (prettify-checker arg-type))))))))))

			      (cond ((not (and (eq? (car arg1) 'equal?) ; (and (equal? (car a1) (car a2)) (equal? (cdr a1) (cdr a2))) -> (equal? a1 a2)
					       (eq? (car arg2) 'equal?)
					       (pair? (cadr arg1))
					       (pair? (caddr arg1))
					       (pair? (cadr arg2))
					       (pair? (caddr arg2))
					       (eq? (caadr arg1) (caaddr arg1)))))

				    ((assq (caadr arg1)
					   '((car cdr #t)
					     (caar cdar car) (cadr cddr cdr)
					     (caaar cdaar caar) (caadr cdadr cadr) (caddr cdddr cddr) (cadar cddar cdar)
					     (cadddr cddddr cdddr) (caaaar cdaaar caaar) (caaadr cdaadr caadr) (caadar cdadar cadar)
					     (caaddr cdaddr caddr) (cadaar cddaar cdaar) (cadadr cddadr cdadr) (caddar cdddar cddar)))
				     => (lambda (x)
					  (if (and (eq? (caadr arg2) (cadr x))
						   (eq? (caaddr arg2) (cadr x))
						   (equal? (cadadr arg1) (cadadr arg2))
						   (equal? (cadr (caddr arg1)) (cadr (caddr arg2))))
					      (return (if (symbol? (caddr x))
							  `(equal? (,(caddr x) ,(cadadr arg1)) (,(caddr x) ,(cadr (caddr arg1))))
							  `(equal? ,(cadadr arg1) ,(cadr (caddr arg1))))))))))))

			;; len > 3 or nothing was caught above
			(invert-successive-nots return form len env)

			(if (lint-every? (lambda (a)
					   (and (len>1? a)
						(or (eq? (car a) 'zero?)
						    (and (eq? (car a) '=)
							 (len=2? (cdr a))
							 (or (eqv? (cadr a) 0)
							     (eqv? (caddr a) 0))))))
					 (cdr form))
			    (return (cons '= (cons 0 (lint-remove-duplicates
						      (map (lambda (a)
							     ((if (or (eq? (car a) 'zero?)
								      (not (eqv? (cadr a) 0)))
								  cadr caddr)
							      a))
							   (cdr form))
						      env)))))

			(let ((diff (apply and-redundants env (cdr form))))
			  (when diff
			    (if (null? (cdr diff))
				(return (car diff)))
			    (return (simplify-boolean (cons 'and diff) () () env))))
			;; now there are redundancies below (see subsumes?) but they assumed the tests were side-by-side
#|
			;; (and (or x y) x y) -> (and x y) which doesn't happen to anyone but me
			(when (= (length form) 4) ; len here does not always reflect form length?
			  (let ((arg1 (cadr form))
				(arg2 (caddr form))
				(arg3 (cadddr form)))
			    (when (and (pair? arg1) (eq? (car arg1) 'or) (len=3? arg1)
				       (equal? (cadr arg1) arg2)
				       (equal? (caddr arg1) arg3))
			      (return (list 'and arg2 arg3)))))
|#
			(reduce-and return form len false env)))))))))

	(define (bsimp x env) ; quick check for common easy cases
	  (set! last-simplify-boolean-line-number line-number)
	  (if (not (len>1? x))
	      x
	      (case (car x)
		((and) (and (cadr x)              ; (and #f ...) -> #f
			    x))
		((or) (if (and (cadr x)           ; (or #t ...) -> #t
			       (code-constant? (cadr x)))
			  (cadr x)
			  x))
		(else
		 (if (not (and (len=2? x)
			       (pair? (cadr x))
			       (symbol? (caadr x))))
		     x
		     (let ((rt (if (and (eq? (caadr x) 'quote)
					(pair? (cdadr x)))
				   (->simple-type (cadadr x))
				   (return-type (caadr x) env)))
			   (head (car x)))
		       (or (and (subsumes? head rt) #t) ; don't return the memq list!
			   (and (or (memq rt '(#t #f values))
				    (any-compatible? head rt))
				(case head
				  ((null?) (if (eq? (caadr x) 'list)
					       (null? (cdadr x))
					       x))
				  ((pair?) (if (eq? (caadr x) 'list)
					       (pair? (cdadr x))
					       x))
				  ((negative?) (and (not (hash-table-ref non-negative-ops (caadr x)))
						    x))
				  (else x))))))))))

	(define (bcomp x true false env) ; not so quick...
	  (cond ((not (pair? x))
		 x)

		((eq? (car x) 'and)
		 (call-with-exit
		  (lambda (return)
		    (let ((newx (list 'and)))
		      (do ((p (cdr x) (cdr p))
			   (sidex newx)
			   (endx newx))
			  ((not (pair? p))
			   (and (null? p) newx))
			(let ((next (car p)))
			  (if (or (not next)        ; #f in and -> end of expr
				  (member next false))
			      (if (eq? sidex newx)  ; no side-effects
				  (return #f)
				  (begin
				    (set-cdr! endx (list #f))
				    (return newx)))
			      (if (or (code-constant? next)  ; (and ... true-expr ...)
				      (member next sidex)    ; if a member, and no side-effects since, it must be true
				      (member next true))
				  (if (and (null? (cdr p))
					   (not (equal? next (car endx))))
				      (set-cdr! endx (list next)))
				  (begin
				    (set-cdr! endx (list next))
				    (set! endx (cdr endx))
				    (if (side-effect? next env)
					(set! sidex endx)))))))))))

		((not (eq? (car x) 'or))
		 x)

		(else
		 (call-with-exit
		  (lambda (return)
		    (let ((newx (list 'or)))
		      (do ((p (cdr x) (cdr p))
			   (sidex newx)
			   (endx newx))
			  ((not (pair? p))
			   (if (null? p) newx x))
			(let ((next (car p)))
			  (if (or (and next                 ; (or ... #t ...)
				       (code-constant? next))
				  (member next true))
			      (begin
				(set-cdr! endx (list next))
				(return newx))          ; we're done since this is true
			      (if (or (not next)
				      (member next sidex) ; so its false in some way
				      (member next false))
				  (if (and (null? (cdr p))
					   (not (equal? next (car endx))))
				      (set-cdr! endx (list next)))
				  (begin
				    (set-cdr! endx (list next))
				    (set! endx (cdr endx))
				    (if (side-effect? next env)
					(set! sidex endx)))))))))))))

	;; --------------------------------
	;; simplify-boolean
	;;   this is not really simplify boolean as in boolean algebra because in scheme there are many unequal truths, but only one falsehood
	;;   'and and 'or are not boolean operators in a sense
	(lambda (in-form true false env)
	  ;(format *stderr* "simplify-boolean ~S~%" in-form)
	  (and (not (or (reversible-member in-form false)
			(and (len>1? in-form)
			     (eq? (car in-form) 'not)
			     (reversible-member (cadr in-form) true))))
	       (or (and (reversible-member in-form true) #t)
		   (and (len>1? in-form)
			(eq? (car in-form) 'not)
			(reversible-member (cadr in-form) false)
			#t)
		   (if (not (pair? in-form))
		       in-form
		       (let ((form (bcomp (bsimp in-form env) true false env)))
			 (if (not (and (pair? form)
				       (memq (car form) '(or and not))))
			     (classify form env)
			     (let ((len (length form)))
			       (if (< len 0)
				   form
				   (bool-simp-1 form true false len env)))))))))))


    ;; --------------------------------
    (define undumb
      (let ((dumb-ops '((fix:+ . +) (fx+ . +) (flo:+ . +) (fl+ . +)
			(fix:* . *) (fx* . *) (flo:* . *) (fl* . *)
			(fix:- . -) (fx- . -) (flo:- . -) (fl- . -)
			(fix:/ . /) (fx/ . /) (flo:/ . /) (fl/ . /)
			(fix:= . =) (fx= . =) (flo:= . =) (fl= . =)
			(fix:< . <) (fx< . <) (flo:< . <) (fl< . <)
			(fix:> . >) (fx> . >) (flo:> . >) (fl> . >)
			(fix:<= . <=) (fx<= . <=) (flo:<= . <=) (fl<= . <=)
			(fix:>= . >=) (fx>= . >=) (flo:>= . >=) (fl>= . >=)
			(fxlogand . logand) (fxlogior . logior) (fxlogxor . logxor) (fxlognot . lognot)
			(fxand . logand) (fxior . logior) (fxxor . logxor) (fxnot . lognot)
			(fix:quotient . quotient) (fix:min . min) (fix:max . max) (fxquotient . quotient)
			(flmax . max) (flmin . min)
			(flo:abs . abs) (flabs . abs)
			(flo:sin . sin) (flsin . sin)
			(flo:cos . cos) (flcos . cos)
			(flo:tan . tan) (fltan . tan)
			(flo:asin . asin) (flasin . asin)
			(flo:acos . acos) (flacos . acos)
			(flo:atan . atan) (flatan . atan)
			(flo:sqrt . sqrt) (flsqrt . sqrt)
			(flo:exp . exp) (flexp . exp) (flexpt . expt)
			(flo:log . log) (fllog . log)
			(bignum+ . +) (bignum* . *) (bignum/ . /) (bignum- . -)
			(bignum< . <) (bignum<= . <=) (bignum> . >) (bignum>= . >=) (bignum= . =)
			(bignum-quotient . quotient) (bignum-negative? . negative?) (bignum-magnitude . magnitude)
			(bignum-expt . expt) (bignum-zero? . zero?) (bignum-abs . abs) (bignum-remainder . remainder))))
	(lambda (tree)
	  (cond ((assq tree dumb-ops) => cdr)
		((or (not (pair? tree))
		     (eq? (car tree) 'quote))
		 tree)
		(else (cons (undumb (car tree))
			    (undumb (cdr tree))))))))

    (define (splice-if func lst)
      (cond ;((null? lst) ())
	    ((not (pair? lst)) lst)
	    ((and (pair? (car lst))
		  (eq? func (caar lst))
		  (proper-list? (cdar lst))) ; for apply
	     (append (splice-if func (cdar lst))
		     (splice-if func (cdr lst))))
	    (else (cons (car lst)
			(splice-if func (cdr lst))))))

    (define simplify-numerics
      ;; this returns a form, possibly the original simplified

      (let ()

	(define (integer-result? op)
	  (memq op '(logior lognot logxor logand numerator denominator floor round truncate ceiling ash int-vector-ref)))

	(define (remove-inexactions val)
	  (when (and (or (assq 'exact->inexact val)
			 (assq 'inexact val))
		     (not (tree-memq 'random val))
		     (any-numbers? val))
	    (set! val (map (lambda (x)
			     (if (and (len>1? x)
				      (memq (car x) '(inexact exact->inexact)))
				 (cadr x)
				 x))
			   val))
	    (if (not (lint-any? (lambda (x)
				  (and (number? x)
				       (inexact? x)))
				val))
		(do ((p val (cdr p)))
		    ((or (null? p)
			 (number? (car p)))
		     (if (pair? p)
			 (set-car! p (* 1.0 (car p))))))))
	  val)

	;; polar notation (@) is never used anywhere except test suites

	(define numerics-table
	  (let ((h (make-hash-table)))

	    (let ((coeffs (make-vector 4 0)))
	      (define (horners-rule form)
		(and (pair? form)
		     (call-with-exit
		      (lambda (return)
			(fill! coeffs 0)
			(do ((p form (cdr p))
			     (top 0)
			     (sym #f))
			    ((not (pair? p))
			     (do ((x (- top 1) (- x 1))
				  (result (vector-ref coeffs top)))
				 ((< x 0)
				  result)
			       (set! result
				      (case (vector-ref coeffs x)
					((0) (list '* sym result))
					((0.0) `(inexact (* ,sym ,result)))
					(else `(+ ,(vector-ref coeffs x) (* ,sym ,result)))))))
			  (let ((cx (car p)))
			    (cond ((number? cx)
				   (vector-set! coeffs 0 (+ (vector-ref coeffs 0) cx)))

				  ((symbol? cx)
				   (if (not sym)
				       (set! sym cx)
				       (if (not (eq? sym cx))
					   (return #f)))
				   (set! top (max top 1))
				   (vector-set! coeffs 1 (+ (vector-ref coeffs 1) 1)))

				  ((not (and (pair? cx)
					     (eq? (car cx) '*)))
				   (return #f))

				  (else
				   (let ((ctr 0)
					 (ax 1))
				     (for-each (lambda (qx)
						 (if (symbol? qx)
						     (if (not sym)
							 (begin
							   (set! sym qx)
							   (set! ctr 1))
							 (if (not (eq? sym qx))
							     (return #f)
							     (set! ctr (+ ctr 1))))
						     (if (number? qx)
							 (set! ax (* ax qx))
							 (return #f))))
					       (cdr cx))
				     (if (>= ctr (length coeffs))
					 (set! coeffs (copy coeffs (make-vector (* ctr 2) 0))))
				     (set! top (max top ctr))
				     (vector-set! coeffs ctr (+ (vector-ref coeffs ctr) ax)))))))))))

	      (define (num+ args form env)
		(case (length args)
		  ((0))
		  ((1) (car args))
		  (else
		   (let ((val (remove-all 0 (splice-if '+ args))))
		     (if (lint-every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			 (let ((rats (collect-if-rational val)))
			   (if (len>1? rats)
			       (let ((y (apply + rats)))
				 (set! val (if (zero? y)
					       (collect-if-not-number val)
					       (cons y (collect-if-not-number val))))))))
		     (set! val (remove-inexactions val))
		     (if (lint-any? (lambda (p)        ; collect all + and - vals -> (- (+ ...) ...)
				      (and (pair? p)
					   (eq? (car p) '-)))
				    val)
			 (let ((plus ())
			       (minus ())
			       (c 0))
			   (for-each (lambda (p)
				       (if (not (and (pair? p)
						     (eq? (car p) '-)))
					   (if (rational? p)
					       (set! c (+ c p))
					       (set! plus (cons p plus)))
					   (if (pair? (cdr p)) ; drop (-)?
					       (if (null? (cddr p))
						   (if (rational? (cadr p))
						       (set! c (- c (cadr p)))
						       (if (and (pair? (cadr p))
								(eq? (caadr p) '+))
							   (set! minus (append (cdadr p) minus))
							   (set! minus (cons (cadr p) minus))))
						   (begin
						     (if (rational? (cadr p))
							 (set! c (+ c (cadr p)))
							 (set! plus (cons (cadr p) plus)))
						     (for-each (lambda (p1)
								 (if (rational? p1)
								     (set! c (- c p1))
								     (set! minus (cons p1 minus))))
							       (cddr p)))))))
				     val)

			   (if (not (rational? c)) ; overflow: (+ 9223372036854775806 x (- 12 x))?
			       (cons '+ val)
			       (begin
				 ;; (+ (- x 1) 1000): plus '(x), minus (), c 999 -> (+ x 999)
				 ;; (+ (- 1 x) 1000):       ()         '(x), 1001 -> (- 1001 x)
				 ;; (+ (- x y 1) 2 z)      '(z x)      '(y), 1 -> (- (+ x z 1) y)
				 (when (and (pair? plus)
					    (pair? minus))
				   (do ((p plus (cdr p))
					(np ()))
				       ((null? p)
					(set! plus (reverse np)))
				     (if (and (member (car p) minus)            ; not just member because it matches (random...) etc
					      (not (side-effect? (car p) env)))
					 (set! minus (remove-one (car p) minus))
					 (set! np (cons (car p) np)))))

				 ;; perhaps compare tree-length of the new and old versions, and choose the smaller?
				 (let ((new-form
					(if (null? minus)
					    (simplify-numerics `(+ ,@(reverse plus) ,c) env)
					    (if (null? plus)
						(simplify-numerics `(- ,c ,@(reverse minus)) env)
						(simplify-numerics `(- (+ ,@(reverse plus) ,@(if (positive? c) (list c) ()))
								       ,@(reverse minus) ,@(if (negative? c) (list (abs c)) ()))
								   env)))))
				   (if (> (+ (tree-leaves val) 1) (tree-leaves new-form))
				       new-form
				       (cons '+ val))))))  ; to (let ((plus)... above

			 (case (length val)
			   ((0))                                        ; (+) -> 0
			   ((1) (car val))                              ; (+ x) -> x
			   ((2)
			    (let ((arg1 (car val))
				  (arg2 (cadr val)))
			      (cond ((and (real? arg2)                  ; (+ x -1) -> (- x 1)
					  (negative? arg2)
					  (not (number? arg1))
					  (not (= arg2 (*s7* 'most-negative-fixnum)))) ; abs here is now an error
				     (list '- arg1 (abs arg2)))

				    ((and (real? arg1)                  ; (+ -1 x) -> (- x 1)
					  (negative? arg1)
					  (not (number? arg2))
					  (not (= arg1 (*s7* 'most-negative-fixnum))))
				     (list '- arg2 (abs arg1)))

				    ((and (pair? arg1)                  ; (+ (if x 0 y) z) -> (if x z (+ y z))
					  (eq? (car arg1) 'if)
					  (= (length arg1) 4))
				     (if (and (pair? arg2)
					      (eq? (car arg2) 'if))
					 (cons '+ val)
					 (let ((true (caddr arg1))
					       (false (cadddr arg1)))
					   (if (eqv? true 0) ; does not include 0.0
					       (if (eqv? false 0)
						   arg2
						   `(if ,(cadr arg1) ,arg2 (+ ,false ,arg2)))
					       (if (eqv? false 0)
						   `(if ,(cadr arg1) (+ ,true ,arg2) ,arg2)
						   (cons '+ val))))))

				    ((and (pair? arg2)                   ; (+ z (if x 0 y)) -> (if x z (+ z y))
					  (eq? (car arg2) 'if)
					  (= (length arg2) 4))
				     (let ((true (caddr arg2))
					   (false (cadddr arg2)))
				       (if (eqv? true 0) ; does not include 0.0
					   (if (eqv? false 0)
					       arg1
					       `(if ,(cadr arg2) ,arg1 (+ ,arg1 ,false)))
					   (if (eqv? false 0)
					       `(if ,(cadr arg2) (+ ,arg1 ,true) ,arg1)
					       (cons '+ val)))))

				    ((not (and (pair? arg1)
					       (pair? arg2)))
				     (cons '+ val))

				    ((and (eq? (car arg1) '*)           ; (+ (* a b) (* a c)) -> (* a (+ b c))
					  (eq? (car arg2) '*)
					  (lint-any? (lambda (a)
						       (member a (cdr arg2)))
						     (cdr arg1)))
				     (do ((times ())
					  (pluses ())
					  (rset (cdr arg2))
					  (p (cdr arg1) (cdr p)))
					 ((null? p)
					  ;; times won't be () because we checked above for a match
					  ;;  if pluses is (), arg1 is completely included in arg2
					  ;;  if rset is (), arg2 is included in arg1
					  (simplify-numerics `(* ,@(reverse times)
								 (+ (* ,@(reverse (if (pair? pluses) pluses (list (if (null? pluses) 1 pluses)))))
								    (* ,@rset)))
							     env))
				       (if (member (car p) rset)
					   (begin
					     (set! times (cons (car p) times))
					     (set! rset (remove-one (car p) rset)))
					   (set! pluses (cons (car p) pluses)))))

				    ((and (eq? (car arg1) '/)  ; (+ (/ a b) (/ c b)) -> (/ (+ a c) b)
					  (eq? (car arg2) '/)
					  (pair? (cddr arg1))
					  (pair? (cddr arg2))
					  (equal? (cddr arg1) (cddr arg2)))
				     (cons '/ (cons (list '+ (cadr arg1) (cadr arg2)) (cddr arg1))))

				    ((and (len=2? arg1)         ; (+ (log a) (log b)) -> (log (* a b))
					  (eq? (car arg1) 'log)
					  (len=2? arg2)
					  (eq? (car arg2) 'log))
				     (list 'log (list '* (cadr arg1) (cadr arg2))))

				    (else (cons '+ val)))))
			   (else
			    (or (horners-rule val)
				;; not many cases here, oddly enough, Horner's rule gets most
				(cons '+ val)))))))))
	      (hash-table-set! h '+ num+)

	      (define (dumb+ args form env) (if (var-member (car form) env) form (num+ (undumb args) #f env))) ; not (undone form) because num+ ignores arg2
	      (for-each (lambda (f) (hash-table-set! h f dumb+)) '(fix:+ fx+ flo:+ fl+ bignum+)))

	    (let ()
	      (define (num* args form env)
		(case (length args)
		  ((0) 1)
		  ((1) (car args))
		  (else
		   (let ((val (remove-all 1 (splice-if '* args))))
		     (if (lint-every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			 (let ((rats (collect-if-rational val)))
			   (if (len>1? rats)
			       (let ((y (apply * rats)))
				 (set! val (if (= y 1)
					       (collect-if-not-number val)
					       (cons y (collect-if-not-number val))))))))
		     (set! val (remove-inexactions val))

		     (if (lint-any? (lambda (p)
				      (and (pair? p)
					   (eq? (car p) '/)))
				    val)
			 (let ((plus ())
			       (minus ())
			       (c 1))
			   (for-each (lambda (p)
				       (if (not (and (pair? p)
						     (eq? (car p) '/)))
					   (if (rational? p)
					       (set! c (* c p))
					       (set! plus (cons p plus)))
					   (if (pair? (cdr p)) ; drop (/)?
					       (if (null? (cddr p))
						   (if (rational? (cadr p))
						       (set! c (/ c (cadr p)))
						       (set! minus (cons (cadr p) minus)))
						   (begin
						     (if (rational? (cadr p))
							 (set! c (* c (cadr p)))
							 (set! plus (cons (cadr p) plus)))
						     (for-each (lambda (p1)
								 (if (rational? p1)
								     (set! c (/ c p1))
								     (set! minus (cons p1 minus))))
							       (cddr p)))))))
				     val)
			   (if (not (rational? c))
			       (cons '* val)
			       (begin
				 (when (and (pair? plus)
					    (pair? minus))
				   (do ((p plus (cdr p))
					(np ()))
				       ((null? p)
					(set! plus (reverse np)))
				     (if (and (member (car p) minus)            ; not just member because it matches (random...) etc
					      (not (side-effect? (car p) env)))
					 (set! minus (remove-one (car p) minus))
					 (set! np (cons (car p) np)))))

				 (let ((new-form
					(if (null? minus)
					    (if (null? plus)                      ; (* (/ x y) (/ y x)) -> 1
						c
						(if (and (eqv? c 1)
							 (null? (cdr plus)))
						    (car plus)                    ; (* x y (/ y)) -> x
						    (simplify-numerics `(* ,@(if (eqv? c 1) () (list c))
									   ,@(reverse plus))
								       env)))     ; (* x (/ y 2) 2 z) -> (* x y z)
					    (if (null? plus)
						(if (and (eqv? c 1)
							 (null? (cdr minus)))
						    `(/ ,(car minus))             ; (* (/ x) (x x y)) -> (/ y)
						    (simplify-numerics `(/ ,c ,@(reverse minus)) env)) ; (* (/ x) (/ y) (/ z) -> (/ 1 x y z)
						(simplify-numerics `(/ ,@(if (and (eqv? c 1)
										  (null? (cdr plus)))
									     plus
									     (list (cons '* (if (eqv? c 1)
												(reverse plus)
												(cons c (reverse plus))))))
								       ,@(reverse minus))
								   env))))) ; to (let ((plus)... above
				   (if (> (+ (tree-leaves val) 1) (tree-leaves new-form))
				       new-form
				       (cons '* val))))))  ; to (let ((plus)... above

			 (case (length val)
			   ((0) 1)
			   ((1) (car val))                         ; (* x) -> x
			   ((2)
			    (let ((arg1 (car val))
				  (arg2 (cadr val)))
			      (cond ((just-rationals? val)
				     (let ((new-val (apply * val))) ; huge numbers here are less readable
				       (if (< (abs new-val) 1000000)
					   new-val
					   (cons '* val))))

				    ((memv 0 val)                         ; (* x 0) -> 0
				     0)
				    ((memv -1 val)
				     (cons '- (remove-one -1 val)))       ; (* -1 x) -> (- x)

				    ((and (pair? arg1)                    ; (* (if x 1 y) z) -> (if x z (* y z))
					  (eq? (car arg1) 'if)            ; (* (if x 0 y) z) -> (if x 0 (* y z))
					  (= (length arg1) 4))
				     (if (and (pair? arg2)
					      (eq? (car arg2) 'if))
					 (cons '* val)
					 (let ((true (caddr arg1))
					       (false (cadddr arg1)))
					   (if (memv true '(0 1)) ; does not include 0.0
					       `(if ,(cadr arg1) ,(if (eqv? true 1) arg2 0) (* ,false ,arg2))
					       (if (memv false '(0 1))
						   `(if ,(cadr arg1) (* ,true ,arg2) ,(if (eqv? false 1) arg2 0))
						   (cons '* val))))))

				    ((and (pair? arg2)                   ; (* z (if x 1 y)) -> (if x z (* z y))
					  (eq? (car arg2) 'if)
					  (= (length arg2) 4))
				     (let ((true (caddr arg2))
					   (false (cadddr arg2)))
				       (if (memv true '(0 1)) ; does not include 0.0
					   `(if ,(cadr arg2) ,(if (eqv? true 1) arg1 0) (* ,arg1 ,false))
					   (if (memv false '(0 1))
					       `(if ,(cadr arg2) (* ,arg1 ,true) ,(if (eqv? false 1) arg1 0))
					       (cons '* val)))))

				    ((or (equal? arg2 (list '/ arg1))        ; (* a (/ a)) -> 1
					 (equal? arg2 (list '/ 1 arg1)))
				     1)
				    ((or (equal? arg1 (list '/ arg2))        ; (* (/ a) a) -> 1
					 (equal? arg1 (list '/ 1 arg2)))
				     1)

				    ((not (pair? arg2))
				     (cons '* val))

				    ((pair? arg1)
				     (let ((op1 (car arg1))
					   (op2 (car arg2)))
				       (cond ((and (eq? op1 '-)           ; (* (- x) (- y)) -> (* x y)
						   (null? (cddr arg1))
						   (eq? op2 '-)
						   (null? (cddr arg2)))
					      (list '* (cadr arg1) (cadr arg2)))

					     ((and (eq? op1 '/)           ; (* (/ x) (/ y)) -> (/ (* x y)) etc
						   (eq? op2 '/))
					      (let ((op1-arg1 (cadr arg1))
						    (op2-arg1 (cadr arg2)))
						(if (null? (cddr arg1))
						    (if (null? (cddr arg2))
							(list '/ (list '* op1-arg1 op2-arg1))
							(if (equal? op1-arg1 op2-arg1)
							    (list '/ (caddr arg2))
							    (simplify-numerics `(/ ,op2-arg1 (* ,op1-arg1 ,(caddr arg2))) env)))
						    (if (null? (cddr arg2))
							(if (equal? op1-arg1 op2-arg1)
							    (list '/ (caddr arg1))
							    (simplify-numerics `(/ ,op1-arg1 (* ,(caddr arg1) ,op2-arg1)) env))
							(simplify-numerics `(/ (* ,op1-arg1 ,op2-arg1) (* ,@(cddr arg1) ,@(cddr arg2))) env)))))

					     ((and (= (length arg1) 3)
						   (equal? (cdr arg1) (cdr arg2))
						   (case op1
						     ((gcd) (eq? op2 'lcm))
						     ((lcm) (eq? op2 'gcd))
						     (else #f)))
					      (list 'abs (cons '* (cdr arg1))))    ; (* (gcd a b) (lcm a b)) -> (abs (* a b)) but only if 2 args?

					     ((and (eq? op1 'exp)         ; (* (exp a) (exp b)) -> (exp (+ a b))
						   (eq? op2 'exp))
					      (list 'exp (list '+ (cadr arg1) (cadr arg2))))

					     ((and (eq? op1 'sqrt)                ; (* (sqrt x) (sqrt y)) -> (sqrt (* x y))??
						   (eq? op2 'sqrt))
					      (list 'sqrt (list '* (cadr arg1) (cadr arg2))))

					     ((not (and (eq? op1 'expt) (eq? op2 'expt)))
					      (cons '* val))

					     ((equal? (cadr arg1) (cadr arg2)) ; (* (expt x y) (expt x z)) -> (expt x (+ y z))
					      (list 'expt (cadr arg1) (list '+ (caddr arg1) (caddr arg2))))

					     ((equal? (caddr arg1) (caddr arg2)) ; (* (expt x y) (expt z y)) -> (expt (* x z) y)
					      (list 'expt (list '* (cadr arg1) (cadr arg2)) (caddr arg1)))

					     (else (cons '* val)))))

				    ((and (eq? (car arg2) '/) (null? (cddr arg2))) ; (* a (/ b)) -> (/ a b)
				     (list '/ arg1 (cadr arg2)))

				    ((and (number? arg1)                  ; (* 2 (random 3.0)) -> (random 6.0) [except for (random 1)...]
					  (eq? (car arg2) 'random)
					  (pair? (cdr arg2))
					  (number? (cadr arg2))
					  (not (rational? (cadr arg2))))
				     (list 'random (* arg1 (cadr arg2))))

				    (else (cons '* val)))))
			   (else
			    (cond ((just-rationals? val)
				   (let ((new-val (apply * val))) ; huge numbers here are less readable
				     (if (< (abs new-val) 1000000)
					 new-val
					 (cons '* val))))

				  ((memv 0 val)                   ; (* x 0 2) -> 0
				   0)

				  ((memv -1 val)
				   (list '- (cons '* (remove-one -1 val))))    ; (* -1 x y) -> (- (* x y))

				  ((let search ((args val))       ; (* x (if y 0 z) w) -> (if y 0 (* x z w))
				     (and (pair? args)
					  (let ((has-zero (and (pair? (car args))
							       (eq? (caar args) 'if)
							       (= (length (car args)) 4)
							       (or (eqv? (caddar args) 0)
								   (eqv? (car (cdddar args)) 0))
							       (car args))))
					    (or has-zero
						(search (cdr args))))))
				   => (lambda (gif)
					(let ((other-args (remove-one gif val)))
					  (list 'if (cadr gif)
						(if (eqv? (caddr gif) 0) 0 (cons '* (cons (caddr gif) other-args)))
						(if (eqv? (cadddr gif) 0) 0 (cons '* (cons (cadddr gif) other-args)))))))

				  ((lint-any? (lambda (p)              ; collect * and / vals -> (/ (* ...) ...)
						(and (pair? p)
						     (eq? (car p) '/)))
					      val)
				   (let ((mul ())
					 (div ()))
				     (for-each (lambda (p)
						 (if (not (and (len>1? p)
							       (eq? (car p) '/)))
						     (set! mul (cons p mul))
						     (if (null? (cddr p))
							 (set! div (cons (cadr p) div))
							 (begin
							   (set! mul (cons (cadr p) mul))
							   (set! div (append (cddr p) div))))))
					       val)
				     (for-each (lambda (n)
						 (when (member n div)
						   (set! div (remove-one n div))
						   (set! mul (remove-one n mul))))
					       (copy mul))
				     (let ((expr (if (null? mul)
						     (if (null? div)
							 (list '*)       ; for simplify-numerics' benefit
							 (cons '/ (cons 1 (reverse div))))
						     (if (null? div)
							 (cons '* (reverse mul))
							 `(/ (* ,@(reverse mul)) ,@(reverse div))))))
				       (if (equivalent? expr form) ; possible NaN
					   form
					   (simplify-numerics expr env)))))

				  (else (cons '* val))))))))))
	      (hash-table-set! h '* num*)

	      (define (dumb* args form env) (if (var-member (car form) env) form (num* (undumb args) form env)))
	      (for-each (lambda (f) (hash-table-set! h f dumb*)) '(fix:* fx* flo:* fl* bignum*)))

	    (let ()
	      (define (num- args form env)
		(let ((args (remove-inexactions args)))
		  (case (length args)
		    ((0) form)
		    ((1) ; negate
		     (if (number? (car args))
			 (- (car args))
			 (if (not (list? (car args)))
			     (cons '- args)
			     (case (length (car args))
			       ((2) (if (eq? (caar args) '-)
					(cadar args)                 ; (- (- x)) -> x
					(let ((arg2 (car args)))
					  (if (and (eq? (car arg2) 'floor) ; (- (floor (- x))) -> (ceiling x)
						   (pair? (cadr arg2))
						   (eq? (caadr arg2) '-)
						   (pair? (cdadr arg2))
						   (null? (cddadr arg2)))
					      `(ceiling ,(cadadr arg2))
					      (cons '- args)))))
			       ((3) (if (eq? (caar args) '-)
					(list '- (caddar args) (cadar args)) ; (- (- x y)) -> (- y x)
					(cons '- args)))
			       (else (cons '- args))))))
		    (else
		     (if (just-rationals? args)
			 (apply - args)
			 (let ((val (remove-all 0 (splice-if '+ (cdr args)))))
			   (if (lint-every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			       (let ((rats (collect-if-rational val)))
				 (if (len>1? rats)
				     (let ((y (apply + rats)))
				       (set! val (if (zero? y)
						     (collect-if-not-number val)
						     (cons y (collect-if-not-number val))))))))
			   (let ((first-arg (car args))
				 (nargs val))

			     (when (and (member first-arg nargs)
					(not (side-effect? first-arg env)))
			       (set! nargs (remove-one first-arg nargs))
			       (set! first-arg 0))

			     (let ((plus ())
				   (minus ())
				   (c 0))

			       (if (not (pair? first-arg))
				   (set! plus (list first-arg))
				   (case (car first-arg)
				     ((+) (set! plus (cdr first-arg)))
				     ((-)
				      (if (null? (cddr first-arg))
					  (set! minus (list (cadr first-arg)))
					  (begin
					    (set! plus (list (cadr first-arg)))
					    (set! minus (cddr first-arg)))))
				     (else (set! plus (list first-arg)))))

			       (for-each
				(lambda (arg)
				  (if (not (pair? arg))
				      (set! minus (cons arg minus))
				      (case (car arg)
					((+) (set! minus (append (cdr arg) minus)))
					((-)
					 (if (null? (cddr arg))
					     (set! plus (cons (cadr arg) plus))
					     (begin
					       (set! minus (cons (cadr arg) minus))
					       (set! plus (append (cddr arg) plus)))))
					(else (set! minus (cons arg minus))))))
				nargs)

			       (let ((new-minus ())
				     (new-plus ()))
				 (for-each
				  (lambda (arg)
				    (if (and (member arg plus)
					     (not (side-effect? arg env)))
					(set! plus (remove-one arg plus))
					(if (rational? arg)
					    (set! c (- c arg))
					    (set! new-minus (cons arg new-minus)))))
				  minus)

				 (for-each
				  (lambda (arg)
				    (if (rational? arg)
					(set! c (+ c arg))
					(set! new-plus (cons arg new-plus))))
				  plus)

				 (if (not (rational? c))
				     (cons '- args)
				     (let ((new-form
				 (if (null? new-plus)
				     (if (null? new-minus)
					 c                                                     ; (- (+ x y) (+ y x)) -> 0
					 (if (positive? c)
					     `(- ,c ,@new-minus)                               ; (- (+ x 1) (+ y x)) -> (- 1 y)
					     (if (negative? c)
						 `(- (+ ,@new-minus ,(abs c)))                 ; (- x (+ y x 1)) -> (- (+ y 1))
						 (if (null? (cdr new-minus))
						     `(- ,(car new-minus))                     ; (- 0 0 x) -> (- x)
						     `(- (+ ,@new-minus))))))                  ; (- 0 0 x y) -> (- (+ x y))
				     (if (null? new-minus)
					 (if (zero? c)
					     (if (null? (cdr new-plus))
						 (car new-plus)                                ; (- (+ x 1) (- x y) 1) -> y
						 `(+ ,@new-plus))                              ; (- (+ x z) (- x y) 0) -> (+ z y)
					     (if (and (negative? c)
						      (null? (cdr new-plus)))
						 `(- ,(car new-plus) ,(abs c))                 ; (- (+ x 1) (- x y) 2) -> (- y 1)
						 `(+ ,@new-plus ,c)))                          ; (- (+ x 1) (- x y) -2) -> (+ y 3)
					 (if (positive? c)
					     `(- (+ ,@new-plus ,c) ,@new-minus)                ; (- (+ x z 2) x y 1) -> (- (+ z 1) y)
					     (if (negative? c)
						 (if (null? (cdr new-plus))
						     `(- ,(car new-plus) ,@new-minus ,(abs c)) ; (- (+ x z) x y 1) -> (- z y 1)
						     `(- (+ ,@new-plus) ,@new-minus ,(abs c))) ; (- (+ x z w) x y 1) -> (- (+ w z) y 1)
						 (if (null? (cdr new-plus))
						     `(- ,(car new-plus) ,@new-minus)          ; (- (+ x z) x y) -> (- z y)
						     `(- (+ ,@new-plus) ,@new-minus))))))))    ; (- (+ x z w) x y) -> (- (+ w z) y)

				       (if (len=3? new-form)
					   (let ((arg1 (cadr new-form))
						 (arg2 (caddr new-form)))
					     (cond
					      ((and (len>1? arg2)                     ; (- x (truncate x)) -> (remainder x 1)
						    (eq? (car arg2) 'truncate)
						    (equal? arg1 (cadr arg2)))
					       (list 'remainder arg1 1))

					      ((and (len>2? arg1)
						    (len>2? arg2)
						    (eq? (car arg1) '/)               ; (- (/ a b) (/ c b)) -> (/ (- a c) b)
						    (eq? (car arg2) '/)
						    (equal? (cddr arg1) (cddr arg2)))
					       (cons '/ (cons (list '- (cadr arg1) (cadr arg2)) (cddr arg1))))

					      ((and (len=3? arg1)
						    (len=3? arg2)
						    (eq? (car arg1) '*)               ; (- (* a b) (* a c)) -> (* a (- b c))
						    (eq? (car arg2) '*)               ; (- (* b a) (* c a)) -> (* a (- b c)) etc
						    (or (member (cadr arg1) arg2)
							(member (caddr arg1) arg2)))
					       (if (member (cadr arg1) arg2)
						   (list '* (cadr arg1) (list '- (caddr arg1) ((if (equal? (cadr arg1) (cadr arg2)) caddr cadr) arg2)))
						   (list '* (caddr arg1) (list '- (cadr arg1) ((if (equal? (caddr arg1) (cadr arg2)) caddr cadr) arg2)))))

					      ((and (len=2? arg1)                     ; (- (log a) (log b)) -> (log (/ a b))
						    (eq? (car arg1) 'log)
						    (len=2? arg2)
						    (eq? (car arg2) 'log))
					       (list 'log (list '/ (cadr arg1) (cadr arg2))))

					      ((and (pair? arg2)                      ; (- x (if y 0 z)) -> (if y x (- x z))
						    (eq? (car arg2) 'if)              ; (- x (if y z 0)) -> (if y (- x z) x)
						    (= (length arg2) 4)
						    (or (eqv? (caddr arg2) 0)
							(eqv? (cadddr arg2) 0)))
					       (let ((true (caddr arg2))
						     (false (cadddr arg2)))
						 `(if ,(cadr arg2)
						      ,(if (eqv? true 0) arg1 (list '- arg1 true))
						      ,(if (eqv? true 0) (list '- arg1 false) arg1))))

					      ((and (len=3? arg2)                     ; (- x (* y (quotient x y))) or reversed -> (remainder x y)
						    (eq? (car arg2) '*)
						    (or (and (len=3? (caddr arg2))    ; arg2 here is (* y (quotient x y)), arg1 is x
							     (eq? (caaddr arg2) 'quotient)
							     (equal? arg1 (cadr (caddr arg2)))
							     (equal? (cadr arg2) (caddr (caddr arg2)))
							     `(remainder ,arg1 ,(cadr arg2)))
							(and (len=3? (cadr arg2))     ; arg2 here is (* (quotient x y) y), arg1 is x
							     (eq? (caadr arg2) 'quotient)
							     (equal? arg1 (cadadr arg2))
							     (equal? (caddr arg2) (caddr (cadr arg2)))
							     `(remainder ,arg1 ,(caddr arg2))))))

					      (else
					       (if (> (tree-leaves form) (tree-leaves new-form))
						   new-form
						   form))))

					   (if (or (eqv? c 0)
						   (> (tree-leaves form) (tree-leaves new-form)))
					       new-form
					       form)))))))))))))
	      (hash-table-set! h '- num-)

	      (define (dumb- args form env) (if (var-member (car form) env) form (num- (undumb args) (undumb form) env)))
	      (for-each (lambda (f) (hash-table-set! h f dumb-)) '(fix:- fx- flo:- fl- bignum-)))


	    (let ()
	      (define (num/ args form env)
		(let* ((args (remove-inexactions args))
		       (arg1 (and (pair? args) (car args))))
		  (case (length args)
		    ((0) form)
		    ((1) ; invert
		     (if (number? arg1)
			 (if (zero? arg1)
			     (list '/ arg1)
			     (/ arg1))
			 (if (not (pair? arg1))
			     (cons '/ args)
			     (case (car arg1)
			       ((/)
				(case (length arg1)
				  ((1) form) ; (/)?
				  ((2)                         ; (/ (/ x)) -> x
				   (cadr arg1))
				  ((3)                         ; (/ (/ z x)) -> (/ x z)
				   (cons '/ (reverse (cdr arg1))))
				  (else
				   (if (eqv? (cadr arg1) 1)
				       (cons '* (cddr arg1))   ; (/ (/ 1 x y)) -> (* x y)
				       `(/ (* ,@(cddr arg1)) ,(cadr arg1)))))) ; (/ (/ z x y)) -> (/ (* x y) z)
			       ((expt)                         ; (/ (expt x y)) -> (expt x (- y))
				(if (len=2? (cdr arg1))
				    (list 'expt (cadr arg1) (list '- (caddr arg1)))
				    (cons '/ args)))
			       ((exp)                          ; (/ (exp x)) -> (exp (- x))
				(if (len>1? arg1)
				    (list 'exp (list '- (cadr arg1)))
				    (cons '/ args)))
			       (else (cons '/ args))))))
		    ((2)
		     (if (and (just-rationals? args)
			      (not (zero? (cadr args))))
			 (apply / args)                         ; including (/ 0 12) -> 0
			 (let ((arg2 (cadr args)))
			   (let ((op1 (and (pair? arg1) (car arg1)))
				 (op2 (and (pair? arg2) (car arg2))))
			     (let ((op1-arg1 (and op1 (pair? (cdr arg1)) (cadr arg1)))
				   (op2-arg1 (and op2 (pair? (cdr arg2)) (cadr arg2))))
			       (cond ((eqv? arg1 1)                  ; (/ 1 x) -> (/ x)
				      (simplify-numerics (list '/ arg2) env))

				     ((eqv? arg2 1)                  ; (/ x 1) -> x
				      arg1)

				     ((memv arg1 '(0 0.0))           ; (/ 0 x) -> 0 -- this used to worry that arg2 might be 0 or NaN, but the others don't
				      (if (memv arg2 '(0 0.0))
					  form                       ; not sure what they intended by (/ 0 0)
					  arg1))

				     ((eqv? arg1 arg2)               ; (/ x x) -> 1 ignoring infs/NaNs
				      1)

				     ((or (and (len=2? arg2)         ; (/ (- x) x) -> -1
					       (eq? op2 '-)
					       (eqv? arg1 (cadr arg2)))
					  (and (len=2? arg1)
					       (eq? op1 '-)
					       (eqv? (cadr arg1) arg2)))
				      -1)

				     ((and (len>2? arg1)             ; (/ (/ a b) c) -> (/ a b c)
					   (eq? op1 '/)
					   (not (and (pair? arg2)
						     (eq? op2 '/))))
				      `(/ ,op1-arg1 ,@(cddr arg1) ,arg2))

				     ((and (pair? arg1)              ; (/ (/ a) (/ b)) -> (/ b a)??
					   (eq? op1 '/)
					   (pair? arg2)
					   (eq? '/ op2))
				      (let ((a1 (if (null? (cddr arg1)) (list 1 op1-arg1) (cdr arg1)))
					    (a2 (if (null? (cddr arg2)) (list 1 op2-arg1) (cdr arg2))))
					(simplify-numerics `(/ (* ,(car a1) ,@(cdr a2)) (* ,@(cdr a1) ,(car a2))) env)))

				     ((and (pair? arg2)
					   (eq? op2 '*)
					   (not (side-effect? arg1 env))
					   (member arg1 (cdr arg2)))
				      (let ((n (remove-one arg1 (cdr arg2))))
					(cons '/ (if (len=1? n)
						     n                ; (/ x (* y x)) -> (/ y)
					             (cons 1 n)))))   ; (/ x (* y x z)) -> (/ 1 y z)

				     ((and (len>1? arg2)              ; (/ c (/ a b)) -> (/ (* c b) a)
					   (eq? op2 '/))
				      (cond ((null? (cddr arg2))
					     (list '* arg1 op2-arg1)) ; ignoring divide by zero here (/ x (/ y)) -> (* x y)
					    ((memv op2-arg1 '(0 0.0))
					     (cons 'a args))          ; same: (/ x (/ 0 ...)) -- give up
					    ((eqv? op2-arg1 1)
					     (cons '* (cons arg1 (cddr arg2))))  ; (/ x (/ 1 y z)) -> (* x y z) -- these never actually happen
					    ((not (pair? (cddr arg2)))
					     (cons '/ args))          ; no idea...
					    ((and (rational? arg1)
						  (rational? op2-arg1)
						  (null? (cdddr arg2)))
					     (let ((val (/ arg1 op2-arg1)))
					       (case val
						 ((1)  (caddr arg2))
						 ((-1) (list '- (caddr arg2)))
						 (else (list '* val (caddr arg2))))))
					    (else `(/ (* ,arg1 ,@(cddr arg2)) ,op2-arg1))))

				     ((and (len=2? arg1)             ; (/ (log x) (log y)) -> (log x y) -- (log number) for (log y) never happens
					   (len=2? arg2)
					   (case op1
					     ((log) (eq? op2 'log))
					     ((sin)
					      (and (eq? op2 'cos)
						   (equal? op1-arg1 op2-arg1)))
					     (else #f)))
				      (if (eq? op1 'log)
					  (list 'log op1-arg1 op2-arg1)
					  (list 'tan op1-arg1)))

				     ((and (eq? op1 'sqrt)           ; (/ (sqrt x) (sqrt y)) -> (sqrt (/ x y))
					   (eq? op2 'sqrt))
				      (list 'sqrt (list '/ (cadr arg1) (cadr arg2))))

				     ((and (eq? op1 'exp)            ; (/ (exp x) (exp y)) -> (exp (- x y))
					   (eq? op2 'exp))
				      (list 'exp (list '- (cadr arg1) (cadr arg2))))

				     ((and (eq? op1 'expt)            ; (/ (expt a x) (expt a y)) -> (expt a (- x y))
					   (eq? op2 'expt)
					   (equal? (cadr arg1) (cadr arg2)))
				      (list 'expt (cadr arg1) (list '- (caddr arg1) (caddr arg2))))

				     ((and (len=2? arg1)             ; (/ (- x) (- y)) -> (/ x y)
					   (len=2? arg2)
					   (eq? op1 '-)
					   (eq? op2 '-))
				      (list '/ op1-arg1 op2-arg1))

				     ((and (len=3? arg1)
					   (eq? op1 '-)
					   (len=3? arg2)
					   (memq op2 '(+ -))
					   (len=3? (cadr arg1))
					   (eq? (caadr arg1) '*)
					   (equal? (cadadr arg1) (cadr(cdadr arg1)))
					   (len=3? (caddr arg1))
					   (eq? (caaddr arg1) '*)
					   (equal? (cadr (caddr arg1)) (caddr (caddr arg1)))
					   (equal? (cadr arg2) (cadadr arg1))
					   (equal? (caddr arg2) (cadr (caddr arg1))))
				      (cons (if (eq? op2 '-) '+ '-) (cdr arg2)))

				     ((and (pair? arg1)             ; (/ (* x y) (* z y)) -> (/ x z)
					   (pair? arg2)
					   (eq? op1 '*)
					   (case op2
					     ((*)
					      (and (= (length arg1) (length arg2) 3)
						   (equal? (caddr arg1) (caddr arg2))))
					     ((log)
					      (cond ((assq 'log (cdr arg1))
						     => (lambda (p)
							  (= (length p) 2)))
						    (else #f)))
					     (else #f))           ; (/ (* 12 (log x)) (log 2)) -> (* 12 (log x 2))
					   (if (eq? op2 '*)
					       (list '/ op1-arg1 op2-arg1)
					       (let ((used-log op2-arg1))
						 (cons '* (map (lambda (p)
								  (if (and used-log
									   (pair? p)
									   (eq? (car p) 'log))
								      (let ((val (list 'log (cadr p) used-log)))
									(set! used-log #f)
									val)
								      p))
								(cdr arg1)))))))

				     ((and (len>1? arg1)           ; (/ (sqrt x) x) -> (/ (sqrt x))
					   (eq? (car arg1) 'sqrt)
					   (equal? (cadr arg1) arg2))
				      (list '/ arg1))

				     ((and (len>1? arg2)           ; (/ x (sqrt x)) -> (sqrt x)
					   (eq? (car arg2) 'sqrt)
					   (equal? (cadr arg2) arg1))
				      arg2)

				     (else (cons '/ args))))))))

		    (else
		     ;; spot check of divides in lg didn't turn up any that need the plus/minus style handling of + et al above
		     (if (memv arg1 '(0 0.0))                      ; (/ 0 x y) -> 0 (to be consistent with other results)
			 arg1
			 (if (and (just-rationals? args)
				  (not (memv 0 (cdr args)))
				  (not (memv 0.0 (cdr args))))
			     (catch #t
			       (lambda ()
				 (apply / args)) ; if no overflow catch we can hit divide by zero here
			       (lambda a form))
			     (let ((nargs                            ; (/ x a (* b 1 c) d) -> (/ x a b c d)
				    (remove-all 1 (splice-if '* (cdr args)))))
			       (if (null? nargs) ; (/ x 1 1) -> x
				   arg1
				   (if (and (member (car args) (cdr args))
					    (not (side-effect? arg1 env)))
				       (let ((n (remove-one arg1 (cdr args))))
					 (cons '/ (if (null? (cdr n))
						      n                ; (/ x y x) -> (/ y)
						      (cons 1 n))))    ; (/ x y x z) -> (/ 1 y z)
				       (cons '/ (cons arg1 nargs)))))))))))
	      (hash-table-set! h '/ num/)

	      (define (dumb/ args form env) (if (var-member (car form) env) form (num/ (undumb args) (undumb form) env)))
	      (for-each (lambda (f) (hash-table-set! h f dumb/)) '(fix:/ fx/ flo:/ fl/ bignum/)))

	    (let ()
	      (define (numtrig args form env)
		(let ((head (car form)))
		  ;; perhaps someday, for amusement:
		  ;;    (sin (acos x)) == (cos (asin x)) == (sqrt (- 1 (expt x 2)))
		  ;;    (asin (cos x)) == (acos (sin x)) == (- (* 1/2 pi) x)
		  ;; also since (for example) sin(x - y) = -sin(y - x) and cos(x - y) = cos(y - x), can other simplifiers swap?
		  (cond ((not (len=1? args))
			 (cons head args))

			((and (len=2? (car args))                 ; (sin (asin x)) -> x
			      (eq? (caar args)
				   (case head
				     ((sin) 'asin)
				     ((cos) 'acos)
				     ((tan) 'atan)
				     ((asin) 'sin)
				     ((acos) 'cos)
				     ((atan) 'tan)
				     ((sinh) 'asinh)
				     ((cosh) 'acosh)
				     ((tanh) 'atanh)
				     ((asinh) 'sinh)
				     ((acosh) 'cosh)
				     ((atanh) 'tanh)
				     ((log) 'exp)
				     ((exp) 'log)
				     (else 'error))))
			 (cadar args))

			((eqv? (car args) 0)                     ; (sin 0) -> 0
			 (case head
			   ((sin asin sinh asinh tan tanh atanh) 0)
			   ((exp cos cosh) 1)
			   (else (cons head args))))

			((and (eq? head 'cos)                    ; (cos (- x)) -> (cos x)
			      (len=2? (car args))
			      (eq? (caar args) '-))
			 (list 'cos (cadar args)))

			((or (eq? (car args) 'pi)                ; (sin pi) -> 0.0
			     (and (len=2? (car args))
				  (eq? (caar args) '-)
				  (eq? (cadar args) 'pi)))
			 (case head
			   ((sin tan) 0.0)
			   ((cos) -1.0)
			   (else (cons head args))))

			((eqv? (car args) 0.0)             ; (sin 0.0) -> 0.0
			 ((symbol->value head (rootlet)) 0.0))

			((and (eq? head 'acos)             ; (acos -1) -> pi
			      (eqv? (car args) -1))
			 'pi)

			((and (eq? head 'exp)              ; (exp (* a (log b))) -> (expt b a)
			      (pair? (car args))
			      (eq? (caar args) '*))
			 (let ((targ (cdar args)))
			   (cond ((not (= (length targ) 2))
				  (cons head args))
				 ((and (len=2? (car targ))
				       (eq? (caar targ) 'log))
				  (list 'expt (cadar targ) (cadr targ)))
				 ((and (len=2? (cadr targ))
				       (eq? (caadr targ) 'log))
				  (list 'expt (cadadr targ) (car targ)))
				 (else (cons head args)))))

			(else (cons head args)))))
	      (for-each
	       (lambda (f)
		 (hash-table-set! h f numtrig))
	       '(sin cos tan asin acos sinh cosh tanh asinh acosh atanh exp)))

	    (let ()
	      (define (numlog args form env)
		(let ((len (length args)))
		  (cond ((not (pair? args)) form)

			((eqv? (car args) 1) 0)      ; (log 1 ...) -> 0

			((and (= len 1)              ; (log (exp x)) -> x
			      (len=2? (car args))
			      (eq? (caar args) 'exp))
			 (cadar args))

			((and (= len 1)
			      (pair? (car args))
			      (eq? (caar args) '/)
			      (or (len=2? (car args))
				  (and (len=3? (car args))
				       (eqv? (cadar args) 1))))
			 (list '- (list 'log (if (len=2? (car args)) (cadar args) (caddar args)))))

			((and (len=2? (car args))     ; (log (sqrt x)) -> (* 1/2 (log x))
			      (eq? (caar args) 'sqrt))
			 `(* 1/2 (log ,(cadar args) ,@(cdr args))))

			((and (len=3? (car args))     ; (log (expt x y)) -> (* y (log x))
			      (eq? (caar args) 'expt))
			 `(* ,(caddar args) (log ,(cadar args) ,@(cdr args))))

			((not (and (= len 2)         ; (log x x) -> 1.0
				   (equal? (car args) (cadr args))))
			 (cons 'log args))

			((integer? (car args)) 1)

			(else 1.0))))
	      (hash-table-set! h 'log numlog))

	    (let ()
	      (define (numintlen args form env)
		(if (and (len=1? (cdr form))
			 (integer? (cadr form)))
		    (integer-length (cadr form))
		    form))
	      (hash-table-set! h 'integer-length numintlen))

	    (let ()
	      (define (numsqrt args form env)
		(cond ((not (pair? args))
		       form)
		      ((and (rational? (car args))
			    (rational? (sqrt (car args)))
			    (= (car args) (sqrt (* (car args) (car args)))))
		       (sqrt (car args))) ; don't collapse (sqrt (* a a)), a=-1 for example, or -1-i -> 1+i whereas 1-i -> 1-i etc
		      ((and (len>1? (car args))
			    (eq? (caar args) 'exp))
		       (list 'exp (list '/ (cadar args) 2))) ; (sqrt (exp x)) -> (exp (/ x 2))
		      (else (cons 'sqrt args))))
	      (hash-table-set! h 'sqrt numsqrt))

	    (let ()
	      (define (numfloor args form env)
		(cond ((not (len=1? args))
		       form)

		      ((number? (car args))
		       (catch #t
			 (lambda () (apply (symbol->value (car form) (rootlet)) args))
			 (lambda any (cons (car form) args))))

		      ((not (len>1? (car args)))
		       (cons (car form) args))

		      ((or (integer-result? (caar args))
			   (and (eq? (caar args) 'random)
				(pair? (cdar args))
				(integer? (cadar args))))
		       (car args))

		      ((and (eq? (car form) 'truncate)  ; (truncate (/ x y)) -> (quotient x y)
			    (pair? (cdr form))
			    (pair? (cadr form))
			    (eq? (caadr form) '/)
			    (len=3? (cadr form)))
		       (cons 'quotient (cdadr form)))
		      ;; if we know arg1 is int, (quotient arg1 -1) -> (- arg1)

		      ((memq (caar args) '(inexact->exact exact))
		       (list (car form) (cadar args)))

		      ((memq (caar args) '(* + / -)) ; maybe extend this list
		       `(,(car form) (,(caar args) ,@(map (lambda (p)
							    (if (and (len=2? p)
								     (memq (car p) '(inexact->exact exact)))
								(cadr p)
								p))
							  (cdar args)))))
		      ((and (eq? (caar args) 'random)
			    (eq? (car form) 'floor)
			    (null? (cddar args))
			    (float? (cadar args))
			    (not (nan? (cadar args)))   ; (floor (random +nan.0))!
			    (= (floor (cadar args)) (cadar args)))
		       (list 'random (floor (cadar args))))

		      (else (cons (car form) args))))

	      (for-each
	       (lambda (f)
		 (hash-table-set! h f numfloor))
	       '(floor round ceiling truncate)))

	    (let ()
	      (define (numabs args form env)
		(cond ((not (len=1? args))
		       form)

		      ((and (pair? (car args))        ; (abs (abs x)) -> (abs x)
			    (hash-table-ref non-negative-ops (caar args)))
		       (car args))

		      ((rational? (car args))
		       (abs (car args)))

		      ((not (pair? (car args)))
		       (cons (car form) args))

		      ((and (memq (caar args) '(modulo random))
			    (= (length (car args)) 3) ; (abs (modulo x 2)) -> (modulo x 2)
			    (real? (caddar args))
			    (positive? (caddar args)))
		       (car args))

		      ((and (eq? (caar args) '-)      ; (abs (- x)) -> (abs x)
			    (len=1? (cdar args)))
		       (list (car form) (cadar args)))

		      ;; make-polar as arg never happens

		      (else (cons (car form) args))))
	      (hash-table-set! h 'abs numabs)
	      (hash-table-set! h 'magnitude numabs))

	    (let ()
	      (define (real-result? op)
		(memq op '(imag-part real-part abs magnitude angle max min exact->inexact inexact modulo remainder quotient lcm gcd)))

	      (define (numimag args form env)
		(if (not (len=1? args))
		    form
		    (if (or (real? (car args))
			    (and (pair? (car args))
				 (real-result? (caar args))))
			0.0
			(cons 'imag-part args))))
	      (hash-table-set! h 'imag-part numimag)

	      (define (numreal args form env)
		(if (not (len=1? args))
		    form
		    (if (or (real? (car args))
			    (and (pair? (car args))
				 (real-result? (caar args))))
			(car args)
			(cons 'real-part args))))
	      (hash-table-set! h 'real-part numreal))

	    (let ()
	      (define (numden args form env)
		(if (not (len=1? args))
		    form
		    (if (or (integer? (car args))
			    (and (pair? (car args))
				 (integer-result? (caar args))))
			1
			(list 'denominator (car args)))))
	      (hash-table-set! h 'denominator numden))

	    (let ()
	      (define (numnum args form env)
		(cond ((not (len=1? args))
		       form)
		      ((or (integer? (car args))
			   (and (pair? (car args))
				(integer-result? (caar args))))
		       (car args))
		      ((rational? (car args))
		       (numerator (car args)))
		      (else (list 'numerator (car args)))))
	      (hash-table-set! h 'numerator numnum))

	    (let ()
	      (define (numran args form env)
		(if (not (and (len=1? args)
			      (number? (car args))))
		    (cons 'random args)
		    (case (car args)
		      ((0 1 -1) 0)
		      ((0.0) 0.0)
		      (else (cons 'random args)))))
	      (hash-table-set! h 'random numran))

	    (let ()
	      (define (numcmplx args form env)
		;; (complex (/ a b) (/ c b)) can't be simplified because a b c might be complex and
		;;    (/ (complex a c) b) would raise an error that a and c were not real (similarly for other such cases)
		(if (and (len=2? args)
			 (memv (cadr args) '(0 0.0))) ; (complex 2 0.0) -> 2 which is dubious...
		    (car args)
		    (cons 'complex args)))
	      (hash-table-set! h 'complex numcmplx)
	      (hash-table-set! h 'make-rectangular numcmplx))

	    (let ()
	      (define (numpol args form env)
		(if (and (len=2? args)
			 (equivalent? (cadr args) 0.0))
		    (car args)
		    (cons 'make-polar args)))
	      (hash-table-set! h 'make-polar numpol))

	    (let ()
	      (define (numrat args form env)
		(let ((len2 (= (length args) 2))
		      (head (car form)))
		  (cond ((just-rationals? args)
			 (catch #t ; catch needed here for things like (ash 2 64)
			   (lambda ()
			     (apply (symbol->value head (rootlet)) args))
			   (lambda ignore
			     (cons head args)))) ; use this form to pick up possible arg changes

			((and (eq? head 'ash)          ; (ash x 0) -> x
			      len2
			      (eqv? (cadr args) 0))
			 (car args))

			((case head
			   ((quotient)                       ; (quotient (remainder x y) y) -> 0
			    (and len2
				 (len=3? (car args))
				 (eq? (caar args) 'remainder)
				 (eqv? (caddar args) (cadr args))))
			   ((ash modulo)                     ; (modulo 0 x) -> 0
			    (and len2 (eqv? (car args) 0)))
			   (else #f))
			 0)

			((and (eq? head 'modulo)       ; (modulo (abs x) y) -> (modulo x y)
			      len2
			      (pair? (car args))
			      (eq? (caar args) 'abs))
			 (list 'modulo (cadar args) (cadr args)))

			(else (cons head args)))))
	      (for-each
	       (lambda (f)
		 (hash-table-set! h f numrat))
	       '(rationalize lognot ash modulo remainder quotient)))

	    (let ()
	      (define (numexpt args form env)
		(cond ((not (len=2? args))
		       form)

		      ((and (eqv? (car args) 0)            ; (expt 0 x) -> 0
			    (not (eqv? (cadr args) 0)))
		       (if (and (integer? (cadr args))
				(negative? (cadr args)))
			   (lint-format "attempt to divide by 0: ~A" 'expt (truncated-list->string form)))
		       0)

		      ((or (and (eqv? (cadr args) 0)       ; (expt x 0) -> 1
				(not (eqv? (car args) 0)))
			   (eqv? (car args) 1))            ; (expt 1 x) -> 1
		       1)

		      ((eqv? (cadr args) 1)                ; (expt x 1) -> x
		       (car args))

		      ((eqv? (cadr args) -1)               ; (expt x -1) -> (/ x)
		       (list '/ (car args)))

		      ((just-rationals? args)              ; (expt 2 3) -> 8
		       (catch #t
			 (lambda ()
			   (let ((val (apply expt args)))
			     (if (and (integer? val)
				      (< (abs val) 1000000))
				 val
				 (cons 'expt args))))
			 (lambda args
			   (cons 'expt args))))

		      ((and (pair? (car args))             ; (expt (expt x y) z) -> (expt x (* y z))
			    (eq? (caar args) 'expt))
		       `(expt ,(cadar args) (* ,(caddar args) ,(cadr args))))

		      ((and (pair? (car args))             ; (expt (exp x) y) -> (exp (* x y))
			    (eq? (caar args) 'exp))
		       `(exp (* ,(cadar args) ,(cadr args))))

		      (else (cons 'expt args))))
	      (hash-table-set! h 'expt numexpt))

	    (let ()
	      (define (numang args form env)
		(cond ((not (pair? args)) form)
		      ((eqv? (car args) -1) 'pi)
		      ((or (equivalent? (car args) 0.0)
			   (eq? (car args) 'pi))
		       0.0)
		      (else (cons 'angle args))))
	      (hash-table-set! h 'angle numang))

	    (let ()
	      (define (numatan args form env)
		(cond ((and (len=1? args)                    ; (atan (x y)) -> (atan x y)
			    (len=3? (car args))
			    (eq? (caar args) '/))
		       (cons 'atan (cdar args)))
		      ((and (len=2? args)                    ; (atan 0 -1) -> pi
			    (eqv? (car args) 0)
			    (eqv? (cadr args) -1))
		       'pi)
		      (else (cons 'atan args))))
	      (hash-table-set! h 'atan numatan))

	    (let ()
	      (define (rational-result? op)
		(memq op '(rationalize inexact->exact exact)))

	      (define (numexact args form env)  ; inexact->exact
		(cond ((not (len=1? args))
		       form)

		      ((or (rational? (car args))
			   (and (pair? (car args))
				(or (rational-result? (caar args))
				    (integer-result? (caar args))
				    (and (eq? (caar args) 'random)
					 (pair? (cdar args))
					 (rational? (cadar args)))))) ; perhaps (exact (random 10.0)) -> (random 10)??
		       (car args))

		      ((number? (car args))
		       (catch #t (lambda () (inexact->exact (car args))) (lambda any (cons (car form) args))))

		      (else (cons (car form) args))))
	      (hash-table-set! h 'inexact->exact numexact)
	      (hash-table-set! h 'exact numexact))

	    (let ()
	      (define (numinexact args form env) ; exact->inexact
		(cond ((not (len=1? args))
		       form)

		      ((number? (car args))
		       (catch #t (lambda () (exact->inexact (car args))) (lambda any (cons (car form) args))))

		      ((and (pair? (car args))  ; this should be expanded
			    (eq? (caar args) 'float-vector-ref))
		       (car args))

		      ((not (and (pair? (car args))
				 (not (eq? (caar args) 'random))
				 (hash-table-ref numeric-ops (caar args))
				 (any-numbers? (cdar args))))
		       (cons (car form) args))

		      ((lint-any? (lambda (x)
				    (and (number? x)
					 (inexact? x)))
				  (cdar args))
		       (car args))

		      (else
		       (let ((new-form (copy (car args))))
			 (do ((p (cdr new-form) (cdr p)))
			     ((or (null? p)
				  (number? (car p)))
			      (if (pair? p)
				  (set-car! p (* 1.0 (car p))))
			      new-form))))))
	      ;; not (inexact (random 3)) -> (random 3.0) because results are different
	      (hash-table-set! h 'exact->inexact numinexact)
	      (hash-table-set! h 'inexact numinexact))

	    (let ()
	      (define (numior args form env)
		(let ((args (lint-remove-duplicates (remove-all 0 (splice-if 'logior args)) env)))
		  (if (lint-every? (lambda (x) (or (not (number? x)) (integer? x))) args)
		      (let ((rats (collect-if-integer args)))
			(if (len>1? rats)
			    (let ((y (apply logior rats)))
			      (set! args (if (zero? y)
					     (collect-if-not-number args)
					     (cons y (collect-if-not-number args))))))))
		  (cond ((null? args) 0)                ; (logior) -> 0
			((null? (cdr args)) (car args)) ; (logior x) -> x
			((memv -1 args) -1)             ; (logior ... -1 ...) -> -1
			((just-integers? args) (apply logior args))
			(else (cons 'logior args)))))
	      (hash-table-set! h 'logior numior)

	      (define (numand args form env)
		(let ((args (lint-remove-duplicates (remove-all -1 (splice-if 'logand args)) env)))
		  (if (lint-every? (lambda (x) (or (not (number? x)) (integer? x))) args)
		      (let ((rats (collect-if-integer args)))
			(if (len>1? rats)
			    (let ((y (apply logand rats)))
			      (set! args (if (= y -1)
					     (collect-if-not-number args)
					     (cons y (collect-if-not-number args))))))))
		  (cond ((null? args) -1)
			((null? (cdr args)) (car args)) ; (logand x) -> x
			((memv 0 args) 0)
			((just-integers? args) (apply logand args))
			(else (cons 'logand args)))))

	      ;; (logand 1 (logior 2 x)) -> (logand 1 x)?
	      ;; (logand 1 (logior 1 x)) -> 1
	      ;; (logand 3 (logior 1 x))?
	      ;; similarly for (logior...(logand...))

	      (hash-table-set! h 'logand numand)

	      (define (numxor args form env)
		(let ((args (splice-if 'logxor args))) ; is this correct??
		  (cond ((null? args) 0)                                    ; (logxor) -> 0
			((null? (cdr args)) (car args))                     ; (logxor x) -> x??
			((just-integers? args) (apply logxor args))         ; (logxor 1 2) -> 3
			((and (len=2? args) (equal? (car args) (cadr args))) 0) ; (logxor x x) -> 0
			(else (cons 'logxor args)))))                           ; (logxor x (logxor y z)) -> (logxor x y z)
	      (hash-table-set! h 'logxor numxor))

	    (let ()
	      (define (numgcd args form env)
		(let ((args (lint-remove-duplicates (splice-if 'gcd args) env)))
		  (cond ((null? args) 0)
			((memv 1 args) 1)
			((just-rationals? args)
			 (catch #t  ; maybe (gcd -9223372036854775808 -9223372036854775808)
			   (lambda ()
			     (apply gcd args))
			   (lambda ignore
			     (cons 'gcd args))))
			((null? (cdr args))   (list 'abs (car args)))
			((eqv? (car args) 0)  (list 'abs (cadr args)))
			((eqv? (cadr args) 0) (list 'abs (car args)))
			(else (cons 'gcd args)))))
	      (hash-table-set! h 'gcd numgcd))

	    (let ()
	      (define (numlcm args form env)
		(let ((args (lint-remove-duplicates (splice-if 'lcm args) env)))
		  (cond ((null? args) 1)         ; (lcm) -> 1
			((memv 0 args) 0)        ; (lcm ... 0 ...) -> 0
			((just-rationals? args)  ; (lcm 3 4) -> 12
			 (catch #t
			   (lambda ()
			     (apply lcm args))
			   (lambda ignore
			     (cons 'lcm args))))
			((null? (cdr args))      ; (lcm x) -> (abs x)
			 (list 'abs (car args)))
			(else (cons 'lcm args)))))
	      (hash-table-set! h 'lcm numlcm))

	    (let ()
	      (define (nummax args form env)
		(if (not (pair? args))
		    form
		    (begin
		      (set! args (lint-remove-duplicates (splice-if (car form) args) env))
		      (if (lint-any? (lambda (p) ; if non-negative-op, remove any non-positive numbers
				       (and (pair? p)
					    (hash-table-ref non-negative-ops (car p))))
				     args)
			  (set! args (remove-if (lambda (x)
						  (and (real? x)
						       (not (positive? x))))
						args)))
		      (if (len=1? args)
			  (car args)
			  (if (and (len>1? args)
				   (just-reals? args))
			      (apply (symbol->value (car form) (rootlet)) args)
			      (let ((nums (collect-if-real args))
				    (other (if (eq? (car form) 'min) 'max 'min)))
				(if (pair? nums)
				    (let ((relop (if (eq? (car form) 'min) >= <=)))
				      (if (pair? (cdr nums))
					  (set! nums (list (apply (symbol->value (car form) (rootlet)) nums))))
				      (let ((new-args (append nums (collect-if-not-number args))))
					(let ((c1 (car nums)))
					  (set! new-args (collect-if (lambda (x)
								       (or (not (pair? x))
									   (<= (length x) 2)
									   (not (eq? (car x) other))
									   (let ((c2 (lint-find-if real? (cdr x))))
									     (or (not c2)
										 (relop c1 c2)))))
								     new-args)))
					(if (< (length new-args) (length args))
					    (set! args new-args)))))            ; might set args to ()?

				;; if (max c1 (min c2 . args1) . args2) where (> c1 c2) -> (max c1 . args2), if = -> c1
				;; if (min c1 (max c2 . args1) . args2) where (< c1 c2) -> (min c1 . args2), if = -> c1
				;;   and if (max 4 x (min x 4)) -- is it (max x 4)?
				;; (max a b) is (- (min (- a) (- b))), but that doesn't help here -- the "-" gets in our way
				;;   (min (- a) (- b)) -> (- (max a b))?
				;; (+ a (max|min b c)) = (max|min (+ a b) (+ a c)))

				(cond ((not (pair? args))  ; something is messed up in the original expression
				       form)
				      ((null? (cdr args))  ; (max (min x 3) (min x 3)) -> (max (min x 3)) -> (min x 3)
				       (car args))
				      ((and (null? (cddr args))   ; (max|min x (min|max x ...) -> x
					    (or (and (pair? (car args))
						     (eq? (caar args) other)
						     (member (cadr args) (car args))
						     (not (side-effect? (cadr args) env)))
						(and (pair? (cadr args))
						     (eq? (caadr args) other)
						     (member (car args) (cadr args))
						     (not (side-effect? (car args) env)))))
				       ((if (len>1? (car args)) cadr car) args))
				      (else (cons (car form) args)))))))))

	      (hash-table-set! h 'max nummax)
	      (hash-table-set! h 'min nummax))
	    h)) ; define numerics-table

	(lambda (form env)                         ; simplify-numerics??
	  (define (simplify-arg x)
	    (if (or (null? x)                      ; constants and the like look dumb if simplified
		    (not (proper-list? x))
		    (not (hash-table-ref no-side-effect-functions (car x)))
		    (var-member (car x) env))
		x
		(let ((f (simplify-numerics x env)))
		  (if (and (pair? f)
			   (just-rationals? f))
		      (catch #t
			(lambda ()
			  (eval f))
			(lambda ignore f))
		      f))))
	  (let ((args (map simplify-arg (cdr form))))
	    (cond ((hash-table-ref numerics-table (car form))
		   => (lambda (f)
			(f args form env)))
		  (else (cons (car form) args)))))))

    (define (binding-ok? caller head binding env second-pass)
      ;; check let-style variable binding for various syntactic problems
      (cond (second-pass
	     (and (len>1? binding)
		  (symbol? (car binding))
		  (not (constant? (car binding)))
		  (or (null? (cddr binding))
		      (and (eq? head 'do)
			   (len=1? (cddr binding)))))) ; (do ((i 0 . 1))...)

	    ((not (pair? binding)) 	   (lint-format "~A binding is not a list? ~S" caller head binding) #f)    ; (let (a) a)
	    ((not (symbol? (car binding))) (lint-format "~A variable is not a symbol? ~S" caller head binding) #f) ; (let ((1 2)) #f)
	    ((keyword? (car binding))	   (lint-format "~A variable is a keyword? ~S" caller head binding) #f)    ; (let ((:a 1)) :a)
	    ((constant? (car binding))	   (lint-format "can't bind a constant: ~S" caller binding) #f)            ; (let ((pi 2)) #f)
	    ((not (pair? (cdr binding)))
	     (lint-format (if (null? (cdr binding))
			      "~A variable value is missing? ~S"     ; (let ((a)) #f)
			      "~A binding is an improper list? ~S")  ; (let ((a . 1)) #f)
			  caller head binding)
	     #f)
	    ((and (pair? (cddr binding))               ; (let loop ((pi 1.0) (+ pi 1))...)
		  (or (not (eq? head 'do))
		      (pair? (cdddr binding))))
	     (lint-format "~A binding is messed up: ~A" caller head binding)
	     #f)
	    ((and (eq? caller (car binding))
		  (let ((fv (var-member caller env)))
		    (and fv
			 (memq (var-ftype fv) '(define lambda let define* lambda*)))))
	     (lint-format "~A variable ~A in ~S shadows the current function?" caller head caller binding)
	     #t)
	    (*report-shadowed-variables*               ; (let ((x 1)) (+ (let ((x 2)) (+ x 1)) x))
	     (report-shadower caller head 'variable (car binding) binding env)
	     #t)
	    (else #t)))

    (define (check-char-cmp caller op form)
      (if (and (tree-memq 'char->integer (cdr form))
	       (lint-every? (lambda (x)
			      (or (and (integer? x)
				       (<= 0 x 255))
				  (and (len=2? x)
				       (eq? (car x) 'char->integer))))
			    (cdr form)))
	  (lint-format "perhaps ~A" caller             ; (< (char->integer x) 95) -> (char<? x #\_)
		       (lists->string form
				      (cons (case op ((=) 'char=?) ((>) 'char>?) ((<) 'char<?) ((>=) 'char>=?) (else 'char<=?))
					    (map (lambda (arg)
						   ((if (integer? arg) integer->char cadr) arg))
						 (cdr form)))))))

    (define (write-port expr) ; ()=not specified (*stdout*), #f=something is wrong (not enough args)
      (and (pair? expr)
	   (if (eq? (car expr) 'newline)
	       (if (pair? (cdr expr))
		   (cadr expr)
		   ())
	       (and (pair? (cdr expr))
		    (if (pair? (cddr expr))
			(caddr expr)
			())))))

    (define (display->format d)
      (case (car d)
	((newline) (copy "~%"))

	((display)
	 (let* ((arg (cadr d))
		(arg-arg (and (len>1? arg)
			      (cadr arg))))
	   (cond ((string? arg)
		  arg)

		 ((char? arg)
		  (string arg))

		 ((and (pair? arg)
		       (eq? (car arg) 'number->string))
		  (if (= (length arg) 3)
		      (case (caddr arg)
			((2) (values "~B" arg-arg))
			((8) (values "~O" arg-arg))
			((10) (values "~D" arg-arg))
			((16) (values "~X" arg-arg))
			(else (values "~A" arg)))
		      (values "~A" arg-arg)))

		 ((not (and (pair? arg)
			    (eq? (car arg) 'string-append)))
		  (values "~A" arg))

		 ((null? (cddr arg))
		  (if (string? arg-arg)
		      arg-arg
		      (values "~A" arg-arg)))

		 ((not (null? (cdddr arg)))
		  (values "~A" arg))

		 ((string? arg-arg)
		  (values (string-append arg-arg "~A") (caddr arg)))

		 ((string? (caddr arg))
		  (values (string-append "~A" (caddr arg)) arg-arg))

		 (else (values "~A" arg)))))

	((write)
	 ;; very few special cases actually happen here, unlike display above
	 (if (string? (cadr d))
	     (string-append "\"" (cadr d) "\"")
	     (if (char? (cadr d))
		 (string (cadr d))
		 (values "~S" (cadr d)))))

	((write-char)
	 (if (char? (cadr d))
	     (string (cadr d))
	     (values "~C" (cadr d))))

	((write-string)  ; same as display but with possible start|end indices
	 (let ((indices (and (len>1? (cddr d)) ; port
			     (cdddr d))))
	   (if (string? (cadr d))
	       (if (not indices)
		   (cadr d)
		   (if (and (integer? (car indices))
			    (or (null? (cdr indices))
				(and (pair? indices)
				     (integer? (cadr indices)))))
		       (apply substring (cadr d) indices)
		       (values "~A" (cons 'substring (cons (cadr d) indices)))))
	       (values "~A" (if indices (cons 'substring (cons (cadr d) indices)) (cadr d))))))))

    (denote (lint-set!? form env)
      (and *report-any-!-as-setter* ; (inc! x) when inc! is unknown, assume it sets x
	   (symbol? (car form))
	   (pair? (cdr form))
	   (or (symbol? (cadr form))
	       (and (pair? (cddr form))
		    (symbol? (caddr form))))
	   (not (hash-table-ref built-in-functions (car form)))
	   (let ((str (symbol->string (car form))))
	     (char=? (string-ref str (- (length str) 1)) #\!))
	   (not (var-member (car form) env))))

    (denote (easy-lambda? x)
      (and (len>2? x)
	   (eq? (car x) 'lambda)
	   (len=1? (cadr x))))

    (denote (identity? x) ; (lambda (x) x), or (define (x) x) -> procedure-source
      (and (easy-lambda? x)
	   (null? (cdddr x))
	   (eq? (caddr x) (caadr x))))

    (denote (simple-lambda? x)
      (and (easy-lambda? x)
	   (null? (cdddr x))
	   (= (tree-count (caadr x) (caddr x) 2) 1)))

    (define (less-simple-lambda? x)
      (and (easy-lambda? x)
	   (= (tree-count (caadr x) (cddr x) 2) 1)))

    (define (cdr-count c)
      (case c ((cdr) 1) ((cddr) 2) ((cdddr) 3) (else 4)))

    (define* (find-unique-name f1 f2 (i 1))
      (let ((sym (string->symbol (format #f "<~D>" i))))
	(if (not (or (eq? sym f1)
		     (eq? sym f2)
		     (tree-memq sym f1)
		     (tree-memq sym f2)))
	    sym
	    (find-unique-name f1 f2 (+ i 1)))))

    (define (unrelop caller head form)         ; assume len=3
      (let ((arg1 (cadr form))
	    (arg2 (caddr form)))
	(if (len=3? arg1)
	    (if (eq? (car arg1) '-)
		(if (memv arg2 '(0 0.0))               ; (< (- x y) 0) -> (< x y), need both 0 and 0.0 because (eqv? 0 0.0) is #f
		    (lint-format "perhaps ~A" caller
				 (lists->string form
						(list head (cadr arg1) (caddr arg1))))
		    (if (and (integer? arg2)           ; (> (- x 50868) 256) -> (> x 51124)
			     (integer? (caddr arg1)))
			(lint-format "perhaps ~A" caller
				     (lists->string form
						    (list head (cadr arg1) (+ (caddr arg1) arg2))))))
		;; (> (- x) (- y)) (> (- x 1) (- y 1)) and so on -- do these ever happen? (no, not even if we allow +-*/)

		(if (and (eq? (car arg1) '+)           ; (< (+ x 1) 3) -> (< x 2)
			 (integer? arg2)
			 (integer? (caddr arg1)))
		    (lint-format "perhaps ~A" caller
				 (lists->string form
						(list head (cadr arg1) (- arg2 (caddr arg1)))))))
	    (if (len=3? arg2)
		(if (eq? (car arg2) '-)
		    (if (memv arg1 '(0 0.0))           ; (< 0 (- x y)) -> (> x y)
			(lint-format "perhaps ~A" caller
				     (lists->string form
						    (list (hash-table-ref reversibles head) (cadr arg2) (caddr arg2))))
			(if (and (integer? arg1)
				 (integer? (caddr arg2)))
			    (lint-format "perhaps ~A" caller
					 (lists->string form
							(list (hash-table-ref reversibles head) (cadr arg2) (+ arg1 (caddr arg2)))))))
		    (if (and (eq? (car arg2) '+)       ; (< 256 (+ fltdur 50868)) -> (> fltdur -50612)
			     (integer? arg1)
			     (integer? (caddr arg2)))
			(lint-format "perhaps ~A" caller
				     (lists->string form
						    (list (hash-table-ref reversibles head) (cadr arg2) (- arg1 (caddr arg2)))))))))))


    (define (check-start-and-end caller head form ff env)
      (if (len>1? form)
	  (if (pair? (cddr form))
	      (lint-format "~A: too many indices: ~A" caller head ff)
	      (if (or (and (integer? (car form))
			   (integer? (cadr form))
			   (apply >= form))
		      (and (equal? (car form) (cadr form))
			   (not (side-effect? (car form) env))))
		  (lint-format "these ~A indices make no sense: ~A" caller head ff)))))  ; (copy x y 1 0)

    (define (other-case c)
      ((if (char-upper-case? c) char-downcase char-upcase) c))

    (define (check-boolean-affinity caller form env)
      ;; does built-in boolean func's arg make sense
      (when (= (length form) 2)

	(unless (or (and (symbol? (cadr form))
			 (not (keyword? (cadr form))))
		    (= line-number last-simplify-boolean-line-number))
	  (let ((expr (simplify-boolean form () () env)))
	    (if (not (equal? expr form))
		(lint-format "perhaps ~A" caller (lists->string form expr))  ; (char? '#\a) -> #t
		(if (code-constant? (cadr form))
		    (let ((val (eval/error caller form)))
		      (if (not (eq? val :error))
			  (lint-format "perhaps ~A" caller (lists->string form val))))))))

	(if (and (symbol? (cadr form))                  ; (number? pi) -> #t
		 (not (keyword? (cadr form)))
		 (not (var-member (cadr form) env)))
	    (let ((val (checked-eval form)))
	      (if (not (eq? val :checked-eval-error))
		  (lint-format "perhaps ~A" caller (lists->string form val)))))

	(when (and (pair? (cadr form))
		   (symbol? (caadr form)))
	  (let ((rt (if (and (eq? (caadr form) 'quote)
			     (pair? (cdadr form)))
			(->simple-type (cadadr form))
			(return-type (caadr form) env)))
		(head (car form)))
	    (if (subsumes? head rt)
		(lint-format "~A is always #t" caller (truncated-list->string form)) ; (char? '#\a) is always #t
		(if (not (or (memq rt '(#t #f values))
			     (any-compatible? head rt)))
		    (lint-format "~A is always #f" caller (truncated-list->string form)))))))) ; (number? (make-list 1)) is always #f

    (define combinable-cxrs (let ((h (make-hash-table)))
			      (for-each (lambda (c)
					  (hash-table-set! h c (let ((name (symbol->string c)))
								 (substring name 1 (- (length name) 1)))))
					'(car cdr caar cadr cddr cdar caaar caadr caddr cdddr cdaar cddar cadar cdadr cadddr cddddr))
			      h))
    ;; not combinable: caaaar caaadr caadar caaddr cadaar cadadr caddar cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar

    (define combine-cxrs
      (let ((cxr? (lambda (s)
		    (and (pair? (cdr s))
			 (len=2? (cadr s))
			 (hash-table-ref combinable-cxrs (caadr s))))))
	(lambda (form)
	  (and (cxr? form)
	       (let* ((arg1 (cadr form))
		      (arg2 (and arg1 (cxr? arg1) (cadr arg1)))
		      (arg3 (and arg2 (cxr? arg2) (cadr arg2))))
		 (values (string-append (hash-table-ref combinable-cxrs (car form))
					(hash-table-ref combinable-cxrs (car arg1))
					(if arg2 (hash-table-ref combinable-cxrs (car arg2)) "")
					(if arg3 (hash-table-ref combinable-cxrs (car arg3)) ""))
			 (let ((val (cadr (or arg3 arg2 arg1))))
			   (if (keyword? val) ; tricky! ...(cddr :hi)... here is passed to lambda* below as last arg -> error because :hi has no argument!
			       (lint-format "~A can't be a pair" (car form) val)
			       val))))))))
#|
    ;; this builds the lists below:
    (let ((ci ())
	  (ic ()))
      (for-each
       (lambda (c)
	 (let ((name (reverse (substring (symbol->string c) 1 (- (length (symbol->string c)) 1)))))
	   (do ((sum 0)
		(len (length name))
		(i 0 (+ i 1))
		(bit 0 (+ bit 2)))
	       ((= i len)
		(set! ci (cons (cons c sum) ci))
		(set! ic (cons (cons sum c) ic)))
	     (set! sum (+ sum (expt 2 (if (char=? (name i) #\a) bit (+ bit 1))))))))
       '(car cdr caar cadr cddr cdar caaar caadr caddr cdddr cdaar cddar cadar cdadr cadddr cddddr
	     caaaar caaadr caadar caaddr cadaar cadadr caddar cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar))
      (list (reverse ci) (reverse ic)))
|#
    (define match-cxr
      (let ((int->cxr (hash-table 1 'car 2 'cdr
				  5 'caar 6 'cadr 10 'cddr 9 'cdar
				  21 'caaar 22 'caadr 26 'caddr 42 'cdddr 37 'cdaar 41 'cddar 25 'cadar 38 'cdadr
				  106 'cadddr 170 'cddddr 85 'caaaar 86 'caaadr 89 'caadar 90 'caaddr 101 'cadaar 102 'cadadr
				  105 'caddar 149 'cdaaar 150 'cdaadr 153 'cdadar 154 'cdaddr 165 'cddaar 166 'cddadr 169 'cdddar))
	    (cxr->int (hash-table 'car 1  'cdr 2
				  'caar 5  'cadr 6  'cddr 10  'cdar 9
				  'caaar 21  'caadr 22  'caddr 26  'cdddr 42  'cdaar 37  'cddar 41  'cadar 25  'cdadr 38
				  'cadddr 106  'cddddr 170  'caaaar 85  'caaadr 86  'caadar 89  'caaddr 90  'cadaar 101  'cadadr 102
				  'caddar 105  'cdaaar 149  'cdaadr 150 'cdadar 153 'cdaddr 154 'cddaar 165 'cddadr 166  'cdddar 169)))
	(lambda (c1 c2)
	  (hash-table-ref int->cxr (logand (or (hash-table-ref cxr->int c1) 0)
					   (or (hash-table-ref cxr->int c2) 0))))))

    (define (mv-range producer env)
      (if (symbol? producer)
	  (let ((v (var-member producer env)))
	    (and (pair? v)
		 (pair? ((cdr v) 'nvalues))
		 ((cdr v) 'nvalues)))
          (and (pair? producer)
               (case (car producer)
		 ((lambda lambda*)
		  (and (len>1? (cdr producer))
		       (count-values (cddr producer))))
		 ((values)
		  (let ((len (- (length producer) 1)))
		    (for-each
		     (lambda (p)
		       (if (and (pair? p) (eq? (car p) 'values))
			   (set! len (+ len (length p) -2))))
		     (cdr producer))
		    (list len len)))
		 (else (mv-range (car producer) env))))))

    (define (eval-constant-expression caller form)
      (if (just-code-constants? (cdr form))
	  (catch #t
	    (lambda ()
	      (let ((val (eval (copy form)))); :readable))))
		(lint-format "perhaps ~A" caller (lists->string form val)))) ; (eq? #(0) #(0)) -> #f
	    (lambda args
	      #t))))

    (define (unlist-values tree)
      (if (not (and (pair? tree)
		    (list? (cdr tree))
		    (not (eq? (car tree) 'quote))))
	  tree
	  (case (car tree)
	    ((list-values)
	     (if (and (assq 'apply-values (cdr tree))
		      (len=2? (cdr tree))
		      (pair? (caddr tree)))
		 (if (and (pair? (cadr tree))
			  (eq? (caadr tree) 'apply-values))
		     (list 'append (cadadr tree) (cadr (caddr tree)))
		     (list 'cons (cadr tree) (cadr (caddr tree))))
		 (cons 'list (unlist-values (cdr tree))))) ; #_list perhaps? and #_cons #_append above?

	    ((append)
	     (if (and (len=2? (cdr tree))
		      (pair? (cadr tree))
		      (eq? (caadr tree) 'list-values))
		 (let ((lst (unlist-values (cadr tree)))
		       (rest (caddr tree)))
		   (if (pair? rest) (set! rest (unlist-values rest)))
		   (let ((lst-len (length lst)))
		     (case lst-len
		       ((2) (list 'cons (cadr lst) rest))
		       ((3) (list 'cons (cadr lst) (list 'cons (caddr lst) rest)))
		       (else (cons 'append (unlist-values (cdr tree)))))))))

	    (else (cons (unlist-values (car tree))
			(unlist-values (cdr tree)))))))

    (define (qq-tree? tree)
      (and (pair? tree)
	   (or (eq? (car tree) 'apply-values)
	       (if (and (eq? (car tree) 'list-values)
			(pair? (cdr tree))              ; (assq '(list-values  .\ " (  \"))...)
			(assq 'apply-values (cdr tree)))
		   (or (not (= (length tree) 3))
		       (not (and (pair? (caddr tree))
				 (eq? (caaddr tree) 'apply-values)))
		       (qq-tree? (cadr (caddr tree)))
		       (let ((applying (and (pair? (cadr tree))
					    (eq? (caadr tree) 'apply-values))))
			 (qq-tree? ((if applying cadadr cadr) tree))))
		   (or (qq-tree? (car tree))
		       (qq-tree? (cdr tree)))))))


    (define (func-definer? f)
      (and (len>2? f)
	   (or (eq? (car f) 'define*)
	       (and (eq? (car f) 'define)
		    (or (pair? (cadr f))
			(and (symbol? (cadr f))
			     (pair? (caddr f))
			     (memq (caaddr f) '(lambda lambda*))))))))


    (define special-case-functions
      (let ((special-case-table (make-hash-table)))

	(define (hash-special key value)
	  (if (hash-table-ref special-case-table key)
	      (format *stderr* "~A already has a value: ~A~%" key (hash-table-ref special-case-table key)))
	  (hash-table-set! special-case-table key value))

	(define string->char=
	  (let ((substring->char? (lambda (s2)
				    (and (eq? (car s2) 'substring)
					 (= (length s2) 4)
					 (integer? (caddr s2))
					 (integer? (cadddr s2))
					 (= (+ (caddr s2) 1) (cadddr s2))))))
	    (lambda (caller form original-form)
	      ;; called by string=? and equal? returns true if it makes a suggestion
	      ;;   look for args of form "x" (string x) (substring x 1 2) and reduces to char= (caller also tries args reversed)
	      (and (pair? (caddr form))
		   (let ((sug made-suggestion)
			 (s1 (cadr form))
			 (s2 (caddr form)))
		     (cond ((string? s1)
			    (when (= (length s1) 1)
			      (if (eq? (car s2) 'string)          ; (equal? "[" (string r)) -> (char=? #\[ r)
				  (if (not (len=2? s2))           ; (eqv? "a" (string)) or (string=? "a" (string a b))
				      (lint-format "~A is #f" caller original-form)
				      (lint-format "perhaps ~A" caller
						   (lists->string original-form (list 'char=? (string-ref s1 0) (cadr s2)))))
				  (if (substring->char? s2)       ; (equal? "^" (substring s 0 1)) -> (char=? #\^ (string-ref s 0))
				      (lint-format "perhaps ~A" caller
						   (lists->string original-form `(char=? ,(string-ref s1 0) (string-ref ,(cadr s2) ,(caddr s2)))))))))
			   ((not (pair? s1)))

			   ((not (eq? (car s1) 'string))
			    (if (and (substring->char? s1)        ; (string=? (substring x 0 1) (substring y 2 3))
				     (substring->char? s2))
				(lint-format "perhaps ~A" caller
					     (lists->string original-form `(char=? (string-ref ,(cadr s1) ,(caddr s1)) (string-ref ,(cadr s2) ,(caddr s2)))))))

			   ((eq? (car s2) 'string)                ; (string=? (string x) (string y)) -> (char=? x y)
			    (if (not (eqv? (length s1) (length s2)))
				(lint-format "~A is #f" caller original-form)
				(if (len=2? s1)
				    (lint-format "perhaps ~A" caller (lists->string original-form (list 'char=? (cadr s1) (cadr s2)))))))

			   ((and (len=2? s1)                      ; (string=? (string x) (substring y 3 4)) -> (char=? x (string-ref y 3))
				 (substring->char? s2))
			    (lint-format "perhaps ~A" caller
					 (lists->string original-form `(char=? ,(cadr s1) (string-ref ,(cadr s2) ,(caddr s2)))))))

		     (not (= sug made-suggestion)))))))

	;; ---------------- member and assoc ----------------
	(let ()
	  (define sp-memx
	    (let ((list-one? (lambda (p)
			       (and (len=2? p)
				    (case (car p)
				      ((list) cadr)
				      ((quote)
				       (and (len=1? (cadr p))
					    (if (symbol? (caadr p))
						(lambda (x)
						  (list 'quote (caadr x)))
						caadr)))
				      (else #f))))))
	      (lambda (caller head form env)
		;(format *stderr* "memx: ~S~%" form)
		(case (length form)
		  ((3)
		   (let ((selector (cadr form))
			 (items (caddr form)))

		     (let ((current-eqf (case head ((memq assq) 'eq?) ((memv assv) 'eqv?) (else 'equal?)))
			   (selector-eqf (car (eqf selector env)))
			   (one-item (and (memq head '(memq memv member)) (list-one? items))))
		       ;; one-item assoc doesn't simplify cleanly
		       (if one-item
			   (let* ((target (one-item items))
				  (iter-eqf (eqf target env)))
			     (if (or (symbol? target)
				     (unquoted-pair? target))
				 (set! target (list 'quote target))) ; ; (member x (list "asdf")) -> (string=? x "asdf") -- maybe equal? here?
			     (lint-format "perhaps ~A" caller (lists->string form (list (cadr iter-eqf) selector target))))

			   ;; not one-item
			   (letrec ((duplicates? (lambda (lst fnc)
						   (and (pair? lst)
							(or (fnc (car lst) (cdr lst))
							    (duplicates? (cdr lst) fnc)))))
				    (duplicate-constants? (lambda (lst fnc)
							    (and (pair? lst)
								 (or (and (constant? (car lst))
									  (fnc (car lst) (cdr lst)))
								     (duplicate-constants? (cdr lst) fnc))))))
			     (if (and (symbol? selector-eqf)   ; (memq 1.0 x): perhaps memq -> memv
				      (not (eq? selector-eqf current-eqf))
				      (or (not (eq? 'eq? current-eqf))
					  (not (memq 'char=? (eqf selector env))))) ; eq? used with chars is ok in s7: (assq (string-ref name 0) bad-var-names)
				 (lint-format "~A: perhaps ~A -> ~A" caller (truncated-list->string form) head
					      (if (memq head '(memq memv member))
						  (case selector-eqf ((eq?) 'memq) ((eqv?) 'memv) ((equal?) 'member) (else 'error))
						  (case selector-eqf ((eq?) 'assq) ((eqv?) 'assv) ((equal?) 'assoc) (else 'error)))))

			     ;; --------------------------------
			     ;; check for head mismatch with items
			     (when (pair? items)
			       (when (or (eq? (car items) 'list)
					 (quoted-pair? items))
				 (let ((elements ((if (eq? (car items) 'quote) cadr cdr) items)))
				   (let ((baddy #f))
				     (catch #t
				       (lambda ()
					 (set! baddy ((if (eq? (car items) 'list) duplicate-constants? duplicates?)
						      elements (symbol->value head))))
				       (lambda args #f))
				     (if (pair? baddy) ; (member x (list "asd" "abc" "asd"))
					 (lint-format "duplicated entry ~S in ~A" caller (car baddy) items)))

				   (when (proper-list? elements)
				     (let ((maxf #f)
					   (keys (if (eq? (car items) 'quote)
						     (if (memq head '(memq memv member))
							 elements
							 (and (just-pairs? elements)
							      (map car elements)))
						     (if (memq head '(memq memv member))
							 (and (just-code-constants? elements)
							      elements)
							 (and (lint-every? (lambda (e)
									     (and (len=2? e)
										  (eq? (car e) 'quote)
										  (pair? (cadr e))))
									   elements)
							      (map caadr elements))))))
				       (when (proper-list? keys)
					 (if (eq? (car items) 'quote)
					     (do ((p keys (cdr p)))
						 ((or (null? p)
						      (memq maxf '(equal? #t))))
					       (let ((element (car p)))
						 (if (symbol? element)
						     (if (not maxf)
							 (set! maxf 'eq?))
						     (if (pair? element)
							 (begin
							   (if (and (eq? (car element) 'quote)
								    (pair? (cdr element)))
							       (lint-format "stray quote? ~A" caller form)) ; (memq x '(a 'b c))
							   (set! maxf #t))
							 (let ((type (if (or (symbol? element) (eq? element #<undefined>))
									 'eq?
									 (car (->eqf (->simple-type element))))))
							   (if (or (memq maxf '(#f eq?))
								   (memq type '(#t equal?)))
							       (set! maxf type)))))))
					     ;; else (list ...)
					     (do ((p keys (cdr p)))
						 ((or (null? p)
						      (memq maxf '(equal? #t))))
					       (let ((element (car p)))
						 (if (symbol? element)
						     (set! maxf #t)
						     (let ((type (car (eqf element env))))
						       (if (or (memq maxf '(#f eq?))
							       (memq type '(#t equal?)))
							   (set! maxf type)))))))
					 (case maxf
					   ((eq?)
					    (if (not (memq head '(memq assq))) ; (member (car op) '(x y z))
						(lint-format "~A could be ~A in ~A" caller
							     head
							     (if (memq head '(memv member)) 'memq 'assq)
							     form)))
					   ((eqv?)
					    (if (not (memq head '(memv assv))) ; (memq (strname 0) '(#\{ #\[ #\()))
						(lint-format "~A ~Aould be ~A in ~A" caller
							     head
							     (if (memq head '(memq assq)) "sh" "c")
							     (if (memq head '(memq member)) 'memv 'assv)
							     form)))
					   ((equal? #t)                        ; (memq (car op) '("a" #()))
					    (if (not (memq head '(member assoc)))
						(lint-format "~A should be ~A in ~A" caller
							     head
							     (if (memq head '(memq memv)) 'member 'assoc)
							     form))))))

				     (if (and (= (length elements) 2)  ; (memq expr '(#t #f))
					      (memq #t elements)
					      (memq #f elements))
					 (lint-format "perhaps ~A" caller (lists->string form (list 'boolean? selector)))))))
			       ;; not (memv x '(0 0.0)) -> (zero? x) because x might not be a number

			       (case (car items)
				 ((map)
				  (let ((memx (memq head '(memq memv member))))
				    (when (and memx (= (length items) 3))
				      (let ((mapf (cadr items))
					    (map-items (caddr items)))
					(cond ((eq? mapf 'car)         ; (memq x (map car y)) -> (assq x y)
					       (lint-format "perhaps use assoc: ~A" caller
							    (lists->string form (list (case current-eqf ((eq?) 'assq) ((eqv?) 'assv) ((equal?) 'assoc) (else 'error))
										      selector map-items))))

					      ((eq? selector #t)
					       (if (eq? mapf 'null?)   ; (memq #t (map null? items)) -> (memq () items)
						   (lint-format "perhaps ~A" caller
								(lists->string form (list 'memq () map-items)))
						   (let ((b (if (eq? mapf 'b) 'c 'b)))
						     ;; (memq #t (map cadr items)) -> (member #t items (lambda (a b) (cadr b)))
						     (lint-format "perhaps avoid 'map: ~A" caller
								  (lists->string form `(member #t ,map-items (lambda (a ,b) (,mapf ,b))))))))

					      ((and (pair? selector)
						    (eq? (car selector) 'string->symbol) ; this could be extended, but it doesn't happen
						    (eq? mapf 'string->symbol)
						    (not (and (pair? map-items)
							      (eq? (car map-items) 'quote))))
					       (lint-format "perhaps ~A" caller
							    ;; (memq (string->symbol x) (map string->symbol y)) -> (member x y string=?)
							    (lists->string form `(member ,(cadr selector) ,map-items string=?))))

					      (else
					       ;; (member x (map b items)) -> (member x items (lambda (a c) (equal? a (b c))))
					       (let ((b (if (eq? mapf 'b) 'c 'b))) ; a below can't collide because eqf won't return 'a
						 (lint-format "perhaps avoid 'map: ~A" caller
							      (lists->string form `(member ,selector ,map-items
											   (lambda (a ,b) (,current-eqf a (,mapf ,b)))))))))))))

				 ((string->list)             ; (memv c (string->list s)) -> (char-position c s)
				  (lint-format "perhaps ~A" caller
					       (lists->string form (cons 'char-position (cons (cadr form) (cdr items))))))

				 ((cons)                     ; (member x (cons y z)) -> (or (equal? x y) (member x z))
				  (if (and (not (pair? selector))
					   (len=3? items))
				      (lint-format "perhaps avoid 'cons: ~A" caller
						   (lists->string form `(or (,current-eqf ,selector ,(cadr items))
									    (,head ,selector ,(caddr items)))))))

				 ((append)                   ; (member x (append (list x) y)) -> (or (equal? x x) (member x y))
				  (if (and (not (pair? selector))
					   (len=3? items)
					   (len=2? (cadr items))
					   (eq? (caadr items) 'list))
				      (lint-format "perhaps ~A" caller
						   (lists->string form `(or (,current-eqf ,selector ,(cadadr items))
									    (,head ,selector ,(caddr items))))))))))))
		     (when (and (memq head '(memq memv))
				(quoted-pair? items))
		       (let ((nitems (length (cadr items))))

			 (if (pair? selector)                        ; (memv (string-ref x 0) '(+ -)) -> #f etc
			     (let ((sig (arg-signature (car selector) env)))
			       (if (and (pair? sig)
					(symbol? (car sig))
					(not (eq? (car sig) 'values)))
				   (let ((vals (let ((car-sig-val (symbol->value (car sig))))
						 (map (lambda (item)
							(if (car-sig-val item) item (values)))
						      (cadr items)))))
				     (if (not (= (length vals) nitems))
					 (lint-format "perhaps ~A" caller
						      (lists->string form
								     (and (pair? vals)
									  `(,head ,selector ',vals)))))))))
			 (if (> nitems 20)
			     (lint-format "perhaps use a hash-table here, rather than ~A" caller (truncated-list->string form)))

			 (let ((bad (lint-find-if (lambda (x)
						    (not (or (symbol? x)
							     (char? x)
							     (number? x)
							     (procedure? x) ; (memq abs '(1 #_abs 2)) !
							     (memq x '(#f #t () #<unspecified> #<undefined> #<eof>)))))
						  (cadr items))))
			   (if bad
			       (lint-format (if (and (pair? bad)
						     (eq? (car bad) 'unquote))
						(values "stray comma? ~A" caller)  ; (memq x '(a (unquote b) c))
						(values "pointless list member: ~S in ~A" caller bad))
					    ;; quoted item here is caught above    ; (memq x '(a (+ 1 2) 3))
					    form)))))))

		  ((4)
		   (let ((func (list-ref form 3)))
		     (if (symbol? func)
			 (if (memq func '(eq? eqv? equal?))   ; (member x y eq?) -> (memq x y)
			     (let ((op (if (eq? head 'member) ; (member (car x) entries equal?) -> (member (car x) entries)
					   (case func ((eq?) 'memq) ((eqv?) 'memv) (else 'member))
					   (case func ((eq?) 'assq) ((eqv?) 'assv) (else 'assoc)))))
			       (lint-format "perhaps ~A" caller (lists->string form (list op (cadr form) (caddr form)))))
			     (let ((sig (signature (symbol->value func)))) ; arg-signature here is too cranky
			       (if (and (pair? sig)
					(not (eq? 'boolean? (car sig)))
					(or (not (pair? (car sig)))
					    (not (or (memq 'boolean? (car sig))
						     (memq 'not (car sig)))))) ; (assoc a b string-position) sig: ((integer? not) string? string? integer?)
				   (lint-format "~A is a questionable ~A function" caller func head)))) ; (member 1 x abs)
			 ;; func not a symbol
			 (when (and (len>2? func)
				    (eq? (car func) 'lambda)
				    (pair? (cadr func)))
			   (if (not (memv (length (cadr func)) '(2 -1)))
			       (lint-format "~A equality function (optional third arg) should take two arguments" caller head)
			       (when (eq? head 'member)
				 (when (and (= (length func) 3)                    ; (member 'a x (lambda (a b c) (eq? a b)))
					    (pair? (caddr func)))
				   (let ((eq (caddr func))
					 (args (cadr func)))
				     (if (and (memq (car eq) '(eq? eqv? equal?))
					      (eq? (car args) (cadr eq))
					      (len>1? (caddr eq))
					      (eq? (caaddr eq) 'car)
					      (pair? (cdr args))
					      (eq? (cadr args) (cadr (caddr eq))))
					 (lint-format "member might perhaps be ~A" ; (member 'a x (lambda (a b) (eq? a (car b))))
						      caller
						      (if (or (eq? func 'eq?)
							      (eq? (caaddr func) 'eq?))
							  'assq
							  (if (eq? (caaddr func) 'eqv?)
							      'assv
							      'assoc))))))
				 (when (pair? (cadr form))                         ; (member (abs x) lst (lambda (a b) (< b 2)))
				   (let ((args (cadr func)))
				     (when (and (pair? args)
						(not (tree-memq (car args) (cddr func))))
				       (lint-format "~A is ignored, so perhaps (member #f ...)" caller (car args)))))))))))))))

	  (for-each (lambda (f)
		      (hash-special f sp-memx))
		    '(memq assq memv assv member assoc)))

	;; ---------------- car, cdr, etc ----------------
	(let ()
	  (define (sp-crx caller head form env)
	    (if (not (= line-number last-simplify-cxr-line-number))
		((lambda* (cxr arg)
		   (when cxr
		     (set! last-simplify-cxr-line-number line-number)
		     (cond ((< (length cxr) 5)                           ; (car (cddr x)) -> (caddr x)
			    (lint-format "perhaps ~A" caller
					 (lists->string form (list (symbol "c" cxr "r") arg))))

			   ;; if it's car|cdr followed by cdr's, use list-ref|tail
			   ((not (char-position #\a cxr))                ; (cddddr (cddr x)) -> (list-tail x 6)
			    (lint-format "perhaps ~A" caller (lists->string form (list 'list-tail arg (length cxr)))))

			   ((not (char-position #\a (substring cxr 1)))  ; (car (cddddr (cddr x))) -> (list-ref x 6)
			    (lint-format "perhaps ~A" caller (lists->string form (list 'list-ref arg (- (length cxr) 1)))))

			   (else (set! last-simplify-cxr-line-number -1)))))
		 (combine-cxrs form)))

	    (when (and (len>1? form)
		       (len>1? (cadr form)))
	      (let ((arg (cadr form)))

		(when (eq? head 'car)
		  (case (car arg)
		    ((list-tail)                      ; (car (list-tail x y)) -> (list-ref x y)
		     (if (len=3? arg)
			 (lint-format "perhaps ~A" caller (lists->string form (list 'list-ref (cadr arg) (caddr arg))))))

		    ((list)                           ; (car (list x ...)) -> x, (car (list)) is handled elsewhere, (car (cons...)) is below
		     (unless (and (pair? (cdr arg))
				  (pair? (cadr arg))  ; (car (list (eval ...))) to catch first of mv
				  (memq (caadr arg) '(eval-string eval apply values))) ; actually any closure here might return multiple values
		       (lint-format "perhaps ~A" caller (lists->string form (cadr arg)))))

		    ((append)                      ; (car (append x ...)) -> (car x)
		     (lint-format "perhaps ~A" caller (lists->string form (list 'car (cadr arg)))))

		    ((memq memv member assq assv assoc)
		     (if (pair? (cdr arg))         ; (car (memq x ...)) is either x or (car #f) -> error
			 (lint-format "~A is ~A, or an error" caller (truncated-list->string form) (cadr arg))))))

		(when (and (eq? (car arg) 'or)     ; (cdr (or (assoc x y) (cons 1 2))) -> (cond ((assoc x y) => cdr) (else 2))
			   (not (eq? form last-rewritten-internal-define))
			   (len=3? arg))
		  (let ((arg1 (cadr arg))
			(arg2 (caddr arg)))
		    (if (and (pair? arg2)
			     (or (and (memq (car arg2) '(cons list list-values))
				      (eq? head 'cdr))
				 (memq (car arg2) '(error throw))
				 (quoted-pair? arg2)))
			(let ((else-val (catch #t
					  (lambda ()
					    (case (car arg2)
					      ((quote) ((symbol->value head) (cadr arg2)))
					      ((cons) (caddr arg2))
					      ((error throw) arg2)
					      (else (cons 'list (cddr arg2)))))
					  (lambda args
					    :error))))
			  (if (not (eq? else-val :error))
			      (lint-format "perhaps ~A" caller
					   (lists->string form ; (cdr (or (assoc n oi) (list n y))) -> (cond ((assoc n oi) => cdr) (else (list y)))
							  `(cond (,arg1 => ,head) (else ,else-val)))))))))

		(if (and (quoted-pair? arg)   ; (cdr '(a)) -> ()
			 (null? (cddr form))  ; else possibly bad trailing args in form (like (read-char))
			 (not (var-member head env)))
		    (let ((val (checked-eval form)))
		      (if (not (eq? val :checked-eval-error))
			  (lint-format "perhaps ~A -> ~A~A" caller
				       (object->string form)
				       (if (or (pair? val) (symbol? val)) "'" "")
				       (object->string val)))))

		(if (and (memq head '(car cdr))
			 (eq? (car arg) 'cons)
			 (len>1? (cdr arg)))
		    (lint-format "(~A~A) is the same as ~A"       ; (car (cons 1 2)) is the same as 1
				 caller head
				 (truncated-list->string arg)
				 (truncated-list->string ((if (eq? head 'car) cadr caddr) arg))))

		(when (memq head '(car cadr caddr cadddr))
		  (case (car arg)
		    ((string->list vector->list)    ; (car (string->list x)) -> (string-ref x 0)
		     (lint-format "perhaps ~A" caller (lists->string form
								     (list (if (eq? (car arg) 'string->list) 'string-ref 'vector-ref)
									   (cadr arg)
									   (case head ((car) 0) ((cadr) 1) ((caddr) 2) (else 3))))))
		    ((reverse reverse!)
		     (lint-format "perhaps ~A~A" caller           ; (car (reverse x)) -> (list-ref x (- (length x) 1))
				  (if (eq? head 'car)
				      "use 'last from srfi-1, or "
				      "")
				  (lists->string form
						 (if (symbol? (cadr arg))
						     `(list-ref ,(cadr arg)
								(- (length ,(cadr arg))
								   ,(case head ((car) 1) ((cadr) 2) ((caddr) 3) (else 4))))
						     `(let ((<1> ,(cadr arg)))  ; let is almost certainly cheaper than reverse
							(list-ref <1> (- (length <1>)
									 ,(case head ((car) 1) ((cadr) 2) ((caddr) 3) (else 4))))))))))))))
	  (for-each (lambda (f)
		      (hash-special (car f) sp-crx))
		    combinable-cxrs))
	;; not combinable cxrs:
	;;    caaaar caaadr caadar caaddr cadaar cadadr caddar
	;;    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar

	;; ---------------- set-car! ----------------
	(let ()
	  (define (sp-set-car! caller head form env)
	    (when (= (length form) 3)
	      (let ((target (cadr form)))
		(if (code-constant? target)
		    (lint-format "~A is a constant, so ~A is problematic" caller
				 target
				 (truncated-list->string form)))
		(if (pair? target)
		    (case (car target)

		      ((list-tail)                              ; (set-car! (list-tail x y) z) -> (list-set! x y z)
		       (lint-format "perhaps ~A" caller (lists->string form (list 'list-set! (cadr target) (caddr target) (caddr form)))))

		      ((cdr cddr cdddr cddddr)                  ; (set-car! (cddr (cdddr x)) y) -> (list-set! x 5 y)
		       (set! last-simplify-cxr-line-number line-number)
		       (lint-format "perhaps ~A" caller
				    (lists->string form
						   (if (and (pair? (cadr target))
							    (memq (caadr target) '(cdr cddr cdddr cddddr)))
						       ;; (set-car! (cdr (cddr x)) y) -> (list-set! x 3 y)
						       (list 'list-set!
							     (cadadr target)
							     (+ (cdr-count (car target)) (cdr-count (caadr target)))
							     (caddr form))
						       ;; (set-car! (cdr x) y) -> (list-set! x 1 y)
						       (list 'list-set!
							     (cadr target)
							     (cdr-count (car target))
							     (caddr form)))))))))))
	  (hash-special 'set-car! sp-set-car!))

	(let ()
	  (define (sp-set-cdr! caller head form env)
	    (when (= (length form) 3)
	      (let ((target (cadr form)))
		(if (code-constant? target)
		    (lint-format "~A is a constant, so ~A is problematic" caller
				 target
				 (truncated-list->string form))))))
	  (hash-special 'set-cdr! sp-set-cdr!))

	;; ---------------- not ----------------
	(let ()
	  (define (sp-not caller head form env)
	    ;(format *stderr* "sp-not ~S~%" form)
	    (if (and (pair? (cdr form))
		     (pair? (cadr form)))
		(if (eq? (caadr form) 'not)
		    (when (len>1? (cadr form))
		      (let* ((arg (cadadr form))
			     (str (truncated-list->string arg))) ; (not (not x)) -> (and x #t)
			(if (and (pair? arg)                     ; (not (not (pair? x))) -> (pair? x)
				 (eq? (return-type (car arg) env) 'boolean?))
			    (lint-format "~A returns a boolean, so (not (not ~A)) -> ~A" 'paranoia (car arg) arg arg)
			    (lint-format "if you want a boolean, (not (not ~A)) -> (and ~A #t)" 'paranoia str str))))
		    (unless (eq? (caadr form) 'for-each)
		      (let ((sig (arg-signature (caadr form) env)))
			(if (and (pair? sig)
				 (if (pair? (car sig))                      ; (not (+ x y))
				     (not (or (memq 'boolean? (car sig))
					      (memq 'not (car sig))))
				     (not (memq (car sig) '(#t values boolean? not)))))
			    (lint-format "~A can't be true (~A never returns #f)" caller (truncated-list->string form) (caadr form)))))))

	    (if (not (= line-number last-simplify-boolean-line-number))
		(let ((val (simplify-boolean form () () env)))
		  (set! last-simplify-boolean-line-number line-number)
		  (if (not (equal? form val))                               ;  (not (and (> x 2) (not z))) -> (or (<= x 2) z)
		      (lint-format "perhaps ~A" caller (lists->string form val))))))

	  (hash-special 'not sp-not))

	;; ---------------- and/or ----------------
	(let ()
	  (define (sp-and caller head form env)
	    (if (not (= line-number last-simplify-boolean-line-number))
		(let ((val (simplify-boolean form () () env)))
		  (set! last-simplify-boolean-line-number line-number)
		  (if (not (equal? form val))          ; (and (not x) (not y)) -> (not (or x y))
		      (lint-format "perhaps ~A" caller (lists->string form val)))))
	    (if (pair? (cdr form))
		(do ((p (cdr form) (cdr p)))
		    ((null? (cdr p)))
		  (if (and (len=3? (car p))            ; (and (member n cvars) (if (pair? open) (not (member n open))) (not (eq? n open)))
			   (eq? (caar p) 'if))
		      (lint-format "one-armed if might cause confusion here: ~A" caller form)))))
	  (hash-special 'and sp-and)
	  (hash-special 'or sp-and))

	;; ---------------- = ----------------
	(let ()
	  (define (any-real? lst)                      ; ignore 0.0 and 1.0 in this since they normally work
	    (and (pair? lst)
		 (or (and (number? (car lst))
			  (not (rational? (car lst)))
			  (not (member (car lst) '(0.0 1.0) =)))
		     (any-real? (cdr lst)))))    ; (= x 1.5)
	 (define (sp-= caller head form env)
	   ;; repeated factors (= (+ x y) (+ x z)) never happen
	   (let ((len (length form)))
	     (if (and (> len 2)
		      (any-real? (cdr form)))
		 (lint-format "= can be troublesome with floats: ~A" caller (truncated-list->string form)))

	     (let ((cleared-form (cons = (remove-if (lambda (x) (not (number? x))) (cdr form)))))
	       (if (and (> (length cleared-form) 2)
			(not (checked-eval cleared-form)))    ; (= 1 y 2)
		   (lint-format "this comparison can't be true: ~A" caller (truncated-list->string form))))

	     (when (= len 3)
	       (let ((arg1 (cadr form))
		     (arg2 (caddr form)))
		 (if (or (and (memv arg1 '(0 1))
			      (len>1? arg2)
			      (eq? (car arg2) 'denominator))
			 (and (memv arg2 '(0 1))
			      (len>1? arg1)
			      (eq? (car arg1) 'denominator)))
		     (if (or (eqv? arg1 0) (eqv? arg2 0))
			 (lint-format "denominator is never 0: ~A" caller form)
			 (lint-format "perhaps ~A" caller (lists->string form (list 'integer? (cadr (if (pair? arg1) arg1 arg2))))))

		     ;; (= (+ x a) (+ y a)) and various equivalents happen very rarely (only in test suites it appears)
		     (let ((var (or (and (memv arg1 '(0 1))
					 (len>1? arg2)
					 (eq? (car arg2) 'length)
					 (cadr arg2))
				    (and (memv arg2 '(0 1))
					 (len>1? arg1)
					 (eq? (car arg1) 'length)
					 (cadr arg1)))))
		       ;; we never seem to have var-member/initial-value/history here to distinguish types
		       ;;   and a serious attempt to do so was a bust. (the enclosing expr is not in the var-history yet)
		       (if var
			   (if (or (eqv? arg1 0)    ;  (= (length x) 0) -> (null? x)
				   (eqv? arg2 0))
			       (lint-format "perhaps (assuming ~A is a list), ~A" caller var
					    (lists->string form (list 'null? var)))
			       (if (symbol? var)    ;  (= (length x) 1) -> (and (pair? x) (null? (cdr x)))
				   (lint-format "perhaps (assuming ~A is a list), ~A" caller var
						(lists->string form `(and (pair? ,var) (null? (cdr ,var)))))))))))
	       (unrelop caller '= form))
	     (check-char-cmp caller '= form)))
	 (hash-special '= sp-=))

	(let ()
	  (define (sp-=->eq caller head form env)
	    (if (= (length form) 3)
		(let ((old made-suggestion))
		  (lint-format "perhaps 'eq? not '~A in ~A" caller head (truncated-list->string form))
		  (set! made-suggestion old))))
	  (hash-special 'boolean=? sp-=->eq)
	  (hash-special 'symbol=? sp-=->eq))

	;; ---------------- < > <= >= ----------------
	(let ()
	  (define (sp-< caller head form env)
	    (let ((len (length form)))
	      (let ((cleared-form (cons head ; keep operator
					(remove-if (lambda (x)
						     (not (number? x)))
						   (cdr form)))))
		(if (and (> (length cleared-form) 2)
			 (not (checked-eval cleared-form)))  ; (< x 1 2 0 y)
		    (lint-format "this comparison can't be true: ~A" caller (truncated-list->string form))))

	      (if (= len 3)
		  (unrelop caller head form)
		  (when (> len 3)
		    (if (memq head '(< >))              ; (< x y x) -> #f
			(if (or (repeated-member? (cdr form) env)
				(and (= len 4)          ; (< 0 (floor x) 1) -> #f -- this gets no hits, but I like it...
				     (integer? (cadr form))
				     (pair? (caddr form))
				     (integer? (cadddr form))
				     (= (abs (- (cadr form) (cadddr form))) 1)
				     (cond ((arg-signature (caaddr form) env) =>
					    (lambda (sig)
					      (eq? 'integer? (car sig))))
					   (else #f))))
				(lint-format "perhaps ~A" caller (lists->string form #f)))
			(if (and (memq head '(<= >=))
				 (repeated-member? (cdr form) env))
			    (do ((last-arg (cadr form))
				 (new-args (list (cadr form)))
				 (lst (cddr form) (cdr lst)))
				((null? lst)
				 (if (repeated-member? new-args env)       ; (<= x y x z x) -> (= x y z)
				     (lint-format "perhaps ~A" caller (truncated-lists->string form (cons '= (lint-remove-duplicates (reverse new-args) env))))
				     (if (< (length new-args) (- len 1))
					 (lint-format "perhaps ~A" caller  ; (<= x x y z) -> (= x y z)
						      (truncated-lists->string form (or (null? (cdr new-args))
											(cons '= (reverse new-args))))))))
			      (unless (equal? (car lst) last-arg)
				(set! last-arg (car lst))
				(set! new-args (cons last-arg new-args))))))))

	      (cond ((not (= len 3)))

		    ((and (real? (cadr form))
			  (or (< (cadr form) 0)
			      (and (zero? (cadr form))
				   (eq? head '>)))
			  (pair? (caddr form))   ; (> 0 (string-length x))
			  (hash-table-ref non-negative-ops (caaddr form)))
		     (lint-format "~A can't be negative: ~A" caller (caaddr form) (truncated-list->string form)))

		    ((and (real? (caddr form))
			  (or (< (caddr form) 0)
			      (and (zero? (caddr form))
				   (eq? head '<)))
			  (pair? (cadr form))    ; (< (string-length x) 0)
			  (hash-table-ref non-negative-ops (caadr form)))
		     (lint-format "~A can't be negative: ~A" caller (caadr form) (truncated-list->string form)))

		    ((and (pair? (cadr form))
			  (eq? (caadr form) 'length))
		     (let ((arg (cadadr form)))
		       (when (symbol? arg)       ;  (>= (length x) 0) -> (list? x)
			 ;; see comment above about distinguishing types!  (twice I've wasted my time)
			 (if (eqv? (caddr form) 0)
			     (lint-format "perhaps~A ~A" caller
					  (if (eq? head '<) "" (format #f " (assuming ~A is a proper list)," arg))
					  (lists->string form
							 (case head
							   ((<)  `(and (pair? ,arg) (not (proper-list? ,arg))))
							   ((<=) (list 'null? arg))
							   ((>)  (list 'pair? arg))
							   ((>=) (list 'list? arg)))))
			     (if (and (eqv? (caddr form) 1)
				      (not (eq? head '>)))  ; (<= (length x) 1) -> (or (null? x) (null? (cdr x)))
				 (lint-format "perhaps (assuming ~A is a proper list), ~A" caller arg
					      (lists->string form
							     (case head
							       ((<)  (list 'null? arg))
							       ((<=) `(or (null? ,arg) (null? (cdr ,arg))))
							       ((>)  `(and (pair? ,arg) (pair? (cdr ,arg))))
							       ((>=) (list 'pair? arg))
							       (else 'error)))))))))
		    ((and (len>1? (caddr form))
			  (eq? (caaddr form) 'length))
		     (let ((arg (cadr (caddr form))))
		       (when (symbol? arg)                  ;  (>= 0 (length x)) -> (null? x)
			 (if (eqv? (cadr form) 0)
			     (lint-format "perhaps~A ~A" caller
					  (if (eq? head '>) "" (format #f " (assuming ~A is a proper list)," arg))
					  (lists->string form
							 (case head
							   ((<)  (list 'pair? arg))
							   ((<=) (list 'list? arg))
							   ((>)  `(and (pair? ,arg) (not (proper-list? ,arg))))
							   ((>=) (list 'null? arg))
							   (else 'error))))
			     (if (and (eqv? (cadr form) 1)
				      (not (eq? head '<))) ; (> 1 (length x)) -> (null? x)
				 (lint-format "perhaps (assuming ~A is a proper list), ~A" caller arg
					      (lists->string form
							     (case head
							       ((<)  `(and (pair? ,arg) (pair? (cdr ,arg))))
							       ((<=) (list 'pair? arg))
							       ((>)  (list 'null? arg))
							       ((>=) `(or (null? ,arg) (null? (cdr ,arg))))
							       (else 'error)))))))))
		    ((and (eq? head '<)
			  (eqv? (caddr form) 1)
			  (len>1? (cadr form))           ; (< (vector-length x) 1) -> (equal? x #())
			  (memq (caadr form) '(string-length vector-length)))
		     (lint-format "perhaps ~A" caller (lists->string form (list (if (eq? (caadr form) 'string-length) 'string=? 'equal?)
										(cadadr form)
										(if (eq? (caadr form) 'string-length) "" #())))))
		    ((and (eq? head '>)
			  (eqv? (cadr form) 1)
			  (len>1? (caddr form))          ; (> 1 (string-length x)) -> (string=? x "")
			  (memq (caaddr form) '(string-length vector-length)))
		     (lint-format "perhaps ~A" caller (lists->string form (list (if (eq? (caaddr form) 'string-length) 'string=? 'equal?)
										(cadr (caddr form))
										(if (eq? (caaddr form) 'string-length) "" #())))))
		    ((and (eqv? (caddr form) 0)
			  (pair? (cadr form))
			  (eq? (caadr form) 'denominator))
		     (lint-format "denominator is ~A ~A than 0: ~A" caller
				  (if (memq head '(< <=)) "never" "always")
				  head
				  form))

		    ((and (memq head '(<= >=))
			  (or (and (eqv? (caddr form) 0)
				   (pair? (cadr form))  ; (<= (string-length m) 0) -> (= (string-length m) 0)
				   (hash-table-ref non-negative-ops (caadr form)))
			      (and (eqv? (cadr form) 0)
				   (pair? (caddr form))
				   (hash-table-ref non-negative-ops (caaddr form)))))
		     (lint-format "~A is never negative, so ~A" caller
				  ((if (eqv? (caddr form) 0) caadr caaddr) form)
				  (lists->string form (or (not (eq? (eq? head '<=)
								    (eqv? (caddr form) 0)))
							  (cons '= (cdr form))))))
		    ((and (eqv? (caddr form) 256)
			  (pair? (cadr form))          ; (< (char->integer key) 256) -> #t
			  (eq? (caadr form) 'char->integer))
		     (lint-format "perhaps ~A" caller
				  (lists->string form (and (memq head '(< <=)) #t))))

		    ((or (and (eqv? (cadr form) 0)     ; (> (numerator x) 0) -> (> x 0)
			      (len>1? (caddr form))
			      (eq? (caaddr form) 'numerator))
			 (and (eqv? (caddr form) 0)
			      (len>1? (cadr form))
			      (eq? (caadr form) 'numerator)))
		     (lint-format "perhaps ~A" caller
				  (lists->string form
						 (if (eqv? (cadr form) 0)
						     (list head (cadr form) (cadr (caddr form)))
						     (list head (cadadr form) (caddr form)))))))
	      (check-char-cmp caller head form)))
	  ;; could change (> x 0) to (positive? x) and so on, but the former is clear and ubiquitous

	  (for-each (lambda (f)
		      (hash-special f sp-<))
		    '(< > <= >=))) ; '= handled by sp-= above

	;; ---------------- char< char> etc ----------------
	(let ()
	  (define (sp-char< caller head form env)
	    ;; only once: (char<=? #\0 c #\1)
	    (let ((cleared-form (cons head ; keep operator
				      (remove-if (lambda (x)
						   (not (char? x)))
						 (cdr form)))))
	      (if (and (> (length cleared-form) 2)   ; (char>? x #\a #\b y)
		       (not (checked-eval cleared-form)))
		  (lint-format "this comparison can't be true: ~A" caller (truncated-list->string form))))
	    (if (and (eq? head 'char-ci=?)           ; (char-ci=? x #\return)
		     (len=3? form)
		     (or (and (char? (cadr form))
			      (char=? (cadr form) (other-case (cadr form))))
			 (and (char? (caddr form))
			      (char=? (caddr form) (other-case (caddr form))))))
		(lint-format "char-ci=? could be char=? here: ~A" caller form)

		(when (and (eq? head 'char=?)        ; (char=? #\a (char-downcase x)) -> (char-ci=? #\a x)
			   (pair? (cdr form))
			   (let ((casef (let ((op #f))
					  (lambda (a)
					    (or (char? a)
						(and (pair? a)
						     (memq (car a) '(char-downcase char-upcase))
						     (if op
							 (eq? op (car a))
							 (set! op (car a)))))))))
			     (lint-every? casef (cdr form))))
		  (lint-format "perhaps ~A" caller
			       (lists->string form   ; (char=? #\a (char-downcase x)) -> (char-ci=? #\a x)
					      (cons 'char-ci=? (map (lambda (a)
								      (if (and (len>1? a)
									       (memq (car a) '(char-upcase char-downcase)))
									  (cadr a)
									  a))
								    (cdr form))))))))
	  (for-each (lambda (f)
		      (hash-special f sp-char<))
		    '(char<? char>? char<=? char>=? char=? char-ci<? char-ci>? char-ci<=? char-ci>=? char-ci=?)))


	;; ---------------- string< string> etc ----------------
	(let ()
	  (define (sp-string< caller head form env)
	    (let ((cleared-form (cons head ; keep operator
				      (remove-if (lambda (x)
						   (not (string? x)))
						 (cdr form)))))
	      (if (and (> (length cleared-form) 2)  ; (string>? "a" x "b" y)
		       (not (checked-eval cleared-form)))
		  (lint-format "this comparison can't be true: ~A" caller (truncated-list->string form))))

	    ;; all the ops are upcase|downcase and all ops match.  Not unmatched ops because (string=? (upcase x) (downcase x)) is not (string-ci=? x x)
	    (if (and (> (length form) 2)
		     (let ((casef (let ((op #f))
				    (lambda (a)
				      (and (pair? a)
					   (memq (car a) '(string-downcase string-upcase))
					   (if op
					       (eq? op (car a))
					       (set! op (car a))))))))
		       (lint-every? casef (cdr form))))
		(lint-format "perhaps ~A" caller    ; (string=? (string-downcase x) (string-downcase y)) -> (string-ci=? x y)
			     (lists->string form
					    (let ((op (case head
							((string=?) 'string-ci=?)
							((string<=?) 'string-ci<=?)
							((string>=?) 'string-ci>=?)
							((string<?) 'string-ci<?)
							((string>?) 'string-ci>?)
							(else))))
					      (cons op (map (lambda (a)
							      (if (and (pair? a)
								       (memq (car a) '(string-upcase string-downcase)))
								  (cadr a)
								  a))
							    (cdr form)))))))

	    (if (lint-any? (lambda (a) ; string-copy is redundant in arg list
			     (and (len=2? a)
				  (memq (car a) '(copy string-copy))))
			   (cdr form))
		(let cleaner ((args (cdr form)) (new-args ())) ; (string=? "" (string-copy "")) -> (string=? "" "")
		  (if (not (pair? args))
		      (lint-format "perhaps ~A" caller (lists->string form (cons head (reverse new-args))))
		      (let ((a (car args)))
			(cleaner (cdr args)
				 (cons (if (and (len=2? a)
						(memq (car a) '(copy string-copy)))
					   (cadr a)
					   a)
				       new-args))))))

	    (when (and (eq? head 'string=?)
		       (= (length form) 3))
	      (unless (string->char= caller form form)     ; (string=? "x" (string y))
		(string->char= caller (cons (car form) (reverse (cdr form))) form))

	      (if (and (pair? (cadr form))                 ; (string=? (symbol->string a) (symbol->string b)) -> (eq? a b)
		       (eq? (caadr form) 'symbol->string)
		       (pair? (caddr form))
		       (eq? (caaddr form) 'symbol->string))
		  (lint-format "perhaps ~A" caller (lists->string form (list 'eq? (cadadr form) (cadr (caddr form))))))))

	  (for-each (lambda (f)
		      (hash-special f sp-string<))
		    '(string<? string>? string<=? string>=? string=? string-ci<? string-ci>? string-ci<=? string-ci>=? string-ci=?)))

	;; ---------------- length ----------------
	(let ()
	 (define (sp-length caller head form env)
	   (when (pair? (cdr form))
	     (if (pair? (cadr form))
		 (let ((arg (cadr form)))
		   (if (eq? (car arg) 'list)              ; (length (list 'a 'b 'c)) -> 3
		       (lint-format "perhaps ~A" caller (lists->string form (- (length arg) 1)))
		       (let ((arg-args (cdadr form)))
			 (when (pair? arg-args)
			   (case (car arg)
			     ((string->list vector->list)
			      (if (null? (cdr arg-args))           ; string->list has start:end etc ; (length (string->list x)) -> (length x)
				  (lint-format "perhaps ~A" caller (lists->string form (list 'length (car arg-args))))
				  (if (len>2? arg-args)
				      (if (and (integer? (caddr arg-args))  ; (length (vector->list x 1)) -> (- (length x) 1)
					       (integer? (cadr arg-args)))
					  (lint-format "perhaps ~A -> ~A" caller (truncated-list->string form) (max 0 (- (caddr arg-args) (cadr arg-args))))
					  (lint-format "perhaps ~A" caller (lists->string form (list '- (caddr arg-args) (cadr arg-args)))))
				      (lint-format "perhaps ~A" caller (lists->string form (list '- (list 'length (car arg-args)) (cadr arg-args)))))))

			     ((reverse reverse! list->vector list->string let->list)
			      (lint-format "perhaps ~A" caller (lists->string form (list 'length (car arg-args)))))

			     ((cons)                          ; (length (cons item items)) -> (+ (length items) 1)
			      (if (pair? (cdr arg-args))
				  (lint-format "perhaps ~A" caller (lists->string form (list '+ (list 'length (cadr arg-args)) 1)))))

			     ((make-list)                     ; (length (make-list 3)) -> 3
			      (lint-format "perhaps ~A" caller (lists->string form (car arg-args))))

			     ((append)                        ; (length (append x y)) -> (+ (length x) (length y))
			      (if (= (length arg) 3)
				  (lint-format "perhaps ~A" caller (lists->string form `(+ (length ,(car arg-args)) (length ,(cadr arg-args)))))))

			     ((quote)                         ; (length '(1 2 3)) -> 3
			      (if (list? (car arg-args))
				  (lint-format "perhaps ~A" caller (lists->string form (length (car arg-args)))))))))))

		 ;; not pair cadr
		 (if (code-constant? (cadr form))     ; (length 0) -> #f
		     (lint-format "perhaps ~A -> ~A" caller
				  (truncated-list->string form)
				  (length ((if (and (pair? (cadr form))
						    (eq? (caadr form) 'quote))
					       cadadr cadr)
					   form)))))))
	 (hash-special 'length sp-length))

	;; ---------------- zero? positive? negative? ----------------
	(let ()
	  (define (sp-zero? caller head form env)
	    (when (pair? (cdr form))
	      (let ((arg (cadr form)))
		(if (and (real? arg)                   ; (zero? 0) -> #t
			 (null? (cddr form))
			 (not (var-member head env)))
		    (lint-format "perhaps ~A" caller (lists->string form (eval/error caller form))))

		(when (pair? arg)
		  (if (and (eq? head 'negative?)   ; (negative? (string-length s))
			   (hash-table-ref non-negative-ops (car arg)))
		      (lint-format "~A can't be negative: ~A" caller (car arg) (truncated-list->string form))
		      (if (and (eq? head 'zero?)   ; (zero? (denominator x)) -> error
			       (eq? (car arg) denominator))
			  (lint-format "denominator can't be zero: ~A" caller form)
			  (when (proper-pair? (cdr arg))
			    (case (car arg)
			      ((-)
			       (lint-format "perhaps ~A" caller  ; (zero? (- x)) -> (zero? x), (positive? (- x y)) -> (> x y)
					    (lists->string form
							   (let ((op '((zero? = zero?) (positive? > negative?) (negative? < positive?))))
							     (if (null? (cddr arg))
								 (list (caddr (assq head op)) (cadr arg))
								 (list (cadr (assq head op)) (cadr arg)
								       (if (null? (cdddr arg))
									   (caddr arg)
									   (cons '+ (cddr arg)))))))))

			      ((abs magnitude)          ; (zero? (abs x)) -> (zero? x)
			       (if (eq? head 'zero?)
				   (lint-format "perhaps ~A" caller (lists->string form (cons 'zero? (cdr arg))))))

			      ((imag-part)              ; (zero? (imag-part x)) -> (real? x)
			       (if (eq? head 'zero?)
				   (lint-format "perhaps ~A" caller (lists->string form (cons 'real? (cdr arg))))))

			      ((numerator)              ; (negative? (numerator x)) -> (negative? x)
			       (lint-format "perhaps ~A" caller (lists->string form (list head (cadr arg)))))

			      ((remainder modulo)
			       (if (integer? (caddr arg))
				   (if (and (eq? head 'zero?)
					    (= (caddr arg) 2))
				       (lint-format "perhaps (assuming ~A is an integer) ~A"
						    caller (cadr arg) (lists->string form `(even? ,(cadr arg))))
				       (if (and (eq? (car arg) 'modulo)
						(not (eq? head 'zero?))
						(not (zero? (caddr arg))))
					   (lint-format "perhaps ~A" caller
							(lists->string form (case head
									      ((positive?) (positive? (caddr arg)))
									      ((negative?) (negative? (caddr arg)))
									      (else #f))))))))

			      ((string-length)          ; (zero? (string-length x)) -> (string=? x "")
			       (if (eq? head 'zero?)
				   (lint-format "perhaps ~A" caller (lists->string form (list 'string=? (cadr arg) "")))))

			      ((vector-length)          ; (zero? (vector-length c)) -> (equal? c #())
			       (if (eq? head 'zero?)
				   (lint-format "perhaps ~A" caller (lists->string form (list 'equal? (cadr arg) #())))))

			      ((length)                 ;  (zero? (length routes)) -> (null? routes)
			       (if (eq? head 'zero?)
				   (lint-format "perhaps (assuming ~A is list) use null? instead of length: ~A" caller (cadr arg)
						(lists->string form (list 'null? (cadr arg))))))))))))))

	  ;; (zero? (logand...)) is nearly always preceded by not and handled elsewhere
	  (for-each (lambda (f)
		      (hash-special f sp-zero?))
		    '(zero? positive? negative?)))

	;; ---------------- / ----------------
	(let ()
	 (define (sp-/ caller head form env)
	   (cond ((not (pair? (cdr form))))

		 ((and (null? (cddr form))
		       (number? (cadr form))
		       (zero? (cadr form)))                    ; (/ 0)
		  (lint-format "attempt to invert zero: ~A" caller (truncated-list->string form)))

		 ((and (pair? (cddr form))                     ; (/ x y 2 0)
		       (memv 0 (cddr form)))
		  (lint-format "attempt to divide by 0: ~A" caller (truncated-list->string form)))

		 (else
		  (let ((len (if (null? (cddr form))           ; (/ (length x))
				 (and (len=2? (cadr form))
				      (eq? (caadr form) 'length)
				      (cadr form))
				 (assq 'length (cddr form))))) ; (/ x y (length z))
		    (if (len=2? len)
			(lint-format "~A will cause division by 0 if ~A is empty" caller len (cadr len)))))))

	 (hash-special '/ sp-/))

	;; ---------------- copy ----------------
	(let ()
	  (define (sp-copy caller head form env)
	    (cond ((not (pair? (cdr form))))        ; (copy) argnum error reported elsewhere

		  ((or (number? (cadr form))        ; (copy 1)
		       (boolean? (cadr form))
		       (char? (cadr form))
		       (and (pair? (cadr form))     ; (copy (copy x)) -> (copy x)
			    (null? (cddr form))     ;   not (copy (copy x) (list ...))
			    (memq (caadr form) '(copy string-copy)))) ; or any maker?
		   (lint-format "~A could be ~A" caller (truncated-list->string form) (cadr form)))

		  ((and (pair? (cadr form))         ; (copy (copy x) ...) -> (copy x ...)
			(pair? (cddr form))
			(eq? (caadr form) 'copy)
			(pair? (cdadr form))
			(null? (cddadr form)))
		   (lint-format "~A could be ~A" caller (truncated-list->string form) `(copy ,(cadadr form) ,@(cddr form))))

		  ((equal? (cadr form) '(owlet))    ; (copy (owlet)) -> (owlet)
		   (lint-format "~A could be (owlet): owlet is copied internally" caller form))

		  ((not (pair? (cddr form))))

		  ((and (eq? (cadr form) (caddr form))  ; (copy x x)
			(or (null? (cdddr form))
			    (and (eqv? (cadddr form) 0) ; (copy x x 0)
				 (null? (cddddr form)))))
		   (lint-format "~A is a no-op" caller form))

		  ((= (length form) 5)
		   (check-start-and-end caller 'copy (cdddr form) form env))))
	  (hash-special 'copy sp-copy))

	;; ---------------- string-copy ----------------
	(let ()
	  (define (sp-string-copy caller head form env)
	    (if (and (pair? (cdr form))
		     (pair? (cadr form))   ; (string-copy (string-copy x)) could be (string-copy x)
		     (memq (caadr form) '(copy string-copy string make-string string-upcase string-downcase
					  string-append list->string symbol->string number->string)))
		(if (null? (cddr form))
		    (lint-format "~A could be ~A" caller (truncated-list->string form) (cadr form))
		    (if (and (pair? (cddr form))
			     (pair? (cdadr form))
			     (null? (cddadr form)))
			(lint-format "~A could be ~A" caller (truncated-list->string form) `(string-copy ,(cadadr form) ,@(cddr form)))))))
	  (hash-special 'string-copy sp-string-copy))

	;; ---------------- string-down|upcase ----------------
	(let ()
	  (define (sp-string-upcase caller head form env)
	    (if (and (pair? (cdr form))
		     (string? (cadr form))) ; (string-downcase "SPEAK") -> "speak"
	       (lint-format "perhaps ~A" caller (lists->string form
							       ((if (eq? head 'string-upcase) string-upcase string-downcase)
								(cadr form))))))
	  (hash-special 'string-upcase sp-string-upcase)
	  (hash-special 'string-downcase sp-string-upcase))

	;; ---------------- string ----------------
	(let ()
	  (define (sp-string caller head form env)
	    (if (lint-every? (lambda (x)
			       (and (char? x)
				    (char<=? #\space x #\~))) ; #\0xx chars here look dumb
			     (cdr form))
		(lint-format "~A could be ~S" caller (truncated-list->string form) (apply string (cdr form)))
		(if (and (pair? (cdr form))              ; (string (string-ref x 0)) -> (substring x 0 1)
			 (pair? (cadr form)))
		    (if (null? (cddr form))
			(if (eq? (caadr form) 'string-ref)
			    (let ((arg (cdadr form)))
			      (if (and (len>1? arg)
				       (integer? (cadr arg)))     ; (string (string-ref x 0)) -> (substring x 0 1)
				  (lint-format "perhaps ~A" caller
					       (lists->string form
							      (list 'substring (car arg) (cadr arg) (+ 1 (cadr arg))))))))
			(if (and (memq (caadr form) '(char-upcase char-downcase))
				 (lint-every? (lambda (p)
						(eq? (caadr form) (car p)))
					      (cddr form)))
			    ;; (string (char-downcase (string-ref x 1)) (char-downcase (string-ref x 2))) ->
			    ;;    (string-downcase (string (string-ref x 1) (string-ref x 2)))
			    (lint-format "perhaps ~A" caller
					 (lists->string form `(,(if (eq? (caadr form) 'char-upcase) 'string-upcase 'string-downcase)
							       (string ,@(map cadr (cdr form)))))))))))
	  ;; repeated args as in vector/list (sp-list below) got no hits
	  (hash-special 'string sp-string))

	;; ---------------- string? ----------------
	(let ()
	  (define (sp-string? caller head form env)
	    (if (and (pair? (cdr form))
		     (pair? (cadr form))
		     (memq (caadr form) '(format number->string)))
		(if (eq? (caadr form) 'format)                      ; (string? (number->string x)) -> #t
		    (lint-format "format returns either #f or a string, so ~A" caller (lists->string form (cadr form)))
		    (lint-format "number->string always returns a string, so ~A" caller (lists->string form #t)))
		(check-boolean-affinity caller form env)))
	  (hash-special 'string? sp-string?))

	;; ---------------- number? ----------------
	(let ()
	  (define (sp-number? caller head form env)
	    (if (and (pair? (cdr form))
		     (pair? (cadr form))
		     (eq? (caadr form) 'string->number))  ; (number? (string->number x)) -> (string->number x)
		(lint-format "string->number returns either #f or a number, so ~A" caller (lists->string form (cadr form)))
		(check-boolean-affinity caller form env)))
	  (hash-special 'number? sp-number?))

	;; ---------------- exact? inexact? infinite? nan? ----------------
	(let ()
	  (define (sp-exact? caller head form env)
	    (if (and (pair? (cdr form))
		     (number? (cadr form)))
		(check-boolean-affinity caller form env)))
	  (for-each (lambda (f)
		      (hash-special f sp-exact?))
		    '(exact? inexact? infinite? nan?)))

	;; ---------------- symbol? etc ----------------
	(let ()
	  (define (sp-symbol? caller head form env)
	    (check-boolean-affinity caller form env))
	  (for-each (lambda (f)
		      (hash-special f sp-symbol?))
		    '(symbol? rational? real? complex? float? keyword? gensym? byte-vector? proper-list? sequence? constant?
		      char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? byte?
		      output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?
		      syntax? undefined? unspecified?)))

	;; ---------------- pair? list? ----------------
	(let ()
	  (define (sp-pair? caller head form env)
	    (check-boolean-affinity caller form env)
	    (if (and (pair? (cdr form))     ; (pair? (member x y)) -> (member x y)
		     (pair? (cadr form))
		     (memq (caadr form) '(memq memv member assq assv assoc signature)))
		(lint-format "~A returns either #f or a pair, so ~A" caller (caadr form)
			     (lists->string form (cadr form)))))
	  (for-each (lambda (f)
		      (hash-special f sp-pair?))
		    '(pair? list?)))

	;; ---------------- integer? ----------------
	(let ()
	  (define (sp-integer? caller head form env)
	    (check-boolean-affinity caller form env)
	    (if (and (pair? (cdr form))    ; (integer? (char-position x y)) -> (char-position x y)
		     (pair? (cadr form))
		     (memq (caadr form) '(char-position string-position)))
		(lint-format "~A returns either #f or an integer, so ~A" caller (caadr form)
			     (lists->string form (cadr form)))))
	  (hash-special 'integer? sp-integer?))

	;; ---------------- null? ----------------
	(let ()
	  (define (sp-null? caller head form env)
	    (check-boolean-affinity caller form env)
	    (if (and (pair? (cdr form))  ; (null? (string->list x)) -> (zero? (length x))
		     (len>1? (cadr form))
		     (memq (caadr form) '(vector->list string->list let->list)))
		(lint-format "perhaps ~A" caller
			     (lists->string form (list 'zero? (list 'length (cadadr form)))))))
	  (hash-special 'null? sp-null?))

	;; ---------------- odd? even? ----------------
	(let ()
	  (define (sp-odd? caller head form env)
	    (if (and (pair? (cdr form))   ; (odd? (- x 1)) -> (even? x)
		     (len=3? (cadr form))
		     (memq (caadr form) '(+ -)))
		(let* ((arg1 (cadadr form))
		       (arg2 (caddr (cadr form)))
		       (int-arg (or (and (integer? arg1) arg1)
				    (and (integer? arg2) arg2))))
		  (if int-arg
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  (if (and (integer? arg1)
							   (integer? arg2))
						      (eval/error caller form)
						      (list (if (eq? (eq? head 'even?) (even? int-arg)) 'even? 'odd?)
							    (if (integer? arg1) arg2 arg1)))))))))
	  (hash-special 'odd? sp-odd?)
	  (hash-special 'even? sp-odd?))

	;; ---------------- string-ref ----------------
	(let ()
	  (define (sp-string-ref caller head form env)
	    (when (= (length form) 3)

	      (if (equal? (cadr form) "")
		  (lint-format "~A is an error" caller form)
		  (when (just-code-constants? (cdr form))      ; (string-ref "abc" 0) -> #\a
		    (catch #t
		      (lambda ()
			(let ((val (eval form)))
			  (lint-format "perhaps ~A" caller (lists->string form val))))
		      (lambda args
			(lint-format "~A: ~A" caller
				     (object->string form)
				     (apply format #f (cadr args)))))))

	      (when (pair? (cadr form))
		(let ((target (cadr form)))
		  (case (car target)
		    ((substring)                      ;  (string-ref (substring x 1) 2) -> (string-ref x (+ 2 1))
		     (if (= (length target) 3)
			 (lint-format "perhaps ~A" caller (lists->string form `(string-ref ,(cadr target) (+ ,(caddr form) ,(caddr target)))))))

		    ((symbol->string)                 ;  (string-ref (symbol->string 'abs) 1) -> #\b
		     (if (and (integer? (caddr form))
			      (pair? (cadr target))
			      (eq? (caadr target) 'quote)
			      (symbol? (cadadr target)))
			 (lint-format "perhaps ~A" caller (lists->string form (string-ref (symbol->string (cadadr target)) (caddr form))))))

		    ((make-string)                    ;  (string-ref (make-string 3 #\a) 1) -> #\a
		     (if (and (integer? (cadr target))
			      (integer? (caddr form))
			      (> (cadr target) (caddr form)))
			 (lint-format "perhaps ~A" caller (lists->string form (if (= (length target) 3) (caddr target) #\space))))))))))

	  (hash-special 'string-ref sp-string-ref))

	;; ---------------- vector-ref etc ----------------
	(let ()
	  (define (sp-vector-ref caller head form env)
	    (unless (= line-number last-checker-line-number)
	      (when (= (length form) 3)
		(let ((seq (cadr form)))

		  (when (code-constant? (cadr form))
		    (if (eqv? (length (cadr form)) 0)
			(lint-format "~A is an error" caller form)
			(when (just-code-constants? (cddr form))      ; (vector-ref #(1 2) 0) -> 1
			  (catch #t
			    (lambda ()
			      (let ((val (eval form)))
				(lint-format "perhaps ~A -> ~A~A" caller
					     (truncated-list->string form)
					     (if (or (pair? val)
						     (symbol? val))
						 "'" "")
					     (object->string val))))
			    (lambda args
			      (lint-format "~A: ~A" caller
					   (object->string form)
					   (apply format #f (cadr args))))))))

		  (if (and (eq? head 'list-ref)                       ; (list-ref lst 0) -> car etc
			   (memv (caddr form) '(0 1 2))
			   (not (and (pair? seq)
				     (memq (car seq) '(cdr cddr cdddr)))))
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  (if (and (pair? seq)
							   (eq? (car seq) 'list-ref)
							   (memv (caddr seq) '(0 1)))
						      (list (vector-ref #2d((caar cadar caddar)     ; 0 [0 1 2]
									    (caadr cadadr caddadr)) ; 1 [0 1 2]
									(caddr seq)
									(caddr form))
							    (cadr seq))
						      (list (case (caddr form)
							      ((0) 'car)
							      ((1) 'cadr)
							      ((2) 'caddr)
							      (else 'error))
							    seq))))
		      (when (pair? seq)
			(if (and (memq (car seq) '(vector-ref int-vector-ref float-vector-ref list-ref hash-table-ref let-ref))
				 (= (length seq) 3))  ; (vector-ref (vector-ref x i) j) -> (x i j)
			    (let ((seq1 (cadr seq)))  ;   x
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  (if (and (pair? seq1)   ; (vector-ref (vector-ref (vector-ref x i) j) k) -> (x i j k)
								   (memq (car seq1) '(vector-ref int-vector-ref float-vector-ref list-ref hash-table-ref let-ref))
								   (= (length seq1) 3))
							      (list (cadr seq1) (caddr seq1) (caddr seq) (caddr form))
							      (list seq1 (caddr seq) (caddr form))))))
			    (if (memq (car seq) '(make-vector make-list vector list
							      make-float-vector make-int-vector float-vector int-vector
							      make-hash-table hash-table
							      inlet))
				(lint-format "this doesn't make much sense: ~A" caller form)))
			(when (eq? head 'list-ref)
			  (if (eq? (car seq) 'quote)
			      (if (proper-pair? (cadr seq))  ; ignore dumb (list-ref () 0), (list-ref '(#t #f) (random 2)) -> (vector-ref #(#t #f) (random 2))
				  (lint-format "perhaps use a vector: ~A" caller
					       (lists->string form (list 'vector-ref (apply vector (cadr seq)) (caddr form)))))
			      (let ((index (caddr form)))    ; (list-ref (cdddr f) 2) -> (list-ref f 5)
				(if (and (memq (car seq) '(cdr cddr cdddr))
					 (or (integer? index)
					     (and (pair? index)
						  (eq? (car index) '-)
						  (integer? (caddr index)))))
				    (let ((offset (cdr (assq (car seq) '((cdr . 1) (cddr . 2) (cdddr . 3))))))
				      (lint-format "perhaps ~A" caller
						   (lists->string form
								  (list 'list-ref (cadr seq)
									(if (integer? index)
									    (+ index offset)
									    (let ((noff (- (caddr index) offset)))
									      (if (zero? noff)
										  (cadr index)
										  (list '- (cadr index) noff))))))))))))))))
	      (set! last-checker-line-number line-number)))
	  (for-each (lambda (f)
		      (hash-special f sp-vector-ref))
		    '(vector-ref list-ref hash-table-ref let-ref int-vector-ref float-vector-ref)))

	;; ---------------- vector-set! etc ----------------
	(let ()
	  (define (sp-vector-set! caller head form env)
	    (when (= (length form) 4)
	      (let ((target (cadr form))
		    (index (caddr form))
		    (val (cadddr form)))

		(cond ((and (len=3? val)                   ; (vector-set! x 0 (vector-ref x 0))
			    (eq? target (cadr val))
			    (equal? index (caddr val))
			    (memq (car val) '(vector-ref list-ref hash-table-ref string-ref let-ref float-vector-ref int-vector-ref)))
		       (lint-format "redundant ~A: ~A" caller head (truncated-list->string form)))

		      ((code-constant? target)             ; (vector-set! #(0 1 2) 1 3)??
		       (let ((len (length target)))
			 (cond ((eqv? len 0)
				(lint-format "~A has no elements, so ~A makes no sense" caller target (truncated-list->string form)))
			       ((and (integer? index)
				     (integer? len)
				     (>= index len))
				(lint-format "index ~A is too large in ~A" caller index (truncated-list->string form)))
			       (else
				(lint-format "~S is a constant, so ~A is problematic, and ~S is discarded; perhaps ~A" caller
					     target head target (lists->string form val))))))

		      ((not (pair? target)))

		      ((and (not (memq head '(string-set! hash-table-set! let-set!))) ; (vector-set! (vector-ref x 0) 1 2) -- vector within vector
			    (memq (car target) '(vector-ref list-ref float-vector-ref int-vector-ref)))
		       (lint-format "perhaps ~A" caller (lists->string form `(set! (,@(cdr target) ,index) ,val))))

		      ((and (not (eq? head 'string-set!)) ; (hash-table-set! (vector-ref x 0) 'a 2) -> (set! ((x 0) 'a) 2)
			    (memq (car target) '(vector-ref list-ref)))
		       (lint-format "perhaps ~A" caller (lists->string form `(set! ((,@(cdr target)) ,index) ,val))))

		      ((memq (car target) '(make-vector vector make-string string make-list list append cons
					    vector-append inlet sublet copy vector-copy string-copy list-copy
					    int-vector float-vector byte-vector string-append make-byte-vector
					    make-int-vector make-float-vector make-hash-table hash-table
					    )) ;list-copy is from r7rs
		       (lint-format "~A is simply discarded; perhaps ~A" caller
				    (truncated-list->string target)   ; (vector-set! (make-vector 3) 1 1) -- does this ever happen?
				    (lists->string form val)))

		      ((and (eq? head 'list-set!)
			    (memq (car target) '(cdr cddr cdddr cddddr))
			    (integer? (caddr form)))                  ; (list-set! (cdr x) 0 y) -> (list-set! x 1 y)
		       (lint-format "perhaps ~A" caller
				    (lists->string form
						   `(list-set! ,(cadr target) ,(+ (caddr form) (cdr-count (car target))) ,(cadddr form)))))))))
	  (for-each (lambda (f)
		      (hash-special f sp-vector-set!))
		    '(vector-set! list-set! hash-table-set! float-vector-set! int-vector-set! string-set! byte-vector-set! let-set!)))

	;; ---------------- subvector ----------------
	(let ()
	  (define (sp-subvector caller head form env)
	    (when (and (pair? (cdr form))
		       (pair? (cddr form)))
	      (let ((start (caddr form)))
		(if (null? (cdddr form))
		    (if (eqv? start 0)                           ; (subvector v 0) -> (subvector v)
		      (lint-format "perhaps ~A" caller (lists->string form (list 'subvector (cadr form)))))
		    (let ((end (cadddr form)))
		      (if (null? (cddddr form))
			  (if (and (eqv? start 0)
				   (len=2? end)
				   (eq? (cadr form) (cadr end))
				   (memq (car end) '(length vector-length)))
			      (lint-format "perhaps ~A" caller (lists->string form (list 'subvector (cadr form)))))
			  (let ((dims (list-ref form 4))) ;(car (cddddr form))))
			    (if (and (quoted-pair? dims)
				     (integer? start)
				     (integer? end)
				     (null? (cdadr dims))
				     (eqv? (caadr dims) (- end start)))
				(lint-format "perhaps ~A" caller (lists->string form (list 'subvector (cadr form) start end)))))))))))

	  (hash-special 'subvector sp-subvector))

	;; ---------------- object->string ----------------
	(let ()
	  (define (sp-object->string caller head form env)
	    (when (pair? (cdr form))
	      (if (pair? (cadr form)) ; (object->string (object->string x)) could be (object->string x)
		  (if (memq (caadr form) '(object->string symbol->string list->string number->string))
		      (lint-format "~A could be ~A" caller (truncated-list->string form) (cadr form))
		      (if (eq? (caadr form) 'string->symbol) ; (object->string (string->symbol x)) -> x
			  (lint-format "~A could be ~A" caller (truncated-list->string form) (cadadr form))))
		  (if (pair? (cddr form))
		      (let ((arg2 (caddr form)))
			(if (and (code-constant? arg2)                   ; (object->string x :else)
				 (not (memq arg2 '(#f #t :readable))))   ; #f and #t are display|write choice, :readable = ~W
			    (lint-format "bad second argument: ~A" caller arg2)))))))

	(hash-special 'object->string sp-object->string))

	(define (all-caps-warning arg)
	  (and (string? arg)
	       (or (string-position "ERROR" arg)
		   (string-position "WARN" arg))))

	;; ---------------- display ----------------
	(let ()
	  (define (sp-display caller head form env)
	    (when (pair? (cdr form))
	      (let ((arg (cadr form))
		    (port (if (pair? (cddr form))
			      (caddr form)
			      ())))
		(cond ((all-caps-warning arg)
		       (lint-format  "There's no need to shout: ~A" caller (truncated-list->string form)))

		      ((not port)
		       (lint-format "~A could be ~A" caller form (cadr form)))

		      ((not (len>1? arg)))

		      ((and (eq? (car arg) 'format)    ; (display (format #f str x)) -> (format () str x)
			    (not (cadr arg)))
		       (lint-format "perhaps ~A" caller (lists->string form (cons 'format (cons port (cddr arg))))))

		      ((and (eq? (car arg) 'apply)     ; (display (apply format #f str x) p) -> (apply format p str x)
			    (eq? (cadr arg) 'format)
			    (pair? (cddr arg))
			    (not (caddr arg)))
		       (lint-format "perhaps ~A" caller (lists->string form `(apply format ,port ,@(cdddr arg)))))

		      ((and (pair? port)
			    (eq? (car port) 'current-output-port))
		       (lint-format "(current-output-port) is the default port for display: ~A" caller form))))))

	  (hash-special 'display sp-display))

	;; ---------------- flush-output-port, newline, close-output-port ----------------
	(let ()
	  (define (sp-flush-output-port caller head form env)
	    (when (pair? (cdr form))
	      (if (and (pair? (cadr form))
		       (eq? (caadr form) 'current-output-port))
		  (lint-format "(current-output-port) is the default port for ~A: ~A" caller head form)
		  (unless (cadr form)
		    (lint-format "~A is a no-op, returning ~A" caller form (if (eq? head 'flush-output-port) #f))))))
	  (hash-special 'flush-output-port sp-flush-output-port)
	  (hash-special 'close-output-port sp-flush-output-port)
	  (hash-special 'newline sp-flush-output-port))

	;; ---------------- write-char, write-byte, write ----------------
	(let ()
	  (define (sp-write-char caller head form env)
	    (when (pair? (cdr form))
	      (when (pair? (cddr form))
		(if (and (pair? (caddr form))
			 (eq? (caaddr form) 'current-output-port))
		    (lint-format "(current-output-port) is the default port for ~A: ~A" caller head form)
		    (if (not (caddr form))
			(lint-format "~A could be ~A" caller form (cadr form)))))
	      (case head
		((write-byte)
		 (if (and (integer? (cadr form))
			  (not (<= 0 (cadr form) 255)))
		     (lint-format "write-byte argument must be (<= 0 byte 255): ~A" caller form)))
		((write-char)
		 (if (eqv? (cadr form) #\newline)
		     (lint-format "perhaps ~A" caller (lists->string form (cons 'newline (cddr form))))
		     (if (and (len>1? (cadr form))
			      (eq? (caadr form) 'integer->char))
			 (lint-format "perhaps ~A" caller (lists->string form (cons 'write-byte (cons (cadadr form) (cddr form)))))))))))

	  (hash-special 'write-char sp-write-char)
	  (hash-special 'write-byte sp-write-char)
	  (hash-special 'write sp-write-char))

	;; ---------------- read, port-filename, port-line-number, read-char, read-byte ----------------
	(let ()
	  (define (sp-read caller head form env)
	    (when (len=2? form)
	      (if (and (pair? (cadr form))
		       (eq? (caadr form) 'current-input-port))
		  (lint-format "(current-input-port) is the default port for ~A: ~A" caller head form)
		  (if (and (eq? head 'port-filename)
			   (memq (cadr form) '(*stdin* *stdout* *stderr*)))
		      (lint-format "~A: ~S" caller form
				   (case (cadr form) ((*stdin*) "*stdin*") ((*stdout*) "*stdout*") ((*stderr*) "*stderr*") (else 'error)))))))
	  (for-each (lambda (c)
		      (hash-special c sp-read))
		    '(read port-filename port-line-number read-char read-byte peek-char close-input-port)))

	;; ---------------- char-alphabetic? char-lower-case? char-numeric? char-upper-case? char-whitespace? etc ----------------
	(let ()
	  (define (sp-char-numeric caller head form env)
	    (if (and (len=1? (cdr form))
		     (char? (cadr form))
		     (not (var-member (car form) env)))
		(lint-format "perhaps ~A" caller (lists->string form (eval/error caller form)))))
	  (for-each (lambda (c)
		      (hash-special c sp-char-numeric))
		    '(char-alphabetic? char-lower-case? char-numeric? char-upper-case? char-whitespace? char-upcase char-downcase)))

	;; ---------------- make-vector etc ----------------
	(let ()
	  (define (sp-make-vector caller head form env)
	    ;; type of initial value (for make-float|int-vector) is checked elsewhere
	    (when (>= (length form) 3)
	      (case (caddr form)
		((#<unspecified>)
		 (if (eq? head 'make-vector)  ;  (make-vector 3 #<unspecified>)
		     (lint-format "#<unspecified> is the default initial value in ~A" caller form)))
		((0)
		 (if (not (eq? head 'make-vector))
		     (lint-format "0 is the default initial value in ~A" caller form)))
		((0.0)
		 (if (eq? head 'make-float-vector)
		     (lint-format "0.0 is the default initial value in ~A" caller form))))

	      (when (= (length form) 4)
		(let ((typer (cadddr form)))
		  (unless (memq typer setters)
		    (lint-format "~A is not a built-in type" caller (cadddr form))))))

	    (when (and (pair? (cdr form))
		       (integer? (cadr form))
		       (zero? (cadr form)))
	      (if (pair? (cddr form))           ; (make-vector 0 0.0)
		  (lint-format "initial value is pointless here: ~A" caller form))
	      (lint-format "perhaps ~A" caller (lists->string form #()))))

	  (for-each (lambda (f)
		      (hash-special f sp-make-vector))
		    '(make-vector make-int-vector make-float-vector)))

	;; ---------------- make-string make-byte-vector ----------------
	(let ()
	  (define (sp-make-string caller head form env)
	    (when (and (pair? (cdr form))
		       (integer? (cadr form))
		       (zero? (cadr form)))
	      (if (pair? (cddr form))           ; (make-byte-vector 0 0)
		  (lint-format "initial value is pointless here: ~A" caller form))
	      (lint-format "perhaps ~A" caller (lists->string form (if (eq? head 'make-string) "" #u())))))
	  (for-each (lambda (f)
		      (hash-special f sp-make-string))
		    '(make-string make-byte-vector)))

	;; ---------------- make-list ----------------
	(let ()
	  (define (sp-make-list caller head form env)
	    (when (and (pair? (cdr form))
		       (integer? (cadr form))
		       (zero? (cadr form)))
	      (if (pair? (cddr form))           ; (make-list 0 #f)
		  (lint-format "initial value is pointless here: ~A" caller form))
	      (lint-format "perhaps ~A" caller (lists->string form ()))))
	  (hash-special 'make-list sp-make-list))

	;; ---------------- reverse string->list etc ----------------
	(let ()
	  (define (sp-reverse caller head form env)
	    ;; not string->number -- no point in copying a number and it's caught below
	    (when (pair? (cdr form))

	      (if (just-code-constants? (cdr form))
		  (let ((seq (checked-eval form)))
		    (if (not (eq? seq :checked-eval-error))   ;  (symbol->string 'abs) -> "abs"
			(lint-format "perhaps ~A -> ~A~A" caller
				     (truncated-list->string form)
				     (if (pair? seq) "'" "")
				     (if (symbol? seq)
					 (object->string seq :readable)
					 (object->string seq))))))

	      (when (len>1? (cadr form))
		(let ((inverses '((reverse . reverse)
				  (reverse! . reverse!)
				  ;; reverse and reverse! are not completely interchangable:
				  ;;   (reverse (cons 1 2)): (2 . 1)
				  ;;   (reverse! (cons 1 2)): error: reverse! argument, (1 . 2), is a pair but should be a proper list
				  (list->vector . vector->list)
				  (vector->list . list->vector)
				  (symbol->string . string->symbol)
				  (string->symbol . symbol->string)
				  (list->string . string->list)
				  (string->list . list->string)
				  (number->string . string->number))))
		  (let ((inv-op (assq head inverses))
			(arg (cadr form))
			(arg-args (cdadr form))
			(arg-of-arg (cadadr form))
			(func-of-arg (caadr form)))
		    (if (pair? inv-op) (set! inv-op (cdr inv-op)))

		    (cond ((eq? func-of-arg inv-op)               ; (vector->list (list->vector x)) -> x
			   (if (eq? head 'string->symbol)
			       (lint-format "perhaps ~A" caller (lists->string form arg-of-arg))
			       (lint-format "~A could be (copy ~S)" caller form arg-of-arg)))

			  ((and (eq? head 'list->string)          ; (list->string (vector->list x)) -> (copy x (make-string (length x)))
				(eq? func-of-arg 'vector->list))
			   (lint-format "perhaps ~A" caller (lists->string form `(copy ,arg-of-arg (make-string (length ,arg-of-arg))))))

			  ((and (eq? head 'list->string)          ; (list->string (make-list x y)) -> (make-string x y)
				(eq? func-of-arg 'make-list))
			   (lint-format "perhaps ~A" caller (lists->string form (cons 'make-string arg-args))))

			  ((and (eq? head 'string->list)          ; (string->list (string x y)) -> (list x y)
				(eq? func-of-arg 'string))
			   (lint-format "perhaps ~A" caller (lists->string form (cons 'list arg-args))))

			  ((and (eq? head 'list->vector)          ; (list->vector (make-list ...)) -> (make-vector ...)
				(eq? func-of-arg 'make-list))
			   (lint-format "perhaps ~A" caller (lists->string form (cons 'make-vector arg-args))))

			  ((and (eq? head 'list->vector)          ; (list->vector (string->list x)) -> (copy x (make-vector (length x)))
				(eq? func-of-arg 'string->list))
			   (lint-format "perhaps ~A" caller (lists->string form `(copy ,arg-of-arg (make-vector (length ,arg-of-arg))))))

			  ((and (eq? head 'list->vector)          ; (list->vector (append (vector->list v1) ...)) -> (append v1 ...)
				(eq? func-of-arg 'append)
				(lint-every? (lambda (a)
					       (and (pair? a)
						    (eq? (car a) 'vector->list)))
					     (cdadr form)))
			   (lint-format "perhaps ~A" caller
					(lists->string form (cons 'append (map cadr (cdadr form))))))

			  ((and (eq? head 'vector->list)          ; (vector->list (make-vector ...)) -> (make-list ...)
				(eq? func-of-arg 'make-vector))
			   (lint-format "perhaps ~A" caller (lists->string form (cons 'make-list arg-args))))

			  ((and (eq? head 'vector->list)          ; (vector->list (vector ...)) -> (list ...)
				(eq? func-of-arg 'vector))
			   (lint-format "perhaps ~A" caller (lists->string form (cons 'list arg-args))))

			  ((and (eq? head 'vector->list)          ; (vector->list (vector-copy ...)) -> (vector->list ...)
				(eq? func-of-arg 'vector-copy))
			   (lint-format "perhaps ~A" caller (lists->string form (cons 'vector->list arg-args))))

			  ((and (memq func-of-arg '(reverse reverse! copy))
				(len>1? arg-of-arg)                ; (list->string (reverse (string->list x))) -> (reverse x)
				(eq? (car arg-of-arg) inv-op))
			   (lint-format "perhaps ~A" caller (lists->string form (list (if (eq? func-of-arg 'reverse!) 'reverse func-of-arg) (cadr arg-of-arg)))))

			  ((and (memq head '(reverse reverse!))   ; (reverse (string->list x)) -> (string->list (reverse x)) -- often redundant
				(memq func-of-arg '(string->list vector->list sort!)))
			   (cond ((not (eq? func-of-arg 'sort!))
				  (if (null? (cdr arg-args))
				      (lint-format "perhaps less consing: ~A" caller
						   (lists->string form (list func-of-arg (list 'reverse arg-of-arg))))))
				 ((and (len>1? arg-args)          ; (reverse (sort! x <)) -> (sort! x >)
				       (hash-table-ref reversibles (cadr arg-args)))
				  => (lambda (op)
				       (lint-format "possibly ~A" caller (lists->string form (list 'sort! arg-of-arg op)))))))

			  ((and (len>1? arg-of-arg)
				(or (memq func-of-arg '(cdr cddr cdddr cddddr))
				    (and (eq? func-of-arg 'list-tail)
					 (len=2? arg-args)))
				(case head
				  ((list->string) (eq? (car arg-of-arg) 'string->list))
				  ((list->vector) (eq? (car arg-of-arg) 'vector->list))
				  (else #f)))
			   (let ((len-diff (if (eq? func-of-arg 'list-tail)
					       (cadr arg-args)
					       (cdr-count func-of-arg))))
			     (lint-format "perhaps ~A" caller     ; (list->string (cdr (string->list x))) -> (substring x 1)
					  (lists->string form (if (eq? head 'list->string)
								  (list 'substring (cadr arg-of-arg) len-diff)
								  `(copy ,(cadr arg-of-arg) (make-vector (- (length ,(cadr arg-of-arg)) ,len-diff))))))))

			  ((and (memq head '(list->vector list->string))
				(eq? func-of-arg 'sort!)          ; (list->vector (sort! (vector->list x) y)) -> (sort! x y)
				(len>1? arg-of-arg)
				(len>1? arg-args)
				(eq? (car arg-of-arg) (if (eq? head 'list->vector) 'vector->list 'string->list)))
			   (lint-format "perhaps ~A" caller (lists->string form (list 'sort! (cadr arg-of-arg) (cadr arg-args)))))

			  ((and (memq head '(list->vector list->string))
				(or (memq func-of-arg '(list cons))
				    (quoted-undotted-pair? arg)))
			   (let ((maker (if (eq? head 'list->vector) 'vector 'string)))
			     (case func-of-arg
			       ((list)
				(if (var-member maker env)    ; (list->string (list x y z)) -> (string x y z)
				    (lint-format "~A could be simplified, but you've shadowed '~A" caller (truncated-list->string form) maker)
				    (lint-format "perhaps ~A" caller (lists->string form (cons maker arg-args)))))

			       ((cons)
				(if (and (len>1? arg-args)
					 (any-null? (cadr arg-args)))
				    (if (var-member maker env) ; (list->string (cons x ())) -> (string x)
					(lint-format "~A could be simplified, but you've shadowed '~A" caller (truncated-list->string form) maker)
					(lint-format "perhaps ~A" caller (lists->string form (list maker arg-of-arg)))))))))

			  ((and (memq head '(list->string list->vector))    ; (list->string (reverse x)) -> (reverse (apply string x))
				(memq func-of-arg '(reverse reverse!)))
			   (lint-format "perhaps ~A" caller (lists->string form (list 'reverse (list head arg-of-arg)))))

			  ((and (eq? head 'string->symbol)        ; (string->symbol (string-append...)) -> (symbol ...)
				(or (memq func-of-arg '(string-append append))
				    (and (eq? func-of-arg 'apply)
					 (memq arg-of-arg '(string-append append)))))
			   (lint-format "perhaps ~A" caller
					(lists->string form
						       (if (eq? func-of-arg 'apply)
							   (cons 'apply (cons 'symbol (cdr arg-args)))
							   (cons 'symbol arg-args)))))

			  ((and (eq? head 'string->symbol)        ; (string->symbol (if (not (null? x)) x "abc")) -> (if (not (null? x)) (string->symbol x) 'abc)
				(eq? func-of-arg 'if)
				(len>2? arg-args)
				(or (string? (cadr arg-args))
				    (string? (caddr arg-args)))
				(not (or (equal? (cadr arg-args) "")  ; this is actually an error -- should we complain?
					 (equal? (caddr arg-args) ""))))
			   (lint-format "perhaps ~A" caller
					(lists->string form
						       (if (string? (cadr arg-args))
							   (if (string? (caddr arg-args))
							       `(if ,arg-of-arg ',(string->symbol (cadr arg-args)) ',(string->symbol (caddr arg-args)))
							       `(if ,arg-of-arg ',(string->symbol (cadr arg-args)) (string->symbol ,(caddr arg-args))))
							   `(if ,arg-of-arg (string->symbol ,(cadr arg-args)) ',(string->symbol (caddr arg-args)))))))

			  ((case head                             ; (reverse (reverse! x)) could be (copy x)
			     ((reverse) (eq? func-of-arg 'reverse!))
			     ((reverse!) (eq? func-of-arg 'reverse))
			     (else #f))
			   (lint-format "~A could be (copy ~S)" caller form arg-of-arg))

			  ((and (len>1? arg-of-arg)                ; (op (reverse (inv-op x))) -> (reverse x)
				(eq? func-of-arg 'reverse)
				(eq? inv-op (car arg-of-arg)))
			   (lint-format "perhaps ~A" caller (lists->string form (list 'reverse (cadr arg-of-arg)))))))))

	      (when (pair? (cddr form))                           ; (string->list x y y) is ()
		(when (and (memq head '(vector->list string->list))
			   (pair? (cdddr form)))
		  (check-start-and-end caller head (cddr form) form env))

		(when (and (eq? head 'number->string)             ; (number->string saturation 10)
			   (eqv? (caddr form) 10))
		  (lint-format "10 is the default radix for number->string: ~A" caller (truncated-list->string form))))

	      (when (memq head '(reverse reverse!))
		(if (eq? head 'reverse!)
		    (if (symbol? (cadr form))
			(let ((v (var-member (cadr form) env)))
			  (if (and v
				   (eq? (var-definer v) 'parameter))
			      (lint-format "if ~A (a function argument) is a pair, ~A is ill-advised" caller
					   (cadr form)
					   (truncated-list->string form))))
			(if (code-constant? (cadr form))
			    (lint-format "~A is a constant, so ~A is problematic" caller
					   (cadr form)
					   (truncated-list->string form)))))

		(when (pair? (cadr form))
		  (let ((arg (cadr form))
			(arg-op (caadr form))
			(arg-args (cdadr form))
			(arg-arg (and (pair? (cdadr form)) (cadadr form))))
		    (when (and (pair? arg-args)
			       (pair? arg-arg))
		      (if (and (case arg-op
				 ((cdr)       (len=1? arg-args))  ; (reverse (cdr (reverse lst))) = all but last of lst -> copy to len-1
				 ((list-tail) (len=2? arg-args))
				 (else #f))
			       (memq (car arg-arg) '(reverse reverse!))
			       (pair? (cdr arg-arg))
			       (symbol? (cadr arg-arg)))
			  (lint-format "perhaps ~A" caller
				       (lists->string form `(copy ,(cadr arg-arg)
								  (make-list (- (length ,(cadr arg-arg)) ,(if (eq? arg-op 'cdr) 1 (cadr arg-args))))))))

		      (if (and (eq? arg-op 'append)           ; (reverse (append (reverse b) res)) = (append (reverse res) b)
			       (eq? (car arg-arg) 'reverse)
			       (len=1? (cdr arg-args)))
			  (lint-format "perhaps ~A" caller (lists->string form `(append (reverse ,(cadr arg-args)) ,(cadr arg-arg))))))

		    (when (and (= (length arg) 3)
			       (pair? (cadr arg-args)))
		      (cond ((and (eq? arg-op 'map)              ; (reverse (map abs (sort! x <))) -> (map abs (sort! x >))
				  (eq? (caadr arg-args) 'sort!)
				  (len=2? (cdadr arg-args))
				  (hash-table-ref reversibles (caddr (cadr arg-args))))
			     => (lambda (op)
				  (lint-format "possibly ~A" caller (lists->string form `(,arg-op ,arg-arg (sort! ,(cadadr arg-args) ,op)))))))
		      ;; (reverse (apply vector (sort! x <))) doesn't happen (nor does this map case, but it's too pretty to leave out)

		      (if (and (eq? arg-op 'cons)             ; (reverse (cons x (reverse lst))) -- adds x to end -- (append lst (list x))
			       (memq (caadr arg-args) '(reverse reverse!))
			       (len=1? (cdadr arg-args)))
			  (lint-format "perhaps ~A" caller (lists->string form `(append ,(cadadr arg-args) (list ,arg-arg)))))))))))

	  (for-each (lambda (f)
		      (hash-special f sp-reverse))
		    '(reverse reverse! list->vector vector->list list->string string->list symbol->string string->symbol number->string)))

	;; ---------------- char->integer string->number etc ----------------
	(let ()
	  (define (sp-char->integer caller head form env)
	    ;(format *stderr* "c->i ~S~%" form)
	    (when (pair? (cdr form))
	      (let ((inverses '((char->integer . integer->char)
				(integer->char . char->integer)
				(symbol->keyword . keyword->symbol)
				(keyword->symbol . symbol->keyword)
				(string->number . number->string)))
		    (arg (cadr form)))
		(if (and (len>1? arg)                       ;  (string->number (number->string x)) could be x
			 (eq? (car arg) (cond ((assq head inverses) => cdr))))
		    (lint-format "~A could be ~A" caller (truncated-list->string form) (cadr arg))
		    (case head
		      ((integer->char)
		       (if (let walk ((tree (cdr form)))
			     (if (pair? tree)
				 (and (walk (car tree))
				      (walk (cdr tree)))
				 (or (code-constant? tree)
				     (not (side-effect? tree env)))))
			   (let ((chr (checked-eval form)))  ; (integer->char (+ (char->integer #\space) 215)) -> #\xf7
			     (if (char? chr)
				 (lint-format "perhaps ~A" caller (lists->string form chr))))))

		      ((char->integer)
		       (if (and (pair? arg)
				(memq (car arg) '(read read-char)))
			   (lint-format "perhaps ~A" caller (lists->string form (cons 'read-byte (cdr arg))))))

		      ((string->number)
		       (if (and (pair? (cddr form))
				(integer? (caddr form)) ; type error is checked elsewhere
				(not (<= 2 (caddr form) 16)))  ;  (string->number "123" 21)
			   (lint-format "string->number radix should be between 2 and 16: ~A" caller form)
			   (if (and (len=2? arg)
				    (eq? (car arg) 'string)
				    (null? (cddr form)))  ; (string->number (string num-char)) -> (- (char->integer num-char) (char->integer #\0))
			       (lint-format "perhaps ~A" caller
					    (lists->string form `(- (char->integer ,(cadr arg)) (char->integer #\0)))))))

		      ((symbol->keyword)
		       (if (and (len>1? arg)             ;  (symbol->keyword (string->symbol x)) -> (string->keyword x)
				(eq? (car arg) 'string->symbol))
			   (lint-format "perhaps ~A" caller (lists->string form (list 'string->keyword (cadr arg))))
			   (if (quoted-symbol? arg)
			       (lint-format "perhaps ~A" caller (lists->string form (symbol->keyword (cadr arg)))))))

		      ((keyword->symbol)
		       (if (and (len>1? arg)
				(eq? (car arg) 'string->keyword))
			   (lint-format "perhaps ~A" caller (lists->string form (list 'string->symbol (cadr arg))))
			   (if (keyword? arg)
			       (lint-format "perhaps ~A -> '~A" caller (object->string form) (object->string (keyword->symbol arg)))))))))))

	  (for-each (lambda (f)
		      (hash-special f sp-char->integer))
		    '(char->integer integer->char symbol->keyword keyword->symbol string->number)))

	;; ---------------- symbol ----------------
	(let ()
	  (define (sp-symbol caller head form env)
	    (when (pair? (cdr form))

	      (if (just-code-constants? (cdr form))           ; (symbol "a") -> 'a
		  (let ((seq (checked-eval form)))
		    (if (and (not (eq? seq :checked-eval-error))
			     (not (char=? (string-ref (object->string seq :readable) 0) #\()))   ; (symbol "a b") turns into itself
			(lint-format "perhaps ~A -> ~A~A" caller
				     (truncated-list->string form)
				     (if (pair? seq) "'" "")
				     (if (symbol? seq)
					 (object->string seq :readable)
					 (object->string seq))))))

	      (when (and (null? (cddr form))                  ; (symbol (string-append a b)) -> (symbol a b)
			 (pair? (cadr form))
			 (or (memq (caadr form) '(string-append append))
			     (and (eq? (caadr form) 'apply)
				  (memq (cadadr form) '(string-append append)))))
		(lint-format "perhaps ~A" caller
			     (lists->string form
					    (if (eq? (caadr form) 'apply)
						(cons 'apply (cons 'symbol (cddadr form)))
						(cons 'symbol (cdadr form))))))))
	  (hash-special 'symbol sp-symbol))

	;; ---------------- string->keyword ----------------
	(let ()
	  (define (sp-str->key caller head form env)
	    (if (and (pair? (cdr form))
		     (len>1? (cadr form)))
		(if (eq? (caadr form) 'symbol->string)
		    (lint-format "perhaps ~A" caller (lists->string form (list 'symbol->keyword (cadadr form))))
		    (if (and (memq (caadr form) '(string-append append))
			     (string? (cadadr form))
			     (> (length (cadadr form)) 0)
			     (eqv? ((cadadr form) 0) #\:))
			(lint-format "string->keyword prepends #\\: for you: ~A" caller form)))))
	  (hash-special 'string->keyword sp-str->key))

	;; ---------------- string-append ----------------
	(let ()
	  (define (sp-string-append caller head form env)
	    (unless (= line-number last-checker-line-number)
	      (let ((args (remove-all "" (splice-if 'string-append (cdr form))))
		    (combined #f))
		(when (lint-any? (lambda (s)
				   (or (string? s)
				       (and (pair? s)
					    (memq (car s) '(string apply)))))
				 args)
		  (do ((nargs ())               ; look for (string...) (string...) in the arg list and combine
		       (p args (cdr p)))
		      ((null? p)
		       (set! args (reverse nargs)))
		    (let ((arg (car p)))
		      (cond ((and (len=3? arg)            ; (string-append (apply string-append strs) str) -> (string-append (apply values strs) str)
				  (eq? (car arg) 'apply)  ;    unfortunately the values version is only slightly faster
				  (eq? (cadr arg) 'string-append))
			     (set! nargs (cons (list 'apply 'values (caddr arg)) nargs)))

			    ((not (pair? (cdr p)))
			     (set! nargs (cons arg nargs)))

			    ((and (pair? arg)
				  (eq? (car arg) 'string)
				  (pair? (cadr p))
				  (eq? (caadr p) 'string))
			     (set! nargs (cons (cons 'string (append (cdr arg) (cdadr p))) nargs))
			     (set! combined #t)
			     (set! p (cdr p)))

			    ((and (string? arg)
				  (string? (cadr p)))
			     (set! nargs (cons (string-append arg (cadr p)) nargs))
			     (set! combined #t)
			     (set! p (cdr p)))

			    (else (set! nargs (cons (car p) nargs)))))))

		;; (if ... "" ...) as arg split out got a couple dozen hits but we still need copy for the "" branch, so it's not much better

		(cond ((null? args)                 ; (string-append) -> ""
		       (lint-format "perhaps ~A" caller (lists->string form "")))

		      ((null? (cdr args))           ; (string-append a) -> a
		       (if (not (tree-memq 'values (cdr form)))
			   (lint-format "perhaps ~A~A" caller (lists->string form (car args))
					(if combined "" ", or use copy")))) ; (string-append x "") appears to be a common substitute for string-copy

		      ((lint-every? string? args)        ; (string-append "a" "b") -> "ab"
		       (lint-format "perhaps ~A" caller (lists->string form (apply string-append args))))

		      ((lint-every? (lambda (a)          ; (string-append "a" (string #\b)) -> "ab"
				      (or (string? a)
					  (and (pair? a)
					       (eq? (car a) 'string)
					       (or (null? (cdr a))
						   (char? (cadr a))))))
				    args)
		       (catch #t
			 (lambda ()                 ; (string-append (string #\C) "ZLl*()def") -> "CZLl*()def"
			   (let ((val (if (not (any-pairs? args))
					  (apply string-append args)
					  (eval (cons 'string-append args)))))
			     (lint-format "perhaps ~A -> ~S" caller (truncated-list->string form) val)))
			 (lambda args #f)))

		      ((lint-every? (lambda (c)          ; (string-append (make-string 3 #\a) (make-string 2 #\b)) -> (format #f "~NC~NC" 3 #\a 2 #\b)
				      (and (len>2? c)
					   (eq? (car c) 'make-string)))
				    (cdr form))
		       (lint-format "perhaps ~A" caller
				    (lists->string form
						   `(format #f ,(apply string-append (make-list (abs (length (cdr form))) "~NC"))
							    ,@(map (lambda (c) (values (cadr c) (caddr c))) (cdr form))))))

		      ((not (equal? args (cdr form)))  ; (string-append x (string-append y z)) -> (string-append x y z)
		       (lint-format "perhaps ~A" caller (lists->string form (cons 'string-append args)))))
		(set! last-checker-line-number line-number))))
	  (hash-special 'string-append sp-string-append))

	;; ---------------- vector-append ----------------
	(let ()
	  (define (sp-vector-append caller head form env)
	    (unless (= line-number last-checker-line-number)
	      (let ((args (map (lambda (v)
				 (if (and (len=3? v)
					  (eq? (car v) 'apply)
					  (eq? (cadr v) 'vector-append))
				     (list 'apply 'values (caddr v))
				     v))
			       (remove-all #() (splice-if 'vector-append (cdr form))))))
		(cond ((null? args)                    ;  (vector-append) -> #()
		       (lint-format "perhaps ~A" caller (lists->string form #())))

		      ((null? (cdr args))              ; (vector-append x) -> (copy x)
		       (lint-format "perhaps ~A" caller (lists->string form (list 'copy (car args)))))

		      ((lint-every? vector? args)           ;  (vector-append #(1 2) (vector-append #(3))) -> #(1 2 3)
		       (lint-format "perhaps ~A" caller (lists->string form (apply vector-append args))))

		      ((not (equal? args (cdr form)))  ; (vector-append x (vector-append y z)) -> (vector-append x y z)
		       (lint-format "perhaps ~A" caller (lists->string form (cons 'vector-append args)))))
		(set! last-checker-line-number line-number))))
	  (hash-special 'vector-append sp-vector-append))

	;; ---------------- cons ----------------
	(let ()
	  (define (sp-cons caller head form env)
	    (when (and (= (length form) 3)
		       (not (= last-cons-line-number line-number)))
	      (if (any-null? (caddr form))                  ; (cons x '()) -> (list x)
		  (lint-format "perhaps ~A" caller (lists->string form (list 'list (cadr form))))

		  (when (pair? (caddr form))
		    (let ((op (caaddr form)))

		      (cond ((or (eq? op 'list)         ; (cons x (list ...)) -> (list x ...)
				 (and (eq? op 'list-values)
				      (not (tree-memq 'apply-values (cdaddr form)))))
			     (lint-format "perhaps ~A" caller (lists->string form (cons 'list (cons (cadr form) (unlist-values (cdaddr form)))))))

			    ((and (pair? (cadr form))                   ; (cons (car x) (cdr x)) -> (copy x)
				  (let ((x (assq (caadr form)           ;    but if cdr is a pair, copy is more expensive and slightly different
						 '((car cdr #t)
						   (caar cdar car) (cadr cddr cdr)
						   (caaar cdaar caar) (caadr cdadr cadr) (caddr cdddr cddr) (cadar cddar cdar)
						   (cadddr cddddr cdddr) (caaaar cdaaar caaar) (caaadr cdaadr caadr) (caadar cdadar cadar)
						   (caaddr cdaddr caddr) (cadaar cddaar cdaar) (cadadr cddadr cdadr) (caddar cdddar cddar)))))
				    (and x
					 (eq? (cadr x) op)
					 (caddr x))))
			     => (lambda (cfunc)
				  (if (and cfunc
					   (equal? (cadadr form) (cadr (caddr form)))
					   (not (side-effect? (cadadr form) env)))
				      (lint-format "possibly ~A" caller
						   (lists->string form
								  (list 'copy
									(if (symbol? cfunc)
									    (list cfunc (cadadr form))
									    (cadadr form))))))))
			    (else
			     (case op
			       ((cons)   ; list handled above
					; (cons a (cons b (cons ...))) -> (list a b ...), input ending in nil of course
				(let loop ((args (list (cadr form))) (chain (caddr form)))
				  (if (pair? chain)
				      (if (eq? (car chain) 'list)
					  (begin
					    (lint-format "perhaps ~A" caller
							 (lists->string form
									(cons 'list (append (reverse args) (cdr chain)))))
					    (set! last-cons-line-number line-number))
					  (if (and (eq? (car chain) 'cons)
						   (len>1? (cdr chain)))
					      (if (any-null? (caddr chain))
						  (begin
						    (lint-format "perhaps ~A" caller (lists->string form `(list ,@(reverse args) ,(cadr chain))))
						    (set! last-cons-line-number line-number))
						  (if (and (pair? (caddr chain))
							   (memq (caaddr chain) '(cons list)))
						      (loop (cons (cadr chain) args) (caddr chain)))))))))

			       ((else)
				(lint-format "else (as car of second argument to cons) makes no sense: ~A" caller form))))))))))

	  (hash-special 'cons sp-cons))

	;; ---------------- append ----------------
	(let ()
	  (define (append->list . items)
	    (let ((lst (list 'list)))
	      (for-each
	       (lambda (item)
		 (set! lst (append lst (if (eq? (car item) 'list)
					   (cdr item)
					   ((if (eq? (car item) 'cons) list distribute-quote)
					    (cadr item))))))
	       items)
	      lst))

	  (define (transform-append form)
	    (let ((len1 (- (length form) 1))
		  (args (cdr form)))
	      (case len1
		((0) ())
		((1) (car args))
		((2)
		 (let ((arg2 (cadr args))
		       (arg1 (car args)))
		   (cond ((or (any-null? arg2)
			      (equal? arg2 '(list)))
			  (list 'copy arg1))

			 ((null? arg1)
			  arg2)

			 ((not (pair? arg1))
			  form)

			 ((and (pair? arg2)
			       (or (eq? (car arg1) 'list)
				   (and (eq? (car arg1) 'cons)
					(any-null? (caddr arg1)))
				   (quoted-undotted-pair? arg1))
			       (or (eq? (car arg2) 'list)
				   (and (eq? (car arg2) 'cons)
					(len>1? (cdr arg2))
					(any-null? (caddr arg2)))
				   (quoted-undotted-pair? arg2)))
			  (apply append->list args))

			 ((and (eq? (car arg1) 'list)
			       (len=1? (cdr arg1)))
			  (list 'cons (cadr arg1) arg2))

			 ((eq? (car arg1) 'cons)
			  (if (any-null? (caddr arg1))
			      (list 'cons (cadr arg1) arg2)
			      (let ((cargs `(append ,(caddr arg1) ,arg2)))
				`(cons ,(cadr arg1) ,cargs))))

			 ((and (eq? (car arg1) 'list)
			       (len=2? (cdr arg1)))
			  `(cons ,(cadr arg1) (cons ,(caddr arg1) ,arg2)))

			 ((and (quoted-pair? arg1)
			       (null? (cdadr arg1)))
			  (if (or (symbol? (caadr arg1))
				  (pair? (caadr arg1)))
			      `(cons ',(caadr arg1) ,arg2)
			      (list 'cons (caadr arg1) arg2)))

			 ((not (equal? (cdr form) args))
			  (cons 'append args))

			 (else form))))

		      (else
		       (cond ((lint-every? (lambda (item)
					     (and (pair? item)
						  (or (eq? (car item) 'list)
						      (and (eq? (car item) 'cons)
							   (len>1? (cdr item))
							   (any-null? (caddr item)))
						      (quoted-undotted-pair? item))))
					   args)
			      (apply append->list args))

			     ((and (len=2? (car args))
				   (eq? (caar args) 'list))
			      (let ((cargs (transform-append `(append ,@(cdr args)))))
			      `(cons ,(cadar args) ,cargs)))

			     ((and (pair? (car args))
				   (eq? (caar args) 'cons))
			      (let ((cargs `(append ,(caddar args) ,@(cdr args))))
				`(cons ,(cadar args) ,cargs)))

			     ((let ((n-1 (list-ref args (- len1 2))))
				(and (len=2? n-1)
				     (eq? (car n-1) 'list)))
			      `(append ,@(copy args (make-list (- len1 2)))
				       (cons ,(cadr (list-ref args (- len1 2)))
					     ,(list-ref args (- len1 1)))))

			     ((not (equal? (cdr form) args))
			      (cons 'append args))

			     (else form))))))

	  (define (sp-append caller head form env)
	    (unless (= line-number last-checker-line-number)
	      (set! last-checker-line-number line-number)
	      (letrec ((splice-append (lambda (lst)
					(cond ((null? lst)
					       ())

					      ((not (pair? lst))
					       lst)

					      ((and (pair? (car lst))
						    (eq? (caar lst) 'append)
						    (proper-list? (cdar lst))) ; for append below
					       (if (null? (cdar lst))          ; (append) at end -> () to keep copy intact?
						   (case (cdr lst) ((()) => list) (else => splice-append))
						   (append (splice-append (cdar lst))
							   (splice-append (cdr lst)))))

					      ((and (len=2? (car lst))
						    (eq? (caar lst) 'copy)
						    (pair? (cdr lst)))
					       (cons (cadar lst) (splice-append (cdr lst))))

					      ((and (len=3? (car lst))   ; (append (apply append x)...) -> (append (apply values x)...)
						    (eq? (caar lst) 'apply)
						    (memq (cadar lst) '(append string-append vector-append)))
					       (cons (list 'apply 'values (caddar lst))
						     (splice-append (cdr lst))))

					      ((or (null? (cdr lst))
						   (not (or (any-null? (car lst))
							    (and (len=1? (car lst))
								 (eq? (caar lst) 'list)))))
					       (cons (car lst)
						     (splice-append (cdr lst))))

					      (else (splice-append (cdr lst)))))))

		(let ((new-args (splice-append (cdr form))))     ; (append '(1) (append '(2) '(3))) -> (append '(1) '(2) '(3))
		  (let ((len1 (length new-args))
			(suggestion made-suggestion))

		    (when (and (> len1 2)
			       (null? (list-ref new-args (- len1 1)))
			       (pair? (list-ref new-args (- len1 2)))
			       (memq (car (list-ref new-args (- len1 2))) '(list cons append map string->list vector->list make-list)))
		      (set-cdr! (list-tail new-args (- len1 2)) ())
		      (set! len1 (- len1 1)))

		    (if (positive? len1)
			(let ((last (list-ref new-args (- len1 1))))
			  ;; (define (f) (append '(1) '(2))) (define a (f)) (set! (a 1) 32) (f) -> '(1 32)
			  (if (quoted-pair? last)
			      (lint-format "append does not copy its last argument, so ~A is dangerous" caller
					   (truncated-list->string form)))))

		    (case len1
		      ((0)					        ; (append) -> ()
		       (lint-format "perhaps ~A" caller (lists->string form ())))
		      ((1)                                               ; (append x) -> x
		       (lint-format "perhaps ~A" caller (lists->string form (car new-args))))
		      ((2)                                               ; (append (list x) ()) -> (list x)
		       (let ((arg2 (cadr new-args))
			     (arg1 (car new-args)))

			 (cond ((or (any-null? arg2)
				    (member arg2 '((list) "" (string) #() (vector)))) ; (append '(1 2) ()) -> (copy '(1 2)), #() includes #i() et al
				(lint-format "perhaps clearer: ~A" caller (lists->string form (list 'copy arg1))))

			       ((null? arg1)                             ; (append () x) -> x
				(lint-format "perhaps ~A" caller (lists->string form arg2)))

			       ((not (pair? arg1)))

			       ((and (pair? arg2)                        ; (append (list x y) '(z)) -> (list x y z) or extensions thereof
				     (or (eq? (car arg1) 'list)
					 (and (eq? (car arg1) 'cons)
					      (any-null? (caddr arg1)))
					 (quoted-undotted-pair? arg1))
				     (or (eq? (car arg2) 'list)
					 (and (eq? (car arg2) 'cons)
					      (len>1? (cdr arg2))
					      (any-null? (caddr arg2)))
					 (quoted-undotted-pair? arg2)))
				(lint-format "perhaps ~A" caller (lists->string form (apply append->list new-args))))

			       ((and (eq? (car arg1) 'list)              ; (append (list x) y) -> (cons x y)
				     (len=1? (cdr arg1)))
				(lint-format "perhaps ~A" caller (lists->string form (list 'cons (cadr arg1) arg2))))

			       ((eq? (car arg1) 'cons)                   ; (append (cons x y) z) -> (cons x z) or (cons z (append y z))
				(lint-format "perhaps ~A" caller         ;    append insists on proper lists, so this should be equivalent
					     (lists->string form
							    (if (any-null? (caddr arg1))
								(list 'cons (cadr arg1) arg2)
								`(cons ,(cadr arg1) (append ,(caddr arg1) ,arg2))))))

			       ((and (eq? (car arg1) 'list)              ; (append (list x y) z) -> (cons x (cons y z))
				     (len=2? (cdr arg1)))
				(lint-format "perhaps ~A" caller (lists->string form `(cons ,(cadr arg1) (cons ,(caddr arg1) ,arg2)))))

			       ;; not sure about this: reports the un-qq'd form
			       ((and (eq? (car arg1) 'list-values)
				     (not (qq-tree? arg1)))
				(set! last-checker-line-number -1)
				(sp-append caller 'append (list 'append (unlist-values arg1) arg2) env))

			       ((and (eq? (car arg1) 'vector->list)
				     (pair? arg2)
				     (eq? (car arg2) 'vector->list))
				(lint-format "perhaps ~A" caller (lists->string form (list 'vector->list (list 'append (cadr arg1) (cadr arg2))))))

			       ((and (quoted-pair? arg1)                 ; (append '(x) y) -> (cons 'x y)
				     (null? (cdadr arg1)))
				(lint-format "perhaps ~A" caller
					     (lists->string form
							    (if (or (symbol? (caadr arg1))
								    (pair? (caadr arg1)))
								`(cons ',(caadr arg1) ,arg2)
								(list 'cons (caadr arg1) arg2)))))

			       ((not (equal? (cdr form) new-args))      ; (append () '(1 2) 1) -> (append '(1 2) 1)
				(lint-format "perhaps ~A" caller (lists->string form (cons 'append new-args)))))))
		      (else
		       (cond ((lint-every? (lambda (item)
					     (and (pair? item)
						  (or (eq? (car item) 'list)
						      (and (eq? (car item) 'cons)
							   (len>1? (cdr item))
							   (any-null? (caddr item)))
						      (quoted-undotted-pair? item))))
					   new-args)                         ; (append '(1) (append '(2) '(3)) '(4)) -> (list 1 2 3 4)
			      (lint-format "perhaps ~A" caller (lists->string form (apply append->list new-args))))

			     ((and (len=2? (car new-args))               ; (append (list x) y (list z)) -> (cons x (append y (list z)))?
				   (eq? (caar new-args) 'list))
			      (let ((cargs (transform-append `(append ,@(cdr new-args)))))
				(lint-format "perhaps ~A" caller (lists->string form `(cons ,(cadar new-args) ,cargs)))))

			     ((and (pair? (car new-args))
				   (eq? (caar new-args) 'cons))
			      (let ((cargs (transform-append `(append ,(caddar new-args) ,@(cdr new-args)))))
				(lint-format "perhaps ~A" caller
					     (lists->string form `(cons ,(cadar new-args) ,cargs)))))

			     ((let ((n-1 (list-ref new-args (- len1 2))))
				(and (len=2? n-1)
				     (eq? (car n-1) 'list)))            ; (append x (list y) z) -> (append x (cons y z))
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  `(append ,@(copy new-args (make-list (- len1 2)))
								   (cons ,(cadr (list-ref new-args (- len1 2)))
									 ,(list-ref new-args (- len1 1)))))))

			     ((not (equal? (cdr form) new-args))        ; (append x y (append)) -> (append x y ())
			      (lint-format "perhaps ~A" caller (lists->string form (cons 'append new-args)))))))

		    (if (and (= made-suggestion suggestion)
			     (not (equal? (cdr form) new-args)))
			(lint-format "perhaps ~A" caller (lists->string form (cons 'append new-args)))))))))
	  (hash-special 'append sp-append)
	  ;(hash-special '[list*] sp-append) ; [list*] in the new code?
	  )

	;; ---------------- apply ----------------
	(let ()
	  (define (sp-apply caller head form env)
	    (when (pair? (cdr form))
	      (let ((len (length form))
		    (suggestion made-suggestion))
		(if (= len 2)                ; (apply f) -> (f)
		    (lint-format "perhaps ~A" caller (lists->string form (list (cadr form))))
		    (if (not (or (<= len 2) ; it might be (apply)...
				 (symbol? (cadr form))
				 (applicable? (cadr form))))
			(lint-format "~S is not applicable: ~A" caller (cadr form) (truncated-list->string form))
			(let ((f (cadr form)))
			  (unless (or (<= len 2)
				      (any-macro? f env)
				      (eq? f 'macroexpand)) ; handled specially (syntactic, not a macro)

			    (when (and (symbol? f)
				       (not (var-member f env)))
			      (let ((func (symbol->value f *e*)))
				(if (procedure? func)
				    (let ((ary (arity func)))
				      (when (pair? ary)             ; (apply real? 1 3 rest)
					(if (> (- len 3) (cdr ary)) ; last apply arg might be var=()
					    (lint-format "too many arguments for ~A: ~A" caller f form))
					(if (and (= len 3)
						 (= (car ary) 1)
						 (= (cdr ary) 1))   ; (apply car x) -> (car (car x))
					    (lint-format "perhaps ~A" caller (lists->string form (list f (list 'car (caddr form)))))))))))

			    (let ((happy #f)
				  (last-arg (form (- len 1))))
			      (if (and (not (list? last-arg))
				       (code-constant? last-arg))   ; (apply + 1)
				  (lint-format "last argument should be a list: ~A" caller (truncated-list->string form))
				  (if (= len 3)
				      (let ((args (caddr form))
					    (cdr-args (and (pair? (caddr form)) (cdaddr form))))
					(if (identity? f)                         ; (apply (lambda (x) x) y) -> (car y)
					    (lint-format "perhaps (assuming ~A is a list of one element) ~A" caller args
							 (lists->string form (list 'car args)))
					    (if (simple-lambda? f)                ; (apply (lambda (x) (f x)) y) -> (f (car y))
						(lint-format "perhaps (assuming ~A is a list of one element) ~A" caller args
							     (lists->string form (tree-subst (list 'car args) (caadr f) (caddr f))))))

					(cond ((eq? f 'list)                      ; (apply list x) -> x?
					       (lint-format "perhaps ~A" caller (lists->string form args)))

					      ((any-null? args)                   ; (apply f ()) -> (f)
					       (lint-format "perhaps ~A" caller (lists->string form (list f))))

					      ((or (not (pair? args))
						   (case (car args)
						     ((list)             ; (apply f (list a b)) -> (f a b)
						      (lint-format "perhaps ~A" caller (lists->string form (cons f cdr-args))))

						     ((quote)            ; (apply eq? '(a b)) -> (eq? 'a 'b)
						      (and (pair? cdr-args)
							   (pair? (car cdr-args))
							   (lint-format "perhaps ~A" caller
									(lists->string form (cons f (distribute-quote (car cdr-args)))))))

						     ((cons cons*)             ; (apply f (cons a b)) -> (apply f a b)
						      (lint-format "perhaps ~A" caller
								   (lists->string form
										  (if (and (len>1? cdr-args)
											   (len>1? (cadr cdr-args))
											   (eq? (caadr cdr-args) 'cons))
										      `(apply ,f ,(car cdr-args) ,@(cdadr cdr-args))
										      (cons 'apply (cons f cdr-args))))))

						     ((append)           ; (apply f (append (list ...)...)) -> (apply f ... ...)
						      (and (pair? cdr-args)
							   (pair? (car cdr-args))
							   (eq? (caar cdr-args) 'list)
							   (lint-format "perhaps ~A" caller
									(lists->string form `(apply ,f ,@(cdar cdr-args)
												    ,(if (not (pair? (cdr cdr-args)))
													 (cdr cdr-args)
													 (if (null? (cddr cdr-args))
													     (cadr cdr-args)
													     (cons 'append (cdr cdr-args)))))))))

						     ((reverse reverse!)   ; (apply vector (reverse x)) -> (reverse (apply vector x))
						      (and (memq f '(string vector int-vector float-vector))
							   (pair? cdr-args)
							   (lint-format "perhaps ~A" caller
									(lists->string form (list 'reverse (list 'apply f (car cdr-args)))))))

						     ((make-list)          ; (apply string (make-list x y)) -> (make-string x y)
						      (if (memq f '(string vector))
							  (lint-format "perhaps ~A" caller
								       (lists->string form
										      (cons (if (eq? f 'string) 'make-string 'make-vector)
											    cdr-args)))))

						     ((map)
						      (case f
							((string-append)        ; (apply string-append (map ...))
							 (if (eq? (car cdr-args) 'symbol->string)
							     (lint-format "perhaps ~A" caller ; (apply string-append (map symbol->string ...))
									  (lists->string form (list 'format #f "~{~A~}" (cadr cdr-args))))
							     (if (simple-lambda? (car cdr-args))
								 (let ((body (caddar cdr-args)))
								   (if (and (len=3? body)
									    (eq? (car body) 'string-append)
									    (or (and (string? (cadr body))
										     (eq? (caddr body) (caadar cdr-args)))
										(and (string? (caddr body))
										     (eq? (cadr body) (caadar cdr-args)))))
								       (let ((str (string-append "~{"
												 (if (string? (cadr body)) (cadr body) "~A")
												 (if (string? (caddr body)) (caddr body) "~A")
												 "~}")))
									 (lint-format "perhaps ~A" caller
										      (lists->string form (list 'format #f str (cadr cdr-args))))))))))

							((string)          ; (apply string (map char-downcase x)) -> (string-downcase (apply string x))
							 (if (memq (car cdr-args) '(char-upcase char-downcase))
							     (lint-format "perhaps, assuming ~A is a list, ~A" caller (cadr cdr-args)
									  (lists->string form (list (if (eq? (car cdr-args) 'char-upcase)
													'string-upcase 'string-downcase)
												    (list 'apply string (cadr cdr-args)))))))

							((append)        ; (apply append (map vector->list args)) -> (vector->list (apply append args))
							 (case (car cdr-args)
							   ((vector->list)
							    (lint-format "perhaps ~A" caller (lists->string form `(vector->list (apply append ,@(cdr cdr-args))))))
							   ((list)       ; (apply append (map list args)) -> args
							    (lint-format "perhaps ~A" caller (lists->string form (cadr cdr-args))))))

							(else #f)))
						     ;; (apply append (map f ...)) is very common but changing it to
						     ;;     (map (lambda (x) (apply values (f x))) ...)
						     ;;     is not an obvious win.  The code is more complicated, and currently apply values
						     ;;     copies its args as do apply and append -- how many copies are there here?!
						     ;; cursory timing tests indicate that (apply append ...) is faster

						     ;; need to check for only one apply values
						     ((list-values)          ; (apply f `(,x ,@z)) -> (apply f x z)
						      (let ((last-arg (last-ref args)))
							(if (and (pair? last-arg)
								 (eq? (car last-arg) 'apply-values)
								 (= (tree-count 'apply-values args 2) 1))
							    (lint-format "perhaps ~A" caller
									 (lists->string form
											`(apply ,f
												,@(copy args (make-list (- (length args) 2)) 1)
												,(cadr last-arg))))
							    (if (not (tree-set-memq '(append apply-values) cdr-args))
								(lint-format "perhaps ~A" caller
									     (lists->string form
											    (cons f (unlist-values cdr-args)))))))))))))
				      ;; len > 3
				      (unless (hash-table-ref syntaces f)                ; also not any-macro I presume
					(when (and (pair? last-arg)
						   (eq? (car last-arg) 'list))           ; (apply f y z (list a b)) -> (f y z a b)
					  (lint-format "perhaps ~A" caller
						       (lists->string form
								      (append (copy (cdr form) (make-list (- len 2)))
									      (cdr last-arg)))))
					;; can't cleanly go from (apply write o p) to (write o (car p)) since p can be ()

					(unless happy
					  (if (any-null? last-arg)                       ; (apply f ... ()) -> (f ...)
					      (lint-format "perhaps ~A" caller (lists->string form (cons f (copy (cddr form) (make-list (- len 3)))))))
					  (if (and (= len 4)                             ; (apply f (car x) (cdr x)) -> (apply f x)
						   (len=2? (caddr form))
						   (eq? (caaddr form) 'car)
						   (len=2? (cadddr form))
						   (eq? (car (cadddr form)) 'cdr)
						   (eq? (cadr (caddr form)) (cadr (cadddr form))))
					      (lint-format "perhaps ~A" caller (lists->string form `(apply ,(cadr form) ,(cadr (caddr form))))))
					  )))))))))
		(if (and (= suggestion made-suggestion)
			 (symbol? (cadr form)))
		    (let ((ary (arg-arity (cadr form) env)))
		      (if (and (pair? ary)             ; (apply make-string tcnt initializer) -> (make-string tcnt (car initializer))
			       (= (car ary) (cdr ary)) ; else () as last, so can't suggest (car last)
			       (= (cdr ary) (- len 2)))
			  (lint-format "perhaps ~A" caller
				       (lists->string form (append (copy (cdr form) (make-list (- len 2)))
								   (list (list 'car (list-ref form (- len 1)))))))))))))

	  (hash-special 'apply sp-apply))

	;; ---------------- format ----------------
	(let ()
	  (define count-directives
	    (let ((format-control-char (let ((chars (make-vector 256 #f)))
					 (for-each
					  (lambda (c)
					    (vector-set! chars (char->integer c) #t))
					  '(#\A #\S #\C #\F #\E #\G #\O #\D #\B #\X #\P #\N #\W #\, #\{ #\} #\* #\@
					    #\a #\s #\c #\f #\e #\g #\o #\d #\b #\x #\p #\n #\w
					    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
					 chars)))
	      (lambda (head str caller form)
		(let ((curlys 0)
		      (dirs 0)
		      (tildes 1)
		      (returns 0)
		      (pos (char-position #\~ str)))
		  (when pos
		    (do ((len (length str))
			 (tilde-time #t)
			 (i (+ pos 1) (+ i 1)))
			((>= i len)
			 (if tilde-time         ; (format #f "asdf~")
			     (lint-format "~A control string ends in tilde: ~A" caller head (truncated-list->string form))))
		      (if tilde-time
			  (let ((c (string-ref str i)))
			    ;(format *stderr* "~C ~S~%" c str)
			    (when (and (= curlys 0)
				       (not (memv c '(#\~ #\T #\t #\& #\% #\^ #\| #\newline #\}))) ; ~* consumes an arg
				       (not (and (char=? c #\:) (< i (- len 1)) (memv (string-ref str (+ i 1)) '(#\D #\d))))
				       (not (call-with-exit
					     (lambda (return)
					       (do ((k i (+ k 1)))
						   ((= k len) #f)
						 ;; this can be confused by pad chars in ~T
						 (unless (or (char-numeric? (string-ref str k))
							     (char=? (string-ref str k) #\,))
						   ;(format *stderr* "~C ~C~%" (string-ref str k) c)
						   (return (or (char=? (string-ref str k) #\') ; ~12,'-T 
							       (char-ci=? (string-ref str k) #\t)))))))))
			      ;; the possibilities are endless, so I'll stick to the simplest
			      (if (not (vector-ref format-control-char (char->integer c))) ; (format #f "~H" 1)
				  (lint-format "unrecognized format directive: ~C in ~S, ~S" caller c str form))
			      (set! dirs (+ dirs 1))

			      ;; ~n so try to figure out how many args are needed (this is not complete)
			      (when (char-ci=? c #\n)
				(let ((j (+ i 1)))
				  (if (>= j len)             ; (format p "~A~A" x)
				      (lint-format "missing format directive: ~S" caller str)
				      (begin
					;; if ,n -- add another, if then not T, add another
					(cond ((not (char=? (string-ref str j) #\,)))
					      ((>= (+ j 1) len)
					       (lint-format "missing format directive: ~S" caller str))
					      ((char-ci=? (string-ref str (+ j 1)) #\n)
					       (set! dirs (+ dirs 1))
					       (set! j (+ j 2)))
					      ((char-numeric? (string-ref str (+ j 1)))
					       (set! j (+ j 2)))
					      (else (set! j (+ j 1))))
					(if (>= j len)
					    (lint-format "missing format directive: ~S" caller str)
					    (if (not (char-ci=? (string-ref str j) #\t))
						(set! dirs (+ dirs 1)))))))))

			    (set! tilde-time #f)
			    (case c
			      ((#\{) (set! curlys (+ curlys 1)))
			      ((#\}) (set! curlys (- curlys 1)))
			      ((#\%) (set! returns (+ returns 1)))
			      ((#\^ #\|)
			       (if (zero? curlys)   ; (format #f "~^")
				   (lint-format "~A has ~~~C outside ~~{~~}?" caller str c))))
			    (if (and (< (+ i 2) len)
				     (member (substring str i (+ i 3)) '("%~&" "^~^" "|~|" "&~&" "\n~\n") string=?))
				(lint-format "~A in ~A could be ~A" caller  ;  (format #f "~%~&")
					     (substring str (- i 1) (+ i 3))
					     str
					     (substring str (- i 1) (+ i 1)))))
			  (begin ; not tilde-time
			    (set! pos (char-position #\~ str i))
			    (if pos
				(begin
				  (set! tildes (+ tildes 1))
				  (set! tilde-time #t)
				  (set! i pos))
				(set! i len))))))

		  (if (not (= curlys 0))   ;  (format #f "~{~A" 1)
		      (lint-format "~A has ~D unmatched ~A~A: ~A"
				   caller head
				   (abs curlys)
				   (if (positive? curlys) "{" "}")
				   (if (> curlys 1) "s" "")
				   (truncated-list->string form)))
		  (if (and (= tildes returns)
			   (not (cadr form))                ; (format #f "...~%..." -> "...\n..."
			   (positive? tildes))
		      (lint-format "format is not needed in ~S: perhaps use \"~A\" instead"
				   caller form
				   (let ((strc (copy str))
					 (len (length str)))
				     (do ((i 0 (+ i 1)))
					 ((>= i len) strc)
				       (when (char=? (string-ref str i) #\~)
					 (string-set! strc i #\\)
					 (set! i (+ i 1))
					 (string-set! strc i #\n))))))
		  dirs))))

	  (define (sp-format caller head form env)
	    (if (< (length form) 3)
		(begin
		  (cond ((< (length form) 2)               ; (format)
			 (lint-format "~A has too few arguments: ~A" caller head (truncated-list->string form)))

			((and (pair? (cadr form))          ; (format (format #f str))
			      (eq? (caadr form) 'format))
			 (lint-format "redundant format: ~A" caller (truncated-list->string form)))

			((and (code-constant? (cadr form)) ; (format 1)
			      (not (string? (cadr form))))
			 (lint-format "format with one argument takes a string: ~A" caller (truncated-list->string form)))

			((and (string? (cadr form))        ; (format "str") -> str
			      (eq? head 'format)           ; not snd-display, error, etc
			      (not (char-position #\~ (cadr form))))
			 (lint-format "perhaps ~A" caller (lists->string form (cadr form)))))
		  env)

		(let ((control-string ((if (string? (cadr form)) cadr caddr) form))
		      (args ((if (string? (cadr form)) cddr cdddr) form)))

		  (when (eq? head 'format)
		    (if (string? (cadr form)) ; (format "s")
			(lint-format "please include the port argument to format, perhaps ~A" caller (cons 'format (cons () (cdr form))))
			(if (and (not (cadr form))
				 (member (caddr form) '("~S" "~A"))
				 (= (length form) 4)
				 (not (and (pair? (cadddr form))
					   (let ((sig (arg-signature (car (cadddr form)) env)))
					     (and (pair? sig)
						  (or (eq? (car sig) 'string?)
						      (and (pair? (car sig))
							   (memq 'string? (car sig)))))))))
			    (lint-format "perhaps ~A" caller             ; (format #f "~S" x) -> (object->string x)
					 (lists->string form
							(cons 'object->string
							      (cons (cadddr form)
								    (if (string=? (caddr form) "~A") '(#f) ())))))))
		    (if (and (eq? (cadr form) 't)                          ; (format t " ")
			     (not (var-member 't env)))
			(lint-format "'t in ~A should probably be #t" caller (truncated-list->string form))))

		  (if (lint-any? all-caps-warning (cdr form))
		      (lint-format "There's no need to shout: ~A" caller (truncated-list->string form)))

		  (if (not (string? control-string))
		      (if (not (proper-list? args))
			  (lint-format "~S looks suspicious" caller form))
		      (let ((ndirs (count-directives head control-string caller form))
			    (nargs (if (list? args) (length args) 0)))
			(let ((pos (char-position #\null control-string)))
			  (if (and pos (< pos (length control-string)))  ; (format #f "~a\x00b" x)
			      (lint-format "#\\null in a format control string will confuse both lint and format: ~S in ~A" caller control-string form)))
			(cond ((not (or (= ndirs nargs)
					(tree-memq 'values form)))
			       (lint-format "~A has ~A arguments: ~A"       ; (format #f "~nT" 1 2)
					    caller head
					    (if (> ndirs nargs) "too few" "too many")
					    (truncated-list->string form))) ; this can be confused by (e.g) (format () "~A~96,'-T~A~%" red-text normal-text)

			      ((and (eq? head 'format)
				    (not (cadr form))                   ; (format #f "123")
				    (zero? ndirs)
				    (not (char-position #\~ control-string)))
			       (lint-format "~A could be ~S, (format is a no-op here)" caller (truncated-list->string form) (caddr form)))

			      ((string-position "~^~}" control-string)
			       (lint-format "pointless ~~^ in ~S" caller control-string)))))

		  (when (pair? args)
		    (let ((pos (and (string? control-string)
				    (char-position #\~ control-string))))
		      (for-each
		       (lambda (a)
			 (let ((directive (and (integer? pos)
					       (control-string (+ pos 1)))))
			   (if (integer? pos)
			       (set! pos (char-position #\~ control-string (+ pos 2))))

			   (if (quoted-symbol? a) ; not integer -- might be counter, keyword and things like () get no hits
			       (lint-format "perhaps put the argument ~A in the control string: ~A" caller
					    a (truncated-list->string form))

			       (when (and (len>1? a)
					  (not (memv directive '(#\S #\s))))
				 (case (car a)
				   ((number->string)
				    (if (null? (cddr a))                      ; (format #f "~A" (number->string x))
					(lint-format "~A argument ~A could be ~A" caller head a (cadr a))
					(if (and (pair? (cddr a))
						 (integer? (caddr a))
						 (memv (caddr a) '(2 8 10 16)))
					    (if (= (caddr a) 10)
						(lint-format "~A argument ~A could be ~A" caller head a (cadr a))
						(lint-format "~A argument ~A could use the format directive ~~~A and change the argument to ~A" caller head a
							     (case (caddr a) ((2) "B") ((8) "O") (else "X"))
							     (cadr a))))))

				   ((symbol->string string->symbol) ; (format #f "~A" (symbol->string 'x))
				    (lint-format "~A argument ~A could be ~A" caller head a (cadr a)))

				   ((object->string) ; object->string fourth arg is max string len returned
				    (if (< (length a) 4)
					(lint-format "~A argument ~A could be ~A" caller head a (cadr a))))

				   ((make-string)                             ; (format #f "~A" (make-string len c))
				    (if (pair? (cddr a))
					(lint-format "~A argument ~A could use the format directive ~~NC and change the argument to ... ~A ~A ..." caller head a
						     (cadr a) (if (char? (caddr a)) (format #f "~W" (caddr a)) (caddr a)))))

				   ((apply)
				    (if (and (len=3? a)
					     (memq (cadr a) '(append string-append vector-append)))
					(lint-format "use ~~{...~~} rather than ~A: ~A" caller (cadr a) a)))

				   ((string-append)                           ; (format #f "~A" (string-append x y))
				    (if (eq? head 'format)
					(lint-format "format appends strings, so ~A seems wasteful" caller a))))))))
		       args))))))
	  (hash-special 'format sp-format))

	;; ---------------- error/throw ----------------
	(let ()
	  (define (sp-error caller head form env)
	    (when (pair? (cdr form))
	      (let* ((tag (cadr form))
		     (eq (eqf tag env)))
		(if (string? tag)
		    (lint-format "~A's first argument should be a catchable tag, not ~S" caller head tag)
		    (if (not (member eq '((eq? eq?) (#t #t))))
			(lint-format "~A tag ~S is unreliable (catch uses eq? to match tags)" caller 'error tag)))
		(if (and (proper-pair? (cddr form))
			 (string? (caddr form)))
		    ((hash-table-ref special-case-functions 'format) caller head form env)))))

	  (hash-special 'error sp-error)
	  (hash-special 'throw sp-error))

	;; ---------------- sort! ----------------
	(let ()
	  (define (sp-sort caller head form env)
	    (when (= (length form) 3)
		(let ((func (caddr form)))
		  (if (memq func '(= eq? eqv? equal? string=? char=? string-ci=? char-ci=?))
		      (lint-format "sort! with ~A may hang: ~A" caller func (truncated-list->string form))
		      (if (symbol? func)
			  (let ((sig (signature (symbol->value func (rootlet)))))
			    (if (and (pair? sig)
				     (not (eq? 'boolean? (car sig)))
				     (not (and (pair? (car sig))
					       (memq 'boolean? (car sig)))))  ;  (sort! x abs)
				(lint-format "~A is a questionable sort! function" caller func))))))
		(let ((target (cadr form)))
		  (if (code-constant? target)            ; (sort #(1 2) <)
		      (if (eqv? (length target) 0)
			  (lint-format "~A is pointless" caller (truncated-list->string form))
			  (lint-format "~S is a constant, so ~A is problematic" caller
				       target (truncated-list->string form)))))))

	  (hash-special 'sort! sp-sort))

	;; ---------------- substring ----------------
	(let ()
	  (define (sp-substring caller head form env)
	    (if (just-code-constants? (cdr form))
		(catch #t
		  (lambda ()
		    (let ((val (eval form)))     ; (substring "abracadabra" 2 7) -> "racad"
		      (lint-format "perhaps ~A -> ~S" caller (truncated-list->string form) val)))
		  (lambda (type info)
		    (lint-format "~A -> ~A" caller (truncated-list->string form) (apply format #f info))))

		(let ((str (cadr form)))

		  (when (string? str)           ; (substring "++++++" 0 2) -> (make-string 2 #\+)
		    (let ((len (length str)))
		      (when (and (> len 0)
				 (string=? str (make-string len (string-ref str 0))))
			(lint-format "perhaps ~A" caller
				     (lists->string form
						    (let ((chars (if (null? (cddr form))
								     len
								     (if (pair? (cdddr form))
									 (if (eqv? (caddr form) 0)
									     (cadddr form)
									     (list '- (cadddr form) (caddr form)))
									 (list '- len (caddr form))))))
						      (list 'make-string chars (string-ref str 0))))))))

		  (when (pair? (cddr form))
		    (when (and (pair? str)                ; (substring (substring x 1) 2) -> (substring x 3)
			       (eq? (car str) 'substring))
		      (let ((inner-x (caddr str))
			    (inner-y (and (pair? (cdddr str))
					  (cadddr str)))
			    (outer-x (caddr form))
			    (outer-y (and (pair? (cdddr form))
					  (cadddr form))))
			(let ((x (if (and (integer? outer-x)
					  (integer? inner-x))
				     (+ outer-x inner-x)
				     (list '+ outer-x inner-x))))
			  (lint-format "perhaps ~A" caller
				       (lists->string form
						      (if (not (or inner-y outer-y))
							  (list 'substring (cadr str) x)
							  (list 'substring (cadr str) x
								(if outer-y
								    (if (and (integer? outer-y)
									     (integer? inner-x))
									(+ outer-y inner-x)
									(list '+ outer-y inner-x))
								    inner-y))))))))

		    (if (and (eqv? (caddr form) 0)
			     (null? (cdddr form)))   ;  (substring x 0) -> (copy x)
			(lint-format "perhaps clearer: ~A" caller (lists->string form (list 'copy str))))

		    (when (pair? (cdddr form))
		      (let ((end (cadddr form)))
			(if (equal? (caddr form) end)   ; (substring x (+ y 1) (+ y 1)) is ""
			    (lint-format "leaving aside errors, ~A is \"\"" caller form))

			(when (and (len=3? str)
				   (eqv? (caddr form) 0)
				   (eq? (car str) 'string-append))
			  (let ((in-arg2 (caddr str)))
			    (if (and (pair? in-arg2)    ;  (substring (string-append str (make-string len #\space)) 0 len) -> (copy str (make-string len #\space))
				     (eq? (car in-arg2) 'make-string)
				     (equal? (cadddr form) (cadr in-arg2)))
				(lint-format "perhaps ~A" caller
					     (lists->string form `(copy ,(cadr str) (make-string ,(cadddr form) ,(caddr in-arg2))))))))

			(if (and (len>1? end)           ; (substring x start (length|string-length x)) -> (substring s start)
				 (memq (car end) '(string-length length))
				 (equal? (cadr end) str))
			    (lint-format "perhaps ~A" caller (lists->string form (copy form (make-list 3))))

			    (when (symbol? end)
			      (let ((v (var-member end env)))
				(if (and v
					 (equal? (list 'string-length str) (var-initial-value v))
					 (not (lint-any? (lambda (p)
							   (lint-set!? p env))
							 (var-history v))))   ;  if len is still (string-length x), (substring x 1 len) -> (substring x 1)
				    (lint-format "perhaps, if ~A is still ~A, ~A" caller end (var-initial-value v)
						 (lists->string form (copy form (make-list 3))))))))))))))

	  (hash-special 'substring sp-substring))

	;; ---------------- list, *vector ----------------
	(let ((seq-maker (lambda (seq)
			   (cdr (assq seq '((list . make-list)
					    (vector . make-vector)
					    (float-vector . make-float-vector)
					    (int-vector . make-int-vector)
					    (byte-vector . make-byte-vector))))))
	      (seq-default (lambda (seq)
			     (cdr (assq seq '((list . #f)
					      (vector . #<unspecified>)
					      (float-vector . 0.0)
					      (int-vector . 0)
					      (byte-vector . 0)))))))
	  (define (sp-list caller head form env)
	    (let ((len (length form))
		  (val (and (pair? (cdr form))
			    (cadr form))))
	      (when (eq? head 'list)                ; (list) -> () but not (vector) -> #()
		(if (= len 1)                       ;    (eq? (vector) #()) -> #f (same for string etc)
		    (if (not (var-member 'list env))
			(lint-format "perhaps (list) -> (); there is only one nil" caller))
		    (if (and (> len 3)
			     (len=2? (cadr form)))  ; (list (f a) (f b) (f c)) -> (map f (list a b c))
			(let ((f (caadr form)))     ;    map orders this process whereas list is unordered?
			  ;; not any-macro? f?? or no side-effect? in the args?
			  (if (and (not (memq f '(quote values)))
				   (lint-every? (lambda (p)
						  (and (len=2? p)
						       (eq? f (car p))))
						(cddr form)))
			      (lint-format "perhaps ~A" caller
					   (truncated-lists->string form
								    (if (lint-every? (lambda (p)
										       (code-constant? (cadr p)))
										     (cdr form))
						`(map ,f ',(map (lambda (p) ; p = arg which might be quoted (not = f)
								  ((if (and (pair? (cadr p))
									    (eq? (caadr p) 'quote))
								       cadadr cadr)
								   p))
								 (cdr form)))
						`(map ,f (list ,@(map cadr (cdr form))))))))))))
	      ;; *vector here gets a dozen or so hits but (apply vector (map f (list ...))) involves too much consing
	      ;; list-values in this case is always apply-values as 'f
	      ;; the only other hits in this area are and/or and test macros

	      (when (and (> len 4)
			 (lint-every? (lambda (a) (equal? a val)) (cddr form)))
		(if (code-constant? val)        ; (vector 12 12 12 12 12 12) -> (make-vector 6 12)
		    (lint-format "perhaps ~A~A" caller
				 (lists->string form
						(if (eqv? (seq-default head) val)
						    (list (seq-maker head) (- len 1))
						    (list (seq-maker head) (- len 1) val)))
				 (if (and (sequence? val)
					  (not (null? val)))
				     (format #f "~%~NCor wrap (copy ~S) in a function and call that ~A times"
					     lint-left-margin #\space
					     val (- len 1))
				     ""))
		    (if (pair? val)
			(if (or (side-effect? val env)
				(hash-table-ref makers (car val)))
			    (if (> (tree-leaves val) 3)
				;; I think we need to laboriously repeat the function call here:
				;;    (let ((a 1) (b 2) (c 3))
				;;      (define f (let ((ctr 0)) (lambda (x y z) (set! ctr (+ ctr 1)) (+ x y ctr (* 2 z)))))
				;;      (list (f a b c) (f a b c) (f a b c) (f a b c))
				;; so (apply list (make-list 4 (<1>))) or variants thereof fail
				;;   (eval (append '(list) (make-list 4 '(<1>))))
				;; works, but it's too ugly.
				(lint-format "perhaps ~A" caller
					     (lists->string form
							    `(let ((<1> (lambda () ,val)))
							       (,head ,@(make-list (- len 1) '(<1>)))))))
			    ;; if seq copy else
			    (lint-format "perhaps ~A" caller  ; (vector (car x) (car x) (car x) (car x)) -> (make-vector 4 (car x))
					 (lists->string  form (list (seq-maker head) (- len 1) val)))))))))

	  (for-each (lambda (f) (hash-special f sp-list)) '(list vector int-vector float-vector byte-vector)))

	;; ---------------- list-tail ----------------
	(let ()
	  (define (sp-list-tail caller head form env)
	    (cond
	     ((not (= (length form) 3)) #f)

	     ((eqv? (caddr form) 0)                     ; (list-tail x 0) -> x
	      (lint-format "perhaps ~A" caller (lists->string form (cadr form))))

	     ((and (pair? (cadr form))
		   (eq? (caadr form) 'list-tail))
	      (lint-format "perhaps ~A" caller          ; (list-tail (list-tail x 1) 2) -> (list-tail x 3)
			   (lists->string form
					  (list 'list-tail (cadadr form)
						(if (and (integer? (caddr form))
							 (integer? (caddr (cadr form))))
						    (+ (caddr (cadr form)) (caddr form))
						    (list '+ (caddr (cadr form)) (caddr form)))))))

	     ((memv (caddr form) '(1 2))                ; (list-tail x 1) -> (cdr x)
	      (lint-format "perhaps ~A" caller
			   (lists->string form
					  (list (if (eqv? (caddr form) 1) 'cdr 'cddr)
						(cadr form)))))))

	  (hash-special 'list-tail sp-list-tail))

	;; ---------------- eq? ----------------
	(let ()
	  (define (sp-eq? caller head form env)
	    (if (< (length form) 3)  ; (eq?)
		(lint-format "eq? needs 2 arguments: ~A" caller (truncated-list->string form))
		(let* ((arg1 (cadr form))
		       (arg2 (caddr form))
		       (eq1 (eqf arg1 env))
		       (eq2 (eqf arg2 env))
		       (specific-op (and (eq? (cadr eq1) (cadr eq2))
					 (not (memq (cadr eq1) '(eqv? equal?)))
					 (cadr eq1))))

		  (eval-constant-expression caller form)

		  (if (or (eq? (car eq1) 'equal?)
			  (eq? (car eq2) 'equal?))         ; (eq? #(0) #(0))
		      (lint-format "eq? should be equal?~A in ~A" caller
				   (if specific-op (format #f " or ~A" specific-op) "")
				   (truncated-list->string form))
		      (if (or (eq? (car eq1) 'eqv?)
			      (eq? (car eq2) 'eqv?))       ; (eq? x 1.5)
			  (lint-format "eq? should be eqv?~A in ~A" caller
				       (if specific-op (format #f " or ~A" specific-op) "")
				       (truncated-list->string form))))

		  (let ((expr 'unset))
		    (cond ((or (not arg1)                  ; (eq? #f x) -> (not x)
			       (quoted-not? arg1))
			   (set! expr (simplify-boolean (list 'not arg2) () () env)))

			  ((or (not arg2)                  ; (eq? x #f) -> (not x)
			       (quoted-not? arg2))
			   (set! expr (simplify-boolean (list 'not arg1) () () env)))

			  ((and (any-null? arg1)           ; (eq? () x) -> (null? x)
				(not (code-constant? arg2)))
			   (set! expr (or (equal? arg2 '(list)) ; (eq? () (list)) -> #t
					  (list 'null? arg2))))

			  ((and (any-null? arg2)           ; (eq? x ()) -> (null? x)
				(not (code-constant? arg1)))
			   (set! expr (or (equal? arg1 '(list))
					  (list 'null? arg1))))

			  ((and (eq? arg1 #t)              ; (eq? #t <boolean-expr>) -> boolean-expr
				(pair? arg2))              ; (eq? #t <never-#t-expr) -> #f
			   (let ((rtn (return-type (car arg2) env)))
			     (unless (boolean? rtn)        ; #t = any result possible, #f = no signature found, values->#t in return-type
			       (cond ((eq? rtn 'boolean?) (set! expr arg2))
				     ((not (pair? rtn)) (set! expr #f))
				     ((not (or (memq 'boolean? rtn)
					       (memq #t rtn)
					       (memq 'values rtn)))
				      (set! expr #f))))))

			  ((and (eq? arg2 #t)              ; (eq? <boolean-expr> #t) -> boolean-expr
				(pair? arg1))              ; (eq? <never-#t-expr) #t) -> #f
			   (let ((rtn (return-type (car arg1) env)))
			     (unless (boolean? rtn)
			       (cond ((eq? rtn 'boolean?) (set! expr arg1))
				     ((not (pair? rtn)) (set! expr #f))
				     ((not (or (memq 'boolean? rtn)
					       (memq #t rtn)
					       (memq 'values rtn)))
				      (set! expr #f)))))))

		    (let ((t1 (->lint-type arg1))          ; (eq? (floor pi) 'a) -> #f
			  (t2 (->lint-type arg2)))
		      ;; ->lint-type -> #t if unknown
		      (unless (compatible? t1 t2)
			(set! expr #f)))

		    (if (not (eq? expr 'unset))            ; (eq? x '()) -> (null? x)
			(lint-format "perhaps ~A" caller (lists->string form expr)))))))
	  (hash-special 'eq? sp-eq?))

	;; ---------------- eqv? equal? ----------------
	(let ()
	  (define (useless-copy? a)
	    (and (len=2? a)
		 (memq (car a) '(copy string-copy vector-copy list-copy))))

	  (define (sp-eqv? caller head form env)
	    (if (< (length form) 3)
		(lint-format "~A needs 2 arguments: ~A" caller head (truncated-list->string form))
		(let* ((arg1 (cadr form))
		       (arg2 (caddr form))
		       (eq1 (eqf arg1 env))
		       (eq2 (eqf arg2 env))
		       (specific-op (and (eq? (cadr eq1) (cadr eq2))
					 (not (memq (cadr eq1) '(eq? eqv? equal?)))
					 (cadr eq1))))

		  (eval-constant-expression caller form)

		  (if (or (useless-copy? arg1)
			  (useless-copy? arg2))       ; (equal? (vector-copy #(a b c)) #(a b c)) -> (equal? #(a b c) #(a b c))
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  (list head
							(if (useless-copy? arg1) (cadr arg1) arg1)
							(if (useless-copy? arg2) (cadr arg2) arg2)))))
		  (unless (string->char= caller form form)
		    (string->char= caller (cons (car form) (reverse (cdr form))) form))

		  (if (and (not (eq? (cadr eq1) (cadr eq2)))    ; (eqv? ":" (string-ref s 0))
			   (memq (cadr eq1) '(char=? string=?))
			   (memq (cadr eq2) '(char=? string=?)))
		      (lint-format "this can't be right: ~A" caller form))

		  ;; (equal? a (list b)) and equivalents happen a lot (well, a few dozen times), but is the extra consing worse than
		  ;;    (and (pair? a) (null? (cdr a)) (equal? (car a) b)) -- code readability seems more important here
		  ;;    also, a is often an expression, and let+local is worse than list

		  (cond ((or (eq? (car eq1) 'equal?)
			     (eq? (car eq2) 'equal?))
			 (if (eq? head 'equal?)
			     (if specific-op                    ; equal? could be string=? in (equal? (string x) (string-append y z))
				 (lint-format "~A could be ~A in ~S" caller head specific-op form))
			     (lint-format "~A should be equal?~A in ~S" caller head
					  (if specific-op (format #f " or ~A" specific-op) "")
					  form)))

			((or (eq? (car eq1) 'eqv?)
			     (eq? (car eq2) 'eqv?))
			 (if (eq? head 'eqv?)
			     (if specific-op                    ; (eqv? (integer->char x) #\null)
				 (lint-format "~A could be ~A in ~S" caller head specific-op form))
			     (lint-format "~A ~A be eqv?~A in ~S" caller head
					  (if (eq? head 'eq?) "should" "could")
					  (if specific-op (format #f " or ~A" specific-op) "")
					  form)))

			((not (or (eq? (car eq1) 'eq?)
				  (eq? (car eq2) 'eq?))))

			((not (and arg1 arg2))                  ; (eqv? x #f) -> (not x)
			 (lint-format "~A could be not: ~A" caller head (lists->string form (list 'not (or arg1 arg2)))))

			((or (any-null? arg1)
			     (any-null? arg2))                  ; (eqv? x ()) -> (null? x)
			 (lint-format "~A could be null?: ~A" caller head
				      (lists->string form
						     (list 'null? (if (any-null? arg1) arg2 arg1)))))

			(else                                   ; (eqv? x 'a)
			 (lint-format "~A could be eq?~A in ~S" caller head
				      (if specific-op (format #f " or ~A" specific-op) "")
				      form)))

		    (let ((t1 (->lint-type arg1))               ; (eqv? (floor pi) 'a) -> #f
			  (t2 (->lint-type arg2)))
		      (unless (compatible? t1 t2)
			(lint-format "perhaps ~A -> #f" caller form)))
		    )))
		  ;; very few hits:
		  ;; (equal? (reverse em) '((0 -2 0) (0 -1 0) (1 -2 0) (1 -1 0)))

	  (hash-special 'eqv? sp-eqv?)
	  (hash-special 'equal? sp-eqv?))

	(let ()
	  (define (sp-equivalent caller head form env)
	    (if (< (length form) 3)
		(lint-format "~A needs 2 arguments: ~A" caller head (truncated-list->string form))
		(if (= (length form) 3)
		    (let ((arg1 (cadr form))
			  (arg2 (caddr form)))
		      (if (and (code-constant? arg1)
			       (code-constant? arg2))     ; (equivalent? 1 1) -> #t
			  (lint-format "perhaps ~A" caller
				       (lists->string form
						      (apply equivalent? (cdr form)))))
		      (let ((t1 (->lint-type arg1))       ; (morallly-equal? (floor pi) 'a) -> #f
			    (t2 (->lint-type arg2)))
			(unless (compatible? t1 t2)
			  (lint-format "perhaps ~A -> #f" caller form)))))))

	  (hash-special 'equivalent? sp-equivalent))


	;; ---------------- map for-each ----------------
	(let ()
	  (define (sp-map caller head form env)
	    (let* ((len (length form))
		   (args (- len 2)))
	      (if (< len 3)                           ; (map (lambda (v) (vector-ref v 0)))
		  (lint-format "~A missing argument~A in: ~A"
			       caller head
			       (if (= len 2) "" "s")
			       (truncated-list->string form))
		  (let ((func (cadr form))
			(ary #f))

		    ;; if zero or one args, the map/for-each is either a no-op or a function call
		    (if (or (lint-any? any-null? (cddr form))  ; (map abs ())
			    (lint-any? (lambda (p)
					 (or (and (code-constant? p)
						  (eqv? (length p) 0))
					     (and (pair? p)
						  (case (car p)
						    ((vector string)
						     (null? (cdr p)))
						    ((quote)
						     (and (pair? (cdr p))
							  (eqv? (length (cadr p)) 0)))
						    (else #f)))))
				       (cddr form)))
			(lint-format "this ~A has no effect (~A arg)" caller
				     (truncated-list->string form)
				     (if (lint-any? any-null? (cddr form)) "null" "zero length"))
			(if (and (not (tree-memq 'values form)) ; e.g. flatten in s7.html
				 (lint-any? (lambda (p)
					      (or (and (code-constant? p)
						       (eqv? (length p) 1))
						  (and (len>1? p)
						       (case (car p)
							 ((quote)
							  (len=1? (cadr p)))
							 ((list vector string)
							  (null? (cddr p)))
							 ((cons)
							  (and (pair? (cddr p))
							       (any-null? (caddr p))))
							 (else #f)))))
					    (cddr form)))    ; (for-each display (list a)) -> (display a)
			    (lint-format "perhaps ~A" caller
					 (lists->string form
							(let ((args (map (lambda (a)
									   (if (len>1? a)
									       (case (car a)
										 ((list cons vector string)
										  (cadr a))      ; slightly inaccurate
										 ((quote)        ; might be '#(0) or '(0) etc
										  (if (sequence? (cadr a))
										      ((cadr a) 0)
										      (list a 0)))
										 (else (list a 0))) ; not car -- might not be a list
									       (if (and (code-constant? a)
											(sequence? a))
										   (a 0)            ; (map abs #(1)) -> (list (abs 1))
										   (list a 0))))    ;  but still not right -- arg might be a hash-table
									 (cddr form))))
							  (if (eq? head 'for-each)
							      (cons (cadr form) args)
							      (list 'list (cons (cadr form) args))))))))
		    ;; 2 happens a lot, but introduces evaluation order quibbles
		    ;;   we used to check for values if list arg -- got 4 hits!

		    (if (and (symbol? func)
			     (procedure? (symbol->value func *e*)))
			(begin
			  (set! ary (arity (symbol->value func *e*)))
			  (if (and (eq? head 'map)
				   (hash-table-ref no-side-effect-functions func)
				   (= len 3)
				   (pair? (caddr form))
				   (or (eq? (caaddr form) 'quote)
				       (and (eq? (caaddr form) 'list)
					    (just-code-constants? (cdaddr form)))))
			      (catch #t
				(lambda ()          ; (map symbol->string '(a b c d)) -> '("a" "b" "c" "d")
				  (let ((val (eval form)))
				    (lint-format "perhaps ~A" caller (lists->string form (list 'quote val)))))
				(lambda args #f))))

			(when (and (pair? func)
				   (memq (car func) '(lambda lambda*)))
			  (if (pair? (cadr func))
			      (let ((arglen (length (cadr func))))
				(set! ary (if (eq? (car func) 'lambda)
					      (if (negative? arglen)
						  (cons (abs arglen) 512000)
						  (cons arglen arglen))
					      (cons 0 (if (or (negative? arglen)
							      (memq :rest (cadr func)))
							  512000 arglen))))))
			  (if (= len 3)
			      (let ((body (cddr func)))       ; (map (lambda (a) #f) x) -> (make-list (abs (length x)) #f)
				(if (and (null? (cdr body))
					 (code-constant? (car body)))
				    (lint-format "perhaps ~A" caller
						 (lists->string form
								`(make-list (abs (length ,(caddr form))) ,(car body)))))))))
		    (if (pair? ary)
			(if (< args (car ary))                ; (map (lambda (a b) a) '(1 2))
			    (lint-format "~A has too few arguments in: ~A"
					 caller head
					 (truncated-list->string form))
			    (if (> args (cdr ary))            ; (map abs '(1 2) '(3 4))
				(lint-format "~A has too many arguments in: ~A"
					     caller head
					     (truncated-list->string form)))))
		    (for-each
		     (lambda (obj)
		       (if (and (len>1? obj)
				(memq (car obj) '(vector->list string->list let->list))) ; (vector->list #(1 2)) could be simplified to: #(1 2)
			   (lint-format "~A could be simplified to:~%~NC~A ; (~A accepts non-list sequences)" caller
					(truncated-list->string obj)
					(+ lint-left-margin 4) #\space
					(truncated-list->string (cadr obj))
					head)))
		     (cddr form))

		    (when (eq? head 'map)
		      (when (and (memq func '(char-downcase char-upcase))
				 (pair? (caddr form))         ; (map char-downcase (string->list str)) -> (string->list (string-downcase str))
				 (eq? (caaddr form) 'string->list))
			(lint-format "perhaps ~A" caller
				     (lists->string form (list 'string->list (list (if (eq? func 'char-upcase) 'string-upcase 'string-downcase)
										   (cadr (caddr form)))))))
		      (when (identity? func) ; to check f here as var is more work ; (map (lambda (x) x) lst) -> lst
			(lint-format "perhaps ~A" caller (lists->string form (caddr form)))))

		    (let ((arg1 (caddr form)))
		      (when (and (len>1? arg1)
				 (or (memq (car arg1) '(cdr cddr cdddr cddddr))
				     (and (eq? (car arg1) 'list-tail)
					  (pair? (cddr arg1))))
				 (len>1? (cadr arg1))
				 (memq (caadr arg1) '(string->list vector->list)))
			(let ((string-case (eq? (caadr arg1) 'string->list))    ; (cdr (vector->list v)) -> (subvector v 1 (length v))
			      (len-diff (case (car arg1) ((list-tail) (caddr arg1)) (else => cdr-count))))
			  (lint-format "~A accepts ~A arguments, so perhaps ~A" caller head
				       (if string-case 'string 'vector)
				       (lists->string arg1 (if string-case
							       (list 'substring (cadadr arg1) len-diff)
							       `(subvector ,(cadadr arg1) ,len-diff (length ,(cadadr arg1)))))))))
		    (when (and (eq? head 'for-each)
			       (len>1? (cadr form))            ; (for-each (lambda (x) (+ (abs x) 1)) lst)
			       (eq? (caadr form) 'lambda)
			       (not (lint-any? (lambda (x) (side-effect? x env)) (cddadr form))))
		      (lint-format "pointless for-each: ~A" caller (truncated-list->string form)))

		    (when (= args 1)
		      (let ((seq (caddr form)))

			(when (pair? seq)
			  (case (car seq)
			    ((cons)                           ; (for-each display (cons msgs " "))
			     (if (and (len>1? (cdr seq))
				      (code-constant? (caddr seq)))
				 (lint-format "~A will ignore ~S in ~A" caller head (caddr seq) seq)))

			    ((map)
			     (when (= (length seq) 3)
			       ;; a toss-up -- probably faster to combine funcs here, and easier to read?
			       ;;   but only if first arg is only used once in first func, and everything is simple (one-line or symbol)
			       (let* ((seq-func (cadr seq))
				      (arg-name (find-unique-name func seq-func)))

				 (if (symbol? func)            ; (map f (map g h)) -> (map (lambda (<1>) (f (g <1>))) h) -- dubious
				     (if (symbol? seq-func)
					 (lint-format "perhaps ~A" caller
						      (lists->string form `(,head (lambda (,arg-name)
										    (,func (,seq-func ,arg-name)))
										  ,(caddr seq))))
					 (if (simple-lambda? seq-func)
					     ;; (map f (map (lambda (x) (g x)) h)) -> (map (lambda (x) (f (g x))) h)
					     (lint-format "perhaps ~A" caller
							  (lists->string form `(,head (lambda (,arg-name)
											(,func ,(tree-subst arg-name (caadr seq-func) (caddr seq-func))))
										      ,(caddr seq))))))
				     (if (less-simple-lambda? func)
					 (if (symbol? seq-func)
					     ;; (map (lambda (x) (f x)) (map g h)) -> (map (lambda (x) (f (g x))) h)
					     (lint-format "perhaps ~A" caller
							  (lists->string form `(,head (lambda (,arg-name)
											,@(tree-subst (list seq-func arg-name) (caadr func) (cddr func)))
										      ,(caddr seq))))
					     (if (simple-lambda? seq-func)
						 ;; (map (lambda (x) (f x)) (map (lambda (x) (g x)) h)) -> (map (lambda (x) (f (g x))) h)
						 (lint-format "perhaps ~A" caller
							      (lists->string form `(,head (lambda (,arg-name)
											    ,@(tree-subst (tree-subst arg-name (caadr seq-func) (caddr seq-func))
													  (caadr func) (cddr func)))
											  ,(caddr seq)))))))))))))
			;; repetitive code...
			(when (eq? head 'for-each) ; args = 1 above  ; (for-each display (list a)) -> (format () "~A" a)
			  (let ((func (cadr form)))
			    (if (memq func '(display write newline write-char write-string))
				(lint-format "perhaps ~A" caller
					     (if (and (pair? seq)
						      (memq (car seq) '(list quote)))
						 (let ((op (if (eq? func 'write) "~S" "~A"))
						       (len (- (length seq) 1)))
						   (lists->string form `(format () ,(do ((i 0 (+ i 1))
											 (str "" (string-append str op)))
											((= i len) str))
										,@(cdr seq))))
						 (let ((op (if (eq? func 'write) "~{~S~}" "~{~A~}")))
						   (lists->string form (list 'format () op seq)))))
				(when (and (pair? func)
					   (eq? (car func) 'lambda))
				  (let ((body (cddr func)))
				    (let ((op (write-port (car body)))
					  (larg (and (pair? (cadr func))
						     (caadr func))))
				      (when (and (symbol? larg)
						 (null? (cdadr func)) ; just one arg (one sequence to for-each) for now
						 (lint-every? (lambda (x)
								(and (pair? x)
								     (memq (car x) '(display write newline write-char write-string))
								     (or (eq? (car x) 'newline)
									 (eq? (cadr x) larg)
									 (string? (cadr x))
									 (eqv? (cadr x) #\space)
									 (and (len>1? (cadr x))
									      (eq? (caadr x) 'number->string)
									      (eq? (cadadr x) larg)))
								     (eq? (write-port x) op)))
							      body))
					;; (for-each (lambda (x) (display x) (write-char #\space)) msg)
					;; (for-each (lambda (elt) (display elt)) lst)
					(let ((ctrl-string "")
					      (arg-ctr 0))

					  (define* (gather-format str (arg :unset))
					    (set! ctrl-string (string-append ctrl-string str)))

					  (for-each
					   (lambda (d)
					     (if (or (memq larg d)
						     (and (pair? (cdr d))
							  (pair? (cadr d))
							  (memq larg (cadr d))))
						 (set! arg-ctr (+ arg-ctr 1)))
					     (gather-format (display->format d)))
					   body)

					  (when (= arg-ctr 1)  ; (for-each (lambda (x) (display x)) args) -> (format () "~{~A~}" args)
					    (lint-format "perhaps ~A" caller
							 (lists->string form (list 'format op (string-append "~{" ctrl-string "~}") seq))))))))))))))))))
	  (for-each (lambda (f)
		      (hash-special f sp-map))
		    '(map for-each)))

	;; ---------------- magnitude ----------------
	(let ()
	  (define (sp-magnitude caller head form env)
	    (if (and (len=2? form)             ;  (magnitude 2/3)
		     (memq (->lint-type (cadr form)) '(integer? rational? real?)))
		(lint-format "perhaps use abs here: ~A" caller form)))
	  (hash-special 'magnitude sp-magnitude))

	;; ---------------- hash-table ----------------
	(let ()
	  (define (sp-hash caller head form env)
	    (let ((len (length form)))
	      (if (and (positive? len)
		       (even? len))
		  (lint-format "key with no value? ~A" caller (truncated-list->string form))
		  (when (pair? (cdr form)) ; (hash-table)
		    (do ((syms ())
			 (p (cdr form) (cddr p)))
			((null? p))
		      (let ((sym (if (pair? (car p))
				     (cadar p)
				     (and (keyword? (car p))
					  (keyword->symbol (car p))))))
			(if sym
			    (if (memq sym syms)
				(lint-format "repeated key ~S in ~S" caller sym form)
				(set! syms (cons sym syms))))))))))
	  (hash-special 'hash-table sp-hash))

	;; ---------------- inlet sublet ----------------
	(let ()
	  (define (sp-inlet caller head form env)
	    (unless (and (eq? head 'sublet)
			 (or (not (pair? (cdr form)))
			     (keyword? (cadr form))
			     (quoted-symbol? (cadr form))))
	      (when (and (pair? (cdr form)) (pair? (cddr form)))
		(do ((syms ())
		     (p (if (eq? head 'inlet) (cdr form) (cddr form)) (cddr p)))
		    ((null? p))
		  (let ((sym (if (pair? (car p))
				 (cadar p)
				 (and (keyword? (car p))
				      (keyword->symbol (car p))))))
		    (if sym
			(if (memq sym syms)
			    (lint-format "repeated key ~S in ~S" caller sym form)
			    (set! syms (cons sym syms)))))))))
	  (hash-special 'inlet sp-inlet)
	  ;(hash-special 'sublet sp-inlet)
	  )

	;; ---------------- open-input-file open-output-file ----------------
	(let ()
	  (define (sp-open-input-file caller head form env)
	    (if (and (len>1? (cdr form))
		     (string? (caddr form))        ; (open-output-file x "fb+")
		     (or (equal? (caddr form) "")  ; I think an empty string (passed to fopen) is an error
			 (not (memv (string-ref (caddr form) 0) '(#\r #\w #\a))))) ; b + then e m c x if gcc
		(lint-format "unexpected mode: ~A" caller form)))
	  (for-each (lambda (f)
		      (hash-special f sp-open-input-file))
		    '(open-input-file open-output-file)))

	;; ---------------- values ----------------
	(let ()
	  (define (car-values? a b)                              ;  (values 2 (values 3 4) 5) -> (values 2 3 4 5)
	    (and (pair? b)
		 (eq? (car b) 'values)))

	  (define (sp-values caller head form env)
	    (cond ((member #f (cdr form) car-values?)
		   (lint-format "perhaps ~A" caller (lists->string form (cons 'values (splice-if 'values (cdr form))))))
		  ((len=2? form)
		   (lint-format "perhaps ~A" caller
				(lists->string form              ;  (values (list-values 'x (apply-values y))) -> (cons 'x y)
					       (if (and (pair? (cadr form))
							(eq? (caadr form) 'list-values)
							(not (qq-tree? (cadr form))))
						   (unlist-values (cadr form))
						   (cadr form)))))
		  ((and (assq 'list-values (cdr form))
			(not (lint-any? (lambda (a)
					  (and (pair? a)
					       (memq (car a) '(list-values apply-values))
					       (qq-tree? a)))
					(cdr form))))
		   (lint-format "perhaps ~A" caller
				(lists->string form              ;  (values (list-values 'x y) a) -> (values (list 'x y) a)
					       (cons 'values (map (lambda (a)
								    (if (and (pair? a)
									     (eq? (car a) 'list-values))
									(unlist-values a)
									a))
								  (cdr form))))))))
	  (hash-special 'values sp-values))

	;; ---------------- call-with-values ----------------
	(let ()
	  (define (sp-call/values caller head form env)  ; (call/values p c) -> (c (p))
	    (when (= (length form) 3)
	      (let ((producer (cadr form))
		    (consumer (caddr form)))
		(let* ((produced-values (mv-range producer env))
		       (consumed-values (and produced-values
					     (or (and (symbol? consumer)
						      (arg-arity consumer env))
						 (and (len>2? consumer)
						      (eq? (car consumer) 'lambda)
						      (pair? (cadr consumer))
						      (let ((len (length (cadr consumer))))
							(if (negative? len)
							    (cons (abs len) 536870912)
							    (cons len len))))))))
		  (if (and consumed-values
			   (or (> (car consumed-values) (car produced-values))
			       (< (cdr consumed-values) (cadr produced-values))))
		      (let ((clen ((if (> (car consumed-values) (car produced-values)) car cdr) consumed-values)))
			(lint-format "call-with-values consumer ~A wants ~D value~P, but producer ~A returns ~A"
				     caller
				     (truncated-list->string consumer)
				     clen clen
				     (truncated-list->string producer)
				     ((if (> (car consumed-values) (car produced-values)) car cadr) produced-values)))))

		(cond ((not (pair? producer))             ;  (call-with-values log c)
		       (if (and (symbol? producer)
				(not (memq (return-type producer ()) '(#t #f values))))
			   (lint-format "~A does not return multiple values" caller producer)
			   (lint-format "perhaps ~A" caller (lists->string form (list consumer (list producer))))))

		      ((not (eq? (car producer) 'lambda)) ; (call-with-values (eval p env) (eval c env)) -> ((eval c env) ((eval p env)))
		       (lint-format "perhaps ~A" caller (lists->string form (list consumer (list producer)))))

		      ((pair? (cadr producer))            ; (call-with-values (lambda (x) 0) list)
		       (lint-format "~A requires too many arguments" caller (truncated-list->string producer)))

		      ((symbol? (cadr producer))          ; (call-with-values (lambda x 0) list)
		       (lint-format "~A's parameter ~A will always be ()" caller (truncated-list->string producer) (cadr producer)))

		      ((len=1? (cddr producer))           ; (call-with-values (lambda () (read-char p)) cons)
		       (let ((body (caddr producer)))
			 (if (or (code-constant? body)
				 (and (pair? body)
				      (symbol? (car body))
				      (not (memq (return-type (car body) ()) '(#t #f values)))))
			     (lint-format "~A does not return multiple values" caller body)
			     (lint-format "perhaps ~A" caller
					  (lists->string form
							 (if (and (pair? body)
								  (eq? (car body) 'values))
							     (cons consumer (cdr body))
							     (list consumer body)))))))

		      (else (lint-format "perhaps ~A" caller (lists->string form (list consumer (list producer)))))))))
	  (hash-special 'call-with-values sp-call/values))

	;; ---------------- multiple-value-bind ----------------
	(let ()
	  (define (sp-mvb caller head form env)
	    (when (>= (length form) 4)
	      (let ((vars (cadr form))
		    (producer (caddr form))
		    (body (cdddr form)))

		(if (null? vars)
		    (lint-format "this multiple-value-bind is pointless; perhaps ~A" caller
				 (lists->string form
						(if (side-effect? producer env)
						    (cons 'begin (cons producer body))
						    (if (null? (cdr body))
							(car body)
							(cons 'begin body)))))

		    (unless (symbol? vars)                      ; else any number of values is ok
		      (let ((vals (mv-range producer env))      ;  (multiple-value-bind (a b) (values 1 2 3) b)
			    (args (length vars)))
			(if (and (integer? args)
				 (pair? vals)
				 (not (<= (car vals) args (cadr vals))))
			    (lint-format "multiple-value-bind wants ~D values, but ~A returns ~A"
					 caller args
					 (truncated-list->string producer)
					 ((if (< args (car vals)) car cadr) vals)))

			(if (and (pair? producer)               ;  (multiple-value-bind (a b) (f) b) -> ((lambda (a b) b) (f))
				 (symbol? (car producer))
				 (not (memq (return-type (car producer) ()) '(#t #f values))))
			    (lint-format "~A does not return multiple values" caller (car producer))
			    (lint-format "perhaps ~A" caller
					 (lists->string form
							(if (and (null? (cdr body))
								 (pair? (car body))
								 (symbol? (caar body))
								 (equal? vars (cdar body))
								 (defined? (caar body))
								 (equal? (arity (symbol->value (caar body))) (cons args args)))
							    (list (caar body) producer)
							    `((lambda ,vars ,@body) ,producer)))))))))))
	  (hash-special 'multiple-value-bind sp-mvb))

	;; ---------------- let-values ----------------
	(let ()
	  (define (sp-let-values caller head form env)
	    (if (and (pair? (cdr form))
		     (proper-pair? (cadr form)))
		(if (null? (cdadr form))                        ; just one set of vars
		    (let ((call (caadr form)))
		      (if (len>1? call)
			  (lint-format "perhaps ~A" caller      ;  (let-values (((x) (values 1))) x) -> ((lambda (x) x) (values 1))
				       (lists->string form
						      `((lambda ,(car call)
							  ,@(cddr form))
							,(cadr call))))))
		    (if (just-len>1? (cadr form))
			(lint-format "perhaps ~A" caller        ;  (let-values (((x) (values 1)) ((y) (values 2))) (list x y)) ...
				     (lists->string
				      form
				      `(with-let
					   (apply sublet (curlet)
						  (list ,@(map (lambda (v)
								 `((lambda ,(car v)
								     (values ,@(map (lambda (name)
										      (values (symbol->keyword name) name))
										    (args->proper-list (car v)))))
								   ,(cadr v)))
							       (cadr form))))
					 ,@(cddr form))))))))
	  (hash-special 'let-values sp-let-values))

	;; ---------------- let*-values ----------------
	(hash-special 'let*-values
	  (lambda (caller head form env)
	    (if (and (pair? (cdr form))
		     (proper-pair? (cadr form))   ; every? uses for-each which ignores dotted-list cdr?
		     (just-len>1? (cadr form)))
		(lint-format "perhaps ~A" caller
			     (lists->string form               ;   (let*-values (((a) (f x))) (+ a b)) -> (let ((a (f x))) (+ a b))
					    (let loop ((var-data (cadr form)))
					      (let ((v (car var-data)))
						(if (and (pair? (car v))  ; just one var
							 (null? (cdar v)))
						    (if (null? (cdr var-data))
							(cons 'let (cons (list (list (caar v) (cadr v))) (cddr form)))
							`(let ((,(caar v) ,(cadr v))) ,(loop (cdr var-data))))
						    (if (null? (cdr var-data))
							`((lambda ,(car v) ,@(cddr form)) ,(cadr v))
							`((lambda ,(car v) ,(loop (cdr var-data))) ,(cadr v)))))))))))

	;; ---------------- define-values ----------------
	(hash-special 'define-values
	  (lambda (caller head form env)
	    (when (pair? (cdr form))
	      (if (null? (cadr form))
		  (lint-format "~A is pointless" caller (truncated-list->string form))
		  (when (pair? (cddr form))
		    (lint-format "perhaps ~A" caller
				 (cond ((symbol? (cadr form))
					(lists->string form (list 'define (cadr form) (list 'list (caddr form)))))

				       ((len=1? (cadr form))
					(lists->string form (list 'define (caadr form) (caddr form))))

				       (else           ;  (define-values (x y) (values 3 2)) -> (varlet (curlet) ((lambda (x y) (curlet)) (values 3 2)))
					(let-temporarily ((target-line-length 120))
					  (truncated-lists->string form
								   `(varlet (curlet)
								      ((lambda ,(cadr form)
									 (curlet))
								       ,(caddr form)))))))))))))
	;; ---------------- eval ----------------
	(let ()
	  (define (sp-eval caller head form env)
	    (case (length form)
	      ((2)
	       (let ((arg (cadr form)))
		 (if (not (pair? arg))
		     (if (not (symbol? arg))           ;  (eval 32)
			 (lint-format "this eval is pointless; perhaps ~A" caller (lists->string form arg)))
		     (case (car arg)
		       ((quote)                        ;  (eval 'x)
			(if (pair? (cdr arg))
			    (lint-format "perhaps ~A" caller (lists->string form (cadr arg)))))

		       ((string->symbol)               ;  (eval (string->symbol "x")) -> x
			(if (pair? (cdr arg))          ;  (eval (string->symbol x)) -> (eval-string x)
			    (if (string? (cadr arg))
				(if (equal? (cadr arg) "")
				    (lint-format "string->symbol argument can't be a null string:~A" caller (truncated-list->string form))
				    (lint-format "perhaps ~A" caller (lists->string form (string->symbol (cadr arg)))))
				(lint-format "perhaps ~A" caller (lists->string form (list 'eval-string (cadr arg)))))))

		       ((with-input-from-string call-with-input-string)
			(if (and (len>1? (cdr arg))     ;  (eval (call-with-input-string port read)) -> (eval-string port)
				 (eq? (caddr arg) 'read))
			    (lint-format "perhaps ~A" caller (lists->string form (list 'eval-string (cadr arg))))))

		       ((read)
			(if (and (= (length arg) 2)    ;  (eval (read (open-input-string expr))) -> (eval-string expr)
				 (len>1? (cadr arg))
				 (eq? (caadr arg) 'open-input-string))
			    (lint-format "perhaps ~A" caller (lists->string form (list 'eval-string (cadadr arg))))))

		       ((list)
			(if (lint-every? (lambda (p)        ;  (eval (list '* 2 x)) -> (* 2 (eval x))
					   (or (symbol? p)
					       (code-constant? p)))
					 (cdr arg))
			    (lint-format "perhaps ~A" caller
					 (lists->string form
							(map (lambda (p)
							       (if (and (len>1? p)
									(eq? (car p) 'quote))
								   (cadr p)
								   (if (code-constant? p)
								       p
								       (list 'eval p))))
							     (cdr arg))))))))))
	      ((3)
	       (let ((arg (cadr form))
		     (e (caddr form)))
		 (if (and (not (code-constant? e)) ; error reported elsewhere
			  (pair? arg)
			  (eq? (car arg) 'quote))
		     (lint-format "perhaps ~A" caller   ;  (eval 'x env) -> (env 'x)
				  (lists->string form
						 (if (symbol? (cadr arg))
						     (list e arg)
						     (cons 'with-let (cons e (unbegin (cadr arg))))))))))))
	  (hash-special 'eval sp-eval))

	;; ---------------- fill! etc ----------------
	(let ()
	  (define (sp-fill! caller head form env)
	    (if (and (pair? (cdr form))
		     (code-constant? (cadr form)))
		(lint-format "~A is a constant, so ~A is problematic" caller
			     (cadr form)
			     (truncated-list->string form)))
	    (if (= (length form) 5)
		(check-start-and-end caller head (cdddr form) form env)))
	  (for-each (lambda (f)
		      (hash-special f sp-fill!))
		    '(fill! string-fill! list-fill! vector-fill!)))

	;; ---------------- write-string ----------------
	(let ()
	  (define (sp-write-string caller head form env)
	    (cond ((not (len>1? form)))

		  ((= (length form) 4)
		   (check-start-and-end caller 'write-string (cddr form) form env))

		  ((and (len>1? (cdr form))
			(pair? (caddr form))
			(eq? (caaddr form) 'current-output-port))
		   (lint-format "(current-output-port) is the default port for ~A: ~A" caller head form))

		  ((equal? (cadr form) (string #\newline))
		   (lint-format "perhaps ~A" caller (lists->string form (cons 'newline (cddr form)))))

		  ((equal? (cadr form) "")
		   (lint-format "~A is pointless" caller form))))

	(hash-special 'write-string sp-write-string))

	;; ---------------- read-line ----------------
	(let ()
	  (define (sp-read-line caller head form env)
	    (if (and (= (length form) 3)
		     (code-constant? (caddr form))
		     (not (boolean? (caddr form))))   ;  (read-line in-port 'concat)
		(lint-format "the third argument should be boolean (#f=default, #t=include trailing newline): ~A" caller form)
		(if (and (pair? (cdr form))
			 (pair? (cadr form))
			 (eq? (caadr form) 'current-input-port))
		    (lint-format "(current-input-port) is the default port for ~A: ~A" caller head form))))
	  (hash-special 'read-line sp-read-line))

	;; ---------------- string-length ----------------
	(let ()
	  (define (sp-string-length caller head form env)
	    (when (len=2? form)
	      (if (string? (cadr form))              ;  (string-length "asdf") -> 4
		  (lint-format "perhaps ~A -> ~A" caller (truncated-list->string form) (string-length (cadr form)))
		  (if (and (len>1? (cadr form))       ;  (string-length (make-string 3)) -> 3
			   (eq? (caadr form) 'make-string))
		      (lint-format "perhaps ~A" caller (lists->string form (cadadr form)))))))

	  (hash-special 'string-length sp-string-length))

	;; ---------------- vector-length ----------------
	(let ()
	  (define (sp-vector-length caller head form env)
	    (when (len=2? form)
	      (if (vector? (cadr form))
		  (lint-format "perhaps ~A -> ~A" caller (truncated-list->string form) (vector-length (cadr form)))
		  (let ((arg (cadr form)))
		    (if (len>1? arg)
			(case (car arg)
			  ((make-vector)                          ;  (vector-length (make-vector 10)) -> 10
			   (lint-format "perhaps ~A" caller (lists->string form (cadr arg))))
			  ((copy vector-copy)
			   (if (proper-list? arg)
			       (lint-format "perhaps ~A" caller   ;   (vector-length (copy arr)) -> (vector-length arr)
					    (lists->string form   ;   (vector-length (vector-copy arr start end)) -> (- end start)
							   (if (null? (cddr arg))
							       (list 'vector-length (cadr arg))
							       (if (eq? (car arg) 'copy)
								   (list 'vector-length (caddr arg))
								   (let ((start (caddr arg))
									 (end (if (null? (cdddr arg))
										  (list 'vector-length (cadr arg))
										  (cadddr arg))))
								     (list '- end start))))))))))))))
	  (hash-special 'vector-length sp-vector-length))

	;; ---------------- dynamic-wind ----------------
	(let ()
	  (define (sp-dw caller head form env)
	    (when (= (length form) 4)
	      (let ((init (cadr form))
		    (body (caddr form))
		    (end (cadddr form))
		    (empty 0))
		;; (equal? init end) as a mistake doesn't seem to happen

		(when (and (len>1? init)
			   (eq? (car init) 'lambda))
		  (if (not (null? (cadr init)))
		      (lint-format "dynamic-wind init function should be a thunk: ~A" caller init))
		  (if (pair? (cddr init))
		      (let ((last-expr (last-ref init)))
			(if (not (pair? last-expr))
			    (if (null? (cdddr init))
				(set! empty 1))
			    (unless (side-effect? last-expr env)
			      (if (null? (cdddr init))
				  (set! empty 1))    ;  (dynamic-wind (lambda () (+)) (lambda () (list)) (lambda () #f))
			      (lint-format "this could be omitted: ~A in ~A" caller last-expr init))))))

		(if (and (pair? body)
			 (eq? (car body) 'lambda))
		    (if (not (null? (cadr body)))
			(lint-format "dynamic-wind body function should be a thunk: ~A" caller body))
		    (set! empty 3)) ; don't try to access body below

		(when (and (len>1? end)
			   (eq? (car end) 'lambda))
		  (if (not (null? (cadr end)))
		      (lint-format "dynamic-wind end function should be a thunk: ~A" caller end))
		  (if (pair? (cddr end))
		      (let ((last-expr (last-ref end)))
			(if (not (pair? last-expr))
			    (if (null? (cdddr end))
				(set! empty (+ empty 1)))
			    (unless (side-effect? last-expr env) ; or if no side-effects in any (also in init)
			      (if (null? (cdddr end))
				  (set! empty (+ empty 1)))
			      (lint-format "this could be omitted: ~A in ~A" caller last-expr end)))
			(if (= empty 2)          ;  (dynamic-wind (lambda () #f) (lambda () #()) (lambda () #f)) -> #()
			    (lint-format "this dynamic-wind is pointless, ~A" caller
					 (lists->string form (if (null? (cdddr body)) (caddr body) (cons 'begin (cddr body))))))))))))
	  (hash-special 'dynamic-wind sp-dw))

	;; ---------------- with-output-to-string ----------------
	(let ()
	  (define (sp-wots caller head form env)
	    ;; (with-output-to-string (lambda () (display object)))             -> (object->string object #f)
	    ;; (with-output-to-string (lambda () (write (car defs)) (newline))) -> (format #f "~S~%" (car defs))
	    ;; (with-output-to-string (lambda () (write answer)))               -> (object->string answer)
	    (when (and (len=2? form)
		       (len>1? (cadr form))
		       (eq? (caadr form) 'lambda)
		       (null? (cadadr form)))
	      (let ((body (cddadr form)))
		(when (and (pair? body)
			   (len=2? (car body))
			   (memq (caar body) '(write display))) ; write-char write-string never happen
		  (if (null? (cdr body))
		      (lint-format "perhaps ~A" caller
				   (lists->string form (cons 'object->string
							     (cons (cadar body)
								   (if (eq? (caar body) 'display) '(#f) ())))))
		      (if (and (len=1? (cdr body))
			       (len=1? (cadr body))
			       (eq? (caadr body) 'newline))
			  (lint-format "perhaps ~A" caller
				       (lists->string form
						      (list 'format #f
							    (if (eq? (caar body) 'display) "~A~%" "~S~%")
							    (cadar body))))))))))
	  (hash-special 'with-output-to-string sp-wots))

	;; ---------------- help ----------------
	(hash-special 'help
	  (lambda (caller head form env)
	    (if (and (not (var-member 'help env))
		     (len=2? form)
		     (string? (cadr form)))
		(lint-format "s7's help function takes a symbol or a procedure as its argument: ~A" caller (truncated-list->string form)))))


	;; ---------------- curlet ----------------
	(hash-special 'curlet
	  (lambda (caller head form env)
	    (for-each (lambda (v)
			(set! (var-ref v) (+ (var-ref v) 1)))
		      env)))


	;; ---------------- *s7* ----------------
	(hash-special '*s7*
	 (let ((s7-fields (let ((h (make-hash-table)))
			    (for-each (lambda (f)
					(hash-table-set! h f #t))
				      '(stack-top stack-size stacktrace-defaults heap-size free-heap-size major-version minor-version
				        gc-freed gc-protected-objects gc-total-freed gc-info file-names filenames rootlet-size c-types safety
					undefined-identifier-warnings undefined-constant-warnings gc-stats max-heap-size
					max-port-data-size max-stack-size cpu-time catches stack max-string-length
					max-format-length max-list-length max-vector-length max-vector-dimensions
					default-hash-table-length initial-string-port-length default-rationalize-error
					default-random-state equivalent-float-epsilon hash-table-float-epsilon print-length
					bignum-precision memory-usage float-format-precision history history-enabled
					history-size profile profile-info profile-prefix autoloading? accept-all-keyword-arguments
					muffle-warnings? most-positive-fixnum most-negative-fixnum output-port-data-size debug version
					gc-temps-size gc-resize-heap-fraction gc-resize-heap-by-4-fraction openlets expansions?
					number-separator))
			    h)))
	   (lambda (caller head form env)
	     (if (len=2? form)
		 (let ((arg (cadr form)))
		   (if (and (len>1? arg)
			    (eq? (car arg) 'quote)
			    (symbol? (cadr arg))            ;  (*s7* 'vector-print-length)
			    (not (hash-table-ref s7-fields (cadr arg))))
		       (lint-format "unknown *s7* field: ~A" caller arg)))))))

	;; ---------------- make-hash-table ----------------
	(hash-special 'make-hash-table
	 (lambda (caller head form env)
	   (when (pair? (cdr form))                         ; (make-hash-table -1)
	     (when (and (integer? (cadr form)) (<= (cadr form) 0))
	       (lint-format "make-hash-table length: ~S?" caller (cadr form)))
	     (when (pair? (cddr form))
	       (let ((checker (caddr form)))
		 (if (and (symbol? checker)               ; (make-hash-table 8 call/cc)
			  (hash-table-ref built-in-functions checker)
			  (not (memq checker '(eq? eqv? equal? equivalent? char=? char-ci=? string=? string-ci=? =))))
		     (lint-format "make-hash-table function, ~A, is not a hash function" caller checker)))))))

	;; ---------------- cond-expand ----------------
	(let ()
	  (define (sp-cond-expand caller head form env)
	    (if (lint-every? (lambda (c)
			       (not (len>1? c)))
			     (cdr form))
		(lint-format "pointless cond-expand: ~A" caller (truncated-list->string form))
		(for-each (lambda (c)
			    (if (not (or (symbol? (car c))
					 (and (pair? (car c))
					      (memq (caar c) '(and or not library)))))
				(lint-format "messed up cond-expand clause: ~A" caller (truncated-list->string c))
				(if (and (pair? (car c))
					 (eq? (caar c) 'library))
				    (lint-format "the cond-expand 'library' option is not implemented: ~A" caller (truncated-list->string c)))))
			  (cdr form))))
	  (hash-special 'cond-expand sp-cond-expand))

	;; ---------------- macroexpand ----------------
	(let ()
	  (define (sp-macroexpand caller head form env)
	    (let ((arg (and (len=1? (cdr form))
			    (cadr form))))
	      (unless (and (pair? arg)
			   (symbol? (car arg))
			   (or (any-macro? (car arg) env)
			       (macro? (symbol->value (car arg))))) ; macro? includes bacro?
		(lint-format "macroexpand's argument should be an expression whose car is a macro: ~A" caller (truncated-list->string form)))))
	  (hash-special 'macroexpand sp-macroexpand))

	;; ---------------- deprecated funcs ----------------
	(let ((deprecated-ops '((global-environment . rootlet)
				(current-environment . curlet)
				(make-procedure-with-setter . dilambda)
				(procedure-with-setter? . dilambda?)
				(symbol-setter . setter)
				(make-keyword . string->keyword)
				(make-random-state . random-state))))

	  (define (sp-deprecate caller head form env)  ;  (make-random-state 123 432)
	    (lint-format "~A is deprecated; use ~A" caller head (cond ((assq head deprecated-ops) => cdr))))

	  (for-each (lambda (op)
		      (hash-special (car op) sp-deprecate))
		    deprecated-ops))

	;; ---------------- eq null eqv equal ----------------
	(let ()
	  (define sp-null
	    (let ((spellings
		   '((null . null?) (eq . eq?) (eqv . eqv?) (equal . equal?) (not? . not)  ; (null (cdr...))
		     (set-car . set-car!) (set-cdr . set-cdr!) (list-set . list-set!) (vector-set . vector-set!) (string-set . string-set!))))
	      (lambda (caller head form env)
		(if (not (var-member head env))          ;  (if (null (cdr x)) 0)
		    (lint-format "misspelled '~A in ~A?" caller (cdr (assq head spellings)) form)))))
	  (for-each (lambda (f)
		      (hash-special f sp-null))
		    '(null eq eqv equal set-car set-cdr list-set vector-set string-set)))
	  ;; memq? is in scheme48

	(hash-special 'nth
		      (lambda (caller head form env)
			(if (not (var-member head env))
			    (if (integer? (caddr form))
				(lint-format "perhaps use list-ref here: ~A" caller form)
				(if (integer? (cadr form))
				    (lint-format "perhaps ~A" caller (lists->string form (cons 'list-ref (reverse (cdr form))))))))))

	(hash-special 'sort
		      (lambda (caller head form env)
			(if (and (= (length form) 3)
				 (not (and (pair? (cadr form))
					   (eq? (caadr form) 'lambda))))
			    (if (and (pair? (cadr form))
				     (eq? (caadr form) 'map))
				(lint-format "use sort! here: ~A" caller (truncated-list->string form))
				(lint-format "perhaps ~A" caller
					     (truncated-lists->string form
								      (list 'sort! (list 'copy (cadr form)) (caddr form))))))))

	;; ---------------- string-index ----------------
	(let ()
	  (define (sp-string-index caller head form env)
	    (if (and (len>1? (cdr form))
                     (not (var-member 'string-index env))
		     (or (char? (caddr form))
			 (let ((sig (arg-signature (caddr form) env)))
			   (and (pair? sig)
				(eq? (car sig) 'char?)))))
		(lint-format "perhaps ~A" caller          ;  (string-index path #\/) -> (char-position #\/ path)
			     (lists->string form `(char-position ,(caddr form) ,(cadr form) ,@(cdddr form))))))
	  (hash-special 'string-index sp-string-index))

	;; ---------------- cons* ----------------
	(let ()
	  (define (sp-cons* caller head form env)
	    (unless (var-member 'cons env)
	      (case (length form)
		((2) (lint-format "perhaps ~A" caller (lists->string form (cadr form))))
		((3) (lint-format "perhaps ~A" caller
				  (lists->string form      ;  cons* x y) -> (cons x y)
						 (if (any-null? (caddr form))
						     (list 'list (cadr form))
						     (cons 'cons (cdr form))))))
		((4) (lint-format "perhaps ~A" caller
				  (lists->string form      ;  (cons* (symbol->string v) " | " (w)) -> (cons (symbol->string v) (cons " | " (w)))
						 (if (any-null? (cadddr form))
						     (list 'list (cadr form) (caddr form))
						     (list 'cons (cadr form) (cons 'cons (cddr form))))))))))
	  (hash-special 'cons* sp-cons*))

	;; ---------------- the-environment etc ----------------
	(let ((other-names '((->string . object->string)
			     (any . any?)
			     (arithmetic-shift . ash)
			     (bit-and . logand)
			     (bit-not . lognot)
			     (bit-or . logior)
			     (bit-xor . logxor)
			     (bitwise-and . logand)
			     (bitwise-bit-set? . logbit?)
			     (bitwise-ior . logior)
			     (bitwise-not . lognot)
			     (bitwise-xor . logxor)
			     (bytevector . byte-vector)
			     (bytevector-copy . copy)
			     (bytevector-fill! . fill!)
			     (bytevector-length . length)
			     (bytevector? . byte-vector?)
			     (bytevector-ref . byte-vector-ref)
			     (bytevector-set! . byte-vector-set!)
			     (bytevector-u8-ref . byte-vector-ref)
			     (bytevector-u8-set! . byte-vector-set!)
			     (byte-vector-length . length)
			     (copy-list . copy)
			     (copy-string . copy)
			     (copy-vector . copy)
			     (environment-copy . copy)
			     (environment-ref . let-ref)
			     (environment-set! . let-set!)
			     (environment? . let?)
			     (every . every?)
			     (exact-integer? . integer?)
			     (f64vector . float-vector)
			     (f64vector-length . length)
			     (f64vector-ref . float-vector-ref)
			     (f64vector-set! . float-vector-set!)
			     (f64vector? . float-vector?)
			     (fixnum? . integer?)
			     (floor-remainder . modulo)
			     (flo:vector-length . length)
			     (fluid-let . let-temporarily)
			     (hash-for-each . for-each)
			     (hash-ref . hash-table-ref)
			     (hash-set! . hash-table-set!)     ; Guile
			     (hash-table-exists? . hash-table-ref)
			     (hash-table-get . hash-table-ref) ; Gauche
			     (hash-table-copy . copy)
			     (hash-table-num-entries . hash-table-entries)
			     (hash-table-put! . hash-table-set!)
			     (hashq-ref . hash-table-ref)
			     (hashq-set! . hash-table-set!)
			     (hashtable-copy . copy)
			     (hashtable-get . hash-table-ref)
			     (hashtable-put! . hash-table-set!)
			     (hashtable-set! . hash-table-set!)
			     (hashtable-size . hash-table-entries)
			     (hashtable? . hash-table?)        ; Bigloo
			     (hashv-ref . hash-table-ref)
			     (hashv-set! . hash-table-set!)
			     (interaction-environment . curlet)
			     (intern . string->symbol)
			     ;(list-copy . copy)
			     ;(list-fill! . fill!)
			     (list-length . length)
			     (list-reverse . reverse)
			     (make-bytevector . make-byte-vector)
			     (make-f64vector . make-float-vector)
			     (make-hashtable . make-hash-table)
			     (make-s64vector . make-int-vector)
			     (make-u8vector . make-byte-vector)
			     (nreverse . reverse!)
			     (open-input-bytevector . open-input-string)
			     (open-output-bytevector . open-output-string)
			     (peek-u8 . peek-char)
			     (procedure-environment . funclet)
			     (raise . error)
			     (raise-continuable . error)
			     (read-u8 . read-byte)
			     (reverse-u8vector . reverse)
			     (reverse-string . reverse)
			     (s64vector . int-vector)
			     (s64vector-length . length)
			     (s64vector-ref . int-vector-ref)
			     (s64vector-set! . int-vector-set!)
			     (s64vector? . int-vector?)
			     (some . any?)
			     (some? . any?)
			     ;(string-copy . copy)
			     ;(string-fill! . fill!)
			     (string-for-each . for-each)
			     (string-reverse! . reverse!)
			     (string-reverse . reverse)
			     (symbol-name . symbol->string)
			     (system-global-environment . rootlet)
			     (the-environment . curlet)
			     (truncate-quotient . quotient)
			     (truncate-remainder . remainder)
			     (u8-ready? . char-ready?)
			     (u8vector . byte-vector)
			     (u8vector-copy . copy)
			     (u8vector? . byte-vector?)
			     (u8vector-length . length)
			     (u8vector-fill! . fill!)
			     (u8vector-ref . byte-vector-ref)
			     (u8vector-set! . byte-vector-set!)
			     (unquote-splicing apply values ...)
			     (unspecified . #<unspecified>)
			     (unspecific . #<unspecified>)
			     (user-global-environment . rootlet)
			     (user-initial-environment . rootlet)
			     ;(vector-copy . copy)
			     ;(vector-fill! . fill!)
			     (vector-for-each . for-each)
			     (vector-reverse! . reverse!)
			     (write-bytevector . write-string)
			     (write-simple . write)
			     (write-u8 . write-byte))))

	  (define (sp-other-names caller head form env)
	    (if (not (var-member head env))
		(let ((counts (or (hash-table-ref other-names-counts head) 0))
		      (our-name (cdr (assq head other-names))))
		  (when (< counts 2)
		    (hash-table-set! other-names-counts head (+ counts 1))
		    (lint-format "~A is probably ~A" caller head our-name))
		  (cond ((hash-table-ref special-case-functions our-name)
			 => (lambda (f)
			      (f caller our-name (cons our-name (cdr form)) env)))))))
	  (for-each (lambda (f)
		      (hash-special (car f) sp-other-names))
		    other-names))

	(hash-special '1+
	 (lambda (caller head form env)
	   (if (and (not (var-member '1+ env))
		    (pair? (cdr form)))
	       (lint-format "perhaps ~A" caller (lists->string form (list '+ (cadr form) 1))))))

	(let ()
	  (define (sp-1- caller head form env)
	    (if (and (not (var-member '-1+ env))
		     (pair? (cdr form)))
		(lint-format "perhaps ~A" caller (lists->string form (list '- (cadr form) 1)))))

	  (hash-special '-1+ sp-1-)
	  (hash-special '1- sp-1-))

	(let ()
	  (define (sp-dumb-relop caller head form env)
	    (if (not (var-member (car form) env))
		(lint-format "perhaps ~A" caller
			     (lists->string form
					    (simplify-boolean (undumb form) env () ())))))
	  (for-each (lambda (f)
		      (hash-special f sp-dumb-relop))
		    '(fix:= fx= flo:= fl= fix:< fx< flo:< fl< fix:> fx> flo:> fl> fix:<= fx<= flo:<= fl<= fix:>= fx>= flo:>= fl>=
		      bignum= bignum< bignum<= bignum> bignum>= bignum-negative? bignum-zero?)))

	(let ()
	  (define (sp-dumb-fop caller head form env)
	    (if (not (var-member (car form) env))
		(lint-format "perhaps ~A" caller (lists->string form (undumb form)))))

	  (for-each (lambda (f)
		      (hash-special f sp-dumb-fop))
		    '(flo:sin flsin flo:cos flcos flo:tan fltan flo:atan flatan flo:exp flexp flo:log fllog flo:sqrt flsqrt
		      fxlogand fxlogior fxlogxor fxlognot
		      bignum-expt bignum-quotient bignum-magnitude bignum-abs bignum-remainder)))


	;; ---------------- push! pop! ----------------
	(hash-special 'push!
	 (lambda (caller head form env) ; not predefined
	   (if (= (length form) 3)
	       (set-set (caddr form) caller form env))))

	(hash-special 'pop!
	 (lambda (caller head form env) ; also not predefined
	   (if (len=2? form)
	       (set-set (cadr form) caller form env))))

	;; ---------------- receive ----------------
	(hash-special 'receive
	 (lambda (caller head form env) ; this definition comes from Guile
	   (if (and (> (length form) 3)
		    (not (var-member 'receive env)))
	       ((hash-table-ref special-case-table 'call-with-values)
		caller 'call-with-values
		`(call-with-values
		     (lambda () ,(caddr form))
		   (lambda ,(cadr form) ,@(cdddr form)))
		env))))

	;; ---------------- and=> ----------------
	(hash-special 'and=>
	 (lambda (caller head form env)  ;  (and=> (ref w k) v) -> (cond ((ref w k) => v) (else #f))
	   (when (and (= (length form) 3)
		      (not (var-member 'and=> env)))
	     (lint-format "perhaps ~A" caller (lists->string form `(cond (,(cadr form) => ,(caddr form)) (else #f)))))))

	;; ---------------- and-let* ----------------
	(let ()
	  (define (sp-and-let caller head form env)
	    (when (and (> (length form) 2)
		       (not (var-member 'and-let* env)))
	      (let loop ((bindings (cadr form)))
		(cond ((pair? bindings)
		       (if (binding-ok? caller 'and-let* (car bindings) env #f)
			   (loop (cdr bindings))))
		      ((not (null? bindings))
		       (lint-format "~A variable list is not a proper list? ~S" caller 'and-let* bindings))
		      ((and (len=1? (cadr form))           ; (and-let* ((x (f y))) (abs x)) -> (cond ((f y) => abs))
			    (pair? (cddr form)))
		       (lint-format "perhaps ~A" caller
				    (lists->string form   ;  (and-let* ((x (f y))) (abs x)) -> (cond ((f y) => abs))
						   (if (and (null? (cdddr form))
							    (len=2? (caddr form))
							    (eq? (caaadr form) (cadr (caddr form))))
						       (list 'cond (list (cadar (cadr form)) '=> (caaddr form)))
						       `(cond (,(cadar (cadr form)) => (lambda (,(caaadr form)) ,@(cddr form))))))))))))
	  (hash-special 'and-let* sp-and-let))

	special-case-table))
    ;; end special-case-functions
    ;; ----------------------------------------


    (define (unused-parameter? x) #t)
    (define (unused-set-parameter? x) #t)

    (define check-args
      ;; check for obvious argument type problems
      ;; caller = overall caller, head = current caller, checkers = proc or list of procs for checking args
      (letrec ((every-compatible?
		(lambda (type1 type2)
		  (if (symbol? type1)
		      (if (symbol? type2)
			  (compatible? type1 type2)
			  (and (pair? type2)                   ; here everything has to match
			       (compatible? type1 (car type2))
			       (every-compatible? type1 (cdr type2))))
		      (and (pair? type1)                       ; here any match is good
			   (or (compatible? (car type1) type2)
			       (any-compatible? (cdr type1) type2))))))

	       (check-checker
		(lambda (checker at-end)
		  (case checker
		    ((integer:real?) (if at-end 'real? 'integer?))
		    ((integer:any?)  (or at-end 'integer?))
		    (else))))

	       (any-checker?
		(lambda (types arg)
		  (if (and (symbol? types)
			   (not (eq? types 'values)))
		      ((symbol->value types *e*) arg) ; not (rootlet) for *e* since we have some local checkers: unused-parameter? etc
		      (and (pair? types)
			   (or (any-checker? (car types) arg)
			       (any-checker? (cdr types) arg))))))

	       (report-arg-trouble
		(lambda (caller form head arg-number checker arg uop env)
		  (when (and (or arg (not (eq? checker 'output-port?)))
			     (not (and (eq? checker 'string?)
				       (len>1? arg)
				       (eq? (car arg) 'format)
				       (not (null? (cadr arg)))))  ; other case involves a symbol that is an output-port
			     (not (and (pair? arg)
				       (eq? (car arg) 'length))))  ; same for length
		    (let ((op (if (and (eq? checker 'real?)
				       (eq? uop 'number?))
				  'complex?
				  uop))
			  (prettify-arg-number (lambda (argn)
						 (if (or (not (= argn 1))
							 (pair? (cddr form)))
						     (format #f "~:D " argn)
						     ""))))
		      (if (and (pair? op)
			       (member checker op any-compatible?))
			  (unless (or (not *report-sloppy-assoc*)
				      (and (pair? arg)
					   (memq (car arg) '(int-vector-ref float-vector-ref system)))
				      (var-member :catch env))   ; (round (char-position #\a "asb"))
			    (let ((other (remove-if (lambda (o) (any-compatible? checker o)) op)))
			      (when (pair? other)
				(lint-format "in ~A,~%~NC~A's ~Aargument should be ~A, but ~A might also be ~A" caller
					     (truncated-list->string form)
					     (+ lint-left-margin 4) #\space
					     head
					     (prettify-arg-number arg-number)
					     (prettify-checker-unq checker)
					     (truncated-list->string arg)
					     (prettify-checker (car other))))))
			  (if *report-laconically*
			      (lint-format "~A's ~Aargument should be ~A, but ~A is ~A" caller
				       head
				       (prettify-arg-number arg-number)
				       (prettify-checker-unq checker)
				       (truncated-list->string arg)
				       (prettify-checker op))
			      (lint-format "in ~A,~%~NC~A's ~Aargument should be ~A, but ~A is ~A" caller
					   (truncated-list->string form)
					   (+ lint-left-margin 4) #\space
					   head
					   (prettify-arg-number arg-number)
					   (prettify-checker-unq checker)
					   (truncated-list->string arg)
					   (prettify-checker op)))))))))

	(lambda (caller head v form checkers env max-arity)
	  (when (and *report-func-as-arg-arity-mismatch*
		     v
		     (memq (var-ftype v) '(define define* lambda lambda*))
		     (zero? (var-set v)) ; perhaps this needs to wait for report-usage?
		     (pair? (var-arglist v)))
	    (let ((source (var-initial-value v)))
	      (when (len>2? source)
		(let ((vhead (cddr source))
		      (head-arglist (var-arglist v))
		      (arg-number 1))

		  (when (pair? vhead)
		    (for-each
		     (lambda (arg)
		       ;; only check func if head is var-member and has procedure-source (var-[initial-]value?)
		       ;;   and arg has known arity, and check only if arg(par) is car, not (for example) cadr of apply

		       (let ((ari (if (symbol? arg)
				      (arg-arity arg env)
				      (and (len>1? arg)
					   (eq? (car arg) 'lambda)
					   (let ((len (length (cadr arg))))
					     (and (integer? len)
						  (cons (abs len)
							(if (negative? len) 500000 len)))))))
			     (par (and (> (length head-arglist) (- arg-number 1))
				       (list-ref head-arglist (- arg-number 1)))))
			 (when (and (symbol? par)
				    (pair? ari)
				    (or (> (car ari) 0)
					(< (cdr ari) 20)))

			   ;; fwalk below needs to be smart about tree walking so that
			   ;;   it does not confuse (c) in (lambda (c)...) with a call on the function c.
			   ;; check only if current parameter name is not shadowed

			   (let fwalk ((sym par) (tree vhead))
			     (when (pair? tree)
			       (if (eq? (car tree) sym)
				   (let ((args (- (length tree) 1)))
				     (if (> (car ari) args)
					 (lint-format "~A's parameter ~A is passed ~A and called ~A, but ~A needs ~A argument~P" caller
						      head par
						      (truncated-list->string arg)
						      (truncated-list->string tree)
						      (truncated-list->string arg)
						      (car ari) (car ari))
					 (if (> args (cdr ari))
					     (lint-format "~A's parameter ~A is passed ~A and called ~A, but ~A takes only ~A argument~P" caller
							  head par
							  (truncated-list->string arg)
							  (truncated-list->string tree)
							  (truncated-list->string arg)
							  (cdr ari) (cdr ari)))))
				   (case (car tree)
				     ((let let*)
				      (if (len>1? (cdr tree))
					  (let ((vs ((if (symbol? (cadr tree)) caddr cadr) tree)))
					    (if (not (lint-any? (lambda (a) (or (not (pair? a)) (eq? sym (car a)))) vs))
						(fwalk sym ((if (symbol? (cadr tree)) cdddr cddr) tree))))))

				     ((do letrec letrec*)
				      (if (and (len>1? (cdr tree))
					       (not (lint-any? (lambda (a) (or (not (pair? a)) (eq? sym (car a)))) (cadr tree))))
					  (fwalk sym (cddr tree))))

				     ((lambda lambda*)
				      (if (and (len>1? (cdr tree))
					       (not (lint-any? (lambda (a) (eq? sym a)) (args->proper-list (cadr tree)))))
					  (fwalk sym (cddr tree))))

				     ((define define-constant)
				      (if (and (not (eq? sym (cadr tree)))
					       (pair? (cadr tree))
					       (not (lint-any? (lambda (a) (eq? sym a)) (args->proper-list (cdadr tree)))))
					  (fwalk sym (cddr tree))))

				     ((define* define-macro define-macro* define-expansion define-bacro define-bacro*)
				      (if (and (len>1? (cdr tree))
					       (pair? (cadr tree))
					       (not (lint-any? (lambda (a) (eq? sym a)) (args->proper-list (cdadr tree)))))
					  (fwalk sym (cddr tree))))

				     ((quote) #f)

				     ((case)
				      (if (len>1? (cdr tree))
					  (for-each (lambda (c) (fwalk sym (cdr c))) (cddr tree))))

				     (else
				      (if (pair? (car tree))
					  (fwalk sym (car tree)))
				      (if (pair? (cdr tree))
					  (for-each (lambda (p) (fwalk sym p)) (cdr tree))))))))))

		       (set! arg-number (+ arg-number 1)))
		     (cdr form)))))))

	  (when (pair? checkers)
	    (let ((arg-number 1)
		  (flen (- (length form) 1)))

	      (define (check-cond-arg expr checker)
		(unless (symbol? expr)
		  (let ((op (->lint-type expr)))
		    (when (pair? op)
		      (set! op (remove-one 'boolean? op)) ; this is for cond test, no result -- returns test if not #f, so it can't be #f!
		      (if (null? (cdr op))
			  (set! op (car op))))
		    (if (not (or (memq op '(#f #t values))
				 (every-compatible? checker op)))
			(report-arg-trouble caller form head arg-number checker expr op env)))))
	      (call-with-exit
	       (lambda (done)
		 (for-each
		  (lambda (arg)
		    (let ((checker (check-checker (if (pair? checkers) (car checkers) checkers) (= arg-number flen))))
		      ;; check-checker only fixes up :at-end cases

		      (define (check-arg expr)
			(unless (symbol? expr)
			  (let ((op (->lint-type expr)))
			    (if (not (or (memq op '(#f #t values))
					 (every-compatible? checker op)))
				(report-arg-trouble caller form head arg-number checker expr op env)))))
		      ;; special case checker?
		      (if (and (symbol? checker)
			       (not (memq checker '(unused-parameter? unused-set-parameter?)))
			       (not (hash-table-ref built-in-functions checker)))
			  (let ((chk (symbol->value checker)))
			    (if (and (procedure? chk)
				     (equal? (arity chk) '(2 . 2)))
				(catch #t
				  (lambda ()
				    (let ((res (chk form arg-number)))
				      (set! checker #t)
				      (if (symbol? res)
					  (set! checker res)
					  (if (string? res)
					      (lint-format "~A's argument, ~A, should be ~A" caller head arg res)))))
				  (lambda (type info)
				    (set! checker #t))))))

		      (if (and (pair? arg)
			       (pair? (car arg)))
			  (let ((rtn (return-type (caar arg) env)))
			    (if (memq rtn '(boolean? real? integer? rational? number? complex? float? keyword? symbol? null? char?))
				(lint-format "~A's argument ~A looks odd: ~A returns ~A which is not applicable" caller    ;  (cons ((pair? x) 2) y)
					     head
					     (truncated-list->string arg)
					     (caar arg) rtn))))

		      (when (or (pair? checker)
				(symbol? checker)) ; otherwise ignore type check on this argument (#t -> anything goes)
			(if arg
			    (case checker
			      ((unused-parameter?)     ;  (define (f5 a . b) a) (f5 1 2)
			       (lint-format "~A's parameter ~A is not used, but a value is passed: ~A" caller
					    head arg-number
					    (truncated-list->string arg)))
			      ((unused-set-parameter?) ;  (define (f21 x y) (set! x 3) (+ y 1)) (f21 (+ z 1) z)
			       (lint-format "~A's parameter ~A's value is not used, but a value is passed: ~A" caller
					    head arg-number
					    (truncated-list->string arg)))))

			(if (not (pair? arg))
			    (let ((val (cond ((not (symbol? arg))
					      arg)
					     ((constant? arg)
					      (symbol->value arg))
					     ((and (hash-table-ref built-in-functions arg)
						   (not (var-member :with-let env))
						   (not (var-member arg env)))
					      (symbol->value arg *e*))
					     (else arg))))
			      (if (not (or (and (symbol? val)
						(not (keyword? val)))
					   (any-checker? checker val)))
				  (let ((op (->lint-type val)))
				    (unless (or (memq op '(#f #t values))
						(and (keyword? arg) ; (f :par arg) where f is define*
						     (var? v)
						     (memq (var-ftype v) '(define* lambda*))))
				      (report-arg-trouble caller form head arg-number checker arg op env)))))

			    (case (car arg)
			      ((quote)   ; '1 -> 1
			       (when (pair? (cdr arg))
				 (let ((op (if (pair? (cadr arg)) 'list?
					       (if (symbol? (cadr arg))
						   'symbol?
						   (->lint-type (cadr arg))))))
				   ;; arg is quoted expression
				   (if (not (or (memq op '(#f #t values))
						(every-compatible? checker op)))
				       (report-arg-trouble caller form head arg-number checker arg op env)))))

			      ;; arg is an expression
			      ((begin let let* letrec letrec* with-let)
			       (let ((len (length arg)))
				 (if (> len 1)
				     (check-arg (and (pair? (cdr arg))
						     (list-ref arg (- len 1)))))))

			      ((if)
			       (if (len>1? (cdr arg))
				   (let ((f (if (pair? (cdddr arg)) (cadddr arg))))
				     (check-arg (caddr arg))
				     (when (and f (not (symbol? f)))
				       (check-arg f)))))

			      ((dynamic-wind catch)
			       (if (= (length arg) 4)
				   (let ((f (caddr arg)))
				     (if (and (pair? f)
					      (eq? (car f) 'lambda))
					 (let ((len (length f)))
					   (if (> len 2)
					       (check-arg (list-ref f (- len 1)))))))))

			      ((do)
			       (if (len>1? (cdr arg))
				   (let ((end+res (caddr arg)))
				     (if (pair? end+res)
					 (check-arg ((if (> (length end+res) 1) last-ref car) end+res)))))) ; car=test which is returned if no result

			      ((case)
			       (if (len>1? (cdr arg))
				   (for-each
				    (lambda (clause)
				      (if (and (pair? clause)
					       (> (length clause) 1)
					       (not (eq? (cadr clause) '=>)))
					  (check-arg (last-ref clause))))
				    (cddr arg))))

			      ((cond)
			       (when (pair? (cdr arg))
				 (for-each
				  (lambda (clause)
				    (if (pair? clause)
					(if (pair? (cdr clause))
					    (if (and (not (eq? (cadr clause) '=>))
						     (proper-list? (cdr clause)))
						(check-arg (last-ref clause)))
					    (check-cond-arg (car clause) checker))))
				  (cdr arg))))

			      ((call/cc call-with-exit call-with-current-continuation)
			       ;; find func in body (as car of list), check its arg as return value
			       (when (and (pair? (cdr arg))
					  (pair? (cadr arg))
					  (eq? (caadr arg) 'lambda))
				 (let ((f (cdadr arg)))
				   (when (and (pair? f)
					      (len=1? (car f))
					      (symbol? (caar f)))
				     (define c-walk
				       (let ((rtn (caar f)))
					 (lambda (tree)
					   (if (len>1? tree)
					       (if (eq? (car tree) rtn)
						   (check-arg (if (null? (cdr tree)) () (cadr tree)))
						   (begin
						     (c-walk (car tree))
						     (for-each (lambda (x) (if (pair? x) (c-walk x))) (cdr tree))))))))
				     (for-each c-walk (cdr f))))))

			      ((values)
			       (cond ((not (positive? (length arg))))

				     ((null? (cdr arg)) ; #<unspecified>
				      (if (not (any-checker? checker #<unspecified>))
					  (report-arg-trouble caller form head arg-number checker arg 'unspecified? env)))

				     ((null? (cddr arg))
				      (check-arg (cadr arg)))

				     (else
				      (for-each
				       (lambda (expr rest)
					 (check-arg expr)
					 (set! arg-number (+ arg-number 1))
					 (if (> arg-number max-arity) (done))
					 (if (pair? checkers)
					     (if (null? (cdr checkers))
						 (done)
						 (set! checkers (cdr checkers)))))
				       (cdr arg) (cddr arg))
				      (check-arg (last-ref arg)))))

			      (else
			       (let ((op (return-type (car arg) env)))
				 (when (and (eq? (car arg) 'random) ; (make-byte-vector 2 (random 256)!
					    (pair? (cdr arg)))      ; (random) in some schemes
				   (set! op (->lint-type (cadr arg)))
				   (if (and (eq? checker 'byte?)
					    (integer? (cadr arg))
					    (<= 0 (cadr arg) 256))
				       (set! op 'byte?)))
				 (let ((v (var-member (car arg) env)))
				   (when (and v (not (memq form (var-history v))))
				     (set! (var-history v) (cons form (var-history v)))
				     (set! (var-refenv v) env)))

				 ;; checker is arg-type, op is expression type (can also be a pair)
				 (if (and (not (memq op '(#f #t values)))
					  (not (memq checker '(unused-parameter? unused-set-parameter?)))
					  (or (not (every-compatible? checker op))
					      (and (just-constants? arg env) ; try to eval the arg
						   (catch #t
						     (lambda ()
						       (not (any-checker? checker (eval arg))))
						     (lambda ignore-catch-error-args
						       #f)))))
				     (report-arg-trouble caller form head arg-number checker arg op env)))))))

		      (if (pair? checkers)
			  (if (null? (cdr checkers))
			      (done)
			      (set! checkers (cdr checkers)))
			  (if (memq checker '(unused-parameter? unused-set-parameter?))
			      (set! checker #t)))
		      (set! arg-number (+ arg-number 1))
		      (if (> arg-number max-arity) (done))))
		  (cdr form)))))))))

    (define check-unordered-exprs
      (let ((changers (let ((h (make-hash-table)))
			(for-each (lambda (s)
				    (hash-table-set! h s #t))
				  (cons 'set! side-effect-functions))
			h)))
#|
				  '(set!
				    read read-byte read-char read-line read-string peek-char
				    write write-byte write-char write-string format display newline
				    reverse! set-cdr! sort! string-fill! vector-fill! fill!
				    emergency-exit exit error throw))
|#
	(lambda (caller form vals env)
	  (define (report-trouble)  ;  (let ((x (read-byte)) (y (read-byte))) (- x y))
	    (lint-format "order of evaluation of ~A's ~A is unspecified, so ~A is trouble" caller
			 (car form)
			 (if (memq (car form) '(let letrec do)) "bindings" "arguments")
			 (truncated-list->string form)))
	  (let ((reads ())
		(writes ())
		(jumps ()))
	    (call-with-exit
	     (lambda (return)
	       (for-each (lambda (p)
			   (when (and (pair? p)
				      (hash-table-ref changers (car p))
				      (not (var-member (car p) env)))
			     (if (pair? jumps)
				 (return (report-trouble)))

			     (case (car p)

			       ((read read-char read-line read-byte)
				(cond ((pair? (cdr p))
				       (if (memq (cadr p) reads)
					   (return (report-trouble))
					   (set! reads (cons (cadr p) reads))))
				      ((or (not (null? (cdr p)))
					   (memq () reads))
				       (return (report-trouble)))
				      (else
				       (set! reads (cons () reads)))))

			       ((newline)
				(cond ((pair? (cdr p))
				       (if (memq (cadr p) writes)
					   (return (report-trouble))
					   (set! writes (cons (cadr p) writes))))
				      ((or (not (null? (cdr p)))
					   (memq () writes))
				       (return (report-trouble)))
				      (else
				       (set! writes (cons () writes)))))

			       ((read-string)
				(if (not (len>1? (cdr p)))
				    (if (memq () reads)
					(return (report-trouble))
					(set! reads (cons () reads)))
				    (if (memq (caddr p) reads)
					(return (report-trouble))
					(set! reads (cons (caddr p) reads)))))

			       ((display write write-char write-string write-byte)
				(if (pair? (cdr p))
				    (if (null? (cddr p))
					(if (memq () writes)
					    (return (report-trouble))
					    (set! writes (cons () writes)))
					(if (or (not (pair? (cddr p)))
						(memq (caddr p) writes))
					    (return (report-trouble))
					    (set! writes (cons (caddr p) writes))))))

			       ((format)
				(if (and (pair? (cdr p))
					 (not (string? (cadr p)))
					 (cadr p)) ; i.e. not #f
				    (if (memq (cadr p) writes)
					(return (report-trouble))
					(set! writes (cons (cadr p) writes)))))

			       ((fill! string-fill! vector-fill!  reverse! sort! set! set-cdr!)
				;; here there's trouble if cadr used anywhere -- but we need to check for shadowing which is tedious:
				(if (and (len>1? p)
					 (lint-any? (lambda (np)
						      (and (not (eq? np p))
							   (tree-memq (cadr p) np)
							   (not (shadowed? (cadr p) np))))
						    vals))
				    (return (report-trouble))))

			       ((throw error exit emergency-exit)
				(if (or (pair? reads)   ; jumps already checked above
					(pair? writes))
				    (return (report-trouble))
				    (set! jumps (cons p jumps)))))))
			 vals)))))))

    (denote check-call
      (let ((repeated-args-table (let ((h (make-hash-table)))
				   (for-each
				    (lambda (op)
				      (set! (h op) #t))
				    '(= / max min < > <= >= - quotient remainder modulo rationalize and or
					string=? string<=? string>=? string<? string>? string-ci=? string-ci<=? string-ci>=? string-ci<? string-ci>?
					char=? char<=? char>=? char<? char>? char-ci=? char-ci<=? char-ci>=? char-ci<? char-ci>?
					boolean=? symbol=?))
				   h))
	    (repeated-args-table-2 (let ((h (make-hash-table)))
				     (for-each
				      (lambda (op)
					(set! (h op) #t))
				      '(= max min < > <= >= and or
					  string=? string<=? string>=? string<? string>? string-ci=? string-ci<=? string-ci>=? string-ci<? string-ci>?
					  char=? char<=? char>=? char<? char>? char-ci=? char-ci<=? char-ci>=? char-ci<? char-ci>?
					  boolean=? symbol=?))
				     h)))
	(lambda (caller head form env)
	  (let ((data (var-member head env)))
	    (if (and (len>1? (cdr form))
		     (any-procedure? head data))
		(check-unordered-exprs caller form (cdr form) env))

	    (if data
		(let ((fdata (cdr data)))
		  ;; a local var
		  (when (symbol? (let-ref fdata 'ftype))
		    (let ((args (let-ref fdata 'arglist))
			  (ary (let-ref fdata 'arit))
			  (sig (var-signature data)))
		      (when (pair? ary)
			(let ((opt (cdr ary))
			      (pargs (if (pair? args)
					 (proper-list args)
					 (if (symbol? args)
					     (list args)
					     ()))))
			  (let ((call-args (- (length form) 1))
				(req (car ary)))
			    (if (< call-args req)
				(begin
				  (for-each (lambda (p)
					      (if (pair? p)
						  (let ((v (var-member (car p) env)))
						    (if v
							(let ((vals (let-ref (cdr v) 'nvalues)))
							  (if (pair? vals)
							      (set! call-args (+ call-args -1 (cadr vals)))))))))
					    (cdr form))
				  (if (not (or (>= call-args req)
					       (tree-memq 'values (cdr form))
					       (tree-memq 'dilambda (let-ref fdata 'initial-value))))
				      (lint-format "~A needs ~D argument~A: ~A"
						   caller head
						   req (if (> req 1) "s" "")
						   (truncated-list->string form))))
				(if (> (- call-args (keywords (cdr form) 0)) opt) ; multiple-values can make this worse, (values)=nothing doesn't apply here
				    (lint-format "~A has too many arguments: ~A" caller head (truncated-list->string form)))))

			  (unless (let-ref fdata 'allow-other-keys)
			    (let ((last-was-key #f)
				  (have-keys 0)
				  (warned #f)
				  (rest (if (len>1? form) (cddr form) ())))
			      (for-each
			       (lambda (arg)
				 (if (and (keyword? arg)
					  (not last-was-key))  ; keyarg might have key value
				     (begin
				       (set! have-keys (+ have-keys 1))
				       (if (not (member (keyword->symbol arg) pargs
							(lambda (a b)
							  (eq? a (if (pair? b) (car b) b)))))
					   (lint-format "~A keyword argument ~A (in ~A) does not match any argument in ~S" caller
							head arg (truncated-list->string form) pargs))
				       (if (memq arg rest)
					   (lint-format "~W is repeated in ~A" caller arg (cdr form)))
				       (set! last-was-key #t))
				     (begin
				       (when (and (positive? have-keys)
						  (not last-was-key)
						  (not warned))
					 (set! warned #t)
					 (lint-format "non-keyword argument ~A follows previous keyword~P" caller arg have-keys))
				       (set! last-was-key #f)))
				 (if (pair? rest)
				     (set! rest (cdr rest))))
			       (cdr form))))

			  (check-args caller head data form (if (pair? sig) (cdr sig) ()) env opt)

			  ;; for a complete var-history, we could run through the args here even if no type info
			  ;; also if var passed to macro -- what to do?

			  ;; look for problematic macro expansion
			  (when (memq (let-ref fdata 'ftype) '(define-macro define-macro* defmacro defmacro*))

			    (unless (list? (let-ref fdata 'macro-ops))
			      (let ((syms (list () ())))
				(tree-symbol-walk ((if (memq (let-ref fdata 'ftype) '(define-macro define-macro*))
						       cddr cdddr)
						   (let-ref fdata 'initial-value))
						  syms)
				(varlet fdata 'macro-locals (car syms) 'macro-ops (cadr syms))))

			    (when (or (pair? (let-ref fdata 'macro-locals))
				      (pair? (let-ref fdata 'macro-ops)))
			      (let ((bad-locals ())
				    (bad-quoted-locals ()))
				(for-each
				 (lambda (local)
				   (if (tree-unquoted-member local (cdr form))
				       (set! bad-locals (cons local bad-locals))))
				 (let-ref fdata 'macro-locals))
				(when (null? bad-locals)
				  (for-each
				   (lambda (local)
				     (if (tree-member local (cdr form)) ; not tree-memq!
					 (set! bad-quoted-locals (cons local bad-quoted-locals))))
				   (let-ref fdata 'macro-locals)))
				(let ((bad-ops ()))
				  (for-each
				   (lambda (op)
				     (let ((curf (var-member op env))
					   (oldf (var-member op (let-ref fdata 'env))))
				       (if (and (not (eq? curf oldf))
						(or (pair? (let-ref fdata 'env))
						    (defined? op (rootlet))))
					   (set! bad-ops (cons op bad-ops)))))
				   (let-ref fdata 'macro-ops))

				  (if (equal? bad-quoted-locals '(quote)) (set! bad-quoted-locals ()))
				  (when (or (pair? bad-locals)
					    (pair? bad-quoted-locals)
					    ;; (define-macro (mac8 b) `(let ((a 12)) (+ (symbol->value ,b) a)))
					    ;; (let ((a 1)) (mac8 'a))
					    ;; far-fetched!  and can be confused by recursive macros (m1 in tmac.scm)
					    (pair? bad-ops))
				    (lint-format "possible problematic macro expansion:~%  ~A ~A collide with subsequently defined ~A~A~A"
						 caller
						 (truncated-list->string form)
						 (if (or (pair? bad-locals)
							 (pair? bad-ops))
						     "may"
						     "could conceivably")
						 (if (pair? bad-locals)
						     (format #f "~{'~A~^, ~}" bad-locals)
						     (if (pair? bad-quoted-locals)
							 (format #f "~{'~A~^, ~}" bad-quoted-locals)
							 ""))
						 (if (and (pair? bad-locals) (pair? bad-ops)) ", " "")
						 (if (pair? bad-ops)
						     (format #f "~{~A~^, ~}" bad-ops)
						     "")))))))
			  )))))
		;; not local var
		(when (symbol? head)
		  (let ((head-value (symbol->value head *e*))) ; head might be "arity"!
		    (when (or (procedure? head-value)
			      (macro? head-value))
		      ;; check arg number
		      (let ((ary (arity head-value)))
			(let ((args (- (length form) 1))
			      (min-arity (car ary))
			      (max-arity (cdr ary)))
			  (if (< args min-arity)
			      (lint-format "~A needs ~A~D argument~A: ~A"
					   caller head
					   (if (= min-arity max-arity) "" "at least ")
					   min-arity
					   (if (> min-arity 1) "s" "")
					   (truncated-list->string form))
			      (if (and (not (setter head-value))
				       (> (- args (keywords (cdr form) 0)) max-arity))
				  (lint-format "~A has too many arguments: ~A" caller head (truncated-list->string form))))

			  (when (and (procedure? head-value)
				     (pair? (cdr form))) ; there are args (the not-enough-args case is checked above)
			    (if (zero? max-arity)
				(lint-format "too many arguments: ~A" caller (truncated-list->string form))
				(begin

				  (for-each (lambda (arg)
					      (if (pair? arg)
						  (if (negative? (length arg))
						      (lint-format "missing quote? ~A in ~A" caller arg form)
						      (if (eq? (car arg) 'unquote)
							  (lint-format "stray comma? ~A in ~A" caller arg form)))))
					    (cdr form))

				  ;; if keywords, check that they are acceptable
				  ;;    this only applies to lambda*'s that have been previously loaded (lint doesn't create them)
				  (let ((source (procedure-source head-value)))
				    (if (and (pair? source)
					     (eq? (car source) 'lambda*))
					(let ((decls (cadr source)))
					  (if (not (memq :allow-other-keys decls))
					      (for-each
					       (lambda (arg)
						 (if (and (keyword? arg)
							  (not (eq? arg :rest))
							  (not (member arg decls
								       (lambda (a b)
									 (eq? (keyword->symbol a) (if (pair? b) (car b) b))))))
						     (lint-format "~A keyword argument ~A (in ~A) does not match any argument in ~S" caller
								  head arg (truncated-list->string form) decls)))
					       (cdr form))))))

				  ;; we've already checked for head in the current env above
				  (if (and (or (memq head '(eq? eqv?))
					       (and (= (length form) 3)
						    (hash-table-ref repeated-args-table head)))
					   (repeated-member? (cdr form) env))
				      (lint-format "this looks odd: ~A~A"
						   caller
						   ;; sigh (= a a) could be used to check for non-finite numbers, I suppose,
						   ;;   and (/ 0 0) might be deliberate (as in gmp)
						   ;;   but the former is better (not (nan? a)) and the latter is an error
						   ;;   also (min (random x) (random x)) is not pointless
						   (truncated-list->string form)
						   (if (eq? head '=)
						       (format #f ", perhaps use (not (nan? ~A))" (cadr form)) ; len=3 above, so only cadr is possible
						       (if (and (eq? head 'eq?)   ; (eq? x x)
								(eq? (cadr form) (caddr form)))
							   ", isn't it always #t?"
							   "")))
				      (if (and (hash-table-ref repeated-args-table-2 head)
					       (repeated-member? (cdr form) env))
					  (lint-format "it looks odd to have repeated arguments in ~A" caller (truncated-list->string form))))

				  (when (memq head '(eq? eqv?))
				    (define (repeated-member-with-not? lst env)
				      (and (pair? lst)
					   (let ((this-repeats (and (not (and (pair? (car lst))
									      (side-effect? (car lst) env)))
								    (or (member (list 'not (car lst)) (cdr lst))
									(and (len=2? (car lst))
									     (eq? (caar lst) 'not)
									     (member (cadar lst) (cdr lst)))))))
					     (or this-repeats
						 (repeated-member-with-not? (cdr lst) env)))))
				    (if (repeated-member-with-not? (cdr form) env)
					(lint-format "this looks odd: ~A" caller (truncated-list->string form))))

				  ;; now try to check arg types
				  (let ((arg-data (cond ((signature head-value) => cdr) (else #f))))
				    (if (pair? arg-data)
					(check-args caller head data form arg-data env max-arity))
				    ))))))))))))))

    (define (indirect-set? vname func arg1)
      (case func
	((set-car! set-cdr! vector-set! list-set! string-set!)
	 (eq? arg1 vname))
	((set! implicit-set)
	 (and (pair? arg1)
	      (eq? (car arg1) vname)))
	(else #f)))

    (define (env-difference name e1 e2 lst)
      (if (or (null? e1)
	      (null? e2)
	      (eq? (car e1) (car e2)))
	  (reverse lst)
	  (env-difference name (cdr e1) e2
			  (if (eq? name (var-name (car e1)))
			      lst
			      (cons (car e1) lst)))))

    (denote report-usage
      (let ((unwrap-cxr (hash-table 'caar '(car)  'cadr '(cdr)  'cddr '(cdr)  'cdar '(car)
				     'caaar '(caar car)  'caadr '(cadr cdr)  'caddr '(cddr cdr)  'cdddr '(cddr cdr)
				     'cdaar '(caar car)  'cddar '(cdar car)  'cadar '(cadr car)  'cdadr '(cadr cdr)
				     'cadddr '(cdddr cddr cdr)  'cddddr '(cdddr cddr cdr)  'caaaar '(caaar caar car)  'caaadr '(caadr cadr cdr)
				     'caadar '(cadar cdar car)  'caaddr '(caddr cddr cdr)  'cadaar '(cdaar caar car)  'cadadr '(cdadr cadr cdr)
				     'caddar '(cddar cdar car)  'cdaaar '(caaar caar car)  'cdaadr '(caadr cadr cdr)  'cdadar '(cadar cdar car)
				     'cdaddr '(caddr cddr cdr)  'cddaar '(cdaar caar car)  'cddadr '(cdadr cadr cdr)  'cdddar '(cddar cdar car)))
	    (all-types-agree (lambda (v)
			       (let ((base-type (->lint-type (var-initial-value v)))
				     (vname (var-name v)))
				 (let ((typef (lambda (p)
						(or (not (and (len>2? p)
							      (eq? (car p)  'set!)
							      (eq? vname (cadr p))))
						    (let ((nt (->lint-type (caddr p))))
						      (or (subsumes? base-type nt)
							  (and (subsumes? nt base-type)
							       (set! base-type nt))
							  (and (memq nt '(pair? null? proper-list?))
							       (memq base-type '(pair? null? proper-list?))
							       (set! base-type 'list?))))))))
				   (and (lint-every? typef (var-history v))
					base-type))))))

	;; -------- defined-twice
	(define (defined-twice caller head vars)
	  (when (and (not (eq? head 'begin)) ; begin can redefine = set a variable
		     (proper-pair? vars))
	    (do ((cur vars (cdr cur))
		 (rst (cdr vars) (cdr rst)))
		((null? rst))
	      (let ((vn (var-name (car cur))))
		(if (not (memq vn '(:lambda :dilambda)))
		    (let ((repeat (var-member vn rst)))
		      (when repeat
			(let ((type (if (eq? (var-definer repeat) 'parameter) 'parameter 'variable)))
			  (case (var-definer (car cur))
			    ((define)
			     (lint-format "~A ~A ~A is redefined ~A" caller head type vn
					  (if (equal? head "")
					      (if (not (tree-memq vn (var-initial-value (car cur))))
						  "at the top level."
						  (format #f "at the top level. Perhaps use set! instead: ~A"
							  (truncated-list->string (list 'set! vn (var-initial-value (car cur))))))
					      (format #f "in the ~A body.  Perhaps use set! instead: ~A"
						      head (truncated-list->string (list 'set! vn (var-initial-value (car cur))))))))
			    ((define-constant)
			     (lint-format "~A ~A ~A is later redefined as a constant" caller head type vn))
			    (else
			     (lint-format "~A ~A ~A is declared twice" caller head type vn)))))))))))

	;; -------- set->let
	(define (set->let caller outer-form local-var env)
	  ;; (let ((x 0)...) ... (set! x 1)...) -> move the set! value  to let init value
	  ;; car body as set! is handled in let-walker etc
	  (when (and (pair? outer-form)
		     (positive? (var-set local-var))
		     (memq (car outer-form) '(let let*))
		     (list? (cadr outer-form))
		     (not (side-effect? (var-initial-value local-var) env)))
	    (let ((nxt (let ((len (length (var-history local-var))))
			 (and (> len 1)
			      (list-ref (var-history local-var) (- len 2)))))
		  (vname (var-name local-var)))
	      (when (and (pair? nxt)
			 (eq? (car nxt) 'set!)
			 (eq? (cadr nxt) vname)
			 (code-constant? (caddr nxt)) ; so vname is not involved etc
			 (not (tree-memq vname (caddr outer-form))) ; not redundant with next -- need to exclude this case
			 (let ((f (member vname (cdddr outer-form) tree-memq)))
			   (and (pair? f)
				(eq? (car f) nxt))))
		(lint-format "perhaps change ~A's initial value to ~A, and remove ~A in ~A" caller
			     vname (caddr nxt) nxt (truncated-list->string outer-form))))))

	;; -------- use-dilambda
	(denote (use-dilambda caller local-var vars env)
	  ;; translate to dilambda fixing arg if necessary and mention generic set!
	  (let ((init (var-initial-value local-var))
		(vname (var-name local-var)))
	    (when (and (len>1? init)
		       (eq? (car init) 'define)
		       (pair? (cadr init)))
	      (let* ((vstr (symbol->string vname))
		     (len (length vstr)))
		(when (> len 4)
		  (let ((setv #f)
			(newv #f))
		    (if (string=? (substring vstr 0 4) "get-")
			(let ((sv (symbol "set-" (substring vstr 4))))
			  (set! setv (or (var-member sv vars)
					 (var-member sv env)))
			  (set! newv (string->symbol (substring vstr 4))))
			(if (string=? (substring vstr (- len 4)) "-ref")
			    (let ((sv (symbol (substring vstr 0 (- len 4)) "-set!")))
			      (set! setv (or (var-member sv vars)
					     (var-member sv env)))
			      (set! newv (string->symbol (substring vstr 0 (- len 4)))))
			    (let ((pos (string-position "-get-" vstr)))
			      (when pos ; this doesn't happen very often, others: Get-, -ref-, -set!- are very rare
				(let ((sv (let ((s (copy vstr))) (set! (s (+ pos 1)) #\s) (string->symbol s))))
				  (set! setv (or (var-member sv vars)
						 (var-member sv env)))
				  (set! newv (symbol (substring vstr 0 pos)
						     (substring vstr (+ pos 4))))))))) ; +4 to include #\-
		    (when (and setv
			       (not (var-member newv vars))
			       (not (var-member newv env)))
		      (let ((getter init)
			    (setter (var-initial-value setv)))
			(when (and (pair? setter)
				   (eq? (car setter) 'define)
				   (pair? (cadr setter)))
			  (let ((getargs (cdadr getter))
				(setargs (cdadr setter)))
			    (unless (null? setargs)
			      (if (or (eq? newv getargs)
				      (and (pair? getargs)
					   (memq newv getargs)))
				  (let ((unique (find-unique-name getter newv)))
				    (set! getter (tree-subst unique newv getter))
				    (set! getargs (cdadr getter))))
			      (if (or (eq? newv setargs)
				      (and (pair? setargs)
					   (memq newv setargs)))
				  (let ((unique (find-unique-name setter newv)))
				    (set! setter (tree-subst unique newv setter))
				    (set! setargs (cdadr setter))))
			      (let ((getdots (if (null? getargs) "" " ..."))
				    (setdots (if (or (not (pair? setargs)) (null? (cdr setargs))) "" " ..."))
				    (setvalue (and (proper-list? setargs)
						   (last-ref setargs))))
				(if setvalue
				    (out-format outport "~NC~A: perhaps use dilambda and generalized set! for ~A and ~A:~%~
                                                  ~NCreplace (~A~A) with (~A~A) and (~A~A ~A) with (set! (~A~A) ~A)~%~
                                                  ~NC~A~%"
					    lint-left-margin #\space
					    caller
					    vname (var-name setv)
					    (+ lint-left-margin 4) #\space
					    vname getdots newv getdots
					    (var-name setv) setdots setvalue
					    newv setdots setvalue
					    (+ lint-left-margin 4) #\space
					    (lint-pp `(define ,newv (dilambda
								     (lambda ,getargs ,@(cddr getter))
								     (lambda ,setargs ,@(cddr setter)))))))))))))))))))

	;; -------- bad-name
	(denote (bad-name caller head local-var otype)
	  (let ((vname (var-name local-var)))
	    (cond ((hash-table-ref syntaces vname)
		   (lint-format "~A ~A named ~A is asking for trouble" caller head otype vname))

		  ((eq? vname 'l)
		   (lint-format "\"l\" is a really bad variable name" caller))

		  ((and *report-built-in-functions-used-as-variables*
			(hash-table-ref built-in-functions vname))
		   (lint-format "~A ~A named ~A is asking for trouble" caller
				(if (and (len=1? (var-scope local-var))
					 (symbol? (car (var-scope local-var))))
				    (car (var-scope local-var))
				    head)
				otype vname))

		  ((symbol? vname)
		   (check-for-bad-variable-name caller vname)))))

	;; -------- wrappable-var
	(define (wrappable-var caller local-var otype outer-form env)
	  (let ((hist (var-history local-var))
		(vname (var-name local-var)))
	    (let ((first (car hist)))                               ; all but the initial binding have to match this
	      (when (pair? first)
		(let ((op (car first)))
		  (when (and (symbol? op)
			     (not (or (memq op '(unquote apply-values list-values))
				      (hash-table-ref makers op)
				      (eq? vname op)))              ; not a function (this kind if repetition is handled elsewhere)
			     (len>1? (cdr hist))
			     (pair? (cdr first))
			     (not (side-effect? first env))
			     (lint-every? (lambda (a)
					    (or (eq? a vname)
						(code-constant? a)))
					  (cdr first))
			     (or (code-constant? (var-initial-value local-var))
				 (= (tree-count vname first 2) 1))
			     (lint-every? (lambda (a)
					    (and (pair? a)
						 (or (equal? first a)
						     (and (eq? (hash-table-ref reversibles (car first)) (car a))
							  (equal? (cdr first) (reverse (cdr a))))
						     (set! op (match-cxr op (car a))))))
					  (if (eq? otype 'parameter)
					      (cdr hist)
					      (copy (cdr hist) (make-list (- (length hist) 2))))))
		    (let* ((new-op (or op (car first)))
			   (settee (let walker ((tree outer-form)) ; check for new-op dilambda as target of set!
					 (and (pair? tree)
					      (or (and (eq? (car tree) 'set!)
						       (pair? (cdr tree))
						       (pair? (cadr tree))
						       (eq? (caadr tree) new-op))
						  (walker (car tree))
						  (walker (cdr tree)))))))
		      (unless settee
			(if (eq? otype 'parameter)
			    (if (> (var-ref local-var) 2)
				(lint-format "parameter ~A is always accessed (~A times) via ~S" caller
					     vname (var-ref local-var) (cons new-op (cdr first))))
			    (lint-format "~A is not set, and is always accessed via ~A~%~NCso its binding could probably be ~A in ~A" caller
					 ;;        "probably" here because the accesses could have hidden protective assumptions
					 ;;          i.e. full accessor is not valid at point of let binding
					 vname
					 (cons new-op (cdr first))
					 (+ lint-left-margin 4) #\space
					 (list vname (cons new-op (tree-subst (var-initial-value local-var) vname (cdr first))))
					 (truncated-list->string outer-form)))))))))))

	;; -------- use-vector
	(define (use-vector caller local-var)
	  (let ((hist (var-history local-var)))
	    (when (> (length hist) 2) ; an experiment -- if all refs are by list-ref (in effect) suggest a vector
	      (let ((init (var-initial-value local-var))
		    (vname (var-name local-var)))
		(when (and (pair? init)
			   ;; list->vector
			   (or (memq (car init) '(list make-list string->list vector->list))
			       (and (eq? (car init) 'quote)
				    (pair? (cdr init))
				    (pair? (cadr init))))
			   (lint-every? (lambda (p)
					  (and (pair? p)
					       (or (eq? p init)
						   (eq? (car p) vname)
						   (hash-table-ref cxars (car p))
						   (memq (car p) '(list-ref list-set! length reverse map for-each
									    list->vector list->string list? pair? null? quote)))))
					hist))
		  (lint-format "~A could be a vector, rather than a list" caller vname))))))

	;; -------- parlous-port
	(denote (parlous-port caller local-var outer-form)
	  ;; look for port opened but not closed, or not used
	  ;;    (let ((p (open-output-file str))) (display 32 p) x)
	  (when (and (pair? outer-form)
		     (not (memq (var-definer local-var) '(call-with-input-string call-with-input-file call-with-output-string call-with-output-file)))
		     ;; call-with-io-walker below uses open-input-string et al for the initial value to get type checks
		     (let ((last (last-ref outer-form)))
		       (or (not (tree-memq (var-name local-var) last))
			   (and (pair? last)
				(memq (car last) '(close-input-port close-output-port close-port close))))))
	    (let ((hist (var-history local-var))
		  (open-set '(open-input-string open-input-file open-output-string open-output-file))
		  (open-form #f)
		  (vname (var-name local-var)))
	      (when (lint-any? (lambda (tree)
				 (and (pair? tree)
				      (or (and (memq (car tree) open-set)
					       (not (and (pair? (cdr tree))
							 (memq vname (cdr tree)))))
					  (and (eq? (car tree) 'set!)
					       (len>1? (cdr tree))
					       (eq? (cadr tree) vname)
					       (pair? (caddr tree))
					       (memq (caaddr tree) open-set)))
				      (set! open-form tree)))
			       hist)
		(if (not (tree-set-memq '(close-input-port close-output-port close-port close current-output-port current-input-port) hist))
		    (lint-format "in ~A~%     perhaps ~A is opened via ~A, but never closed" caller
				 (truncated-list->string outer-form)
				 vname open-form)
		    (if (= (length hist) 2)
			(lint-format "in ~A~%   ~A is opened and closed, but never used" caller
				     (truncated-list->string outer-form) vname)))))))

	;; -------- parlous-output-string
	(define (parlous-output-string local-var outer-form)
	  (let ((init (var-initial-value local-var))) ; look for various open-output-string problems
	    (when (and (pair? init)
		       (eq? (car init) 'open-output-string)
		       (pair? outer-form)
		       (eq? (car outer-form) 'let)
		       (len=1? (cadr outer-form)))
	      (let ((var (caadr outer-form))
		    (vname (var-name local-var)))
		(when (and (eq? (car var) vname)
			   (pair? (cadr var))
			   (eq? (caadr var) 'open-output-string))
		  (let ((hist (var-history local-var)))
		    (if (and (pair? (car hist))
			     (eq? (caar hist) 'close-output-port)
			     (eq? vname (cadar hist))
			     (not (tree-memq 'get-output-string hist))
			     (= (length hist) 3)
			     (pair? (cadr hist))
			     (memq (caadr hist) '(display write)))
			(lint-format "~A is missing get-output-string" vname (truncated-list->string outer-form))
			(when (len>1? (cddr outer-form))
			  (let ((mid (cadddr outer-form)))
			    (when (and (pair? mid)
				       (eq? (car mid) 'let)
				       (= (length hist) 4))
			      (let ((writer (caddr hist)))
				(when (and (pair? writer)
					   (memq (car writer) '(write display))
					   (pair? (cddr writer))
					   (eq? (caddr writer) vname)
					   (equal? writer (caddr outer-form)))  ; all this is sloppy -- maybe not worth this effort
				  (lint-format "perhaps ~A" vname
					       (lists->string outer-form
							      (cons 'object->string
								    (if (eq? (car writer) 'display)
									(list (cadr writer) #f)
									(list (cadr writer))))))))))))))))))

	;; -------- reducible-scope
	(denote (reducible-scope caller local-var otype env)
	  (if (and (eq? otype 'variable)
		   (or *report-unused-top-level-functions*
		       (not (eq? caller top-level:))))
	      (let ((scope (var-scope local-var)) ; might be #<undefined>?
		    (vname (var-name local-var)))
		(if (pair? scope) (set! scope (remove-one vname scope)))
		(when (and (len=1? scope)
			   (symbol? (car scope))
			   (not (var-member (car scope) (let search ((e env))
							  (if (null? e)
							      env
							      (if (eq? (caar e) vname)
								  e
								  (search (cdr e)))))))
			   (not (and (memq (var-ftype local-var) '(define lambda define* lambda*))
				     (> (tree-leaves (var-initial-value local-var)) 80))))
		  (out-format outport "~NC~A~A is ~A only in ~A~%"
			  lint-left-margin #\space
			  (if (eq? caller top-level:)
			      "top-level: "
			      "")
			  vname
			  (if (memq (var-ftype local-var) '(define lambda define* lambda*)) "called" "used")
			  (car scope))))))

	;; -------- unused-var
	;;   (eval ...) in the form may have hidden var refs confusing this check
	(define (unused-var caller head local-var otype)
	  (let ((vname (var-name local-var)))
	    (when (and (or (not (equal? head ""))
			   *report-unused-top-level-functions*)
		       (or *report-unused-parameters*
			   (not (eq? otype 'parameter))))
	      (if (positive? (var-set local-var))
		  (let ((sets (map (lambda (call)
				     (if (and (pair? call)
					      (not (eq? (var-definer local-var) 'do))
					      (eq? (car call) 'set!)
					      (eq? (cadr call) vname))
					 call
					 (values)))
				   (var-history local-var))))
		    (if (pair? sets)
			(if (null? (cdr sets))
			    (lint-format "~A set, but not used: ~A" caller
					 vname (truncated-list->string (car sets)))
			    (lint-format "~A set, but not used: ~{~S~^ ~}" caller
					 vname sets))
			(lint-format "~A set, but not used: ~A from ~A" caller
				     vname (truncated-list->string (var-initial-value local-var)) (var-definer local-var))))

		  ;; not ref'd or set
		  (unless (memq vname '(+documentation+ +signature+ +setter+ +iterator+ define-animal))
		    (let ((val (truncated-list->string
				(if (pair? (var-history local-var))
				    (car (var-history local-var))
				    (var-initial-value local-var))))
			  (def (var-definer local-var)))
		      (let-temporarily ((line-number (if (eq? caller top-level:) -1 line-number)))
			;; eval confuses this message (eval '(+ x 1)), no other use of x [perhaps check :let initial-value = outer-form]
			;;    so does let-ref syntax: (apply (*e* 'g1)...) will miss this reference to g1
			(if (symbol? def)
			    (if (eq? otype 'parameter)
				(lint-format "~A not used" caller vname)
				(lint-format "~A not used, initially: ~A from ~A" caller vname val def))
			    (lint-format "~A not used, value: ~A" caller vname val)))))))))

	;; -------- move local var inward
	(define (move-var-inward caller local-var)
	  (when (and (not (eq? caller top-level:))         ; don't move top-level vars
		     (zero? (var-set local-var)))          ; counter in for-each, etc
	    (let ((func (memq (var-ftype local-var) '(define define* lambda lambda*)))
		  (deffunc (memq (var-ftype local-var) '(define define*)))
		  (local-env (var-env local-var))
		  (reflet (var-refenv local-var))
		  (deflet (var-env local-var))
		  (source (var-initial-value local-var))
		  (vname (var-name local-var)))
	      ;; if (define f (let ... (lambda ...))) the f is not in the deflet yet

	      (if (and deffunc
		       (pair? local-env))
		  (let crawler ((ref (cdr local-env)))
		    (if (pair? ref)
			(if (keyword? (caar ref))
			    (set! local-env ref)
			    (crawler (cdr ref))))))

	      (when (and (pair? reflet)
			 (pair? deflet)
			 (not (eq? local-env reflet))
			 (or deffunc
			     (code-constant? source))
			 ;; code-constant? is very restrictive, but side-effect? leaves too many complications:
			 ;;   (let ((a (car b))) (set! b c) (let ...))
			 (let ((target (car deflet)))     ; the enclosing env
			   (let crawler ((ref reflet))    ; the var's restricted env
			     (and (pair? ref)
				  (or (eq? (car ref) target)
				      (and (not (eq? (caar ref) :do))     ; try not to move the variable inside a loop
					   (not (and (eq? (caar ref) :let)
						     (symbol? (cadr (var-initial-value (car ref))))))
					   (crawler (cdr ref)))))))
			 (or (not func)
			     (let* ((source-args (args->proper-list ((if deffunc cdadr cadr) source)))
				    (lets (if deffunc 1 0))
				    (let-args (let ((target (car deflet)))
						(let crawler ((ref reflet) (largs ()))
						  ;; this includes locals that can't act as shadows
						  (if (eq? (caar ref) :let)
						      (set! lets (+ lets 1)))
						  (if (eq? (car ref) target)
						      largs
						      (crawler (cdr ref)
							       (if (and (pair? (car ref))
									(not (keyword? (caar ref)))
									(not (eq? (caar ref) vname)))
								   (cons (caar ref) largs)
								   largs)))))))
			       (and (> lets 2)
				    (not (tree-set-memq (let remove-args ((args let-args) (nargs ()))
							    (if (null? args)
								nargs
								(remove-args (cdr args)
									     (if (memq (car args) source-args)
										 nargs
										 (cons (car args) nargs)))))
							  (cddr source)))))))
		;; possibilities are :let :catch :call/exit :call/cc :do :with-let :with-baffle

		(let ((refval (var-initial-value (car reflet)))
		      (makval (var-initial-value (car local-env))))
		  (when (and (pair? refval)
			     (eq? (car refval) 'let)
			     (pair? (cadr refval))
			     (pair? makval)
			     (or (memq (car makval) '(define define* lambda lambda*))
				 (and (memq (car makval) '(let let*))  ; check last var in let*, any var in let
				      (pair? (cadr makval))            ; not a named let
				      (or (eq? (caddr makval) refval)      ;   not a triply nested let
					  (not (memq (caaddr makval) '(let let*))))
				      (not (and (pair? (caddr makval)) ; not lambda's closure
						(null? (cdddr makval))
						(memq (caaddr makval) '(lambda lambda*)))))))
		    (lint-format "perhaps move '~A~A into the inner let~A: ~A" caller
				 vname
				 (if (or deffunc
					 (not (memq (car makval) '(define define*))))
				     ""
				     (local-line-number  (if (pair? (cadr makval))
							     ((if (pair? (caadr makval)) caadr cadr) makval)
							     makval)))
				 (local-line-number (caadr refval))
				 (truncated-lists->string
				  makval
				  (let ((new-var&val (list vname
							   (if (not deffunc)
							       source
							       (cons (if (eq? (var-ftype local-var) 'define) 'lambda 'lambda*)
								     (cons (cdadr source)
									   (cddr source)))))))
				    `(let (,new-var&val
					   ,@(cadr refval))
				       ,@(cddr refval)))))))))))

	;; -------- parlous-return
	(denote (parlous-return caller local-var env)
	  (when (and *report-clobbered-function-return-value*
		     (positive? (var-set local-var)))
	    (let ((start (var-initial-value local-var))
		  (vname (var-name local-var)))
	      (let ((func #f)
		    (retcons? (and (pair? start) ; is this var's initial value from a function that returns a constant sequence?
				   (let ((v (var-member (car start) env)))
				     (and v
					  (eq? (var-retcons v) #t))))))
		(for-each (lambda (f)
			    (when (pair? f)
			      (case (car f)
				((set!)
				 (set! retcons? (and (len>1? (cdr f))
						     (eq? (cadr f) vname)
						     (pair? (caddr f))
						     (let ((v (var-member (caaddr f) env)))
						       (and v
							    (eq? #t (var-retcons v))
							    (set! func f))))))
				((string-set! list-set! vector-set! set-car! set-cdr!)
				 (if (and retcons?
					  (eq? (cadr f) vname))
				     (lint-format "~A returns a constant sequence, but ~A appears to clobber it" caller
						  func f))))))
			  (reverse (var-history local-var)))))))

	;; pointless-if
	(define (pointless-if caller vname vtype func call)
	  (when (and (not (eq? caller top-level:))
		     (not (memq vtype '(boolean? #t values)))
		     (memq func '(if when unless)) ; look for (if x ...) where x is never #f, this happens a dozen or so times
		     (or (eq? (cadr call) vname)
			 (and (pair? (cadr call))
			      (eq? (caadr call) 'not)
			      (eq? (cadadr call) vname))))
	    (lint-format "~A is never #f, so ~A" caller
			 vname
			 (lists->string
			  call
			  (if (eq? vname (cadr call))
			      (case func
				((if) (caddr call))
				((when) (if (pair? (cdddr call)) (cons 'begin (cddr call)) (caddr call)))
				((unless) #<unspecified>)
				(else 'error))
			      (case func
				((if) (if (pair? (cdddr call)) (cadddr call)))
				((when) #<unspecified>)
				((unless) (if (pair? (cdddr call)) (cons 'begin (cddr call)) (caddr call)))
				(else 'error)))))))

	;; -------- arg-mismatch
	(define (arg-mismatch caller vname vtype func call e)
	  (let ((p (memq vname (cdr call))))
	    (when (pair? p)
	      (let ((sig (arg-signature func e))
		    (pos (- (length call) (length p))))
		(when (and (pair? sig)
			   (< pos (length sig)))
		  (let ((desired-type (list-ref sig pos)))
		    (cond ((not (or (symbol? desired-type)
				    (eq? desired-type #t)
				    (pair? desired-type)))
			   (error 'wrong-type-arg "~S signature ~S is invalid" func sig))
			  
			  ((not (or (eq? desired-type #t)
				    (any-compatible? vtype desired-type)))
			   (lint-format "~A is ~A, but ~A in ~A wants ~A" caller
					vname (prettify-checker-unq vtype)
					func (truncated-list->string call)
					(prettify-checker desired-type)))
			  
			  ((and (memq vtype '(float-vector? int-vector?))
				(memq func '(vector-set! vector-ref)))
			   (lint-format "~A is ~A, so perhaps use ~A, not ~A" caller
					vname (prettify-checker-unq vtype)
					(if (eq? vtype 'float-vector?)
					    (if (eq? func 'vector-set!) 'float-vector-set! 'float-vector-ref)
					    (if (eq? func 'vector-set!) 'int-vector-set! 'int-vector-ref))
					func))
			  
			  ((and (eq? vtype 'float-vector?)
				(eq? func 'equal?)
				(or (eq? (cadr call) vname)
				    (not (symbol? (cadr call))))) ; don't repeat the suggestion when we hit the second vector
			   (lint-format "perhaps use equivalent? in ~A" caller (truncated-list->string call)))
			  
			  ((and (eq? vtype 'vector?)
				(memq func '(float-vector-set! float-vector-ref int-vector-set! int-vector-ref byte-vector-set! byte-vector-ref)))
			   (lint-format "~A is ~A, so use ~A, not ~A" caller
					vname (prettify-checker-unq vtype)
					(if (memq func '(float-vector-set! int-vector-set! byte-vector-set!))
					    'vector-set! 'vector-ref)
					func)))))))))

	;; -------- pointless-type-check
	(define (pointless-type-check caller local-var vtype func call call-arg1 vars)
	  (let ((vname (var-name local-var)))
	    (when (and (hash-table-ref bools func)
		       (not (eq? vname func)))
	      (when (or (eq? vtype func)
			(and (compatible? vtype func)
			     (not (subsumes? vtype func))
			     (not (eq? vtype 'proper-list?))))
		(lint-format "~A is ~A, so ~A is #t" caller vname (prettify-checker-unq vtype) call))

	      (unless (compatible? vtype func)
		(lint-format "~A is ~A, so ~A is #f" caller vname (prettify-checker-unq vtype) call)))

	    (case func
	      ;; need a way to mark exported variables so they won't be checked in this process
	      ;; case can happen here, but it never seems to trigger a type error
	      ((eq? eqv? equal?)
	       ;; (and (pair? x) (eq? x #\a)) etc
	       (when (or (and (code-constant? call-arg1)
			      (not (compatible? vtype (->lint-type call-arg1))))
			 (and (pair? (cddr call))
			      (code-constant? (caddr call))
			      (not (compatible? vtype (->lint-type (caddr call))))))
		 (lint-format "~A is ~A, so ~A is #f" caller vname (prettify-checker-unq vtype) call)))

	      ((and or)
	       (when (let amidst? ((lst call))
		       (and (len>1? lst)
			    (or (eq? (car lst) vname)
				(amidst? (cdr lst)))))   ; don't clobber possible trailing vname (returned by expression)
		 (lint-format "~A is ~A, so ~A" caller   ; (let ((x 1)) (and x (< x 1))) -> (< x 1)
			      vname (prettify-checker-unq vtype)
			      (lists->string call
					     (simplify-boolean (remove-one vname call) () () vars)))))
	      ((not)
	       (if (eq? vname (cadr call))
		   (lint-format "~A is ~A, so ~A" caller
				vname (prettify-checker-unq vtype)
				(lists->string call #f))))

	      ((/) (if (and (number? (var-initial-value local-var))
			    (zero? (var-initial-value local-var))
			    (zero? (var-set local-var))
			    (memq vname (cddr call)))
		       (lint-format "~A is ~A, so ~A is an error" caller
				    vname (var-initial-value local-var)
				    call))))))

	;; -------- eqx-type-check
	(define (eqx-type-check caller local-var vtype func call call-arg1)
	  (let ((vname (var-name local-var)))
	    (if (memq func '(eq? equal?))
		(lint-format "~A is ~A, so ~A ~A be eqv? in ~A" caller
			     vname (prettify-checker-unq vtype) func
			     (if (eq? func 'eq?) "should" "could")
			     call))
	    ;; check other boolean exprs
	    (when (and (zero? (var-set local-var))
		       (number? (var-initial-value local-var))
		       (eq? vname call-arg1)
		       (null? (cddr call))
		       (hash-table-ref booleans func))
	      (let ((val (catch #t
			   (lambda ()
			     ((symbol->value func (rootlet)) (var-initial-value local-var)))
			   (lambda args
			     'error))))
		(if (boolean? val)
		    (lint-format "~A is ~A, so ~A is ~A" caller vname (var-initial-value local-var) call val))))))

	;; -------- implicit-type-checks
	(define (implicit-type-checks caller local-var vtype func call call-arg1)
	  ;; these type checks are easily fooled by macros
	  (when (and (memq vtype '(vector? float-vector? int-vector? string? list? byte-vector?))
		     (pair? (cdr call)))
	    (let ((vname (var-name local-var)))
	      (when (eq? func vname)
		(let ((init (var-initial-value local-var)))
		  (if (not (compatible? 'integer? (->lint-type call-arg1)))
		      (lint-format "~A is ~A, but the index ~A is ~A" caller
				   vname (prettify-checker-unq vtype)
				   call-arg1 (prettify-checker (->lint-type call-arg1))))

		  (if (integer? call-arg1)
		      (if (negative? call-arg1)
			  (lint-format "~A's index ~A is negative" caller vname call-arg1)
			  (if (zero? (var-set local-var))
			      (let ((lim (cond ((code-constant? init)
						(length init))

					       ((memq (car init) '(vector float-vector int-vector string list byte-vector))
						(- (length init) 1))

					       (else
						(and (pair? (cdr init))
						     (integer? (cadr init))
						     (memq (car init) '(make-vector make-float-vector make-int-vector
										    make-string make-list make-byte-vector))
						     (cadr init))))))
				(if (and (real? lim)
					 (>= call-arg1 lim))
				    (lint-format "~A has length ~A, but index is ~A" caller vname lim call-arg1))))))))

	      (when (eq? func 'implicit-set)
		;; ref is already checked in other history entries
		(let ((ref-type (case vtype
				  ((float-vector?) 'real?) ; not 'float? because ints are ok here
				  ((int-vector? byte-vector?) 'integer)
				  ((string?) 'char?)
				  (else #f))))
		  (if ref-type
		      (let ((val-type (->lint-type (caddr call))))
			(if (not (compatible? val-type ref-type))
			    (lint-format "~A wants ~A, but the value in ~A is ~A" caller
					 vname (prettify-checker-unq ref-type)
					 (cons 'set! (cdr call))
					 (prettify-checker val-type))))))))))

	;; -------- duplicated-calls
	(define (duplicated-calls caller local-var env)
	  (let ((vname (var-name local-var)))
	    (when (and (> (var-ref local-var) 8)
		       (zero? (var-set local-var))
		       (eq? (var-ftype local-var) #<undefined>))
	      (let ((h (make-hash-table)))
		(for-each (lambda (call)
			    (when (and (pair? call)
				       (not (eq? (car call) vname)) ; ignore functions for now
				       (not (side-effect? call env)))
			      (hash-table-set! h call (+ 1 (or (hash-table-ref h call) 0)))
			      (cond ((hash-table-ref unwrap-cxr (car call))
				     => (lambda (lst)
					  (for-each (lambda (c)
						      (let ((cc (cons c (cdr call))))
							(hash-table-set! h cc (+ 1 (or (hash-table-ref h cc) 0)))))
						    lst))))))
			  (var-history local-var))
		(let ((intro #f)
		      (column 0)
		      (calls 0))
		  (for-each (lambda (call)
			      (when (and (or (and (eq? (caar call) 'length)
						  (> (cdr call) 3))
					     (and (not (memq (caar call) '(make-vector make-float-vector)))
						  (> (cdr call) (max 3 (/ 20 (tree-leaves (car call))))))) ; was 5
					 (or (null? (cddar call))
					     (lint-every? (lambda (p)               ; make sure only current var is involved
							    (or (not (symbol? p))
								(eq? p vname)))
							  (cdar call))))
				(unless intro
				  (let ((str (format #f "~NC~A: ~A is not set, but "
						     lint-left-margin #\space
						     caller vname)))
				    (set! column (length str))
				    (display str outport))
				  (set! intro #t))
				(let ((str (truncated-list->string (car call))))
				  (if (> (+ column (length str) 12) 100)
				      (begin
					(out-format outport "~%~NC" (+ lint-left-margin 4) #\space)
					(set! column lint-left-margin)
					(set! calls 1))
				      (set! calls (+ calls 1)))
				  (set! column (+ column (length str) 12))
				  (out-format outport "~A~A occurs ~A times"
					  (if (> calls 1) ", " "")
					  str (cdr call)))))
			    h)
		  (if intro (newline outport)))))))

	;; -------- repeated-args
	(define (repeated-args caller local-var)
	  ;; check for function parameters whose values never change and are not just symbols
	  ;;   ignore recursive functions (arg=(+ k 1) -> counter or whatever)
	  (let ((vname (var-name local-var)))
	    (when (and (> (var-ref local-var) 1) ; was 3
		       (zero? (var-set local-var))
		       (memq (var-ftype local-var) '(define lambda))
		       (pair? (var-arglist local-var))
		       (not (tree-memq vname (cddr (var-initial-value local-var)))) ; not recursive func
		       (let loop ((calls (var-history local-var)))          ; if func passed as arg, ignore it
			 (or (null? calls)
			     (null? (cdr calls))
			     (and (pair? (car calls))
				  (not (memq vname (cdar calls)))
				  (loop (cdr calls))))))
	      (let ((pars (map list (proper-list (var-arglist local-var)))))
		(do ((clauses (var-history local-var) (cdr clauses)))
		    ((null? (cdr clauses)))                                 ; ignore the initial value
		  (if (and (pair? (car clauses))
			   (eq? (caar clauses) vname))
		      (for-each (lambda (arg par)                           ; collect all arguments for each parameter
				  (if (not (member arg (cdr par)))          ; we haven't seen this argument yet, so
				      (set-cdr! par (cons arg (cdr par))))) ;   add it to the list for this parameter
				(cdar clauses)
				pars)))
		(for-each (lambda (p)
			    (when (and (pair? (cdr p))
				       (not (symbol? (cadr p))))
			      (if (and (null? (cddr p))
				       (code-constant? (cadr p)))
				  (lint-format "~A's '~A parameter is always ~S (~D calls)" caller
					       vname (car p) (cadr p) (var-ref local-var)))
			      (when (and (pair? (cadr p))
					 (eq? (caadr p) 'lambda))
				(let ((pars (cadadr p))
				      (body (cddadr p)))
				  (when (proper-list? pars)
				    (let ((unused (do ((ps pars (cdr ps))
						       (i 0 (+ i 1))
						       (res ()))
						      ((null? ps)
						       res)
						    (if (not (tree-memq (car ps) body))
							(set! res (cons i res))))))
				      (if (and (pair? unused)
					       (or (null? (cddr p))
						   (lint-every? (lambda (arg)
								  (and (pair? arg)
								       (eq? (car arg) 'lambda)
								       (proper-list? (cadr arg))
								       (let ((new-unused (copy unused)))
									 (for-each (lambda (parnum)
										     (let ((par-name (list-ref (cadr arg) parnum)))
										       (if (tree-memq par-name (cddr arg))
											   (set! new-unused (remove-one parnum new-unused)))))
										   unused)
									 (and (pair? new-unused)
									      (set! unused new-unused)))))
								(cddr p))))
					  (lint-format "~A parameter ~A is a function whose parameter~P ~{~A~^, ~} ~A never used" caller
						       vname (car p)
						       (length unused)
						       (map (lambda (p) (+ p 1)) (reverse unused))
						       (if (> (length unused) 1) "are" "is")))))))))
			  pars)))))

	;; -------- report-usage --------
	(lambda (caller head vars env)

	  ;; report unused or set-but-unreferenced variables, then look at the overall history
	  ;;   vars used before defined are kind of a mess -- history has #f for the (unknown) enclosing form
	  ;;   and any definition wipes out the accumulated pre-def uses -- this should be by closed-body and
	  ;;   ignore local defines (i.e. really only define[x] propagates backwards) -- changing this is
	  ;;   tricky (fools current unused func arg + value message for example).

	  (defined-twice caller head vars)

	  ;; main loop -- goes to end
	  (let ((old-line-number line-number)
		(outer-form (cond ((var-member :let env) => var-initial-value) (else #f))))
	    (for-each
	     (lambda (local-var)
	       (let ((otype (if (eq? (var-definer local-var) 'parameter) 'parameter 'variable)))
		 ;; (var-name local-var) can be a pair (probably a bug): '(setter object-info)
		 (set->let caller outer-form local-var env)

		 ;; it's possible for an unused function to have ref=1, null cdr history, but it appears to
		 ;;   always involve curlet exports and the like.

		 ;; var not set
		 (when (zero? (var-set local-var))
		   (when (and (quoted-symbol? (var-initial-value local-var))     ; (let ((abc 'abc)) ...)
			      (eq? (var-name local-var) (cadr (var-initial-value local-var)))
			      (not (keyword? (var-name local-var))))
		     (lint-format "pointless local variable: ~A, just use ~A directly" caller (var-name local-var) (var-initial-value local-var)))

		   ;; do all refs to an unset var go through the same function (at some level)
		   (when (and (> (var-ref local-var) 1)
			      (pair? (var-history local-var))
			      (pair? outer-form)                             ; if outer-form is #f, local-var is probably a top-level var
			      (not (and (memq (car outer-form) '(let let*))  ; not a named-let parameter
					(symbol? (cadr outer-form)))))
		     (use-vector caller local-var)
		     ;; string->byte-vector got no hits, vector->int|float-vector is mostly test stuff, there are only a few a-lists>20 in length
		     (wrappable-var caller local-var otype outer-form env)))

		 (unless (memq (var-name local-var) '(:lambda :dilambda))
		   (use-dilambda caller local-var vars env)
		   (bad-name caller head local-var otype)
		   (reducible-scope caller local-var otype env)
		   (if (and (eq? (var-ftype local-var) 'define-expansion)
			    (not (eq? caller top-level:)))
		       (out-format outport "~NCdefine-expansion for ~A is not at the top-level, so it is simply define-macro~%"
			       lint-left-margin #\space
			       (var-name local-var)))

		   ;; define* -> define is tricky: multiple-values, renaming possibilities, etc
		   ;; redundant vars are hard to find -- tons of false positives

		   (parlous-port caller local-var outer-form)
		   (parlous-output-string local-var outer-form)

		   (if (zero? (var-ref local-var))
		       (unused-var caller head local-var otype)
		       (let ((vtype #f))
			 (move-var-inward caller local-var)
			 (when (and (not (memq (var-definer local-var) '(named-let named-let*)))
				    (pair? (var-history local-var))
				    (or (zero? (var-set local-var))
					(set! vtype (all-types-agree local-var))))
			   (unless vtype
			     (set! vtype (or (eq? caller top-level:) ; might be a global var where init value is largely irrelevant
					     (->lint-type (var-initial-value local-var)))))
			   
			   (let ((lit? (and (code-constant? (var-initial-value local-var))
					    (not (quoted-null? (var-initial-value local-var)))))) ; something fishy is going on...

			     ;; check each use of the local var
			     (do ((clause (var-history local-var) (cdr clause)))
				 ((null? (cdr clause)))             ; ignore the initial value which depends on a different env
			       (let ((call (car clause)))
				 (if (pair? call)
				     (set! line-number (or (pair-line-number call) (max line-number 0))))

				 (when (pair? call)
				   (let ((func (car call))
					 (vname (var-name local-var))
					 (call-arg1 (and (pair? (cdr call)) (cadr call))))

				     ;; check for assignments into constants
				     (if (and lit?
					      (sequence? (var-initial-value local-var)) ; not #f = no default
					      (indirect-set? vname func call-arg1))
					 (lint-format "~A's ~Avalue, ~S, is a literal constant, so this set! is trouble: ~A" caller
						      vname
						      (if (eq? (var-definer local-var) 'parameter) "default " "")
						      (var-initial-value local-var)
						      (truncated-list->string
						       (if (eq? (car call) 'implicit-set)
							   (cons 'set! (cdr call))
							   call))))

				     (when (and (symbol? vtype)
						(not (eq? (var-definer local-var) 'parameter)))
				       (pointless-if caller vname vtype func call)

				       ;; check for incorrect types in function calls
				       (unless (memq vtype '(boolean? null?)) ; null? here avoids problems with macros that call set!
					 (when (len>1? call)
					   (arg-mismatch caller vname vtype func call env))

					 (let ((suggest made-suggestion))
					   (pointless-type-check caller local-var vtype func call call-arg1 vars)
					   ;; the usual eqx confusion
					   (when (and (= suggest made-suggestion)
						      (memq vtype '(char? number? integer? real? float? rational? complex?)))
					     (eqx-type-check caller local-var vtype func call call-arg1)))

					 ;; (x 1) where x is not a sequence is tricky:
					 ;;   problem here is that e.g. let-values|fluid-let|define-record-structure name|field list becomes a call!
					 (implicit-type-checks caller local-var vtype func call call-arg1)))))))

			     (duplicated-calls caller local-var env)
			     (repeated-args caller local-var)))))

		   ;; vars with multiple incompatible ascertainable types don't happen much and obvious type errors are extremely rare
		   (parlous-return caller local-var env))))
	     vars)
	    (set! line-number old-line-number)))))


    (define (find-call sym body)
      (call-with-exit
       (lambda (return)
	 (let tree-call ((tree body))
	   (when (unquoted-pair? tree)
	     (if (eq? (car tree) sym)
		 (return tree))
	     (if (memq (car tree) '(let let* letrec letrec* do lambda lambda* define))
		 (return #f)) ; possible shadowing -- not worth the infinite effort to corroborate
	     (if (pair? (car tree))
		 (tree-call (car tree)))
	     (if (pair? (cdr tree))
		 (do ((p (cdr tree) (cdr p)))
		     ((not (pair? p)) #f)
		   (tree-call (car p)))))))))


    (define (check-returns caller f env) ; f is not the last form in the body
      (if (not (or (side-effect? f env)
		   (eq? '=> f)))
	  (lint-format "this could be omitted: ~A" caller (truncated-list->string f))
	  (when (pair? f)
	    (case (car f)
	      ;; no-side-effect function as (car f) got a couple hits -- not worth the bother
	      ((if)
	       (when (len>1? (cdr f))
		 (let ((true (caddr f))
		       (false (if (pair? (cdddr f)) (cadddr f) 'no-false)))
		   (let ((true-ok (side-effect? true env))
			 (false-ok (or (eq? false 'no-false)
				       (side-effect? false env))))
		     (if true-ok
			 (if (pair? true)
			     (check-returns caller true env))
			 (lint-format "this branch is pointless~A: ~A in ~A" caller
				      (local-line-number true)
				      (truncated-list->string true)
				      (truncated-list->string f)))
		     (if false-ok
			 (if (pair? false)
			     (check-returns caller false env))
			 (lint-format "this branch is pointless~A: ~A in ~A" caller
				      (local-line-number false)
				      (truncated-list->string false)
				      (truncated-list->string f)))))))
	      ((cond case)
	       (when (pair? (cdr f))
		 ;; here all but last result exprs are already checked
		 ;;   redundant begin can confuse this, but presumably we'll complain about that elsewhere
		 ;;   also even in mid-body, if else clause has a side-effect, an earlier otherwise pointless clause might be avoiding that
		 (let ((has-else (let ((last-clause (last-ref f)))
				   (and (pair? last-clause)
					(memq (car last-clause) '(else #t))
					(lint-any? (lambda (c)
						     (side-effect? c env))
						   (cdr last-clause))))))
		   (for-each (lambda (c)
			       (if (and (len>1? c)
					(proper-list? c)
					(not (memq '=> (cdr c))))
				   (let ((last-expr (last-ref c)))
				     (cond ((side-effect? last-expr env)
					    (if (pair? last-expr)
						(check-returns caller last-expr env)))

					   (has-else
					    (if (or (pair? (cddr c))
						    (eq? (car f) 'cond))
						(lint-format "this ~A clause's result could be omitted" caller
							     (truncated-list->string c))
						(if (not (memq last-expr '(#f #t #<unspecified>))) ; it's not already obvious
						    (lint-format "this ~A clause's result could be simply #f" caller
								 (truncated-list->string c)))))
					   ((and (eq? (car f) 'case)
						 (or (eq? last-expr (cadr c))
						     (not (lint-any? (lambda (p) (side-effect? p env)) (cdr c)))))
					    (lint-format "this case clause can be omitted: ~A" caller
							 (truncated-list->string c)))

					   (else (lint-format "this is pointless: ~A in ~A" caller
							      (truncated-list->string last-expr)
							      (truncated-list->string c)))))))
			     ((if (eq? (car f) 'cond) cdr cddr) f)))))

	      ((let let*)
	       (if (and (len>1? (cdr f))
			(not (symbol? (cadr f))))
		   (let ((last-expr (last-ref f)))
		     (if (side-effect? last-expr env)
			 (if (pair? last-expr)
			     (check-returns caller last-expr env))
			 (lint-format "this is pointless~A: ~A in ~A" caller
				      (local-line-number last-expr)
				      (truncated-list->string last-expr)
				      (truncated-list->string f))))))

	      ;; perhaps use truncated-lists->string here??
	      ((and)
	       (let ((len (length f)))
		 (case len
		   ((1) (lint-format "this ~A is pointless" caller f))
		   ((2) (lint-format "perhaps ~A" caller (lists->string f (cadr f))))
		   ((3) (lint-format "perhaps ~A" caller (lists->string f (list 'if (cadr f) (caddr f))))) ; (begin (and x (display y)) (log z)) -> (if x (display y))
		   (else (lint-format "perhaps ~A" caller (lists->string f (list 'if (cadr f) (cons 'and (cddr f)))))))))

	      ((or)
	       (let ((len (length f)))
		 (case len
		   ((1) (lint-format "this ~A is pointless" caller f))
		   ((2) (lint-format "perhaps ~A" caller (lists->string f (cadr f))))
		   ((3) (lint-format "perhaps ~A" caller (lists->string f `(if (not ,(cadr f)) ,(caddr f)))))
		   (else (lint-format "perhaps ~A" caller (lists->string f `(if (not ,(cadr f)) (or ,@(cddr f)))))))))

	      ((not)
	       (lint-format "this ~A is pointless" caller f))

	      ((letrec letrec* with-let unless when begin with-baffle)
	       (if (len>1? (cdr f))
		   (let ((last-expr (last-ref f)))
		     (if (side-effect? last-expr env)
			 (if (pair? last-expr)
			     (check-returns caller last-expr env))
			 ;; (begin (if x (begin (display x) z)) z)
			 (lint-format "this is pointless~A: ~A in ~A" caller
				      (local-line-number last-expr)
				      (truncated-list->string last-expr)
				      (truncated-list->string f))))))
	      ((do)
	       (let ((returned (if (and (len>1? (cdr f))
					(list? (caddr f)))
				   (let* ((end+res (caddr f))
					  (len (or (length end+res) -1)))
				     (if (> len 1)
					 (last-ref end+res))))))
		 (unless (eq? returned #<unspecified>)
		   (if (and (pair? returned)
			    (side-effect? returned env))
		       (check-returns caller returned env)
		       ;; (begin (do ((i 0 (+ i 1))) ((= i 10) i) (display i)) x)
		       (lint-format "~A: result ~A~A is not used" caller
				    (truncated-list->string f)
				    (truncated-list->string returned)
				    (local-line-number returned))))))
	      ((call-with-exit)
	       (if (and (pair? (cdr f))
			(len>1? (cadr f))
			(eq? (caadr f) 'lambda)
			(pair? (cadadr f)))
		   (let ((return (car (cadadr f))))
		     (let walk ((tree (cddadr f)))
		       (if (pair? tree)
			   (if (eq? (car tree) return)
			       (if (and (pair? (cdr tree))
					(or (not (boolean? (cadr tree)))
					    (pair? (cddr tree))))
				   ;; (begin (call-with-exit (lambda (quit) (if (< x 0) (quit (+ x 1))) (display x))) (+ x 2))
				   (lint-format "th~A call-with-exit return value~A will be ignored: ~A" caller
						(if (pair? (cddr tree))
						    (values "ese" "s")
						    (values "is" ""))
						tree))
			       (for-each walk tree)))))))

	      ((map)
	       (if (pair? (cdr f))          ;  (begin (map g123 x) x)
		   (lint-format "map could be for-each: ~A" caller (truncated-list->string (cons 'for-each (cdr f))))))

	      ((reverse!)
	       (if (pair? (cdr f))          ;  (let ((x (list 23 1 3))) (reverse! x) x)
		   (lint-format "~A might leave ~A in an undefined state; perhaps ~A" caller (car f) (cadr f)
				(list 'set! (cadr f) f))))

	      ((lambda lambda*)
	       (lint-format "this has no effect: ~A" caller (truncated-list->string f)))

	      ((format)
	       (if (and (pair? (cdr f))
			(eq? (cadr f) #t))  ;  (let () (format #t "~A" x) x)
		   (lint-format "perhaps use () with format since the string value is discarded:~%    ~A"
				caller (cons 'format (cons () (cddr f))))))))))

    (define lint-current-form #f)
    (define lint-mid-form #f)

    (define (escape? form env)
      (and (pair? form)
	   (let ((v (var-member (car form) env)))
	     (if v
		 (memq (var-definer v) '(call/cc call-with-current-continuation call-with-exit))
		 (memq (car form) '(error throw))))))


    (define (lint-walk-body caller head body env)
      (when (pair? body)
	(when (len>1? (car body))

	  (when (and (not (eq? last-rewritten-internal-define (car body))) ; we already rewrote this
		     (pair? (cdr body))                ; define->named let, but this is only ok in a "closed" situation, not (begin (define...)) for example
		     (pair? (cadr body))
		     (memq (caar body) '(define define*))
		     (pair? (cadar body)))
	    (let ((fname (caadar body))
		  (fargs (cdadar body))
		  (fbody (cddar body)))
	      (when (and (symbol? fname)
			 (proper-list? fargs)
			 (proper-list? body)
			 (= (tree-count fname (cdr body) 2) 1)
			 (not (any-keywords? fargs)))
		(let ((call (find-call fname (cdr body))))
		  (when (pair? call)
		    (let ((new-args (if (eq? (caar body) 'define)
					(map list fargs (cdr call))
					(let loop ((pars fargs)
						   (vals (cdr call))
						   (args ()))
					  (if (null? pars)
					      (reverse args)
					      (loop (cdr pars)
						    (if (pair? vals)
							(values (cdr vals)
								(cons (list ((if (pair? (car pars)) caar car) pars) (car vals)) args))
							(values ()
								(cons (if (pair? (car pars)) (car pars) (list (car pars) #f)) args))))))))
			  (new-let (if (eq? (caar body) 'define) 'let 'let*)))
		      (if (and (len>1? fbody)
			       (string? (car fbody)))
			  (set! fbody (cdr fbody)))
		      ;; (... (define* (f1 a b) (+ a b)) (f1 :c 1)) -> (... (let ((a :c) (b 1)) (+ a b)))
		      (lint-format "perhaps ~A" caller
				   (lists->string (cons '... body)
						  (if (= (tree-count fname body 3) 2)
						      (if (null? fargs)
							  (if (null? (cdr fbody))
							      (cons '... (tree-subst (car fbody) call (cdr body)))
							      `(... ,@(tree-subst `(let () ,@fbody) call (cdr body))))
							  `(... ,@(tree-subst `(let ,new-args ,@fbody) call (cdr body))))
						      `(... ,@(tree-subst `(,new-let ,fname ,new-args ,@fbody) call (cdr body))))))))))))

	  ;; look for non-function defines at the start of the body and use let(*) instead
	  ;;   we're in a closed body here, so the define can't propagate backwards
	  (let ((first-expr (car body)))
	    ;; another case: f(args) (let(...)set! arg < no let>)
	    (when (and (eq? (car first-expr) 'define)
		       (symbol? (cadr first-expr))
		       (pair? (cddr first-expr))
		       ;;(not (tree-car-member (cadr first-expr) (caddr first-expr)))
		       ;;(not (tree-set-car-member '(lambda lambda*) (caddr first-expr)))
		       (not (and (pair? (caddr first-expr))
				 (memq (caaddr first-expr) '(lambda lambda*))))
		       (> (length body) 2))
	      ;; this still is not ideal -- we need to omit let+lambda as well
	      (do ((names ())
		   (letx 'let)
		   (vars&vals ())
		   (p body (cdr p)))
		  ((not (and (pair? p)
			     (let ((expr (car p)))
			       (and (len>2? expr)
				    (eq? (car expr) 'define)
				    (symbol? (cadr expr))          ; not (define (f ...))
				    (not (and (pair? (caddr expr)) ; not (define f (lambda...))
					      (memq (caaddr expr) '(lambda let lambda* let* letrec letrec*))))))))
		   ;; (... (define x 3) 32) -> (... (let ((x 3)) ...))
		   (if (pair? vars&vals)
		       (lint-format "perhaps ~A" caller
				    (lists->string (cons '... body)
						   (list '... (list letx (reverse vars&vals) '...))))))
		;; define acts like letrec(*), not let -- reference to name in lambda body is current name
		(let ((expr (cdar p)))
		  (set! vars&vals (cons (if (< (tree-leaves (cdr expr)) 12)
					    expr
					    (list (car expr) '...))
					vars&vals))
		  (if (tree-set-memq names (cdr expr))
		      (set! letx 'let*))
		  (set! names (cons (car expr) names)))))))

	(let ((len (length body)))
	  (when (> len 2)                           ; ... (define (x...)...) (x ...) -> (let (...) ...) or named let -- this happens a lot!
	    (let ((n-1 (list-ref body (- len 2)))   ; or (define (x ...)...) (some expr calling x once) -> named let etc
		  (n (list-ref body (- len 1))))
	      (when (and (pair? n-1)
			 (eq? (car n-1) 'define)
			 (proper-pair? (cadr n-1))
			 (symbol? (caadr n-1))
			 (pair? n)
			 (or (and (eq? (car n) (caadr n-1))
				  (eqv? (length (cdadr n-1)) (length (cdr n)))) ; not values -> let!
			     (and (< (tree-leaves n-1) 12)
				  (tree-car-member (caadr n-1) (cdr n))         ; skip car -- see preceding
				  (= (tree-count (caadr n-1) n 2) 1))))
		(let ((outer-form (cond ((var-member :let env) => var-initial-value) (else #f)))
		      (new-var (caadr n-1)))
		  (when (and (pair? outer-form)
			     (not (let walker ((tree outer-form)) ; check even the enclosing env -- define in do body back ref'd in stepper for example
				    (or (eq? new-var tree)
					(and (unquoted-pair? tree)
					     (not (eq? n tree))
					     (not (eq? n-1 tree))
					     (or (walker (car tree))
						 (walker (cdr tree))))))))
		    (let ((named (if (tree-memq new-var (cddr n-1)) (list new-var) ())))
		      (if (eq? (car n) (caadr n-1))
			  (lint-format "perhaps change ~A to a ~Alet: ~A" caller new-var (if (pair? named) "named " "")
				       (lists->string outer-form `(... (let ,@named ,(map list (cdadr n-1) (cdr n)) ...))))
			  (let ((call (find-call new-var n)))
			    (when (and (pair? call)
				       (eqv? (length (cdadr n-1)) (length (cdr call))))
			      (let ((new-call `(let ,@named ,(map list (cdadr n-1) (cdr call)) ,@(cddr n-1))))
				(lint-format "perhaps embed ~A: ~A" caller new-var
					     (lists->string outer-form (list '... (tree-subst new-call call n)))))))))))))

	    (let ((suggest made-suggestion))
	      (unless (tree-memq 'curlet (list-ref body (- len 1)))
		(do ((q body (cdr q))
		     (k 0 (+ k 1)))
		    ((null? q))
		  (let ((expr (car q)))
		    (when (and (len=3? expr)
			       (eq? (car expr) 'define))
		      (let ((name (and (symbol? (cadr expr)) (cadr expr))))
			(when name
			  (do ((lastref k)
			       (p (cdr q) (cdr p))
			       (i (+ k 1) (+ i 1)))
			      ((null? p)
			       (if (and (< k lastref (+ k 2))
					(pair? (list-ref body (+ k 1))))
				   (let ((end-dots (if (< lastref (- len 1)) '(...) ()))
					 (letx (if (tree-memq name (cddr expr)) 'letrec 'let))
					 (use-expr (list-ref body (+ k 1)))
					 (seen-earlier (or (var-member name env)
							   (do ((s body (cdr s)))
							       ((or (eq? s q)
								    (and (pair? (car s))
									 (tree-memq name (car s))))
								(not (eq? s q)))))))
				     (cond (seen-earlier)

					   ((not (eq? (car use-expr) 'define))
					    (let-temporarily ((target-line-length 120))
					      ;; (... (define f14 (lambda (x y) (if (positive? x) (+ x y) y))) (+ (f11 1 2) (f14 1 2))) ->
					      ;;    (... (let ((f14 (lambda (x y) (if (positive? x) (+ x y) y)))) (+ (f11 1 2) (f14 1 2))))
					      (lint-format "the scope of ~A could be reduced: ~A" caller name
							   (truncated-lists->string (cons '... (cons expr (cons use-expr end-dots)))
										    (cons '... (cons (list letx
													   (list (list name (caddr expr)))
													   use-expr)
												     end-dots))))))
					   ((eq? (cadr use-expr) name)
					    ;; (let () (display 33) (define x 2) (define x (+ x y)) (display 43)) ->
					    ;;    (... (set! x (+ x y)) ...)
					    (lint-format "use set! to redefine ~A: ~A" caller name
							 (lists->string (cons '... (cons use-expr end-dots))
									(cons '... (cons (list 'set! name (caddr use-expr)) end-dots)))))
					   ((pair? (cadr use-expr))
					    (if (symbol? (caadr use-expr))
						(let-temporarily ((target-line-length 120))
						  ;; (let () (display 32) (define x 2) (define (f101 y) (+ x y)) (display 41) (f101 2)) ->
						  ;;    (... (define f101 (let ((x 2)) (lambda (y) (+ x y)))) ...)
						  (lint-format "perhaps move ~A into ~A's closure: ~A" caller name (caadr use-expr)
							       (truncated-lists->string `(... ,expr ,use-expr ,@end-dots)
											`(... (define ,(caadr use-expr)
												(,letx ((,name ,(caddr expr)))
												       (lambda ,(cdadr use-expr)
													 ,@(cddr use-expr))))
											      ,@end-dots))))))
					   ((and (symbol? (cadr use-expr))
						 (pair? (cddr use-expr)))
					    (let-temporarily ((target-line-length 120))
					      (if (and (pair? (caddr use-expr))
						       (eq? (caaddr use-expr) 'lambda))
						  ;; (let () (display 34) (define x 2) (define f101 (lambda (y) (+ x y))) (display 41) (f101 2))
						  ;;    (... (define f101 (let ((x 2)) (lambda (y) (+ x y)))) ...)
						  (lint-format "perhaps move ~A into ~A's closure: ~A" caller name (cadr use-expr)
							       (truncated-lists->string `(... ,expr ,use-expr ,@end-dots)
											`(... (define ,(cadr use-expr)
												(,letx ((,name ,(caddr expr)))
												       ,(caddr use-expr)))
											      ,@end-dots)))
						  ;; (... (define lib (r file)) (define exports (caddr lib)) ...) ->
						  ;;    (... (define exports (let ((lib (r file))) (caddr lib))) ...)
						  (lint-format "the scope of ~A could be reduced: ~A" caller name
							       (truncated-lists->string `(... ,expr ,use-expr ,@end-dots)
											`(... (define ,(cadr use-expr)
												(,letx ((,name ,(caddr expr)))
												       ,(caddr use-expr)))
											      ,@end-dots))))))))
				   (when (and (> len 3)
					      (< k lastref (+ k 3))  ; larger cases happen very rarely -- 3 or 4 altogether
					      (pair? (list-ref body (+ k 1)))
					      (pair? (list-ref body (+ k 2))))
				     (let ((end-dots (if (< lastref (- len 1)) '(...) ()))
					   (letx (if (tree-memq name (cddr expr)) 'letrec 'let))
					   (seen-earlier (or (var-member name env)
							     (do ((s body (cdr s)))
								 ((or (eq? s q)
								      (and (pair? (car s))
									   (tree-memq name (car s))))
								  (not (eq? s q)))))))
				       (unless seen-earlier
					 (let ((use-expr1 (list-ref body (+ k 1)))
					       (use-expr2 (list-ref body (+ k 2))))
					   (if (not (or (tree-set-memq '(define lambda) use-expr1)
							(tree-set-memq '(define lambda) use-expr2)))
					       ;; (... (define f101 (lambda (y) (+ x y))) (display 41) (f101 2)) ->
					       ;;    (... (let ((f101 (lambda (y) (+ x y)))) (display 41) (f101 2)))
					       (lint-format "the scope of ~A could be reduced: ~A" caller name
							    (let-temporarily ((target-line-length 120))
							      (truncated-lists->string `(... ,expr ,use-expr1 ,use-expr2 ,@end-dots)
										       `(... (,letx ((,name ,(caddr expr)))
												    ,use-expr1
												    ,use-expr2)
											     ,@end-dots)))))))))))
			    (when (tree-memq name (car p))
			      (set! lastref i)))))))))

	      (when (= suggest made-suggestion)
		;; look for define+binding-expr at end and combine
		(do ((prev-f #f)
		     (fs body (cdr fs)))
		    ((not (pair? fs)))
		  (let ((f (car fs)))
		    ;; define can come after the use, and in an open body can be equivalent to set!:
		    ;;   (let () (if x (begin (define y 12) (do ((i 0 (+ i 1))) ((= i y)) (f i))) (define y 21)) y)
		    ;;   (let () (define (f x) (+ y x)) (if z (define y 12) (define y 1)) (f 12))
		    ;; so we can't do this check in walk-open-body
		    ;;
		    ;; define + do -- if cadr prev-f not used in do inits, fold into do, else use let
		    ;;   the let case is semi-redundant (it's already reported elsewhere)
		    (when (and (len>1? prev-f)
			       (len>1? f)
			       (eq? (car prev-f) 'define)
			       (symbol? (cadr prev-f))
			       (list? (cadr f))
			       (not (hash-table-ref other-identifiers (cadr prev-f))) ; (cadr prev-f) already ref'd, so it's a member of env
			       (or (null? (cdr fs))
				   (not (tree-memq (cadr prev-f) (cdr fs)))))
		      (if (and (eq? (car f) 'do)
			       (len>2? f)
			       (proper-list? (cadr f)))
			  ;; (... (define z (f x)) (do ((i z (+ i 1))) ((= i 3)) (display (+ z i))) ...) -> (do ((i (f x) (+ i 1))) ((= i 3)) (display (+ z i)))
			  (lint-format "perhaps ~A" caller
				       (lists->string (list '... prev-f f '...)
						      (if (lint-any? (lambda (p)
								       (and (len>1? p)
									    (tree-memq (cadr prev-f) (cadr p))))
								     (cadr f))
							  (if (and (eq? (cadr prev-f) (cadr (caadr f)))
								   (null? (cdadr f)))
							      `(do ((,(caaadr f) ,(caddr prev-f) ,(caddr (caadr f)))) ,@(cddr f))
							      `(let (,(cdr prev-f)) ,f))
							  `(do (,(cdr prev-f)
								,@(cadr f))
							       ,@(cddr f)))))
			  ;; just changing define -> let seems officious, though it does reduce (cadr prev-f)'s scope
			  (if (and (proper-list? (cadr f))
				   (or (and (eq? (car f) 'let)
					    (not (tree-memq (cadr prev-f) (cadr f))))
				       (eq? (car f) 'let*)))
			      (lint-format "perhaps ~A" caller
					   (lists->string
					    `(... ,prev-f ,f ,@(if (null? (cdr fs)) () '(...)))
					    `(... (,(car f) (,(cdr prev-f) ,@(cadr f)) ...) ,@(if (null? (cdr fs)) () '(...))))))))
		    (set! prev-f f))))))))

      ;; definer as last in body is rare outside let-syntax, and tricky -- only one clear optimizable case found
      (lint-walk-open-body caller head body env))

    (define lint-walk-open-body
      (let ()

	;; -------- tree-change-member --------
	(define (tree-change-member set tree)
	  (and (unquoted-pair? tree)
	       (or (and (eq? (car tree) 'set!)
			(pair? (cdr tree)) ; a bug -- case with (set!) as key has confused tree-change-member!
			(memq (cadr tree) set))
		   (tree-change-member set (car tree))
		   (tree-change-member set (cdr tree)))))

	;; -------- combine-successive-ifs
	(define (combine-successive-ifs caller fs prev-f f env)
	  (let ((test1 (cadr prev-f))
		(test2 (cadr f))
		(f-func (and (pair? f) (car f))))
	    ;;     (if A...) (if (not A)...) happens very rarely -- only two rewritable hits
	    (let ((equal-tests  ; test1 = test2 [check for side-effects already]
		   (lambda ()

		     (if (and (pair? (caddr prev-f))
			      (escape? (caddr prev-f) env))
			 ;; (begin (if x (error 'oops)) (if x y)) -> begin: x is #f in (if x y) -- this never happens
			 (lint-format "~A is #f in ~A" caller
				      test2 (truncated-list->string f)))

		     ;; (... (if (and A B) (f C)) (if (and B A) (g E) (h F)) ...) -> (... (if (and A B) (begin (f C) (g E)) (begin (h F))) ...)
		     (lint-format "perhaps ~A" caller
				  (lists->string
				   (list '... prev-f f '...)
				   (if (eq? f-func 'if)
				       (if (and (null? (cdddr prev-f))
						(null? (cdddr f)))
					   ;; if (null (cdr fs)) we have to make sure the returned value is not changed by our rewrite
					   ;;   but when/unless return their last value in s7 (or #<unspecified>), so I think this is ok
					   (if (and (pair? test1)
						    (eq? (car test1) 'not))
					       `(... (unless ,(cadr test1)
						       ,@(unbegin (caddr prev-f))
						       ,@(unbegin (caddr f))) ...)
					       `(... (when ,test1
						       ,@(unbegin (caddr prev-f))
						       ,@(unbegin (caddr f))) ...))
					   `(... (if ,test1
						     (begin
						       ,@(unbegin (caddr prev-f))
						       ,@(unbegin (caddr f)))
						     (begin
						       ,@(if (pair? (cdddr prev-f)) (unbegin (cadddr prev-f)) ())
						       ,@(if (pair? (cdddr f)) (unbegin (cadddr f)) ())))
						 ...))
				       `(,f-func ,test1 ; f-func = when|unless
						 ,@(cddr prev-f)
						 ,@(cddr f)))))))
		  (test1-in-test2
		   (lambda ()
		     (if (null? (cddr test2))
			 (set! test2 (cadr test2)))
		     ;;     (... (if A (f B)) (when (and A C) (g D) (h E)) ...) -> (... (when A (f B) (when C (g D) (h E))) ...)
		     (lint-format "perhaps ~A" caller
				  (lists->string (list '... prev-f f '...)
						 (if (or (null? (cdddr prev-f))
							 (eq? (car prev-f) 'when)) ; so prev-f is when or 1-arm if (as is f)
						     `(... (when ,test1
							     ,@(cddr prev-f)
							     (when ,test2
							       ,@(cddr f)))
							   ,@(if (null? (cdr fs)) () '(...)))
						     ;; prev-f is 2-arm if and f is when or 1-arm if (the other case is too ugly)
						     `(... (if ,test1
							       (begin
								 ,(caddr prev-f)
								 (when ,test2
								   ,@(cddr f)))
							       ,@(cdddr prev-f)) ...))))))

		  (test2-in-test1
		   (lambda ()
		     (if (null? (cddr test1))
			 (set! test1 (cadr test1)))
		     ;; (... (if (and A B) (f C)) (if A (g E)) ...) -> (... (when A (when B (f C)) (g E)))
		     (lint-format "perhaps ~A" caller
				  (lists->string (list '... prev-f f '...)
						 (if (or (null? (cdddr f))
							 (eq? f-func 'when)) ; so f is when or 1-arm if (as is prev-f)
						     `(... (when ,test2
							     (when ,test1
							       ,@(cddr prev-f))
							     ,@(cddr f))
							   ,@(if (null? (cdr fs)) () '(...)))
						     ;; f is 2-arm if and prev-f is when or 1-arm if
						     `(... (if ,test2
							       (begin
								 (when ,test1
								   ,@(cddr prev-f))
								 ,(caddr f))
							       ,(cadddr f))
							   ,@(if (null? (cdr fs)) () '(...)))))))))

	      (cond ((equal? test1 test2)
		     (when (and (eq? f-func (car prev-f))
				(not (side-effect? test1 env)))
		       (if (not (tree-change-member (gather-symbols test1) (cdr prev-f)))
			   (equal-tests)
			   ;; (if A b) (if A c) -> (when A b (if A c)))
			   ;; (when A b) (when A c) -> (when A b (when A c))
			   (if (and (or (not (eq? (car prev-f) 'if))
					(= (length prev-f) 3))
				    (or (not (eq? f-func 'if))
					(= (length f) 3)))
			       (lint-format "perhaps ~A" caller
					    (lists->string (list '... prev-f f '...)
							   (let ((func (if (eq? f-func 'unless) 'unless 'when)))
							     `(... (,func ,test1
									  ,@(if (eq? (car prev-f) 'if)
										(unbegin (caddr prev-f))
										(cddr prev-f))
									  (,func ,test1
										 ,@(if (eq? f-func 'if)
										       (unbegin (caddr f))
										       (cddr f))))
								   ...))))))))
		    ((or (eq? f-func 'unless)
			 (eq? (car prev-f) 'unless))) ; too hard!

		    ;; look for test1 as member of test2 (so we can use test1 as the outer test)
		    ((and (pair? test2)
			  (eq? (car test2) 'and)
			  (member test1 (cdr test2))
			  (or (eq? f-func 'when)     ; f has to be when or 1-arm if
			      (null? (cdddr f)))
			  (or (pair? (cdr fs))        ; if prev-f has false branch, we have to ignore the return value of f
			      (eq? (car prev-f) 'when)
			      (null? (cdddr prev-f)))
			  (not (side-effect? test2 env))
			  (not (tree-change-member (gather-symbols test1) (cddr prev-f))))
		     (set! test2 (remove-one test1 test2))
		     (test1-in-test2))

		    ;; look for test2 as member of test1
		    ((and (pair? test1)
			  (eq? (car test1) 'and)
			  (member test2 (cdr test1))
			  (or (eq? (car prev-f) 'when)     ; prev-f has to be when or 1-arm if
			      (null? (cdddr prev-f)))
			  (not (side-effect? test1 env))
			  (not (tree-change-member (gather-symbols test2) (cddr prev-f))))
		     (set! test1 (remove-one test2 test1))
		     (test2-in-test1))

		    ;; look for some intersection of test1 and test2
		    ((and (pair? test1)
			  (pair? test2)
			  (eq? (car test1) 'and)
			  (eq? (car test2) 'and)
			  (not (side-effect? test1 env))
			  (not (side-effect? test2 env))
			  (not (tree-change-member (gather-symbols test2) (cddr prev-f))))
		     (let ((intersection ())
			   (new-test1 ())
			   (new-test2 ()))
		       (for-each (lambda (tst)
				   (if (member tst test2)
				       (set! intersection (cons tst intersection))
				       (set! new-test1 (cons tst new-test1))))
				 (cdr test1))
		       (for-each (lambda (tst)
				   (if (not (member tst test1))
				       (set! new-test2 (cons tst new-test2))))
				 (cdr test2))
		       (when (pair? intersection)
			 (if (null? new-test1)
			     (if (null? new-test2)
				 (begin
				   (set! test1 (cons 'and (reverse intersection)))
				   (equal-tests))
				 (when (and (or (eq? f-func 'when)
						(null? (cdddr f)))
					    (or (pair? (cdr fs))
						(eq? (car prev-f) 'when)
						(null? (cdddr prev-f))))
				   (set! test1 (cons 'and (reverse intersection)))
				   (set! test2 (cons 'and (reverse new-test2)))
				   (test1-in-test2)))
			     (if (null? new-test2)
				 (when (or (eq? (car prev-f) 'when)
					   (null? (cdddr prev-f)))
				   (set! test2 (cons 'and (reverse intersection)))
				   (set! test1 (cons 'and (reverse new-test1)))
				   (test2-in-test1))

				 (when (and (or (eq? f-func 'when)
						(null? (cdddr f)))
					    (or (eq? (car prev-f) 'when)
						(null? (cdddr prev-f))))
				   ;; (... (if (and A B) (f C)) (when (and B C) (g E)) ...) -> (... (when B (when A (f C)) (when C (g E))))
				   (lint-format "perhaps ~A" caller
						(let ((outer-test (if (null? (cdr intersection))
								      (car intersection)
								      (cons 'and (reverse intersection)))))
						  (set! new-test1 (if (null? (cdr new-test1))
								      (car new-test1)
								      (cons 'and (reverse new-test1))))
						  (set! new-test2 (if (null? (cdr new-test2))
								      (car new-test2)
								      (cons 'and (reverse new-test2))))
						  (lists->string (list '... prev-f f '...)
								 `(... (when ,outer-test
									 (when ,new-test1
									   ,@(cddr prev-f))
									 (when ,new-test2
									   ,@(cddr f)))
								       ,@(if (null? (cdr fs)) () '(...))))))))))))))))

	;; -------- ifs->case --------
	(define (ifs->case caller fs prev-f f)
	  ;; successive if's that can be combined into case
	  ;;   else in last if could be accommodated as well
	  ;; f and prev-f are known to be len=3 pairs here
	  (and (eq? (car f) 'if)
	       (eq? (car prev-f) 'if)
	       (pair? (cadr f))
	       (pair? (cadr prev-f))
	       (memq (caadr prev-f) '(eq? eqv? = char=?)) ; not memx
	       (memq (caadr f) '(eq? eqv? = char=?))
	       (let ((a1 (cadadr prev-f))
		     (a2 (caddr (cadr prev-f)))
		     (b1 (cadadr f))
		     (b2 (caddr (cadr f))))  ; other possibilities are never hit
		 (and (equal? a1 b1)
		      (code-constant? a2)
		      (code-constant? b2)
		      (not (tree-change-member (list a1) (cddr prev-f))) ; or any symbol in a1?
		      ;; (... (if (= x 1) (display y)) (if (= x 2) (f y)) ...) -> (case x ((1) (display y)) ((2) (f y)) ((3) (display z)))
		      (lint-format "perhaps ~A" caller
				   (lists->string (list '... prev-f f '...)
						  `(case ,a1
						     ((,(unquoted a2)) ,@(unbegin (caddr prev-f)))
						     ((,(unquoted b2)) ,@(unbegin (caddr f)))
						     ,@(do ((more ())
							    (nfs (cdr fs) (cdr nfs)))
							   ((let ((nf (if (pair? nfs) (car nfs) ())))
							      (not (and (len=3? nf)
									(eq? (car nf) 'if)
									(pair? (cadr nf))
									(memq (caadr nf) '(eq? eqv? = char=?))
									(equal? a1 (cadadr nf))
									(code-constant? (caddr (cadr nf))))))
							    ;; maybe add (not (tree-change-member (list a1) (cddr last-f)))
							    ;;   but it never is needed
							    (reverse more))
							 (if (pair? nfs)
							     (set! more (cons (cons (list (unquoted (caddr (cadar nfs))))
										    (unbegin (caddar nfs)))
									      more)))))))
		      #t))))

	;; -------- repeats->for-each --------
	(define (repeats->for-each caller prev-f fs-end repeats start-repeats repeat-arg env)
	  ;; check for repeated calls, but only one arg currently can change (more args = confusing separation in code)
	  (if (zero? repeat-arg)		    ; simple case -- all exprs are identical
	      (let ((step 'i))
		(if (tree-memq step prev-f)
		    (set! step (find-unique-name prev-f)))
		(lint-format "perhaps ~A... ->~%~NC(do ((~A 0 (+ ~A 1))) ((= ~A ~D)) ~A)" caller
			     (truncated-list->string prev-f)
			     pp-left-margin #\space
			     step step step (+ repeats 1)                                  ; only use of repeats
			     prev-f))

	      (let ((args ())
		    (constants? #t)
		    (func-name (car prev-f))
		    (new-arg (if (tree-memq 'arg prev-f)
				 (find-unique-name prev-f)
				 'arg)))
		(do ((p start-repeats (cdr p)))
		    ((eq? p fs-end))
		  (set! args (cons (list-ref (car p) repeat-arg) args))
		  (if constants? (set! constants? (code-constant? (car args)))))

		(let ((func (if (and (= repeat-arg 1)
				     (null? (cddar start-repeats)))
				func-name
				(list 'lambda (list new-arg)
				      (let ((call (copy prev-f)))
					(list-set! call repeat-arg new-arg)
					call)))))
		  (if constants?
		      (lint-format "perhaps ~A... ->~%~NC(for-each ~S '(~{~S~^ ~}))" caller
				   (truncated-list->string (car start-repeats))
				   pp-left-margin #\space
				   func
				   (map unquoted (reverse args)))
		      (let ((v (var-member func-name env)))                                  ; only use of env
			(if (or (and v
				     (memq (var-ftype v) '(define define* lambda lambda*)))
				(procedure? func-name) ; e.g. list-values??
				(procedure? (symbol->value func-name *e*)))
			    ;; (let () (write-byte 0) (write-byte 1) (write-byte 2) (write-byte 3) (write-byte 4)) ->
			    ;;    (for-each write-byte '(0 1 2 3 4))
			    (lint-format "perhaps ~A... ->~%~NC(for-each ~S (vector ~{~S~^ ~}))" caller
					 ;; vector rather than list because it is easier on the GC (list copies in s7)
					 (truncated-list->string (car start-repeats))
					 pp-left-margin #\space
					 func
					 (reverse args))
			    (if (not (or v
					 (macro? (symbol->value func-name *e*))))
				;; (let () (writ 0) (writ 1) (writ 2) (writ 3) (writ (* x 2))) -> (for-each writ (vector 0 1 2 3 (* x 2)))
				(lint-format "assuming ~A is not a macro, perhaps ~A" caller
					     func-name
					     (lists->string (list '... (car start-repeats) '...)
							    (list 'for-each func (cons 'vector (reverse args)))))))))))))

	;; -------- set-cxr->copy --------
	(define (set-cxr->copy caller prev-f f ctr len)
	  ;; set-car! + set-cdr! here is usually "clever" code assuming eq?ness, so we can't rewrite it using cons
	  ;;   but copy does not create a new cons... [if at end of body, the return values will differ]
	  (let ((f-func (car f)))
	    (when (and (memq f-func '(set-car! set-cdr!))  ; ...(set-car! x (car y)) (set-cdr! x (cdr y))... -> (copy y x)
		       (memq (car prev-f) '(set-car! set-cdr!))
		       (not (eq? (car prev-f) f-func))
		       (equal? (cadr f) (cadr prev-f)))
	      (let ((ncar (caddr (if (eq? f-func 'set-car!) f prev-f)))
		    (ncdr (caddr (if (eq? f-func 'set-car!) prev-f f))))
		(if (and (pair? ncar)
			 (eq? (car ncar) 'car)
			 (pair? ncdr)
			 (eq? (car ncdr) 'cdr)
			 (equal? (cadr ncar) (cadr ncdr)))
		    (lint-format "perhaps ~A~A ~A~A -> ~A" caller
				 (if (= ctr 0) "" "...")
				 (truncated-list->string prev-f)
				 (truncated-list->string f)
				 (if (= ctr (- len 1)) "" "...")
				 (list 'copy (cadr ncar) (cadr f))))))))

	;; -------- combine-sets --------
	(define (combine-sets caller prev-f f env)
	  (let ((arg1 (caddr prev-f))
		(arg2 (caddr f))
		(settee (cadr f)))

	    (if (and (or (and (equal? settee arg1)          ; (set! x y) (set! y x)
			      (equal? arg2 (cadr prev-f)))
			 (and (equal? settee (cadr prev-f)) ; (set! x y) (set! x y)
			      (equal? arg1 arg2)))
		     (not (tree-equal-member settee arg2)))
		(lint-format "this pair of set!s looks odd: ~A" caller
			     (list '... prev-f f '...)))

	    (cond ((not (eq? settee (cadr prev-f)))
		   (if (and (symbol? (cadr prev-f)) ; (set! x (A)) (set! y (A)) -> (set! x (A)) (set! y x)
			    (unquoted-pair? arg1)   ;   maybe more trouble than it's worth
			    (equal? arg1 arg2)
			    (hash-table-ref no-side-effect-functions (car arg1))
			    (not (tree-unquoted-member (cadr prev-f) arg1))
			    (not (side-effect? arg1 env))
			    (not (maker? arg1)))
		       (lint-format "perhaps ~A" caller (lists->string f (list 'set! settee (cadr prev-f))))))

		  ((not (and (pair? arg2)          ; (set! x 0) (set! x 1) -> "this could be omitted: (set! x 0)"
			     (tree-memq settee arg2)))
		   (if (not (or (side-effect? arg1 env)
				(side-effect? arg2 env)))
		       (lint-format "this could be omitted: ~A" caller prev-f)))

		  ((not (pair? arg2)))

		  ((and (pair? arg1)               ; (set! x (cons 1 z)) (set! x (cons 2 x)) -> (set! x (cons 2 (cons 1 z)))
			(eq? (car arg1) 'cons)
			(eq? (car arg2) 'cons)
			(eq? settee (caddr arg2))
			(not (eq? settee (cadr arg2))))
		   (lint-format "perhaps ~A ~A -> ~A" caller
				prev-f f
				`(set! ,settee (cons ,(cadr arg2) (cons ,@(cdr arg1))))))

		  ((and (pair? arg1)               ; (set! x (append x y)) (set! x (append x z)) -> (set! x (append x y z))
			(eq? (car arg1) 'append)
			(eq? (car arg2) 'append)
			(eq? settee (cadr arg1))
			(eq? settee (cadr arg2))
			(not (tree-memq settee (cddr arg1)))
			(not (tree-memq settee (cddr arg2))))
		   (lint-format "perhaps ~A ~A -> ~A" caller
				prev-f f
				`(set! ,settee (append ,settee ,@(cddr arg1) ,@(cddr arg2)))))

		  ((and (= (tree-count settee arg2 2) 1)   ; (set! x y) (set! x (+ x 1)) -> (set! x (+ y 1))
			(or (not (pair? arg1))
			    (< (tree-leaves arg1) 5)))
		   (lint-format "perhaps ~A ~A ->~%~NC~A" caller
				prev-f f pp-left-margin #\space
				(object->string (list 'set! settee (tree-subst arg1 settee arg2))))))))

	;; -------- redundant-set --------
	(define (redundant-set caller prev-f f env)
	  (cond ((and (eq? f #t)
		      (if (eq? (car prev-f) 'set!)
			  (and (len=2? (cdr prev-f))
			       (pair? (caddr prev-f))
			       (arg-signature (caaddr prev-f) env))
			  (arg-signature (car prev-f) env)))
		 => (lambda (sig)
		      (if (not (or (memq (car sig) '(#t values boolean))
				   (and (pair? (car sig))
					(memq 'boolean (car sig)))))
			  (lint-format "#t is probably redundant; ~A can't return #f" caller
				       ((if (eq? (car prev-f) 'set!) caaddr car) prev-f)))))
		((and (member f '(#<unspecified> (values)))
		      (memq (car prev-f) '(for-each newline close-input-port close-output-port close-port)))
		 (lint-format "~A is redundant; ~A returns #<unspecified>" caller f (car prev-f)))))

	;; -------- redundant-return --------
	(define (redundant-return caller body prev-f f env)
	  (case (car prev-f)
	    ((display write write-char write-byte)
	     (if (and (equal? f (cadr prev-f))
		      (not (side-effect? f env)))
		 ;; (cond ((= x y) y) (else (begin (display x) x)))
		 (lint-format "~A returns its first argument, so this could be omitted: ~A" caller
			      (car prev-f) (truncated-list->string f))))

	    ((vector-set! float-vector-set! int-vector-set! string-set! list-set! hash-table-set! let-set!  set-car! set-cdr!)
	     (if (equal? f (last-ref prev-f))
		 ;; (begin (vector-set! x 0 (* y 2)) (* y 2))
		 (lint-format "~A returns the new value, so this could be omitted: ~A" caller
			      (car prev-f) (truncated-list->string f)))
	     (if (and (len>1? f)
		      (eq? (cadr prev-f) (cadr f))
		      (not (code-constant? (cadr f)))
		      (let ((f-func (car f)))
			(case (car prev-f)
			  ((vector-set! float-vector-set! int-vector-set!)
			   (memq f-func '(vector-ref float-vector-ref int-vector-ref)))
			  ((list-set!)
			   (eq? f-func 'list-ref))
			  ((string-set!)
			   (eq? f-func 'string-ref))
			  ((set-car!)
			   (eq? f-func 'car))
			  ((set-cdr!)
			   (eq? f-func 'cdr))
			  (else #f)))
		      (or (memq (car f) '(car cdr)) ; no indices
			  (and (pair? (cddr f))     ; for the others check that indices match
			       (equal? (caddr f) (caddr prev-f))
			       (pair? (cdddr prev-f))
			       (not (pair? (cddddr prev-f)))
			       (not (pair? (cdddr f)))
			       (not (side-effect? (caddr f) env)))))
		 ;; (let ((x (list 1 2))) (set-car! x 3) (car x))
		 (lint-format "~A returns the new value, so this could be omitted: ~A" caller
			      (car prev-f) (truncated-list->string f))))

	    ((copy)
	     (if (or (and (null? (cddr prev-f))
			  (equal? (cadr prev-f) f))
		     (and (len=1? (cddr prev-f))
			  (equal? (caddr prev-f) f)))
		 (lint-format "~A returns the new value, so ~A could be omitted" caller
			      (truncated-list->string prev-f)
			      (truncated-list->string f))))

	    ((set! define define* define-macro define-constant define-macro*
		   defmacro defmacro* define-expansion define-bacro define-bacro*)
	     (cond ((not (and (pair? (cddr prev-f))                 ; (set! ((L 1) 2)) an error, but lint should keep going
			      (or (and (equal? (caddr prev-f) f)    ; (begin ... (set! x (...)) (...))
				       (not (side-effect? f env)))
				  (and (symbol? f)                  ; (begin ... (set! x ...) x)
				       (eq? f (cadr prev-f)))       ; also (begin ... (define x ...) x)
				  (and (not (eq? (car prev-f) 'set!))
				       (pair? (cadr prev-f))        ; (begin ... (define (x...)...) x)
				       (eq? f (caadr prev-f)))))))

		   ((not (memq (car prev-f) '(define define*)))
		    (lint-format "~A returns the new value, so this could be omitted: ~A" caller
				 (car prev-f) (truncated-list->string f)))

		   ((symbol? (cadr prev-f))
		    (lint-format "perhaps omit ~A and return ~A" caller
				 (cadr prev-f)
				 (caddr prev-f)))

		   ((= (tree-count f body 3) 2)
		    ;; (let () (define (f1 x) (+ x 1)) f1) -> (lambda (x) ...)
		    (lint-format "perhaps omit ~A, and change ~A" caller
				 f
				 (lists->string (list (car prev-f) (cadr prev-f) '...)
						(list (if (eq? (car prev-f) 'define) 'lambda 'lambda*)
						      (cdadr prev-f)
						      '...))))

		   (else (lint-format "~A returns the new value, so this could be omitted: ~A" caller ; possibly still not right if letrec?
				      (car prev-f) f))))))

	;; -------- check-shadows --------
	(define (check-shadows caller head f env)
	  ;; mid-body defines happen by the million, so resistance is futile
	  (let ((vname (if (symbol? (cadr f))
			   (cadr f)
			   (and (pair? (cadr f))
				(symbol? (caadr f))
				(caadr f)))))
	    ;; if already in env, check shadowing request
	    (if (var-member vname env)	; (let ((f33 33)) (define f33 4) (g f33 1))
		(lint-format "~A variable ~A in ~S shadows an earlier declaration" caller head vname f))))

	;; -------- check-escape --------
	(define (check-escape caller fs f next-to-last? env)
	  (if (and (escape? f env)
		   (pair? (cdr fs)) ; do special case
		   (lint-every? (lambda (arg)
				  (not (and (symbol? arg)
					    (let ((v (var-member arg env)))
					      (and v
						   (eq? (var-initial-value v) :call/cc))))))
				(cdr f)))
	      (if next-to-last?
		  ;; (let () (error 'oops "an error") #t)
		  (lint-format "~A makes this pointless: ~A" caller
			       (truncated-list->string f)
			       (truncated-list->string (cadr fs)))
		  ;; (begin (stop) (exit 6) (print 4) (stop))
		  (lint-format "~A makes the rest of the body unreachable: ~A" caller
			       (truncated-list->string f)
			       (truncated-list->string (list '... (cadr fs) '...))))))

	;; -------- displays->format --------
	(define (displays->format caller prev-f f dpy-f dpy-case dpy-len)
	  ;; display sequence starts at dpy-start, goes to ctr (prev-f) unless not dpy-case
	  (let ((ctrl-string "")
		(args ())
		(dctr 0)
		(dpy-last (if (not dpy-case) prev-f f))
		(op (write-port (car dpy-f)))
		(exprs (make-list (if dpy-case (+ dpy-len 1) dpy-len) ())))

	    (define* (gather-format str (arg :unset))
	      (set! ctrl-string (string-append ctrl-string str))
	      (unless (eq? arg :unset) (set! args (cons arg args))))

	    (call-with-exit
	     (lambda (done)
	       (for-each
		(lambda (d)
		  (unless (equal? (write-port d) op)
		    (lint-format "unexpected port change: ~A -> ~A in ~A" caller op (write-port d) d) ; ??
		    (done))
		  (list-set! exprs dctr d)
		  (set! dctr (+ dctr 1))
		  (gather-format (display->format d))
		  (when (eq? d dpy-last) ; op can be null => send to (current-output-port), return #f or #<unspecified>
		    ;; (begin (display x) (newline) (display y) (newline)) -> (format () "~A~%~A~%" x y)
		    (lint-format "perhaps ~A" caller (lists->string (cons '... exprs)
								    `(format ,op ,ctrl-string ,@(reverse args))))
		    (done)))
		dpy-f)))))

	;; -------- define->let --------
	(define (define->let caller fs prev-f f)
	  ;; sometimes redundant -- scope reduction can overlap this
	  (unless (or (null? (cdr fs))
		      (and (null? (cddr fs)) ; ... (define x...) x at end (handled elsewhere)
			   (pair? (cdr f))
			   (eq? (cadr f) (cadr fs)))
		      (and (symbol? (car prev-f))
			   (or (and (eq? (car prev-f) 'begin)
				    (lint-every? (lambda (p)
						   (and (pair? p)
							(symbol? (car p))
							(let ((fstr (symbol->string (car p))))
							  (and (>= (length fstr) 6)
							       (string=? (substring fstr 0 6) "define")))))
						 (cdr prev-f)))
			       (memq (car prev-f) '(use declare require hash-table-set! test assert))
			       ;; check for (if ... (define...)) as prev-f got only 1 hit
			       (let ((fstr (symbol->string (car prev-f))))
				 (and (>= (length fstr) 6)
				      (string=? (substring fstr 0 3) "def"))))))
	    (let ((new-var ((if (pair? (cadr f)) caadr cadr) f)))
	      (lint-format "~A occurs in the midst of the body;~%~NCperhaps use let: ~A" caller
			   (truncated-list->string f)
			   (+ lint-left-margin 4) #\space
			   (truncated-list->string
			    `(let ((,new-var ,(if (pair? (cadr f))
						  (cons (if (eq? (car f) 'define) 'lambda 'lambda*)
							(cons (cdadr f)
							      (cddr f)))
						  (caddr f))))
			       ,@(cdr fs)))))))


	;; -------- walk-open-body --------
	(lambda (caller head body env)
	  ;; walk a body (a list of forms, the value of the last of which might be returned)
	  (if (not (proper-list? body))
	      (lint-format "stray dot? ~A" caller (truncated-list->string body))

	      (let ((len (length body))
		    (old-current-form lint-current-form)
		    (old-mid-form lint-mid-form)
		    (macdef (memq head '(defmacro defmacro* define-macro define-macro* define-bacro define-bacro*))))

		(if (eq? head 'do) (set! len (+ len 1))) ; last form in do body is not returned
		(do ((prev-f #f)
		     (prev-len 0)
		     (f-len 0)
		     (repeats 0)
		     (start-repeats body)
		     (repeat-arg 0)
		     (dpy-f #f)
		     (dpy-start #f)
		     (rewrote-already #f)
		     (fs body (cdr fs))
		     (ctr 0 (+ ctr 1)))
		    ((not (pair? fs)))
		  (let ((f (car fs)))
		    (when (len>1? f)

		      ;; successive combinable conds/cases/dos are tricky and rare
		      (when (and *report-shadowed-variables*
				 (eq? (car f) 'define))
			(check-shadows caller head f env))
		      (when (pair? (cddr f))
			(when (and (len>2? prev-f) ; (if A ...) (if A ...) -> (when A ...) or equivalents
				   (memq (car prev-f) '(if when unless))
				   (memq (car f) '(if when unless)))
			  (combine-successive-ifs caller fs prev-f f env))
			(when (and (pair? prev-f)
				   (memq (car f) '(define define*)))
			  (define->let caller fs prev-f f))))

		    (let ((feq (and (pair? prev-f)
				    (pair? f)
				    (eq? (car f) (car  prev-f))
				    (or (equal? (cdr f) (cdr prev-f))
					(do ((fp (cdr f) (cdr fp))
					     (pp (cdr prev-f) (cdr pp))
					     (i 1 (+ i 1)))
					    ((or (and (null? pp)
						      (null? fp))
						 (not (pair? pp))
						 (not (pair? fp))
						 (if (= i repeat-arg)                   ; ignore the arg that's known to be changing
						     (side-effect? (car pp) env)        ; (car pp) is the current arg (where i = repeat-arg)
						     (and (not (equal? (car pp) (car fp)))
							  (or (positive? repeat-arg)
							      (and (set! repeat-arg i)  ; call this one the changer
								   #f)))))
					     (and (null? pp)
						  (null? fp))))))))
		      (if feq
			  (set! repeats (+ repeats 1)))
		      (when (or (not feq)
				(= ctr (- len 1))) ; this assumes we're not returning the last value?
			(when (and (> repeats 2)
				   (or (zero? repeat-arg)        ; all forms are the identical
				       (not (hash-table-ref syntaces (car prev-f))))) ; macros should be ok here if args are constants
			  (repeats->for-each caller prev-f (if (not feq) fs (cdr fs)) repeats start-repeats repeat-arg env))
			(set! repeats 0)
			(set! repeat-arg 0)
			(set! start-repeats fs)))

		    (if (pair? f)
			(begin
			  (set! f-len (length f))
			  (if (eq? (car f) 'begin)
			      (lint-format "redundant begin: ~A" caller (truncated-list->string f))))
			(begin
			  (if (symbol? f)
			      (set-ref f caller f env))
			  (set! f-len 0)))

		    (when (= f-len prev-len 3)
		      (set-cxr->copy caller prev-f f ctr len)
		      (if (not rewrote-already)
			  (set! rewrote-already (ifs->case caller fs prev-f f)))

		      (when (and (eq? (car f) 'set!)          ; other such funcs like fill! don't seem to happen
				 (eq? (car prev-f) 'set!))
			(combine-sets caller prev-f f env)))
#|
		    (when (and (= f-len prev-len 4)           ; (vector-set! v 0 1) (vector-set! v 0 2) -- this does not happen outside tests
			       (eq? (car f) (car prev-f))
			       (equal? (cadr f) (cadr prev-f))
			       (equal? (caddr f) (caddr prev-f))
			       (memq (car f) '(vector-set! list-set! hash-table-set! float-vector-set! int-vector-set! string-set! let-set!))
			       (not (pair? (caddr f)))
			       (not (side-effect? (cadddr f) env))
			       (not (side-effect? (cadddr prev-f) env)))
		      (lint-format "~S could be omitted" caller (truncated-list->string prev-f)))
|#
		    (if (< ctr (- len 1))
			(begin		             ; f is not the last form, so its value is ignored
			  (check-escape caller fs f (= ctr (- len 2)) env)
			  (check-returns caller f env)) ; look for code that has no effect (not returned, no side-effect)

			;; here f is the last form in the body
			(when (pair? prev-f)
			  (redundant-set caller prev-f f env)
			  (when (pair? (cdr prev-f))
			    (redundant-return caller body prev-f f env))))

		    ;; needs f fs prev-f dpy-f dpy-start ctr len
		    ;;   trap lint-format
		    (let ((dpy-case (and (pair? f)
					 (memq (car f) '(display write newline write-char write-string))))) ; flush-output-port?
		      (when (and dpy-case
				 (not dpy-start))
			(set! dpy-f fs)
			(set! dpy-start ctr))
		      (when (and (integer? dpy-start)
				 (> (- ctr dpy-start) (if dpy-case 1 2))
				 (or (= ctr (- len 1))
				     (not dpy-case)))
			(displays->format caller prev-f f dpy-f dpy-case (- ctr dpy-start))
			(set! dpy-start #f))
		      (unless dpy-case (set! dpy-start #f)))

		    (if (and macdef
			     (pair? f)
			     (tree-memq 'unquote f))
			(lint-format "~A probably has too many unquotes: ~A" caller head (truncated-list->string f)))

		    (set! prev-f f)
		    (set! prev-len f-len)

		    (set! lint-current-form f)
		    (if (= ctr (- len 1))
			(set! env (lint-walk caller f env))
			(begin
			  (set! lint-mid-form f)
			  (let ((e (lint-walk caller f env)))
			    (if (and (pair? e)
				     (not (memq (var-name (car e)) '(:lambda :dilambda))))
				(set! env e)))))
		    (set! lint-current-form #f)
		    (set! lint-mid-form #f)

		    ;; need to put off this ref tick until we have a var for it (lint-walk above)
		    (when (and (= ctr (- len 1))
			       (len>1? f))
		      (if (and (pair? (cadr f))
			       (memq (car f) '(define define* define-macro define-constant define-macro* define-expansion define-bacro define-bacro*)))
			  (set-ref (caadr f) caller f env)
			  (if (memq (car f) '(defmacro defmacro*))
			      (set-ref (cadr f) caller f env))))))

		(set! lint-mid-form old-mid-form)
		(set! lint-current-form old-current-form)))
	  env)))


    (define (return-walker last func)
      (if (not (and (pair? last)
		    (proper-list? last)))
	  (func last)
	  (case (car last)

	    ((begin let let* letrec letrec* when unless with-baffle with-let)
	     (when (pair? (cdr last))
	       (return-walker (last-ref last) func)))

	    ((if)
	     (when (len>1? (cdr last))
	       (return-walker (caddr last) func)
	       (if (pair? (cdddr last))
		   (return-walker (cadddr last) func))))

	    ((cond)
	     (when (pair? (cdr last))
	       (for-each (lambda (c)
			   (when (pair? c)
			     (return-walker (last-ref c) func)))
			 (cdr last))))

	    ((case)
	     (when (len>1? (cdr last))
	       (for-each (lambda (c)
			   (when (pair? c)
			     (return-walker (last-ref c) func)))
			 (cddr last))))

	    ((do)
	     (if (and (len>1? (cdr last))
		      (proper-list? (caddr last))
		      (len>1? (caddr last)))
		 (return-walker (last-ref (caddr last)) func)))

	    ((set!)
	     (if (len>1? (cdr last))
		 (func (caddr last))))

	    ((not)
	     (func #f))

	    ((or)
	     (func #f)
	     (for-each (lambda (c)
			 (return-walker c func))
		       (cdr last)))

	    ((and)
	     (func #f)
	     (return-walker (last-ref last) func))

	    ((dynamic-wind catch)
	     (let ((body (and (= (length last) 4)
			      (caddr last))))
	       (if (and (pair? body)
			(eq? (car body) 'lambda))
		   (return-walker (last-ref (cddr body)) func))))

	    (else (func last)) ; includes quote

	    ;; call-with-exit: walker on last on body, and scan for return func, walker on arg(s...)->values?
	    )))

    (define (check-sequence-constant function-name last) ; if last is a sequence constant, complain
      (return-walker last
		     (lambda (in-seq)
		       (when (or (not (pair? in-seq))
				 (eq? (car in-seq) 'quote))
			 (let ((seq (if (len>1? in-seq)              ; (quote . 1)??
					(cadr in-seq)
					in-seq)))
			   (when (and (sequence? seq)
				      (not (zero? (length seq))))
			     (lint-format "returns ~A constant: ~A~S" function-name ; (define-macro (m a) `(+ 1 a))
					  (if (pair? seq)
					      (values "a list" "'" seq)
					    (values (prettify-checker-unq (->lint-type in-seq)) "" seq)))
			     (throw 'sequence-constant-done))))))) ; just report one constant -- the full list is annoying

    (define (code-equal? a b) ; these extra tests get no hits
      ;; given NaNs, these 'equal?s should probably be equivalent?
      (or (equal? a b)
	  (and (pair? a)
	       (pair? b)
	       (or (and (eq? (car a) (hash-table-ref reversibles (car b)))
			(proper-list? b)
			(equal? (cdr a) (reverse (cdr b))))
		   (and (eq? (car a) 'not)
			(pair? (cdr a))
			(pair? (cadr a))
			(eq? (hash-table-ref notables (caadr a)) (car b))
			(equal? (cdadr a) (cdr b)))
		   (and (eq? (car b) 'not)
			(pair? (cdr b))
			(pair? (cadr b))
			(eq? (hash-table-ref notables (caadr b)) (car a))
			(equal? (cdr a) (cdadr b)))))))


    (define lint-function-body #f) ; a momentary kludge??
    (define lint-function-name #f) ; and another!

    (define (evert-function-locals form vars env)
      ;; look for outer let with var value constant, not set in func body --
      ;;   suggest moving it to closure, modulo endless quibbles of course.
      ;;   ignore nested lets because there we assume locality is more important.
      ;; we get here if (eq? form lint-function-body) and (symbol? lint-function-name)
      ;;   and not named-let (can this happen?) and only this expr in body
      ;; currently called only in let-walker, but might make sense in let*-walker and letrec-walker.
      ;;   in letrec-walker it got only 1 hit.

      (when (and (pair? lint-function-body)  ; (let ((v 3)) v)?
		 (eq? form (car lint-function-body))
		 (symbol? lint-function-name)
		 (pair? form)                ; this is (car lint-function-body)
		 (null? (cdr lint-function-body))
		 ;(not (tree-set-memq definers (cdr form)))
		 )
	(for-each
	 (lambda (local-var)
	   (let ((vname (var-name local-var))
		 (vvalue (var-initial-value local-var)))
	     (when (and (zero? (var-set local-var))
			(not (eq? (var-definer local-var) 'parameter))
			(or (constant-expression? vvalue env)
			    (and (pair? vvalue)
				 (memq (car vvalue) '(list vector float-vector int-vector byte-vector))
				 (not (lint-any? (lambda (x)
						   (or (and (pair? x)
							    (not (eq? (car x) 'quote)))
						       (and (symbol? x) ; (list 1 x 2)
							    (not (constant? x)))))
						 (cdr vvalue)))))
			(not (lint-any? (lambda (p)
					  (and (pair? p)
					       (or (memq (car p) '(vector-set! float-vector-set! int-vector-set!
								   string-set! list-set! hash-table-set! let-set!
								   set-car! set-cdr!))
						   ;; maybe check for anything ending in ! here
						   (lint-set!? p env))
					       (eq? vname (cadr p))))
					(var-history local-var))))
	       (lint-format "~A can ~Abe moved to ~A's closure" lint-function-name
			    vname
			    (if (lint-any? (lambda (p)
					     (and (pair? p)
						  (side-effect? p env)))
					   (var-history local-var))
				"probably "
				"")
			    (if (not (or (keyword? lint-function-name)
					 (memq lint-function-name '(let let* letrec))))
				lint-function-name
				"the enclosing function")))))
	 vars)))

    (define (report-doc-string definer function-name args body)
      (lint-format "old-style doc string: ~S, use '+documentation+:~%~NC~A" function-name
		   (car body) (+ lint-left-margin 4) #\space
		   (lint-pp `(define ,function-name
			       (let ((+documentation+ ,(car body)))
				 (,(case definer
				     ((define) 'lambda)
				     ((define*) 'lambda*)
				     (else))
				  ,args
				  ,@(cdr body)))))))

    (define (lint-walk-function-body definer function-name args body env)
      ;; walk function body, with possible doc string at the start
      (when (and (len>1? body)
		 (string? (car body)))
	(if *report-doc-strings*
	    (report-doc-string definer function-name args body))
	(set! body (cdr body))) ; ignore old-style doc-string
      ;; (set! arg ...) never happens as last in body

      ;; but as first in body, it happens ca 100 times
      (if (and (pair? body)
	       (len>2? (car body))
	       (eq? (caar body) 'set!)
	       (or (eq? (cadar body) args)
		   (and (pair? args)
			(memq (cadar body) args))))
	  ;; (define (f21 x y) (set! x 3) (+ y 1))
	  (lint-format "perhaps ~A" function-name
		       (lists->string (car body) `(let ((,(cadar body) ,(caddar body))) ...))))
      ;; as first in let of body, maybe a half-dozen

      (let ((tag 'yup))
	(catch 'sequence-constant-done
	  (lambda ()
	    (check-sequence-constant function-name (last-ref body)) ; some of these are innocuous -- lambda forms in midst of outer body etc
	    (set! tag 'nope))
	  (lambda args #f))
	(if (eq? tag 'yup)
	    (let ((v (var-member function-name env)))
	      (if (and v
		       (symbol? (var-ftype v)))
		  (set! (var-retcons v) #t)))))

      (let-temporarily ((lint-function-body (and (not (eq? definer 'definstrument)) body))
			(lint-function-name (and (null? (cdr body)) function-name)))
	(lint-walk-body function-name definer body env)))

    (define (lint-walk-function definer function-name args body form env)
      ;; check out function arguments (adding them to the current env), then walk its body
      ;; first check for (define (hi...) (ho...)) where ho has no opt args (and try to ignore possible string constant doc string)
      (when (eq? definer 'define)
	(let ((bval (if (and (len>1? body)
			     (string? (car body)))
			(cdr body)                   ; strip away the (old-style) documentation string
			body)))

	  (cond ((not (and (len=1? bval)             ; not (define (hi a) . 1)!
			   (pair? (car bval))
			   (symbol? (caar bval)))))  ; not (define (hi) ((if #f + abs) 0))

		;; might be nice to (define (f x) x) -> (define f values) but that turns off the arg-num error check

		((or (equal? args (cdar bval))
		     (and (hash-table-ref reversibles (caar bval))
			  (pair? (cdar bval))
			  (equal? args (reverse (cdar bval)))))
		 (let* ((cval (caar bval))
			(p (symbol->value cval *e*))
			(ary (arity p)))
		   (if (or (procedure? p)
			   (let ((e (var-member cval env) ))
			     (and e
				  (symbol? (var-ftype e))
				  (let ((def (var-initial-value e))
					(e-args (var-arglist e)))
				    (and
				     (pair? def)
				     (memq (var-ftype e) '(define lambda))
				     (or (and (null? args)
					      (null? e-args))
					 (and (symbol? args)
					      (symbol? e-args))
					 (and (pair? args)
					      (pair? e-args)
					      (= (length args) (length e-args)))))))))
		       (lint-format "~A~A could be (define ~A ~A)" function-name
				    (if (and (procedure? p)
					     (not (= (car ary) (cdr ary)))
					     (not (= (length args) (cdr ary))))
					(format #f "leaving aside ~A's optional arg~P, " cval (- (cdr ary) (length args)))
					;; optional args is only one of the problems:
					;;   (define (f5 x) (read x)) (with-input-from-string f5) -- f5 is not a thunk
					;;   (define (f5 x y) (macro x y)) -- f5 evals its arguments, passing (values x y) makes this worse
					;;   (define (f5 x) (f6 x)) where f6 can be redefined later -- expectation is that new f6 is called
					;;   (object->string (define (f5 x y) (+ x y)) :readable) returns "+"
					;;   and undoubtedly many more
					"")
				    function-name
				    function-name
				    (if (equal? args (cdar bval))
					cval
					(hash-table-ref reversibles (caar bval))))
		       (if (and (null? args)            ; perhaps this can be extended to any equal args
				(null? (cdar bval)))
			   ;; (define (getservent) (getserv)) -> (define getservent getserv)
			   (lint-format "~A could probably be ~A" function-name
					(truncated-list->string form)
					(truncated-list->string (list 'define function-name cval)))))))

		((and (or (symbol? args)
			  (and (pair? args)
			       (negative? (length args))))
		      (eq? (caar bval) 'apply)
		      (len>1? (cdar bval))
		      (symbol? (cadar bval))
		      (not (memq (cadar bval) '(and or)))
		      (or (and (eq? args (caddar bval))
			       (null? (cdddar bval)))
			  (and (pair? args)
			       (equal? (cddar bval) (proper-list args)))))
		 ;; (define (f1 . x) (apply + x)) -> (define f1 +)
		 (lint-format "~A could be (define ~A ~A)" function-name function-name function-name (cadar bval)))

		((and (hash-table-ref combinable-cxrs (caar bval))
		      (pair? (cdar bval))
		      (pair? (cadar bval)))
		 ((lambda* (cr arg)
		    (and cr
			 (< (length cr) 5)
			 (len=1? args)
			 (eq? (car args) arg)
			 (let ((f (symbol "c" cr "r")))
			   (if (eq? f function-name)
			       ;; (define (cadddr l) (caddr (cdr l)))
			       (lint-format "this redefinition of ~A is pointless (use (with-let (unlet)...) or #_~A)" definer function-name function-name)
			       ;; (define (f1 x) (cdr (car x))) -> (define f1 cdar)
			       (lint-format "~A could be (define ~A ~A)" function-name function-name function-name f)))))
		  (combine-cxrs (car bval))))

		((not (and (len>1? (cdar bval))
			   (memq (caar bval) '(list-ref list-tail))
			   (len=1? args)
			   (eq? (car args) (cadar bval)))))

		((eq? (caar bval) 'list-ref)
		 (case (caddar bval)
		   ((0) (lint-format "~A could be (define ~A car)" function-name function-name function-name))
		   ((1) (lint-format "~A could be (define ~A cadr)" function-name function-name function-name))
		   ((2) (lint-format "~A could be (define ~A caddr)" function-name function-name function-name))
		   ((3) (lint-format "~A could be (define ~A cadddr)" function-name function-name function-name))))

		(else
		 (case (caddar bval)
		   ((1) (lint-format "~A could be (define ~A cdr)" function-name function-name function-name))
		   ((2) (lint-format "~A could be (define ~A cddr)" function-name function-name function-name))
		   ((3) (lint-format "~A could be (define ~A cdddr)" function-name function-name function-name))
		   ((4) (lint-format "~A could be (define ~A cddddr)" function-name function-name function-name)))))))

      (let ((fvar (and (symbol? function-name)
		       (let ((fname (case definer
				      ((lambda lambda*) :lambda)
				      ((dilambda)       :dilambda)
				      (else             function-name)))
			     (fargs ((case definer
				       ((lambda lambda*)     cadr)
				       ((defmacro defmacro*) caddr)
				       (else                 cdadr))
				     form)))
			 (make-fvar fname definer fargs form env)))))
	(when fvar
	  (let-set! (cdr fvar) 'allow-other-keys
		    (case definer
		      ((lambda define-constant) #t)
		      ((lambda*) (eq? (last-ref (cadr form)) :allow-other-keys))
		      ((define*) (eq? (last-ref (cdadr form)) :allow-other-keys))
		      ((defmacro defmacro*)
		       (or (not (eq? definer 'defmacro*))
			   (and (pair? (caddr form))
				(eq? (last-ref (caddr form)) :allow-other-keys))))
		      (else
		       (or (not (memq definer '(define-macro* define-bacro*)))
			   (eq? (last-ref (cdadr form)) :allow-other-keys))))))

	(if (null? args)
	    (begin
	      (if (memq definer '(define* lambda* defmacro* define-macro* define-bacro*))
		  (lint-format "~A could be ~A"       ; (define* (f1) 32)
			       function-name definer
			       (symbol (substring (symbol->string definer) 0 (- (length (symbol->string definer)) 1)))))
	      (let ((cur-env (if fvar (cons fvar env) env)))
		(let ((nvars (let ((e (lint-walk-function-body definer function-name args body cur-env)))
			       (and (not (eq? e cur-env))
				    (env-difference function-name e cur-env ())))))
		  (if (pair? nvars)
		      (report-usage function-name definer nvars cur-env)))
		cur-env))

	    (if (not (or (symbol? args)
			 (pair? args)))
		(begin
		  (lint-format "strange ~A parameter list ~A" function-name definer args)
		  env)
		(let ((args-as-vars
		       (if (symbol? args)                            ; this is getting arg names to add to the environment
			   (begin
			     (if (memq definer '(define* lambda* defmacro* define-macro* define-bacro*))
				 (lint-format "~A could be ~A"       ; (lambda* args ...)
					      function-name definer
					      (symbol (substring (symbol->string definer) 0 (- (length (symbol->string definer)) 1)))))
			     (list (make-lint-var args #f 'parameter)))
			   (let ((star-definer (memq definer '(define* lambda* defmacro* define-macro* define-bacro* definstrument define*-public))))
			     (map (lambda (arg)
				    (if (symbol? arg)
					(if (memq arg '(:rest :allow-other-keys))
					    (values)                  ; omit :rest and :allow-other-keys
					    (make-lint-var arg #f 'parameter))
					(if (not (and star-definer (len=2? arg)))
					    (begin
					      (lint-format "strange parameter for ~A: ~S" function-name definer arg)
					      (values))
					    (begin
					      (if (not (or (cadr arg)                      ; (define* (f4 (a #f)) a)
							   (eq? definer 'define*-public))) ; who knows?
						  (lint-format "the default argument value is #f in ~A, so ~A can be ~A" function-name
							       definer arg (car arg)))
					      (make-lint-var (car arg)
							     (and star-definer (len=2? arg) (cadr arg)) ; default value -> initial-value
							     'parameter)))))
				  (proper-list args))))))

		  (let* ((cur-env (cons (make-lint-var :let form definer)
					(append args-as-vars (if fvar (cons fvar env) env))))
			 (nvars (let ((e (lint-walk-function-body definer function-name args body cur-env)))
				  (and (not (eq? e cur-env))
				       (env-difference function-name e cur-env ())))))
		    (report-usage function-name definer (append (or nvars ()) args-as-vars) cur-env))

		  (if (not fvar)
		      env
		      (begin
			(when (memq definer '(define lambda define-macro))
			  ;; look for unused parameters that are passed a value other than #f
			  (let ((set ())
				(unused ()))
			    (for-each
			     (lambda (arg-var)
			       (if (zero? (var-ref arg-var))
				   (if (positive? (var-set arg-var))
				       (set! set (cons (var-name arg-var) set))
				       (if (not (memq (var-name arg-var) '(+documentation+ +signature+ +setter+ +iterator+)))
					   (set! unused (cons (var-name arg-var) unused))))))
			     args-as-vars)
			    (when (or (pair? set)
				      (pair? unused))
			      (let ((proper-args (args->proper-list args)))
				(let ((sig (var-signature fvar))
				      (len (+ (length proper-args) 1)))
				  (if (not sig)
				      (set! sig (make-list len #t))
				      (if (< (length sig) len)
					  (set! sig (copy sig (make-list len #t)))))
				  (do ((siglist (cdr sig) (cdr siglist))
				       (arg proper-args (cdr arg)))
				      ((null? arg))
				    (if (memq (car arg) unused)
					(set-car! siglist 'unused-parameter?)
					(if (memq (car arg) set)
					    (set-car! siglist 'unused-set-parameter?))))

				  (set! (var-signature fvar) sig))))))
			(cons fvar env))))))))

    (define (check-bool-cond caller form c1 c2 env)
      ;; (cond (x #f) (#t #t)) -> (not x)
      ;; c1/c2 = possibly combined, so in (cond (x #t) (y #t) (else #f)), c1: ((or x y) #t), so -> (or x y)
      (and (len=2? c1)
	   (len>1? c2)
	   (memq (car c2) '(#t else))
	   (or (and (boolean? (cadr c1))
		    (or (and (null? (cddr c2))
			     (boolean? (cadr c2))
			     (not (equal? (cadr c1) (cadr c2))) ; handled elsewhere
			     (lint-format "perhaps ~A" caller
					  (lists->string form (if (eq? (cadr c1) #t)
								  (car c1)
								  (simplify-boolean (list 'not (car c1)) () () env)))))
			(and (not (cadr c1))   ; (cond (x #f) (else y)) -> (and (not x) y)
			     (let ((cc1 (simplify-boolean (list 'not (car c1)) () () env)))
			       (lint-format "perhaps ~A" caller
					    (lists->string form
							   (list 'and cc1
								 (if (null? (cddr c2))
								     (cadr c2)
								     (cons 'begin (cdr c2))))))))
			(and (pair? (car c1))  ; (cond ((null? x) #t) (else y)) -> (or (null? x) y)
			     (eq? (return-type (caar c1) env) 'boolean?)
			     (lint-format "perhaps ~A" caller
					  (lists->string form
							 (list 'or (car c1)
							       (if (null? (cddr c2))
								   (cadr c2)
								   (cons 'begin (cdr c2)))))))))
	       (and (boolean? (cadr c2))
		    (null? (cddr c2))
		    (not (equal? (cadr c1) (cadr c2)))
		    ;; (cond ((= 3 (length eq)) (caddr eq)) (else #f)) -> (and (= 3 (length eq)) (caddr eq))
		    (lint-format "perhaps ~A" caller
				 (lists->string form
						(if (cadr c2)
						    (list 'or (list 'not (car c1)) (cadr c1))
						    (if (and (pair? (car c1))
							     (eq? (caar c1) 'and))
							(append (car c1) (cdr c1))
							(cons 'and c1)))))))))

    (define (case-branch test eqv-select exprs)
      (cons (case (car test)
	      ((eq? eqv? = equal? char=?)
	       (list (unquoted ((if (equal? eqv-select (cadr test)) caddr cadr) test))))
	      ((memq memv member) (unquoted (caddr test)))
	      ((not)         '(#f))
	      ((null?)       '(()))
	      ((eof-object?) '(#<eof>))
	      ((zero?)       '(0 0.0))
	      ((boolean?)    '(#t #f))
	      ((char-ci=?)
	       (if (equal? eqv-select (cadr test))
		   (list (caddr test) (other-case (caddr test)))
		   (list (cadr test) (other-case (cadr test)))))
	      (else
	       (map (lambda (p)
		      (case (car p)
			((eq? eqv? = equal? char=?)
			 (unquoted ((if (equal? eqv-select (cadr p)) caddr cadr) p)))
			((memq memv member) (apply values (caddr p)))
			((not)              #f)
			((null?)            ())
			((eof-object?)      #<eof>)
			((zero?)            (values 0 0.0))
			((boolean?)         (values #t #f))
			((char-ci=?)
			 (if (equal? eqv-select (cadr p))
			     (values (caddr p) (other-case (caddr p)))
			     (values (cadr p) (other-case (cadr p)))))
			(else               (error 'wrong-type-arg "oops"))))
		    (cdr test))))

	    (cond ((not (null? (cdr exprs)))
		   exprs)

		((equal? eqv-select (car exprs))
		 ())

		((and (len=2? (car exprs))
		      (equal? eqv-select (cadar exprs)))
		 (list '=> (caar exprs)))

		(else exprs))))

    (define (cond->case eqv-select new-clauses)
      (cons 'case
	    (cons eqv-select
		  (map (lambda (clause)
			 (let ((test (car clause))
			       (exprs (cdr clause)))
			   (if (null? exprs)                   ; cond returns the test result if no explicit results
			       (set! exprs (list #t)))         ;   but all tests here return a boolean, and we win only if #t?? (memx is an exception)
			   (cond ((not (memq test '(else #t)))
				  (case-branch test eqv-select exprs))

			       ((not (null? (cdr exprs)))
				(cons 'else exprs))

			       ((equal? eqv-select (car exprs))
				(list 'else))

			       ((and (len=2? (car exprs))
				     (equal? eqv-select (cadar exprs)))
				(list 'else '=> (caar exprs)))

			       (else (cons 'else exprs)))))
		       new-clauses))))

    (define (eqv-code-constant? x)
      (or (number? x)
	  (char? x)
	  (and (len>1? x)
	       (eq? (car x) 'quote)
	       (or (symbol? (cadr x))
		   (and (not (pair? (cadr x)))
			(eqv-code-constant? (cadr x)))))
	  (memq x '(#t #f () #<unspecified> #<undefined> #<eof>))))

    (define (cond-eqv? clause eqv-select or-ok)
      (if (not (len>1? clause))
	  (memq clause '(else #t))
	  ;; it's eqv-able either directly or via memq/memv, or via (or ... eqv-able clauses)
	  ;;   all clauses involve the same (eventual case) selector
	  (case (car clause)
	    ((eq? eqv? = equal? char=? char-ci=?)
	     (and (pair? (cddr clause))
		  (if (eqv-code-constant? (cadr clause))
		      (equal? eqv-select (caddr clause))
		      (and (eqv-code-constant? (caddr clause))
			   (equal? eqv-select (cadr clause))))))

	    ((memq memv member)
	     (and (pair? (cddr clause))
		  (equal? eqv-select (cadr clause))
		  (pair? (caddr clause))
		  (eq? (caaddr clause) 'quote)
		  (or (not (eq? (car clause) 'member))
		      (lint-every? (lambda (x)
				     (or (number? x)
					 (char? x)
					 (symbol? x)
					 (memq x '(#t #f () #<unspecified> #<undefined> #<eof>))))
				   (cdaddr clause)))))
	    ((or)
	     (and or-ok
		  (lint-every? (lambda (p)
				 (cond-eqv? p eqv-select #f))
			       (cdr clause))))

	    ((not null? eof-object? zero? boolean?)
	     (equal? eqv-select (cadr clause)))

	    (else #f))))

    (define (partition-form start len)
      (let ((ps (make-vector len))
	    (qs (make-vector len)))
	(do ((i 0 (+ i 1))
	     (p start (cdr p)))
	    ((= i len))
	  (set! (ps i) (cadar p))
	  (set! (qs i) (reverse (cadar p))))

	(let ((header-len (length (ps 0))))
	  (let ((trailer-len header-len)
		(result-min-len header-len))
	    (do ((i 1 (+ i 1)))
		((= i len))
	      (set! result-min-len (min result-min-len (length (ps i))))
	      (do ((k 1 (+ k 1))
		   (p (cdr (ps i)) (cdr p))
		   (f (cdr (ps 0)) (cdr f)))
		  ((or (= k header-len)
		       (not (pair? p))
		       (not (equal? (car p) (car f))))
		   (set! header-len k)))
	      (do ((k 0 (+ k 1))
		   (q (qs i) (cdr q))
		   (f (qs 0) (cdr f)))
		  ((or (= k trailer-len)
		       (not (pair? q))
		       (not (equal? (car q) (car f))))
		   (set! trailer-len k))))

	    (when (= result-min-len header-len)
	      (set! header-len (- header-len 1))
	      (set! trailer-len 0))
	    (if (<= result-min-len (+ header-len trailer-len))
		(set! trailer-len (- result-min-len header-len 1)))

	    (values header-len trailer-len result-min-len)))))

    (define (one-call-and-dots body) ; body is unchanged here, so it's not interesting
      (if (< (tree-leaves body) 30)
	  (if (or (null? body)
		  (null? (cdr body)))
	      body
	      (list (car body) '...))
	  (if (pair? (car body))
	      (list (list (caar body) '...))
	      (list (car body) '...))))

    (define (replace-redundant-named-let caller form outer-name outer-args inner)
      (when (and (proper-list? outer-args)  ; can be null
		 (len>2? inner))
	(let ((inner-name (cadr inner))
	      (inner-args (caddr inner))
	      (inner-body (cdddr inner)))
	  (when (pair? inner-body)
	  (do ((p outer-args (cdr p))
	       (a inner-args (cdr a)))
	      ((or (null? p)
		   (not (pair? a))
		   (not (len=2? (car a)))
		   (pair? (caar a))
		   (and (not (eq? (car p) (caar a)))
			(tree-memq (car p) inner-body)))
	       ;; args can be reversed, but rarely match as symbols
	       (when (and (null? p)
			  (or (null? a)
			      (and (len=1? a)
				   (len>1? (car a))
				   (code-constant? (cadar a)))))
		 (let* ((args-match (do ((p1 outer-args (cdr p1))
					 (a1 inner-args (cdr a1)))
					((or (null? p1)
					     (null? (cdar a1))
					     (not (eq? (car p1) (caar a1)))
					     (not (eq? (caar a1) (cadar a1))))
					 (null? p1))))
			(args-aligned (and (not args-match)
					   (do ((p1 outer-args (cdr p1))
						(a1 inner-args (cdr a1)))
					       ((or (null? p1)
						    (not (eq? (car p1) (cadar a1))))
						(null? p1))))))
		   (when (or args-match args-aligned)
		     (let ((definer (if (null? a) 'define 'define*))
			   (extras (if (and (pair? a)
					    (quoted-null? (cadar a)))
				       (list (list (caar a) ()))
				       a)))
		       ;;     (define (f61 x) (let loop ((y x)) (if (positive? y) (loop (- y 1)) 0))) -> (define (f61 y) (if (positive? y) (f61 (- y 1)) 0))
		       (lint-format "~A ~A" caller
				    (if (null? a) "perhaps" "a toss-up -- perhaps")
				    (lists->string form
						   `(,definer (,outer-name
							       ,@(if args-match
								     outer-args
								     (do ((result ())
									  (p outer-args (cdr p))
									  (a inner-args (cdr a)))
									 ((null? p)
									  (reverse result))
								       (set! result (cons (caar a) result))))
							       ,@extras)
						      ,@(tree-subst outer-name inner-name inner-body))))))))))))))

    (denote (set-target name form env)
      (and (pair? form)
	   (or (and (pair? (cdr form))
		    (or (eq? (cadr form) name)    ; (pop! x)
			(and (pair? (cddr form))  ; (push! y x)
			     (eq? (caddr form) name)))
		    (or (eq? (car form) 'set!)    ; (set! x y)
			(lint-set!? form env)))
	       (set-target name (car form) env)
	       (set-target name (cdr form) env))))


    (define (check-definee caller sym form env)
      (cond ((not (pair? (cddr form))))

	    ((keyword? sym)               ; (define :x 1)
	     (lint-format "keywords are constants ~A" caller sym))

	    ((and (eq? sym 'pi)           ; (define pi (atan 0 -1))
		  (member (caddr form) '((atan 0 -1)
					 (acos -1)
					 (* 2 (acos 0))
					 (* 4 (atan 1))
					 (* 4 (atan 1 1)))))
	     (lint-format "~A is one of its many names, but pi is a predefined constant" caller (caddr form)))

	    ((constant? sym)              ; (define most-positive-fixnum 432)
	     (if (memv sym '(pi +nan.0 -nan.0 +inf.0 -inf.0
			     *unbound-variable-hook* *missing-close-paren-hook* *read-error-hook*
			     *load-hook* *error-hook* *rootlet-redefinition-hook*))
		 (lint-format "~A is a constant: ~A" caller sym form)))

	    ((eq? sym 'quote)
	     (lint-format "either a stray quote, or a really bad idea: ~A" caller (truncated-list->string form)))

	    ((pair? sym)
	     (check-definee caller (car sym) form env))

	    ((var-member sym env) => (lambda (v)
				       (if (and (eq? (var-definer v) 'define-constant)
						(len>2? form)
						(not (equal? (caddr form) (var-initial-value v))))
					   (let ((line (and (pair? (var-initial-value v))
							    (pair-line-number (var-initial-value v)))))
					     (lint-format "~A in ~A is already a constant, defined ~A~A" caller sym
							  (truncated-list->string form)
							  (if (and (integer? line) (> line 0)) (format #f "(line ~D): " line) "")
							  (truncated-list->string (var-initial-value v)))))))

	    ((memq sym '(else =>)) ; also in r7rs ... and _, but that is for syntax-rules
	     (lint-format "redefinition of ~A is a bad idea: ~A" caller sym (truncated-list->string form)))))

    (define walker-functions
      (let ((binders '(let let* letrec letrec* do
		       lambda lambda* define define*
		       call/cc call-with-current-continuation
		       define-macro define-macro* define-bacro define-bacro* define-constant define-expansion
		       load eval eval-string require))
    	    (walker-table (make-hash-table))
	    (lint-let-reduction-factor 3)) ; maybe make this a global switch -- the higher this number, the fewer let-reduction suggestions

	(define (hash-walker key value)
	  (if (hash-table-ref walker-table key)
	      (format *stderr* "~A already has a value: ~A~%" key (hash-table-ref walker-table key)))
	  (hash-table-set! walker-table key value))

	(define (check-pointless-let caller form body args)
	  ;; used in define-walker and lambda-walker
	  (when (pair? (cadr body))
	    (do ((inner-vars (cadr body))
		 (p args (cdr p)))
		((not (pair? p)))
	      (cond ((assq (car p) inner-vars) =>
		     (lambda (v)
		       (if (and (pair? (cdr v))
				(eq? (cadr v) (car p)))
			   (lint-format "in ~A this let binding is pointless: ~A" caller
					(truncated-list->string form)
					v))))))))

	(define (local-movable-funcs body largs)
	  (let ((ok-funcs ())
		(bad-funcs ())
		(all-bad #f))
	    (for-each (lambda (f)
			(if (not (func-definer? f))
			    (set! all-bad #t)
			    (let ((fname ((if (symbol? (cadr f)) cadr (if (pair? (cadr f)) caadr not)) f)))
			      (if (or all-bad
				      (not (symbol? fname))
				      (memq fname largs)
				      (not (pair? ((if (symbol? (cadr f)) (if (len>1? (caddr f)) caddr not) cadr) f)))
				      (let ((fargs (args->proper-list (if (symbol? (cadr f)) (cadr (caddr f)) (cdadr f)))))
					(tree-set-memq (remq-set fargs largs) (cddr f))))
				  (set! bad-funcs (cons fname bad-funcs))
				  (set! ok-funcs (cons (cons fname f) ok-funcs))))))
		      body)
	    (map (lambda (f)
		   (if (tree-set-memq bad-funcs (cdddr f))
		       (values)
		       f))
		 (reverse ok-funcs))))

	;; -------- local-funcs->closure --------
	(define local-funcs->closure
	  (let* ((rewrite-funcs
		  (let* ((funcs->list
			  (lambda (ok-funcs)
			    (map (lambda (f)
				   (let ((def (cdr f)))
				     (if (< (tree-leaves def) local-function-context)
					 def
					 (list (car def) (cadr def) '...))))
				 ok-funcs)))

			 (rewrite-define
			  (lambda (form ok-funcs outer-args let-case)
			    `(define ,(caadr form)       ; define* -> lambda* below
			       (,(or let-case 'let) ,(if let-case (funcs->list ok-funcs) ())
				 ,@(if let-case () (funcs->list ok-funcs))
				 (,(if (eq? (car form) 'define*) 'lambda* 'lambda)
				  ,(cdr outer-args)
				  ...)))))

			 (rewrite-lambda
			  (lambda (form ok-funcs outer-args let-case)
			    `(,(or let-case 'let) ,(if let-case (funcs->list ok-funcs) ())
			       ,@(if let-case () (funcs->list ok-funcs))
			       (,(car form) ,outer-args ...)))))

		    (lambda (caller form ok-funcs outer-args define-case let-case)
		      (let ((msg (string-append "the "
						(if let-case "local" "inner")
						" function~A ~{~A~^, ~} could be moved "
						(if define-case
						    "to ~A's closure: ~A"
						    "outside the ~A: ~A")))
			    (rewriter (if define-case rewrite-define rewrite-lambda))
			    (fname ((if define-case caadr car) form)))
			(lint-format msg caller
				     (if (null? (cdr ok-funcs)) "" "s")
				     (map car ok-funcs)
				     fname
				     (lists->string form (rewriter form ok-funcs outer-args let-case)))))))

		 (largs->let
		  (lambda (caller form body largs outer-args define-case)
		    (let ((ok-funcs (local-movable-funcs body largs)))
		      (when (pair? ok-funcs)
			(set! last-lambda-let body)
			(set! last-lambda-let-funcs ok-funcs)
			(rewrite-funcs caller form ok-funcs outer-args define-case #f)))))

		 (ok-func?
		  (lambda (var&val let-case largs)
		    (and (len=1? (cdr var&val))
			 (len>2? (cadr var&val))
			 (memq (caadr var&val) '(lambda lambda*))
			 (let* ((val (cdadr var&val))
				(fargs (args->proper-list (car val))))
			   (if (memq let-case '(letrec letrec*))
			       (set! fargs (cons (car var&val) fargs)))
			   (not (tree-set-memq (let remove-shadows ((args largs) (nargs ()))
						   (if (null? args)
						       nargs
						       (remove-shadows (cdr args)
								       (if (memq (car args) fargs)
									   nargs
									   (cons (car args) nargs)))))
						 (cdr val))))))))

	    (lambda (caller form outer-args define-case)
	      (let ((largs (args->proper-list outer-args))
		    (body ((if (string? (caddr form)) cdddr cddr) form)))
		(when (pair? body)
		  (if (func-definer? (car body))
		      (largs->let caller form body largs outer-args define-case)
		      (when (and (null? (cdr body))
				 (len>2? (car body))
				 (memq (caar body) '(let let* letrec letrec*))
				 (proper-list? (cadar body))
				 (just-pairs? (cadar body))
				 (pair? (cddar body)))
			(if (func-definer? (caddar body))
			    (largs->let caller form
					(cddar body)
					(if define-case
					    (cons (caadr form) (append largs (map car (cadar body))))
					    (append largs (map car (cadar body))))
					outer-args define-case))
			(let ((let-case (caar body)))            ; if not 'let, add locals to outer-args
			  (unless (or (eq? let-case 'let)
				      (and (eq? let-case 'letrec)
					   (lint-every? (lambda (p)
							  (ok-func? p let-case largs))
							(cadar body))))
			    (set! largs (append largs (map car (cadar body)))))
			  (do ((ok-funcs ())
			       (p (cadar body) (cdr p)))
			      ((null? p)
			       (when (pair? ok-funcs)
				 (if (and (memq let-case '(let* letrec*))
					  (null? (cdr ok-funcs)))
				     (set! let-case 'let))
				 (rewrite-funcs caller form ok-funcs outer-args define-case let-case)))
			    (if (ok-func? (car p) let-case largs)
				(set! ok-funcs (cons (cons (caar p) (car p)) ok-funcs))))))))))))


	;; ---------------- define and defmacro ----------------
	(let ()

	  (define (check-define-macro caller form env)
	    ;; used in define-walker and defmacro-walker
	    (let ((val (cddr form))
		  (outer-name (caadr form))
		  (outer-args (cdadr form)))
	      (when (len=1? val)
		(let ((body (car val)))
		  (if (and (null? outer-args)  ; look for C macros translated as define-macro! -- this happens a lot sad to say
			   (or (not (symbol? body))
			       (keyword? body))
			   (or (not (pair? body))
			       (and (eq? (car body) 'quote)
				    (pair? (cdr body))
				    (not (symbol? (cadr body)))
				    (not (unquoted-pair? (cadr body))))
			       (not (or (memq (car body) '(quote quasiquote list cons append))
					(tree-set-memq '(list-values apply-values append) body)))))
		      (lint-format "perhaps ~A or ~A" caller
				   (lists->string form (list 'define outer-name (unquoted body)))
				   (truncated-list->string (list 'define (list outer-name) (unquoted body)))))

		  (when (pair? body)
		    (let ((args (cdr body)))
		      (case (car body)
			((list-values) ; was list also briefly
			 (when (and (pair? args)
				    (quoted-symbol? (car args)))
			   (if (proper-list? outer-args)
			       (if (and (equal? (cdr args) outer-args)
					(or (not (hash-table-ref syntaces (cadar args))) ; (define-macro (x y) `(lambda () ,y))
					    (memq (cadar args) '(set! define))))
				   (lint-format "perhaps ~A" caller                ; (define-macro (fx x) `(abs ,x)) -> (define fx abs)
						(lists->string form (list 'define outer-name (cadar args))))

				   (if (and (not (hash-table-ref syntaces (cadar args)))
					    (not (any-macro? (cadar args) env))
					    (lint-every? (lambda (a)
							   (or (code-constant? a)
							       (and (memq a outer-args)
								    (= (tree-count a (cdr args) 2) 1))))
							 (cdr args)))
				       ;; marginal -- there are many debatable cases here
				       (lint-format "perhaps ~A" caller
						    (lists->string form `(define (,outer-name ,@outer-args)
									   (,(cadar args) ,@(map unquoted (cdr args))))))))

			       (if (or (and (symbol? outer-args)          ; (define-macro (f . x) `(+ ,@x)) -> (define f +)
					    (len=2? args)
					    (len=2? (cadr args))
					    (eq? (caadr args) 'apply-values)
					    (eq? (cadadr args) outer-args))
				       (and (eqv? (length outer-args) -1) ; (define-macro (f a . x) `(+ a ,@x)) -> (define f +)
					    (len=3? args)
					    (eq? (cadr args) (car outer-args))
					    (len=2? (caddr args))
					    (eq? (caaddr args) 'apply-values)
					    (eq? (cadr (caddr args)) (cdr outer-args))))
				   (lint-format "perhaps ~A"  caller
						(lists->string form (list 'define outer-name (cadar args)))))))

			 (let ((pargs (args->proper-list outer-args)))
			   (for-each (lambda (p)
				       (if (and (quoted-pair? p)
						(tree-set-memq pargs (cadr p)))
					   (lint-format "missing comma? ~A" caller form)))
				     args)))

			((quote)
			 ;; extra comma (unquote) is already caught elsewhere
			 (if (and (pair? args)
				  (pair? (car args))
				  (tree-set-memq (args->proper-list outer-args) (car args)))
			     (lint-format "missing comma? ~A" caller form))))))))))


	  ;; -------- uncurry --------
	  (define (uncurry caller form env)
	    (let ((sym (cadr form))
		  (val (cddr form))
		  (head (car form)))
	      (let ((outer-args (cdr sym))
		    (outer-name (if (eq? head 'define*)
				    (remove-one :optional (car sym))
				    (car sym))))
		(if (symbol? (car outer-name))
		    ;; perhaps a curried definition -- as a public service, we'll rewrite the dumb thing
		    (begin
		      (lint-format "perhaps ~A" caller
				   (lists->string form `(,head ,outer-name
							       (lambda ,outer-args
								 ,@(cddr form)))))
		      (lint-walk-function head (car outer-name) (cdr outer-name) val form env))
		    (when (pair? (car outer-name))
		      (if (symbol? (caar outer-name))
			  (begin
			    (lint-format "perhaps ~A" caller
					 (lists->string form `(,head ,(car outer-name)
								     (lambda ,(cdr outer-name)
								       (lambda ,outer-args
									 ,@(cddr form))))))
			    (lint-walk-function head (caar outer-name) (cdar outer-name) val form env))
			  (if (and (pair? (caar outer-name))
				   (symbol? (caaar outer-name)))
			      (begin
				(lint-format "perhaps ~A" caller
					     (lists->string form `(,head ,(caar outer-name)
									 (lambda ,(cdar outer-name)
									   (lambda ,(cdr outer-name)
									     (lambda ,outer-args
									       ,@(cddr form)))))))
				(lint-walk-function head (caaar outer-name) (cdaar outer-name) val form env))
			      ;; no hits beyond that case
			      env)))))))

	  ;; --------check-built-in-used-as-parameter --------
	  (define (check-built-in-used-as-parameter caller form)
	    ;; look for a built-in name used as a parameter name and used as a function internally(!)
	    ;;   this requires a tree walker to ignore (for example) (let loop ((string string))...)
	    (for-each (lambda (p)
			(let ((par (if (pair? p) (car p) p)))
			  (when (or (hash-table-ref built-in-functions par)
				    (hash-table-ref syntaces par))
			    (let ((call (call-with-exit
					 (lambda (return)
					   (let loop ((tree (cddr form)))
					     (if (pair? tree)
						 (if (eq? (car tree) par)
						     (return tree)
						     (case (car tree)
						       ((quote) #f)
						       ((let let*)
							(if (len>1? (cdr tree))
							    (if (symbol? (cadr tree))
								(if (not (tree-memq par (caddr tree)))
								    (loop (cdddr tree)))
								(if (not (tree-memq par (cadr tree)))
								    (loop (cddr tree))))))
						       ((letrec letrec*)
							(if (and (pair? (cdr tree))
								 (not (tree-memq par (cadr tree))))
							    (loop (cddr tree))))
						       ((do)
							(if (and (len>1? (cdr tree))
								 (not (tree-memq par (cadr tree))))
							    (loop (cdddr tree))))
						       (else
							(if (pair? (cdr tree))
							    (for-each loop (cdr tree)))
							(if (pair? (car tree))
							    (loop (car tree))))))))))))
			      (if (and (len>1? call)
				       (not (eq? par (cadr call)))) ; (define (f50 abs) (abs -1))
				  (lint-format "~A's parameter ~A is called ~A: find a less confusing parameter name!" caller
					       (caadr form) par (truncated-list->string call)))))))
		      (cdadr form)))

	  ;; -------- inner-define->let --------
	  (define (inner-define->let caller form)
	    ;; perhaps this block should be on a *report-* switch --
	    ;;   it translates some internal defines into named lets
	    ;;   (or just normal lets, etc)
	    ;; this is not redundant given the walk-body translations because here
	    ;;   we have the outer parameters and can check those against the inner ones
	    ;;   leading (sometimes) to much nicer rewrites.
	    (let ((val (cddr form))
		  (outer-args (cdadr form))
		  (outer-name (caadr form)))
	      (when (and (eq? (caar val) 'define) ; define* does not happen here
			 (pair? (cdr val))
			 (pair? (cadar val)))     ; inner define (name ...)
		(let ((inner-name (caadar val))
		      (inner-args (cdadar val))
		      (inner-body (cddar val))
		      (outer-body (cdddr form)))
		  (when (and (symbol? inner-name)
			     (proper-list? inner-args)
			     (pair? (car outer-body))
			     (= (tree-count inner-name outer-body 2) 1))
		    (let ((call (find-call inner-name outer-body)))
		      (when (pair? call)
			(set! last-rewritten-internal-define (car val))
			(let ((new-call (if (tree-memq inner-name inner-body)
					    (if (and (null? inner-args)
						     (null? outer-args))
						(if (null? (cdr inner-body))
						    (car (tree-subst outer-name inner-name inner-body))
						    (cons 'begin (tree-subst outer-name inner-name inner-body)))
						`(let ,inner-name
						   ,(if (null? inner-args) () (map list inner-args (cdr call)))
						   ,@inner-body))
					    (if (or (null? inner-args)
						    (and (equal? inner-args outer-args)
							 (equal? inner-args (cdr call))))
						(if (null? (cdr inner-body))
						    (car (tree-subst outer-name inner-name inner-body))
						    (cons 'begin (tree-subst outer-name inner-name inner-body)))
						(cons 'let
						      (cons (map list inner-args (cdr call))
							    inner-body))))))
			  ;; (define (f11 a b) (define (f12 a b) (if (positive? a) (+ a b) b)) (f12 a b)) ->
			  ;;    (define (f11 a b) (if (positive? a) (+ a b) b))
			  (lint-format "perhaps ~A" caller
				       (lists->string form
						      (cons (car form)
							    (cons (cadr form)
								  (let ((p (tree-subst new-call call outer-body)))
								    (if (and (pair? p)
									     (pair? (car p))
									     (eq? (caar p) 'begin))
									(cdar p)
									p))))))))))))))

	  ;; -------- letrec->define --------
	  (define (letrec->define caller form)
	    ;; (define x (letrec ((y (lambda...))) (lambda (...) (y...)))) -> (define (x...)...)
	    ;;     let as well as letrec
	    (when (memq (caaddr form) '(let letrec))
	      (let* ((let-form (cdaddr form))
		     (var (and (len=1? (car let-form))  ; just one var in let/rec
			       (caar let-form))))
		;; let-form here can be cdr of (lambda...) or (let|letrec ... lambda)
		(when (and (len>1? var)
			   (symbol? (car var))
			   (len=1? (cdr let-form))      ; just one form in the let/rec
			   (pair? (cadr let-form))
			   (len>1? (cadr var))
			   (eq? (caadr var) 'lambda)    ; var is lambda
			   (proper-list? (cadadr var))) ; it has no rest arg
		  (let ((body (cadr let-form))          ; the lambda for the outer define ("x" above)
			(sym (cadr form)))
		    (when (and (eq? (car body) 'lambda)     ; let/rec body is lambda calling var
			       (proper-list? (cadr body))   ; rest args are a headache
			       (pair? (caddr body))    ; (lambda (...) (...) where car is letrec func name
			       (= (tree-count (car var) body 2) 1)) ; if more than 1 call, a named-let won't suffice
		      (if (eq? (caaddr body) (car var))
			  (lint-format "perhaps ~A" caller
				       (lists->string form
						      `(define (,sym ,@(cadr body))
							 (let ,(car var)
							   ,(map list (cadadr var) (cdaddr body))
							   ,@(cddadr var)))))
			  (let ((call (find-call (car var) (caddr body))))
			    (when (and (pair? call)       ; inner lambda body is (...some-expr...(sym...) ...)
				       (= (tree-count (car var) (caddr body) 2) 1))
			      (let ((new-call `(let ,(car var)
						 ,(map list (cadadr var) (cdr call))
						 ,@(cddadr var))))
				(lint-format "perhaps ~A" caller
					     (lists->string form
							    `(define (,sym ,@(cadr body))
							       ,(tree-subst new-call call
									    (caddr body)))))))))))))))

	  ;; -------- check-boolean-function --------
	  (define (check-boolean-function caller form env)
	    (let ((sym (caadr form))
		  (val (cddr form)))
	      (when (and (symbol? sym)
			 (let ((sym-name (symbol->string sym)))
			   (char=? #\? (sym-name (- (length sym-name) 1)))))
		(catch 'one-is-enough
		  (lambda ()
		    (return-walker (last-ref val)
				   (lambda (last)
				     (when (or (and (code-constant? last)
						    (not (boolean? last))
						    (not (and (len=2? last)
							      (eq? (car last) 'quote)
							      (boolean? (cadr last)))))
					       (and (pair? last)
						    (let ((sig (arg-signature (car last) env)))
						      (and (pair? sig)
							   (not (if (pair? (car sig))
								    (tree-set-member '(boolean? #t values) (car sig))
								    (memq (car sig) '(boolean? #t values))))))))
				       (lint-format "~A looks boolean, but it can return ~A" caller
						    sym
						    (truncated-list->string last))
				       (throw 'one-is-enough)))))
		  (lambda args #f)))))

	  ;; -------- rewrite-let-optionals --------
	  (define (rewrite-let-optionals caller form outer-name outer-args val)
	    (let ((args (args->proper-list outer-args)))
	      (if (and (eq? (cadar val) (last-ref args))
		       (lint-every? len=2? (caddar val))) ; some seem to include a type check?
		  (lint-format "perhaps ~A" caller
			       (lists->string form
					      `(define* (,outer-name
							 ,@(copy args (make-list (- (length args) 1)))
							 ,@(map (lambda (p)
								  (if (cadr p) p (car p))) ; remove #f default vals
								(caddar val)))
						 ...))))))

	  ;; -------- define-walker --------
	  (define (define-walker caller form env)
	    (if (< (length form) 2)
		(begin
		  (lint-format "~S makes no sense" caller form)
		  env)
		(let ((sym (cadr form))
		      (val (cddr form))
		      (head (car form)))
		  (if (symbol? sym)
		      (begin
			(check-definee caller sym form env)
			(if (memq head '(define define-constant define-envelope
					 define-public define*-public defmacro-public define-inlinable
					 define-integrable define^))
			    (let ((len (length form)))
			      (if (not (= len 3))  ;  (define a b c)
				  (lint-format "~A has ~A value~A?"
					       caller (truncated-list->string form)
					       (if (< len 3)
						   (values "no" "")
						   (values "too many" "s")))))
			    (lint-format "~A is messed up" caller (truncated-list->string form)))

			(if (not (pair? val))
			    env
			    (begin
			      (if (and (null? (cdr val))
				       (equal? sym (car val))) ; (define a a)
				  (lint-format "this ~A is either not needed, or is an error: ~A" caller head (truncated-list->string form)))

			      (if (not (pair? (car val)))
				  (begin
				    (cond ((and (not (memq caller '(module cond-expand)))
						(hash-table-ref other-identifiers sym))
					   => (lambda (p)
						(lint-format "~A is used before it is defined: ~A" caller sym form))))
				    (cons (make-lint-var sym (car val) head) env))

				  (let ((e (lint-walk (if (and (pair? (car val))
							       (eq? (caar val) 'letrec))
							  'define sym)
						      (car val) env)))
				    (if (or (not (pair? e))
					    (eq? e env)
					    (not (memq (var-name (car e)) '(:lambda :dilambda)))) ; (define x (lambda ...))
					(cons (make-lint-var sym (car val) head) env)
					(begin
					  (if (and *report-recursion->iteration* (eq? (var-name (car e)) :lambda))
					      (recursion->iteration sym
								    (var-ftype (car e))
								    (var-arglist (car e))
								    (var-initial-value (car e))
								    e))
					  (set! (var-name (car e)) sym)

					  (let ((val (caddr form)))
					    (when (and (eq? (car val) 'lambda) ; (define sym (lambda args (let name...))), let here happens rarely
						       (proper-list? (cadr val))
						       (len>1? (caddr val))
						       (null? (cdddr val))
						       (eq? (caaddr val) 'let)
						       (symbol? (cadr (caddr val))))
					      (replace-redundant-named-let caller form sym (cadr val) (caddr val))))

					  (letrec->define caller form)
					  e))))))) ; symbol? sym

		      (begin                       ; not (symbol? sym)

			(when (and (memq head '(define define*)) ; can't include define-macro et al because we use lambda(*) as rewrite
				   (pair? val)
				   (pair? sym))
			  (local-funcs->closure caller form sym #t))

			(cond ((not (and (pair? sym)
					 (pair? val)))
			       (lint-format "strange form: ~A" head (truncated-list->string form)))

			      ((pair? (car sym))
			       (uncurry caller form env))  ; curried func info thrown away (not included in returned env)

			      (else
			       (cond ((not *report-forward-functions*))
				     ;; need to ignore macro usages here -- this happens ca 20000 times!
				     ((hash-table-ref other-identifiers (car sym))
				      => (lambda (p)
					   (lint-format "~A is used before it is defined" caller (car sym)))))

			       (if *report-boolean-functions-misbehaving*
				   (check-boolean-function caller form env))

			       (check-definee caller (car sym) form env)

			       (let ((outer-args (cdr sym))
				     (outer-name (car sym)))
				 (when (len>1? (car val))
				   (when (eq? (caar val) 'let)
				     (check-pointless-let caller form (car val) outer-args)

				     ;; define + redundant named-let -- sometimes rewrites to define*
				     (when (and (symbol? (cadar val))
						(null? (cdr val)))
				       (replace-redundant-named-let caller form outer-name outer-args (car val))))

				   (inner-define->let caller form)

				   (if (memq (caar val) '(let-optionals let-optionals*))
				       (rewrite-let-optionals caller form outer-name outer-args val)))

				 (when (pair? outer-args)
				   (if (repeated-member? (proper-list outer-args) env)
				       (lint-format "~A parameter is repeated: ~A" caller head (truncated-list->string sym)))

				   (cond ((memq head '(define* define-macro* define-bacro* define*-public))
					  (check-star-parameters outer-name outer-args env))
					 ((any-keywords? outer-args)
					  (lint-format "~A parameter can't be a keyword: ~A" caller outer-name sym))
					 ((memq 'pi outer-args)
					  (lint-format "~A parameter can't be a constant: ~A" caller outer-name sym)))

				   (check-built-in-used-as-parameter caller form))

				 (when (eq? head 'define-macro)
				   (check-define-macro caller form env))

				 (if (and (eq? head 'definstrument)
					  (string? (car val)))
				     (set! val (cdr val)))

				 (if (not (keyword? outer-name))
				     (set! env (lint-walk-function head outer-name outer-args val form env))))))
			env)))))

	  (for-each (lambda (op)
		      (hash-walker op define-walker))
		    '(define define* define-constant
		      define-macro define-macro* define-bacro define-bacro* define-expansion
		      definstrument define-animal define-envelope        ; for clm
		      define-public define*-public defmacro-public define-inlinable
		      define-integrable define^))                   ; these give more informative names in Guile and scmutils (MIT-scheme))


	  ;; -------- defmacro-walker --------
	  (define (defmacro-walker caller form env)
	    (if (or (< (length form) 4)
		    (not (symbol? (cadr form))))
		(begin
		  (if (and (> (length form) 2)
			   (pair? (cadr form))
			   (symbol? (caadr form)))
		      (lint-format "~A used where s7 uses define-~A: ~A?" caller (car form) (car form) (truncated-list->string form))
		      (lint-format "~A declaration is messed up: ~A" caller (car form) (truncated-list->string form)))
		  env)
		(let ((sym (cadr form))
		      (args (caddr form))
		      (body (cdddr form))
		      (head (car form)))
		  (if (and (pair? args)
			   (repeated-member? args env))                        ; (defmacro hi (a b a) a)
		      (lint-format "~A parameter is repeated: ~A" caller head (truncated-list->string args))
		      (lint-format "~A is deprecated; perhaps ~A" caller head  ; (defmacro hi (a b) `(+ ,a ,b))
				   (truncated-lists->string form
							    (cons (if (eq? head 'defmacro) 'define-macro 'define-macro*)
								  (cons (cons sym
									      (let no-key ((lst args)) ; remove :key and :optional
										(if (not (pair? lst))
										    lst
										    (if (memq (car lst) '(:key :optional))
											(no-key (cdr lst))
											(cons (car lst)
											      (no-key (cdr lst)))))))
									body)))))
		  (if (eq? head 'defmacro)
		      (check-define-macro caller
					  (cons 'define-macro
						(cons (cons sym args)
						      body))
					  env))

		  (lint-walk-function head sym args body form env)
		  (cons (make-lint-var sym form head) env))))

	  (hash-walker 'defmacro defmacro-walker)
	  (hash-walker 'defmacro* defmacro-walker))


	;; ---------------- dilambda ----------------
	(let ()
	  (define (dilambda-walker caller form env)
	    (let ((len (length form)))
	      (if (not (= len 3))
		  (begin
		    (lint-format "dilambda takes two arguments: ~A" caller (truncated-list->string form))
		    env)
		  (let ((getter (cadr form))
			(setter (caddr form)))
		    (check-call caller 'dilambda form env)
		    (lint-walk caller setter env)
		    (let ((e (lint-walk caller getter env))) ; goes to lint-walk-function -> :lambda as first in e
		      (if (and (pair? e)
			       (eq? (var-name (car e)) :lambda))
			  (set! (var-name (car e)) :dilambda))
		      e)))))

	  (hash-walker 'dilambda dilambda-walker))


	;; ---------------- lambda, lambda* ----------------
	(let ()

	  ;; -------- lambda-walker --------
	  (define (lambda-walker caller form env)
	    (let ((len (length form))
		  (head (car form)))
	      (if (or (< len 3)
		      (not (or (list? (cadr form))
			       (symbol? (cadr form)))))
		  (begin
		    (lint-format "~A is messed up in ~A" caller head (truncated-list->string form))
		    env)
		  (let ((args (cadr form))
			(arglen (length (cadr form))))

		    ;; parameter error checks
		    (when (list? args)
		      (if (repeated-member? (proper-list args) env)
			  (lint-format "~A parameter is repeated: ~A" caller head (truncated-list->string args)))
		      (if (eq? head 'lambda*)                 ; (lambda* (a :b) ...)
			  (check-star-parameters head args env)
			  (if (any-keywords? args)            ; (lambda (:key) ...)
			      (lint-format "lambda arglist can't handle keywords (use lambda*)" caller))))

		    (if (and (eq? head 'lambda*)              ; (lambda* ()...) -> (lambda () ...)
			     (null? args))                    ; (lambda* args ...) is caught elsewhere
			(lint-format "lambda* could be lambda ~A" caller form))

		    (when (pair? (cddr form))
		      (local-funcs->closure caller form args #f))

		    (when (or (= len 3)
			      (and (= len 4)
				   (string? (caddr form))))
		      (let ((body ((if (= len 3) caddr cadddr) form)))

			;; lambda -> body if rest args and apply
			(if (or (symbol? args)                 ; (lambda args (apply f args)) -> f
				(and (pair? args)              ; (lambda #\a ...) !
				     (negative? arglen)))
			    (if (and (len>2? body)
				     (eq? (car body) 'apply)
				     (symbol? (cadr body))
				     (not (memq (cadr body) '(and or)))
				     (or (eq? args (caddr body))
					 (and (pair? args)
					      (equal? (cddr body) (proper-list args)))))
				;; (lambda args (apply + args)) -> +
				(lint-format "perhaps ~A" caller (lists->string form (cadr body))))

			    (when (list? args)
			      ;; complain if let rebinds parameter uselessly
			      (when (and (len>1? body)
					 (eq? (car body) 'let))
				(check-pointless-let caller form body (cadr form)))

			      ;; lambda -> body when args are straightforward
			      ;;   (lambda () (f)) -> f, (lambda (a b) (f a b)) -> f
			      (cond ((not (and (pair? body)
					       (symbol? (car body))
					       (not (memq (car body) '(and or))))))

				    ((equal? args (cdr body))           ; (lambda (a b) (> a b)) -> >
				     (lint-format "perhaps ~A" caller (lists->string form (car body))))

				    ((and (eq? (car body) 'not)
					  (pair? (cdr body))
					  (pair? (cadr body))
					  (hash-table-ref notables (caadr body))
					  (equal? args (cdadr body)))   ; (lambda (a b) (not (> a b))) -> <=
				     (lint-format "perhaps ~A" caller (lists->string form (hash-table-ref notables (caadr body)))))

				    ((equal? (reverse args) (cdr body)) ; (lambda (a b) (> b a)) -> <
				     (let ((rf (hash-table-ref reversibles (car body))))
				       (if rf (lint-format "perhaps ~A" caller (lists->string form rf)))))

				    ((and (= arglen 1)                  ; (lambda (x) (cdr (cdr (car x)))) -> cddar
					  (hash-table-ref combinable-cxrs (car body)))
				     ((lambda* (cr arg) ; lambda* not lambda because combine-cxrs might return just #f
					(and cr
					     (< (length cr) 5)
					     (eq? (car args) arg)
					     (lint-format "perhaps ~A" caller
							  (lists->string form (symbol "c" cr "r")))))
				      (combine-cxrs body))))))))

		    (lint-walk-function head caller args (cddr form) form env)
		    ;; not env as return value here -- return the lambda+old env via lint-walk-function
		    ))))

	  (hash-walker 'lambda lambda-walker)
	  (hash-walker 'lambda* lambda-walker))


	;; ---------------- set! ----------------
	(let ()
	 (define (set-walker caller form env)
	   (if (not (= (length form) 3))
	       (begin
		 (lint-format "set! has too ~A arguments: ~S" caller (if (> (length form) 3) "many" "few") form)
		 env)
	       (let ((settee (cadr form))
		     (setval (caddr form)))
		 (if (symbol? setval)
		     (set-ref setval caller form env))
		 (let ((result (lint-walk caller setval env)))

		   (cond ((symbol? settee)
			  (if (constant? settee)  ; (set! pi 3)
			      (lint-format "can't set! ~A (it is a constant)" caller (truncated-list->string form))
			      (let ((v (var-member settee env)))
				(cond (v
				       (if (eq? (var-definer v) 'define-constant)
					   (let ((line (and (pair? (var-initial-value v))
							    (pair-line-number (var-initial-value v)))))
					     (lint-format "can't set! ~A in ~A (it is a constant: ~A~A)" caller settee
							  (truncated-list->string form)
							  (if (and (integer? line) (> line 0)) (format #f "(line ~D): " line) "")
							  (truncated-list->string (var-initial-value v))))))

				      ((and (not lint-in-with-let)
					    (hash-table-ref built-in-functions settee))
				       (lint-format "not recommended: ~A" caller (truncated-list->string form)))

				      ((or (hash-table-ref syntaces settee)
					   (memq settee '(else => ... _))) ; r7rs says (set! else #f) is an error
				       (lint-format "bad idea: ~A" caller (truncated-list->string form)))))))

			 ((not (pair? settee))  ; (set! 3 1)
			  (lint-format "can't set! ~A" caller (truncated-list->string form)))

			 (else
			  (when (proper-list? settee)
			    (let ((target (car settee)))
			      (cond ((memq target '(vector-ref list-ref string-ref hash-table-ref int-vector-ref float-vector-ref byte-vector-ref let-ref))
				     ;; (set! (vector-ref v 0) 3)
				     (lint-format "perhaps ~A" caller
						  (truncated-lists->string
						   form
						   (cons (case target
							   ((vector-ref) 'vector-set!)
							   ((int-vector-ref) 'int-vector-set!)
							   ((float-vector-ref) 'float-vector-set!)
							   ((byte-vector-ref) 'byte-vector-set!)
							   ((list-ref) 'list-set!)
							   ((string-ref) 'string-set!)
							   ((hash-table-ref) 'hash-table-set!)
							   ((let-ref) 'let-set!)
							   (else 'error))
							 (append (cdadr form) (cddr form))))))

				    ((and (eq? target 'setter)
					  (len>1? setval)
					  (eq? (car setval) 'lambda)
					  (list? (cadr setval))
					  (not (= (length (cadr setval)) 2)))
				     (lint-format "setter function should take 2 arguments: ~A" caller (truncated-list->string form)))

				    ((or (string? target)
					 (vector? target))
				     (lint-format "~S is a constant so ~A is problematic" caller target (truncated-list->string form))))))

			  (lint-walk caller settee env) ; this counts as a reference since it's by reference so to speak

			  ;; try type check (dilambda signatures)
			  (when (symbol? (car settee))
			    (let ((f (symbol->value (car settee) *e*)))
			      (when (dilambda? f)
				(let ((sig (signature (setter f)))
				      (settee-len (length settee)))
				  (when (and (pair? sig)
					     (positive? settee-len)
					     (pair? (list-tail sig settee-len)))
				    (let ((checker (list-ref sig settee-len))
					  (arg-type (->lint-type setval)))
				      (when (and (symbol? checker)
						 (not (compatible? checker arg-type)))  ; (set! (print-length) "asd")
					(lint-format "~A: new value should be a~A ~A: ~S: ~A"
						     caller (car settee)
						     (if (char=? (string-ref (object->string checker #f) 0) #\i) "n" "")
						     checker arg-type
						     (truncated-list->string form)))))))))
			  (set! settee (do ((sym (car settee) (car sym)))
					   ((not (pair? sym)) sym)))))

		   (if (symbol? (cadr form)) ; see do directly above -- sets settee so we have to go back to (cadr form)
		       (set-set (cadr form) caller form env)
		       (if (and (pair? (cadr form))
				(symbol? settee))
			   (set-ref settee caller (cons 'implicit-set (cdr form)) env)))

		   (if (or (equal? (cadr form) setval) ; not settee here!          ;  (set! a a)
			   (and (symbol? (cadr form))
				(equal? (caddr form) (list 'copy (cadr form)))))   ;  (set! a (copy a))
		       (lint-format "pointless set! ~A" caller (truncated-list->string form)))

		   (when (and (pair? setval)
			      (symbol? settee))
		     (case (car setval)
		       ((if)                        ; (set! x (if y x 1)) -> (if (not y) (set! x 1))
			(if (= (length setval) 4)
			    (if (eq? settee (caddr setval))
				(lint-format "perhaps ~A" caller
					     (lists->string form `(if (not ,(cadr setval)) (set! ,settee ,(cadddr setval)))))
				(if (eq? settee (cadddr setval))
				    (lint-format "perhaps ~A" caller
						 (lists->string form `(if ,(cadr setval) (set! ,settee ,(caddr setval)))))))))

		       ((cond)                      ; (set! x (cond (z w) (else x))) -> (if z (set! x w)) -- this never happens
			(if (and (= (length setval) 3)
				 (pair? (caddr setval))
				 (memq (caaddr setval) '(#t else))
				 (null? (cddr (caddr setval)))
				 (null? (cddadr setval)))
			    (if (eq? (cadr (caddr setval)) (cadr form))
				(lint-format "perhaps ~A" caller
					     (lists->string form `(if ,(caadr setval) (set! ,(cadr form) ,(cadadr setval)))))
				(if (eq? (cadadr setval) (cadr form))
				    (lint-format "perhaps ~A" caller
						 (lists->string form `(if (not ,(caadr setval)) (set! ,(cadr form) ,(cadr (caddr setval))))))))))

		       ((append)                    ; in do loop, (set! sym (append sym ...)) -> (set! sym (cons ... sym)) + reverse later
			(cond ((var-member :do env) =>
			       (lambda (v)
				 (let search ((tree (cddr (var-initial-value v))))
				   (and (pair? tree)
					(if (and (eq? (car tree) 'set!)
						 (pair? (cdr tree))        ; (set!)?
						 (symbol? (cadr tree))
						 (pair? (cddr tree))
						 (pair? (caddr tree))
						 (eq? (caaddr tree) 'append)
						 (pair? (cdaddr tree))
						 (eq? (cadr tree) (cadr (caddr tree))))
					    (lint-format "perhaps (set! ~A (append ~A ...)) -> (set! ~A (cons ... ~A)), reversing ~A at the end"
							 caller (cadr tree) (cadr tree) (cadr tree) (cadr tree) (cadr tree))
					    (or (search (car tree))
						(search (cdr tree))))))))))

		       ((or)                        ; (set! x (or x y)) -> (if (not x) (set! x y))
			(if (and (= (length setval) 3)   ;    the other case here is not improved by using 'if
				 (eq? settee (cadr setval)))
			    (lint-format "perhaps ~A" caller
					 (lists->string form `(if (not ,settee) (set! ,settee ,(caddr setval)))))))

		       ((and)
			(if (= (length setval) 3)   ; (set! x (and x y)) -> (if x (set! x y))
			    (if (eq? settee (cadr setval))
				(lint-format "perhaps ~A" caller
					     (lists->string form `(if ,settee (set! ,settee ,(caddr setval)))))
				(if (eq? settee (caddr setval))
				    (lint-format "perhaps ~A" caller
						 (lists->string form `(if (not ,(cadr setval)) (set! ,settee #f))))))))))
		   result))))
	 (hash-walker 'set! set-walker))


	;; ---------------- quote ----------------
	(let ()
	 (define (quote-walker caller form env)
	   (let ((len (length form)))
	     (cond ((negative? len)
		    (lint-format "stray dot in quote's arguments? ~S" caller form))

		   ((not (= len 2))
		    (lint-format "quote has too ~A arguments: ~S" caller (if (> len 2) "many" "few") form))

		   (else
		    (let ((arg (cadr form)))
		      (if (pair? arg)
			  (if (> (length arg) 8)
			      (hash-table-set! big-constants arg (+ 1 (or (hash-table-ref big-constants arg) 0))))
			  (unless (or (>= quote-warnings 20)
				      (and (symbol? arg)
					   (not (keyword? arg))))
			    (set! quote-warnings (+ quote-warnings 1))           ; (char? '#\a)
			    (lint-format "quote is not needed here: ~A~A" caller ; this is by far the most common message from lint
					 (truncated-list->string form)
					 (if (= quote-warnings 20) "; will ignore this error henceforth." ""))))))))
	   env)
	 (hash-walker 'quote quote-walker))


	;; ---------------- if ----------------
	(let ()

	  ;; -------- sensible-if? --------
	  (define (sensible-if? caller form test true false env)

	    (if (eq? false #<unspecified>) ; true as #<unspecified> got no hits
		(lint-format "this #<unspecified> is redundant: ~A" caller form))

	    (if (never-false test)
		(lint-format "if test is never false: ~A" caller (truncated-list->string form))
		(if (and (never-true test) true) ; complain about (if #f #f) later, (if #f x y)
		    (lint-format "if test is never true: ~A" caller (truncated-list->string form))))

	    (if (and (symbol? test)
		     (pair? true)
		     (memq test true))
		(and-incomplete form 'if test true env)
		(when (len>1? test)
		  (if (and (eq? (car test) 'not)
			   (symbol? (cadr test))
			   (pair? false)
			   (memq (cadr test) false))
		      (and-incomplete form 'if2 (cadr test) false env))
		  (if (and (hash-table-ref bools (car test))
			   (pair? true))
		      (if (member (cadr test) true)
			  (and-forgetful form 'if test true env)
			  (do ((p true (cdr p)))
			      ((or (not (pair? p))
				   (and (pair? (car p))
					(member (cadr test) (car p))))
			       (if (pair? p)
				   (and-forgetful form 'if test (car p) env)))))
		      (if (and (eq? (car test) 'not)
			       (len>1? (cadr test))
			       (pair? false)
			       (hash-table-ref bools (caadr test)))
			  (if (member (cadadr test) false)
			      (and-forgetful form 'if2 (cadr test) false env)
			      (do ((p false (cdr p)))
				  ((or (not (pair? p))
				       (and (pair? (car p))
					    (member (cadadr test) (car p))))
				   (if (pair? p)
				       (and-forgetful form 'if2 (cadr test) (car p) env))))))))))


	  ;; -------- move-if-inward --------
	  (define (move-if-inward caller form test true false env)
	    (let ((true-op (and (pair? true) (car true)))
		  (true-rest (and (pair? true) (cdr true)))
		  (false-op (and (pair? false) (car false)))
		  (false-rest (and (pair? false) (cdr false))))
	      ;; move-if-inward
	      (when (and (pair? true)
			 (pair? false)
			 (pair? true-rest)
			 (not (memq true-op '(quote list-values not)))
			 (not (any-macro? true-op env))
			 (or (not (hash-table-ref syntaces true-op))
			     (memq true-op '(let let* set! and or begin))))

		(define (tree-subst-eq new old tree)
		  ;; tree-subst above substitutes every occurence of 'old with 'new, so we check
		  ;;   in advance that 'old only occurs once in the tree (via tree-count).  Here
		  ;;   'old may occur any number of times, but we want to change it only once,
		  ;;   so we keep the actual pointer to it and use eq?.  (This assumes no shared code?)
		  (cond ((eq? old tree)
			 (cons new (cdr tree)))
			((not (pair? tree))
			 tree)
			((eq? (car tree) 'quote)
			 (copy tree))
			(else (cons (tree-subst-eq new old (car tree))
				    (tree-subst-eq new old (cdr tree))))))

		(when (and (case true-op                      ; (if old (list form) (cons form old)) -> (cons form (if old () old)) etc
			     ((list) (eq? false-op 'cons))
			     ((cons) (eq? false-op 'list))
			     (else #f))
			   (pair? true-rest)
			   (pair? false-rest)
			   (equal? (car true-rest) (car false-rest)))
		  (if (eq? true-op 'list)
		      (when (null? (cdr true-rest))
			(set! true `(cons ,(car true-rest) ()))
			(set! true-op 'cons)
			(set! true-rest (list (car true-rest) ())))
		      (when (null? (cdr false-rest))
			(set! false `(cons ,(car false-rest) ()))
			(set! false-op 'cons)
			(set! false-rest (list (car false-rest) ())))))

		;; maybe move the unless before this
		;; reversible ops here got no real hits (test case junk)
		(let ((diff (let differ-in-one ((p true)
						(q false))
			      (and (pair? p)
				   (pair? q)
				   (if (equal? (car p) (car q))
				       (differ-in-one (cdr p) (cdr q))
				       (and (equal? (cdr p) (cdr q))
					    (or (and (unquoted-pair? (car p))
						     (unquoted-pair? (car q))
						     (differ-in-one (car p) (car q)))
						(list p (list (car p) (car q))))))))))
		  (if (pair? diff)
		      (unless (or (and (equal? true-op (caadr diff))   ; (if x (+ y 1) (- y 1)) -- are we trying to keep really simple stuff out?
				       (or (hash-table-ref syntaces true-op)
					   (hash-table-ref syntaces false-op))
				       (any-pairs? true-rest))         ; (if x (set! y (+ x 1)) (set! y 1))
				  (and (eq? true-op 'set!)             ; (if x (set! y w) (set! z w))
				       (equal? (caar diff) (car true-rest))))
			(let ((subst-loc (car diff)))
			  ;; for let/let* if tree-subst position can't affect the test, just subst, else save test first
			  ;;   named let diff in args gets no hits
			  (if (memq true-op '(let let*))
			      (if (not (or (symbol? (car true-rest))   ; assume named let is moving an if outside the loop
					   (eq? subst-loc true-rest))) ;   avoid confusion about the vars list
				  (let ((vars (car true-rest)))
				    ;; (if x (let ((y (abs x))) (display z) y) (let ((y (log x))) (display z) y)) -> (let ((y ((if x abs log) x))) (display z) y)
				    (lint-format "perhaps ~A" caller
						 (lists->string form
								(if (and (pair? vars)
									 (case true-op
									   ((let)  (tree-memq subst-loc vars))
									   ((let*) (tree-memq subst-loc (car vars)))
									   (else #f)))
								    (tree-subst-eq (cons 'if (cons test (cadr diff))) subst-loc true)
								    `(let ((<1> ,test))
								       ,(tree-subst-eq `(if <1> ,@(cadr diff)) subst-loc true)))))))

			      ;; also not any-macro? (car true|false) probably
			      ;; (if x (set! y #t) (set! y #f)) -> (set! y x)
			      (let ((nform (cond ((eq? true-op (caadr diff))  ; very common!
						  ;;          (if x (f y) (g y)) -> ((if x f g) y)
						  ;; but f and g can't be or/and unless there are no expressions
						  ;;   I now like all of these -- originally found them odd: CL influence!
						  (cons (if (equal? true-op test)
							    (list 'or test false-op)
							    (list 'if test true-op false-op))
							true-rest))

						 ((and (eq? (caadr diff) #t)
						       (not (cadadr diff)))
						  ;;          (if x (set! y #t) (set! y #f)) -> (set! y x)
						  (tree-subst-eq test subst-loc true))

						 ((and (not (caadr diff))
						       (eq? (cadadr diff) #t))
						  ;;          (if x (set! y #f) (set! y #t)) -> (set! y (not x))
						  (tree-subst-eq (simplify-boolean (list 'not test) () () env)
								 subst-loc true))

						 ((equal? (caadr diff) test)
						  ;;          (if x (set! y x) (set! y 21)) -> (set! y (or x 21))
						  (tree-subst-eq (simplify-boolean (cons 'or (cadr diff)) () () env)
								 subst-loc true))

						 ((and (len=2? test)
						       (eq? (car test) 'not)
						       (equal? (cadadr diff) (cadr test)))
						  ;;          (if (not x) (set! y z) (set! y x)) -> (set! y (or x z))
						  (tree-subst-eq (simplify-boolean (cons 'or (reverse (cadr diff))) () () env)
								 subst-loc true))

						 ((and (eq? true-op 'begin)
						       (eq? (caar diff) (caadr diff)) ; so I think we're changing set! target, not value
						       (pair? true) (pair? (cdr true)) (pair? (cadr true))
						       (eq? (caadr true) 'set!))
						  ;; tricky: (if x (begin (set! y x) (set! j x) k) (begin (set! m x) (set! j x) k)) ->
						  ;;            (begin (set! (if x y m) x) (set! j x) k)
						  ;;  without this check
						  #f)

						 ((or (memq true-op '(set! begin and or))
						      (let list-memq ((a subst-loc) (lst true))
							(and (pair? lst)
							     (or (eq? a lst)
								 (list-memq a (cdr lst)))))

						      ;; (if x (set! y z) (set! y w)) -> (set! y (if x z w))
						      ;; true op moved out, if test moved in
						      ;;  (if A (and B C) (and B D)) -> (and B (if A C D))
						      ;;  here differ-in-one means that preceding/trailing stuff must subst-loc exactly

						      (not (and (pair? test)
								(or (side-effect? test env)
								    (memq (car test) '(list-values apply-values append unquote))))))

						  (tree-subst-eq (cons 'if (cons test (cadr diff))) subst-loc true))

						 (else #f))))

				(if (pair? nform)
				    (lint-format "perhaps ~A" caller (lists->string form nform)))))))

		      ;; else not pair? diff
		      (unless (memq true-op '(let let* format))

			;; differ-in-trailers can (sometimes) take advantage of values
			(let ((enddiff
			       (let ((op (if (memq true-op '(and or + * begin max min)) true-op 'values)))
				 (let differ-in-trailers ((p true)
							  (q false)
							  (c 0))
				   (and (pair? p)
					(pair? q)
					(if (equal? (car p) (car q))
					    (differ-in-trailers (cdr p) (cdr q) (+ c 1))
					    (and (> c (if (eq? op 'values) (max 2 (/ (length true) 2)) 1))
						 (list p
						       (if (null? (cdr p)) (car p) (cons op p))
						       (if (null? (cdr q)) (car q) (cons op q))))))))))

			  ;; (if A (+ B C E) (+ B D)) -> (+ B (if A (+ C E) D))
			  ;; if p/q null, don't change because for example
			  ;;   (if A (or B C) (or B C D F)) can't be (or B C (if A ...))
			  ;;   but if this were not and/or, it could be (+ B (if A C (values C D F)))
			  (if (pair? enddiff)
			      (lint-format "perhaps ~A" caller
					   (lists->string form (tree-subst `((if ,test ,@(cdr enddiff))) (car enddiff) true)))

			      ;; differ-in-headers looks for equal trailers
			      ;;   (if A (+ B B E C) (+ D D E C)) -> (+ (if A (+ B B) (+ D D)) E C)
			      ;;   these are not always (read: almost never) an improvement
			      (when (and (eq? true-op false-op)
					 (not (eq? true-op 'values))
					 (or (not (eq? true-op 'set!))
					     (equal? (car true-rest) (car false-rest))))
				(let ((headdiff (let differ-in-headers ((p true-rest)
									(q false-rest)
									(c 0)
									(rp ())
									(rq ()))
						  (and (pair? p)
						       (pair? q)
						       (if (equal? p q)
							   (and (< 0 c)
								(<= c (length p))
								(list p (reverse rp) (reverse rq)))
							   (differ-in-headers (cdr p) (cdr q)
									      (+ c 1)
									      (cons (car p) rp) (cons (car q) rq)))))))
				  (when (pair? headdiff)
				    (let ((op (if (memq true-op '(and or + * begin max min)) true-op 'values)))
				      (let ((tp (if (null? (cdadr headdiff))
						    (caadr headdiff)
						    (cons op (cadr headdiff))))
					    (tq (if (null? (cdaddr headdiff))
						    (caaddr headdiff)
						    (cons op (caddr headdiff)))))
					;; (if A (+ B B E C) (+ D D E C)) -> (+ (if A (+ B B) (+ D D)) E C)
					(lint-format "perhaps ~A" caller
						     (lists->string form
								    (cons true-op
									  (cons (list 'if test tp tq)
										(car headdiff))))))))))))))))))

	  ;; -------- if->cond --------
	  (define (easy-if->cond caller form)
	    (do ((iff form (cadddr iff))
		 (iffs 0 (+ iffs 1)))
		((not (and (<= iffs 2)
			   (pair? iff)
			   (= (length iff) 4)
			   (eq? (car iff) 'if)))
		 (when (or (> iffs 2)
			   (and (= iffs 2)
				(len=3? iff)
				(eq? (car iff) 'if)))
		   (set! last-if-line-number line-number)
		   ;; (if a b (if c d (if e f g))) -> (cond (a b) (c d) (e f) (else g)) -- what about *report-nested-if*?
		   (lint-format "perhaps use cond: ~A" caller
				(lists->string form
					       `(cond ,@(do ((iff form (cadddr iff))
							     (clauses ()))
							    ((not (and (pair? iff)
								       (= (length iff) 4)
								       (eq? (car iff) 'if)))
							     (append (reverse clauses)
								     (if (and (len=3? iff)
									      (eq? (car iff) 'if))
									 `((,(cadr iff) ,@(unbegin (caddr iff))))
									 `((else ,@(unbegin iff))))))
							  (set! clauses (cons (cons (cadr iff)
										    (unbegin (caddr iff)))
									      clauses))))))))))

	  ;; -------- if->or/and --------
	  (define (if->or/and caller form test true false env)
	    (cond ((side-effect? test env))

		  ((equal? test true)                       ; (if x x y) -> (or x y)
		   (lint-format "perhaps ~A" caller
				(lists->string form
					       (simplify-boolean (list 'or test (if (eq? false 'no-false) #<unspecified> false))
								 () () env))))
		  ((or (equal? test (list 'not true))       ; (if x (not x) y) -> (and (not x) y)
		       (equal? (list 'not test) true))      ; (if (not x) x y) -> (and x y)
		   (lint-format "perhaps ~A" caller
				(lists->string form
					       (simplify-boolean (list 'and true (if (eq? false 'no-false) #<unspecified> false))
								 () () env))))
		  ((equal? test false)                      ; (if x y x) -> (and x y)
		   (lint-format "perhaps ~A" caller
				(lists->string form (simplify-boolean (list 'and test true) () () env))))

		  ((or (equal? (list 'not test) false)      ; (if x y (not x)) -> (or (not x) y)
		       (equal? test (list 'not false)))     ; (if (not x) y x) -> (or x y)
		   (lint-format "perhaps ~A" caller
				(lists->string form (simplify-boolean (list 'or false true) () () env))))))

	  ;; -------- if->when --------
	  (define (if->when caller form test expr true false)
	    (when (and *report-one-armed-if*
		       (eq? false 'no-false)
		       (or (not (integer? *report-one-armed-if*))
			   (and (pair? true) (eq? (car true) 'begin)) ; added 3-Nov-20
			   (> (tree-leaves true) *report-one-armed-if*)))
	      ;; (if a (begin (set! x y) z)) -> (when a (set! x y) z)
	      (lint-format "~A~A~A perhaps ~A" caller
			   (if (and (integer? *report-one-armed-if*)
				    (not (and (pair? true) (eq? (car true) 'begin))))
			       "this one-armed if is too big"
			       "")
			   (local-line-number test)
			   (if (and (integer? *report-one-armed-if*)
				    (not (and (pair? true) (eq? (car true) 'begin))))
			       ";" "")
			   (truncated-lists->string
			    form (if (and (pair? expr)
					  (eq? (car expr) 'not))
				     (cons 'unless (cons (cadr expr) (unbegin true)))
				     (cons 'when (cons expr (unbegin true))))))))

	  ;; -------- move-cond-outward --------
	  (define (move-cond-outward caller form expr true false env)
	    ;; true-op = case happens a lot, but never in a way that (not expr)->false can be combined in the case
	    (when (and (proper-pair? true)
		       (eq? (car true) 'cond)
		       (not (boolean? false)) ; these cases are handled elsewhere via or/and
		       (not (and (pair? false)
				 (eq? (car false) 'cond))))
	      ;; (if A (cond...) B) -> (cond ((not A) B) ...)
	      ;;    if no false and cond is one-shot => this can be optimized to (cond ((and (not A) C) => ...))
	      (lint-format "perhaps ~A" caller
			   (let ((nexpr (simplify-boolean (list 'not expr) () () env))
				 (nfalse (if (eq? false 'no-false)
					     (if (eq? form lint-mid-form)
						 ()
						 '(#<unspecified>))
					     (list (if (not (and (pair? false)
								 (> (tree-leaves false) 100)))
						       false
						       (if (pair? (car false))
							   (list (list (caar false) '...))
							   (list (car false) '...)))))))
			     (lists->string form (cons 'cond (cons (cons nexpr nfalse) (cdr true))))))))

	  ;; -------- if->cond --------
	  (define (if->cond caller form env)
	    ;; unravel complicated if-then-else nestings into a single cond, if possible.
	    ;;
	    ;; The (> new-len *report-nested-if*) below can mean (nearly) all nested ifs are turned into conds.
	    ;;   For a long time I thought the if form was easier to read, but now
	    ;;   I like cond better.  But cond also has serious readability issues:
	    ;;   it needs to be clearer where the test separates from the result,
	    ;;   and in a stack of these clauses, it's hard to see anything at all.
	    ;;   Maybe a different color for the test than the result?
	    ;;
	    ;; Also, the check for tree-leaves being hugely different is taken
	    ;;   from C -- I think it is easier to read a large if statement if
	    ;;   the shortest clause is at the start -- especially in a nested if where
	    ;;   it can be nearly impossible to see which dangling one-liner matches
	    ;;   which if (this even in emacs because it unmarks or doesn't remark the matching
	    ;;   paren as you're trying to scroll up to it).
	    ;;
	    ;; the cond form is not always an improvement:
	    ;;   (if A (if B (if C a b) (if C c d)) (if B (if C e f) (if C g h)))
	    ;;   (cond (A (cond (B (cond (C a) (else b))) ... oh forget it ...))))
	    ;;   perhaps: (case (+ (if A 4 0) (if B 2 0) (if C 1 0)) ((#b000)...))!
	    ;;   how often (and how deeply nested) does this happen? -- not very, but nesting can be ridiculous.
	    ;;   and this assumes all tests are always hit
	    ;;
	    ;; the len>2 below could be len>1 + handling of empty false branch, but
	    ;;   I don't like the rewrites (using cond).  There are a lot of hits here but they
	    ;;   need special-case handling, perhaps.

	    (define (swap-clauses form)
	      (if (not (len>2? (cdr form)))
		  form
		  (let ((expr (cadr form))
			(ltrue (caddr form))
			(lfalse (cadddr form)))

		    (if (or (and (pair? ltrue)
				 (not (proper-list? ltrue)))
			    (and (pair? lfalse)
				 (not (proper-list? lfalse))))
			form

			(let ((true-n (tree-leaves ltrue))
			      (false-n (if (not (pair? lfalse))
					   1
					   (tree-leaves lfalse))))

			  (if (< false-n (/ true-n 4))
			      (let ((new-expr (simplify-boolean (list 'not expr) () () env)))
				(if (and (pair? ltrue)
					 (eq? (car ltrue) 'if))
				    (set! ltrue (swap-clauses ltrue)))
				(if (and (pair? ltrue)
					 (eq? (car ltrue) 'cond))
				    `(cond (,new-expr ,@(unbegin lfalse))
					   ,@(cdr ltrue))
				    `(cond (,new-expr ,@(unbegin lfalse))
					   (else ,@(unbegin ltrue)))))
			      (begin
				(if (and (pair? lfalse)
					 (eq? (car lfalse) 'if))
				    (set! lfalse (swap-clauses lfalse)))

				(if (and (pair? lfalse)
					 (eq? (car lfalse) 'cond))
				    `(cond (,expr ,@(unbegin ltrue))
					   ,@(cdr lfalse))
				    `(cond (,expr ,@(unbegin ltrue))
					   (else ,@(unbegin lfalse)))))))))))

	    (let ((new-if (swap-clauses form)))
	      (when (and (eq? (car new-if) 'cond)
			 (> (length new-if) *report-nested-if*))
		(set! last-if-line-number line-number)
		(lint-format "perhaps ~A" caller (lists->string form new-if)))))

	  ;; -------- if->bool --------
	  (define (if->bool caller form expr true false env no-suggestion)
	    (cond ((eq? expr #t) ; (if #t #f) -> #f
		   (lint-format "perhaps ~A" caller (lists->string form true)))

		  ((not expr)
		   (if (eq? false 'no-false)
		       (if true                             ; (if #f x) as a kludgey #<unspecified>
			   (lint-format "perhaps ~A" caller (lists->string form #<unspecified>)))
		       ;; (if (negative? (gcd x y)) a b) -> b
		       (lint-format "perhaps ~A" caller (lists->string form false))))

		  ((code-equal? true false)   ; (if x (+ y 1) (+ y 1)) -> (+ y 1)
		   (lint-format "if is not needed here: ~A" caller
				(lists->string form (if (not (side-effect? expr env))
							true
							(list 'begin expr true)))))

		   ((boolean? true)
		    (if (boolean? false)          ; !  (if expr #t #f) turned into something less verbose
			;; (if x #f #t) -> (not x)
			(lint-format "perhaps ~A" caller
				     (lists->string form (if true expr (simplify-boolean (list 'not expr) () () env))))
			(when no-suggestion	  ; (if x #f y) -> (and (not x) y)
			  (lint-format "perhaps ~A" caller
				       (lists->string form (if true
							       (if (eq? false 'no-false)
								   expr
								   (simplify-boolean (list 'or expr false) () () env))
							       (if (eq? form lint-mid-form)
								   (if (and (pair? expr)
									    (eq? (car expr) 'not))
								       (cons 'when (cons (cadr expr) (unbegin false)))
								       (cons 'unless (cons expr (unbegin false))))
								   (simplify-boolean
								    (if (eq? false 'no-false)
									(list 'not expr)
									(list 'and (list 'not expr) false))
								    () () env))))))))
		   ((and (boolean? false)
			 no-suggestion)        ; (if x y #t) -> (or (not x) y)
		    (lint-format "perhaps ~A" caller
				 (let ((nexpr (cond (false
						     (list 'or (if (and (len>1? expr)
									(eq? (car expr) 'not))
								   (cadr expr)
								   (list 'not expr))
							   true))
						    ((not (eq? form lint-mid-form))
						     (list 'and expr true))
						    ((and (pair? expr)
							  (eq? (car expr) 'not))
						     (cons 'unless (cons (cadr expr) (unbegin true))))
						    (else (cons 'when (cons expr (unbegin true)))))))
				   (lists->string form
						  (if (and (eq? form lint-mid-form)
							   (eq? (car nexpr) 'when))
						      nexpr
						      (simplify-boolean nexpr () () env))))))))

	  ;; -------- move-let-outward --------
	  (define (move-let-outward caller form expr true false env)
	    ;; this happens occasionally -- scarcely worth this much code! (gather copied vars outside the if)
	    (when (and (len>1? true)     ; len>1? so true-rest is a pair
		       (len>1? false)    ; same for false-rest
		       (eq? (car true) 'let)
		       (eq? (car false) 'let))
	      (let ((true-rest (cdr true))
		    (false-rest (cdr false)))
		(when (and (pair? (car true-rest))
			   (just-pairs? (car true-rest))
			   (pair? (car false-rest))
			   (just-pairs? (car false-rest)))
		  (let ((true-vars (map car (car true-rest)))
			(false-vars (map car (car false-rest)))
			(shared-vars ()))
		    (for-each (lambda (v)
				(if (and (memq v false-vars)
					 (equal? (cadr (assq v (car true-rest)))
						 (cadr (assq v (car false-rest)))))
				    (set! shared-vars (cons v shared-vars))))
			      true-vars)
		    (when (pair? shared-vars)
		      ;; now remake true/false lets (maybe nil) without shared-vars
		      (let ((ntv ())
			    (nfv ())
			    (sv ()))
			(for-each (lambda (v)
				    (if (memq (car v) shared-vars)
					(set! sv (cons v sv))
					(set! ntv (cons v ntv))))
				  (car true-rest))
			(set! ntv (if (or (pair? ntv)
					  (pair? (cddr true-rest))) ; even define is safe here because outer let blocks it just as inner let used to
				      (cons 'let (cons (reverse ntv) (cdr true-rest)))
				      (cadr true-rest)))
			(for-each (lambda (v)
				    (if (not (memq (car v) shared-vars))
					(set! nfv (cons v nfv))))
				  (car false-rest))
			(set! nfv (if (or (pair? nfv)
					  (pair? (cddr false-rest)))
				      (cons 'let (cons (reverse nfv) (cdr false-rest)))
				      (cadr false-rest)))
			;; (if (> (+ a b) 3) (let ((a x) (c y)) (* a (log c))) (let ((b z) (c y)) (+... ->
			;;    (let ((c y)) (if (> (+ a b) 3) (let ((a x)) (* a (log c))) (let ((b z)) (+ b (log c)))))
			(lint-format "perhaps ~A" caller
				     (lists->string form
						    (if (not (or (side-effect? expr env)
								 (tree-set-memq (map car sv) expr)))
							(list 'let (reverse sv) (list 'if expr ntv nfv))
							(let ((uniq (find-unique-name form)))
							  `(let ((,uniq ,expr))
							     (let ,(reverse sv)
							       (if ,uniq ,ntv ,nfv))))))))))))))

	  ;; -------- move-false-outward --------
	  (define (move-false-outward caller form expr true false env)
	    (let ((true-rest (and (pair? true) (cdr true)))
		  (false-rest (cdr false)))
	      (case (car false)
		((cond)                 ; (if a A (cond...)) -> (cond (a  A) ...)
		 (when (proper-list? false-rest)
		   (lint-format "perhaps ~A" caller (lists->string form (cons 'cond (cons (list expr true) false-rest))))))

		((if)
		 (unless (= last-if->case-line-number line-number)
		   (let ((eqv-select (eqv-selector expr))) ; this is just an accessor -- no check for case compatibility of expr
		     ;; (if (= x 1) 2 (if (= x 3) 3 4)) -> (case x ((1) 2) ((3) 3) (else 4))
		     (when (and eqv-select
				(cond-eqv? expr eqv-select #t)
				(pair? (cdr false))
				(cond-eqv? (cadr false) eqv-select #t))
		       (let ((clauses (list (list expr true))))
			 (let gather-clauses ((f false))
			   (if (and (len>2? f)
				    (eq? (car f) 'if)
				    (cond-eqv? (cadr f) eqv-select #t))
			       (begin
				 (set! clauses (cons (list (cadr f) (caddr f)) clauses))
				 (if (pair? (cdddr f))
				     (gather-clauses (cadddr f))))
			       (set! clauses (cons (list 'else f) clauses))))
			 (when (or (> (length clauses) 2)
				   (not (eq? (caar clauses) 'else)))
			   (set! last-if->case-line-number line-number)
			   (lint-format "perhaps ~A" caller
					(lists->string form
						       (cond->case eqv-select (reverse clauses)))))))))

		 (when (= (length false) 4)
		   (let ((false-test (car false-rest))
			 (false-true (cadr false-rest))
			 (false-false (caddr false-rest)))
		     (if (equal? true false-true)
			 ;; (if a A (if b A B)) -> (if (or a b) A B)
			 (lint-format "perhaps ~A" caller
				      (if (and (pair? false-false)
					       (eq? (car false-false) 'if)
					       (equal? true (caddr false-false)))
					  (lists->string form
							 (let ((nexpr (simplify-boolean
								       (list 'or expr false-test (cadr false-false))
								       () () env)))
							   `(if ,nexpr ,true ,@(cdddr false-false))))
					  (if true
					      (let ((nexpr (simplify-boolean (list 'or expr false-test) () () env)))
						(lists->string form (list 'if nexpr true false-false)))
					      (lists->string form
							     (simplify-boolean
							      `(and (not (or ,expr ,false-test)) ,false-false)
							      () () env)))))
			 (when (equal? true false-false)
			   ;; (if a A (if b B A)) -> (if (or a (not b)) A B)
			   (lint-format "perhaps ~A" caller
					(if true
					    (let ((nexpr (simplify-boolean `(or ,expr (not ,false-test)) () () env)))
					      (lists->string form (list 'if nexpr true false-true)))
					    (lists->string form
							   (simplify-boolean
							    `(and (not (or ,expr (not ,false-test))) ,false-true)
							    () () env))))))))
		 ;; (if (and x y) ... (if (and x z) ...)) gets 3 hits (one tricky)

		 (when (and (len=3? true)
			    (eq? (car true) 'if)
			    (= (length false) 3)
			    (equal? (cdr true-rest) (cdr false-rest)))
		   ;; (if a (if b d) (if c d)) -> (if (if a b c) d)
		   (lint-format "perhaps ~A" caller
				(lists->string form
					       (if (> (+ (tree-leaves expr)
							 (tree-leaves (car true-rest))
							 (tree-leaves (car false-rest)))
						      12)
						   `(let ((<1> (if ,expr ,(car true-rest) ,(car false-rest))))
						      (if <1> ,@(cdr true-rest)))
						   `(if (if ,expr ,(car true-rest) ,(car false-rest)) ,@(cdr true-rest)))))))

		((map)  ; (if (null? x) () (map abs x)) -> (map abs x)
		 (when (and (len>1? expr)
			    (eq? (car expr) 'null?)
			    (or (null? true)
				(equal? true (cadr expr)))
			    (equal? (cadr expr) (cadr false-rest))
			    (or (null? (cddr false-rest))
				(not (side-effect? (cddr false-rest) env))))
		   (lint-format "perhaps ~A" caller (lists->string form false))))

		((case)
		 (if (and (pair? expr)
			  (pair? false-rest)
			  (not (code-constant? (car false-rest)))
			  (cond-eqv? expr (car false-rest) #t))
		     ;; (if (eof-object? x) 32 (case x ((#\a) 3) (else 4))) -> (case x ((#<eof>) 32) ((#\a) 3) (else 4))
		     (lint-format "perhaps ~A" caller
				  (lists->string form `(case ,(car false-rest)
							 ,(case-branch expr (car false-rest) (list true))
							 ,@(cdr false-rest))))))

		((else)  ; (if x (f y) (else z)) ! -- this gets 3 hits
		 (if (not (var-member 'else env))
		     (lint-format "else (as car of false branch of if) makes no sense: ~A" caller form))))

	      (let ((false-test (and (pair? false-rest) (car false-rest))))
		(if (and (eq? (car false) 'if)   ; (if x 3 (if (not x) 4)) -> (if x 3 4)
			 (> (or (length false-rest) 0) 1) ; proper-list and len>1?
			 (not (side-effect? expr env)))
		    (if (equal? expr false-test)
			(lint-format "perhaps ~A" caller (lists->string form `(if ,expr ,true ,@(cddr false-rest))))
			(if (and (pair? false-test)
				 (eq? (car false-test) 'not)
				 (equal? expr (cadr false-test)))
			    (lint-format "perhaps ~A" caller (lists->string form (list 'if expr true (cadr false-rest)))))))

		(if (and (eq? (car false) 'if)          ; (if test0 expr (if test1 expr)) -> if (or test0 test1) expr)
			 (len=2? false-rest)         ; other case is dealt with above
			 (equal? true (cadr false-rest)))
		    (let ((test1 (simplify-boolean (list 'or expr false-test) () () env)))
		      (lint-format "~Aperhaps ~A" caller
				   (if (equal? expr false-test) "weird repetition! " "")
				   (lists->string form (list 'if test1 true))))))))

	  ;; -------- if->min/max --------
	  (define (if->min/max caller form test true)
	    (when (pair? test)
	      (let ((test-op (car test))
		    (true-op (car true))
		    (true-rest (cdr true)))
		;; the min+max case is seldom hit, and takes about 50 lines
		(when (and (memq test-op '(< > <= >=))
			   (len=2? (cdr test)))
		  (let ((rel-arg1 (cadr test))
			(rel-arg2 (caddr test)))

		    ;; (if (< x y) (set! x y) -> (set! x (max x y))
		    (case true-op
		      ((set!)
		       (when (len>1? true-rest)
			 (let ((settee (car true-rest))
			       (setval (cadr true-rest)))
			   (if (and (member settee test)
				    (member setval test)) ; that's all there's room for
			       (let ((f (if (equal? settee (if (memq test-op '(< <=)) rel-arg1 rel-arg2)) 'max 'min)))
				 (lint-format "perhaps ~A" caller
					      (lists->string form (list 'set! settee (cons f true-rest)))))))))

		      ;; (if (<= (list-ref ind i) 32) (list-set! ind i 32)) -> (list-set! ind i (max (list-ref ind i) 32))
		      ((list-set! vector-set!)
		       (when (len>1? (cdr true-rest))
			 (let ((settee (car true-rest))
			       (index (cadr true-rest))
			       (setval (caddr true-rest)))
			   (let ((mx-op (if (and (equal? setval rel-arg1)
						 (eqv? (length rel-arg2) 3)
						 (equal? settee (cadr rel-arg2))
						 (equal? index (caddr rel-arg2)))
					    (if (memq test-op '(< <=)) 'min 'max)
					    (and (equal? setval rel-arg2)
						 (eqv? (length rel-arg1) 3)
						 (equal? settee (cadr rel-arg1))
						 (equal? index (caddr rel-arg1))
						 (if (memq test-op '(< <=)) 'max 'min)))))
			     (if mx-op
				 (lint-format "perhaps ~A" caller
					      (lists->string form (list true-op settee index (cons mx-op (cdr test))))))))))))))))

	  ;; -------- inverted-if->min/max ---------
	  (define (inverted-if->min/max caller form test true false)
	    ;; no hits for negative?/positive? and 0/0.0 here
	    (if (and (len=3? test)
		     (memq (car test) '(< <= > >= =))       ; (if (< x y) x y) -> (min x y)
		     (member false test)
		     (member true test))
		(if (eq? (car test) '=)                     ; (if (= x y) y x) -> y [this never happens]
		    (lint-format "perhaps ~A" caller (lists->string form false))
		    (let ((f (if (equal? (cadr test) (if (memq (car test) '(< <=)) true false))
				 'min 'max)))
		      (lint-format "perhaps ~A" caller (lists->string form (list f true false)))))))

	  ;; -------- if+if->and --------
	  (define (if+if->and caller form expr true env)
	    (let ((true-op (car true))
		  (true-rest (cdr true)))
	      (cond ((not (pair? true-rest)))
		    ((not (eq? (car true) 'if))                ; (if test0 (if test1 expr)) -> (if (and test0 test1) expr)
		     (if (memq true-op '(when unless))         ; (if test0 (when test1 expr...)) -> (when (and test0 test1) expr...)
			 (let ((test1 (simplify-boolean (list 'and expr (if (eq? true-op 'when)
									    (car true-rest)
									    (list 'not (car true-rest))))
							() () env)))
			   ;; (if (and (< x 1) y) (when z (display z) x)) -> (when (and (< x 1) y z) (display z) x)
			   (lint-format "perhaps ~A" caller
					(lists->string form
						       (if (and (len>1? test1)
								(eq? (car test1) 'not))
							   (cons 'unless (cons (cadr test1) (cdr true-rest)))
							   (cons 'when (cons test1 (cdr true-rest)))))))))
		    ((len=1? (cdr true-rest))
		     (let ((test1 (simplify-boolean (list 'and expr (car true-rest)) () () env)))
		       (lint-format "perhaps ~A" caller (lists->string form (list 'if test1 (cadr true-rest))))))

		    ((equal? expr (car true-rest))
		     (lint-format "perhaps ~A" caller (lists->string form true)))

		    ((and (equal? (car true-rest) (list 'not expr))
			  (len>1? (cdr true-rest)))
		     (lint-format "perhaps ~A" caller
				  (lists->string form (caddr true-rest)))))))

	  ;; -------- combine-if --------
	  (define (combine-if caller form expr true false env)
	    (let ((true-op (and (pair? true) (car true)))
		  (true-rest (and (pair? true) (cdr true)))
		  (false-op (and (pair? false) (car false)))
		  (false-rest (and (pair? false) (cdr false))))
	      (when (and (len>2? true)
			 (eq? true-op 'if))
		(let ((true-test (car true-rest))
		      (true-true (and (pair? (cdr true-rest)) (cadr true-rest))))
		  (if (= (length true) 4)
		      (let ((true-false (caddr true-rest)))
			(if (equal? expr (simplify-boolean (list 'not true-test) () () env))
			    ;; (if a (if (not a) B C) A) -> (if a C A)
			    (lint-format "perhaps ~A" caller
					 (lists->string form (list 'if expr true-false false))))
			(if (equal? expr true-test)
			    ;; (if x (if x z w) y) -> (if x z y)
			    (lint-format "perhaps ~A" caller
					 (lists->string form (list 'if expr true-true false))))
			(if (equal? false true-false)
			    ;; (if a (if b B A) A) -> (if (and a b) B A)
			    (lint-format "perhaps ~A" caller
					 (lists->string form
							(simplify-boolean
							 (if (not false)
							     (list 'and expr true-test true-true)
							     (list 'if (list 'and expr true-test) true-true false))
							 () () env)))
			    (if (equal? false true-true)
				;; (if a (if b A B) A) -> (if (and a (not b)) B A)
				(lint-format "perhaps ~A" caller
					     (lists->string form
							    (simplify-boolean
							     (if (not false)
								 (list 'and expr (list 'not true-test) true-false)
								 (list 'if (list 'and expr (list 'not true-test)) true-false false))
							     () () env)))))

			;; (if a (if b d e) (if c d e)) -> (if (if a b c) d e)? reversed does not happen.
			;; (if a (if b d) (if c d)) -> (if (if a b c) d)
			;; (if a (if b d e) (if (not b) d e)) -> (if (eq? (not a) (not b)) d e)
			(when (and (pair? false)
				   (eq? false-op 'if)
				   (= (length false) 4)
				   (not (equal? true-test (car false-rest)))
				   (equal? (cdr true-rest) (cdr false-rest)))
			  (let ((false-test (car false-rest)))
			    (lint-format "perhaps ~A" caller
					 (lists->string form
							(cond ((and (len>1? true-test)
								    (eq? (car true-test) 'not)
								    (equal? (cadr true-test) false-test))
							       (cons 'if (cons (list 'not (list 'eq? (list 'not expr) true-test))
									       (cdr true-rest))))

							      ((and (len>1? false-test)
								    (eq? (car false-test) 'not)
								    (equal? true-test (cadr false-test)))
							       (cons 'if (cons (list 'eq? (list 'not expr) false-test)
									       (cdr true-rest))))

							      ((> (+ (tree-leaves expr)
								     (tree-leaves true-test)
								     (tree-leaves false-test))
								  12)
							       `(let ((<1> (if ,expr ,true-test ,false-test)))
								  (if <1> ,@(cdr true-rest))))

							      (else
							       (cons 'if (cons (list 'if expr true-test false-test) (cdr true-rest))))))))))
		      (begin ; (length true) != 4
			(if (equal? expr (simplify-boolean (list 'not true-test) () () env))
			    (lint-format "perhaps ~A" caller  ; (if a (if (not a) B) A) -> (if (not a) A)
					 (lists->string form (list 'if (list 'not expr) false))))
			(if (equal? expr true-test)           ; (if x (if x z) w) -> (if x z w)
			    (lint-format "perhaps ~A" caller
					 (lists->string form (list 'if expr true-true false))))
			(if (equal? false true-true)          ; (if a (if b A) A)
			    (lint-format "perhaps ~A" caller
					 (let ((nexpr (simplify-boolean (list 'or (list 'not expr) true-test) () () env)))
					   (lists->string form (list 'if nexpr false)))))))))))

	  ;; -------- evert-if --------
	  (define (evert-if caller form expr true false env)
	    ;; move repeated start/end statements out of the if
	    (let ((ltrue (if (and (pair? true) (eq? (car true) 'begin)) true (list 'begin true)))
		  (lfalse (if (and (pair? false) (eq? (car false) 'begin)) false (list 'begin false))))
	      (let ((true-len (length ltrue))
		    (false-len (length lfalse)))
		(when (and (integer? true-len)
			   (> true-len 1)
			   (integer? false-len)
			   (> false-len 1))
		  (let ((start (if (and (equal? (cadr ltrue) (cadr lfalse))
					(not (side-effect? expr env))) ; expr might affect start, so we can't pull it ahead
				   (list (cadr ltrue))
				   ()))
			(end (if (and (not (= true-len false-len 2))
				      (equal? (list-ref ltrue (- true-len 1))
					      (list-ref lfalse (- false-len 1))))
				 (list (list-ref ltrue (- true-len 1)))
				 ())))
		    (when (or (pair? start)
			      (pair? end))
		      (let ((new-true (cdr ltrue))
			    (new-false (cdr lfalse)))
			(when (pair? end)
			  (set! new-true (copy new-true (make-list (- true-len 2)))) ; (copy lst ()) -> ()
			  (set! new-false (copy new-false (make-list (- false-len 2)))))
			(when (pair? start)
			  (if (pair? new-true) (set! new-true (cdr new-true)))
			  (if (pair? new-false) (set! new-false (cdr new-false))))
			(when (or (pair? end)
				  (and (pair? new-true)
				       (pair? new-false))) ; otherwise the rewrite changes the returned value
			  (if (pair? new-true)
			      (set! new-true (if (null? (cdr new-true))
						 (car new-true)
						 (cons 'begin new-true))))
			  (if (pair? new-false)
			      (set! new-false (if (null? (cdr new-false))
						  (car new-false)
						  (cons 'begin new-false))))
			  ;; (if x (display y) (begin (set! z y) (display y))) -> (begin (if (not x) (set! z y)) (display y))
			  (let ((not-expr (and (pair? expr)
					       (eq? (car expr) 'not))))
			    ;; these are of the form (if expr1 (begin expr2... val) val) or its many variants
			    ;;   see s7test.scm (lint-test "(if x (begin (display x) y) y)"...) and following
			    ;;   that form goes to          (begin (if x (display x)) y)
			    (lint-format "perhaps ~A" caller
			      (lists->string form
				(let ((body (if (null? new-true)
						(cons (if (null? (cddr new-false)) ; (if|when|unless... )
							  'if
							  (if not-expr
							      'when 'unless))
						      (cons
						       (if (null? (cddr new-false)) ; remove 'not if present else add unless unless
							   (if not-expr
							       (cadr expr)
							       (list 'not expr))
							   (if not-expr
							       (cadr expr)
							       expr))
						       (if (and (pair? (cddr new-false))
								(eq? (car new-false) 'begin))
							   (cdr new-false)
							   (list new-false))))

						(if (null? new-false)
						    (cons (if (null? (cddr new-true)) ; (if|when|unless... )
							      'if
							      (if not-expr
								  'unless 'when))
							  (cons
							   (if (null? (cddr new-true))
							       expr
							       (if not-expr
								   (cadr expr)
								   expr))
							   (if (and (pair? (cddr new-true))
								    (eq? (car new-true) 'begin))
							       (cdr new-true)
							       (list new-true))))
						    (list 'if expr new-true new-false)))))
				  `(begin ,@start
					  ,body
					  ,@end)))))))))))))

	  ;; -------- if+let->when --------
	  (define (if+let->when caller form expr true false)
	    ;; if+let() -> when: about a dozen hits
	    (let ((ntrue (and (len>1? true)                    ; (if A B (let () (display x))) -> (if A B (begin (display x)))
			      (eq? (car true) 'let)
			      (null? (cadr true))
			      (not (tree-set-memq definers (cddr true)))
			      (cddr true)))
		  (nfalse (and (len>1? false)
			       (eq? (car false) 'let)
			       (null? (cadr false))
			       (not (tree-set-memq definers (cddr false)))
			       (cddr false))))
	      (if (or ntrue nfalse)
		  (lint-format "perhaps ~A" caller
			       (lists->string form
					      (if (eq? false 'no-false)
						  (cons 'when (cons expr ntrue))
						  (if ntrue
						      (if nfalse
							  `(if ,expr (begin ,@ntrue) (begin ,@nfalse))
							  `(if ,expr (begin ,@ntrue) ,false))
						      `(if ,expr ,true (begin ,@nfalse)))))))))

	  ;; -------- shorter-branch-first --------
	  (define (shorter-branch-first caller form test true false env)
	    (let ((true-len (tree-leaves (caddr form))))
	      (when (and (> true-len *report-short-branch*)
			 (< (tree-leaves (cadddr form)) (/ true-len *report-short-branch*)))
		(let ((new-expr (simplify-boolean (list 'not test) () () env)))
		  (lint-format "perhaps place the much shorter branch first~A: ~A" caller
			       (local-line-number (cadr form))
			       (truncated-lists->string form (list 'if new-expr false true)))))))

	  ;; -------- flip-out-not --------
	  (define (flip-out-not caller form expr true false)
	    (when (and (len>1? expr)                       ; (if (not a) A B) -> (if a B A)
		       (eq? (car expr) 'not)
		       (> (tree-leaves true) (tree-leaves false)))
	      (lint-format "perhaps ~A" caller
			   (lists->string form (list 'if (cadr expr) false true)))))

	  ;; -------- repeated-test->cond --------
	  (define (repeated-test->cond caller form expr true false)
	    (if (and (pair? expr)
		     (len=1? (cdr true))
		     (equal? expr (cadr true)))
		(lint-format "perhaps ~A" caller
			     (lists->string form
					    (if (eq? false 'no-false)
						(list 'cond (list expr '=> (car true)))
						(list 'cond (list expr '=> (car true)) (list 'else false)))))))

	  ;; -------- unrepeat-test --------
	  (define (unrepeat-test caller form expr true false)
	    ;; move repeated test to top, if no inner false branches -- aren't we assuming A does not affect B?  yes, but this never happens.
	    ;;   (if A (if B C) (if B D)) -> (if B (if A C D))
	    (when (and (len=3? true)
		       (len=3? false)
		       (eq? (car true) 'if)
		       (eq? (car false) 'if)
		       (equal? (cadr true) (cadr false)))
	      (lint-format "perhaps ~A" caller
			   (lists->string form `(if ,(cadr true)
						    (if ,expr
							,(caddr true)
							,(caddr false)))))))

	  ;; -------- simplify-if+ifs --------
	  (define (simplify-if+ifs caller form expr true false env)
	    (when (and (pair? false)
		       (eq? (car true) 'if)
		       (eq? (car false) 'if)
		       (= (length true) (length false) 4)
		       (equal? (cadr true) (cadr false)))
	      (let ((true-rest (and (pair? true) (cdr true)))
		    (false-rest (and (pair? false) (cddr false))))
		(if (and (equal? (cadr true-rest) (cadr false-rest)) ; (if A (if B a b) (if B b a)) -> (if (eq? (not A) (not B)) a b)
			 (equal? (caddr true-rest) (car false-rest)))
		    (let* ((switch #f)
			   (a (if (and (pair? expr)
				       (eq? (car expr) 'not))
				  (begin (set! switch #t) expr)
				  (simplify-boolean (list 'not expr) () () env)))
			   (b (if (and (pair? (car true-rest))
				       (eq? (caar true-rest) 'not))
				  (begin (set! switch (not switch)) (car true-rest))
				  (simplify-boolean (list 'not (car true-rest)) () () env))))
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  (if switch
						      `(if (eq? ,a ,b) ,(car false-rest) ,(cadr true-rest))
						      `(if (eq? ,a ,b) ,(cadr true-rest) ,(car false-rest))))))
		    (unless (or (side-effect? expr env)
				(equal? (cdr true-rest) false-rest)) ; handled elsewhere
		      (if (equal? (cadr true-rest) (car false-rest))  ; (if A (if B a b) (if B a c)) -> (if B a (if A b c))
			  (lint-format "perhaps ~A" caller
				       (lists->string form
						      `(if ,(car true-rest) ,(cadr true-rest)
							   (if ,expr ,(caddr true-rest) ,(cadr false-rest)))))
			  (if (equal? (caddr true-rest) (cadr false-rest)) ; (if A (if B a b) (if B c b)) -> (if B (if A a c) b)
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  `(if ,(car true-rest)
							       (if ,expr ,(cadr true-rest) ,(car false-rest))
							       ,(caddr true-rest)))))))))))

	  ;; -------- if->case-else --------
	  (define (if->case-else caller form test true false) ; (if (eq? (caddr x) 'a) b (caddr x)) -> (case (caddr x) ((a) b) (else))
	    (when (and (not (= last-let->case-line-number line-number))
		       (or (and (len=2? test)
				(memq (car test) '(null? not eof-object?)))   ; memx/charx got no hits
			   (and (len=3? test)
				(memq (car test) '(eq? eqv? =))
				(lint-any? code-constant? test)))
		       (or (and (unquoted-pair? false)
				(or (member false test)
				    (and (len=2? false)
					 (unquoted-pair? (cadr false))
					 (member (cadr false) test))))
			   (and (unquoted-pair? true)
				(or (member true test)
				    (and (len=2? true)
					 (unquoted-pair? (cadr true))
					 (member (cadr true) test))))))
	      (lint-format "perhaps use case: ~A" caller
			   (lists->string form
					  (let ((key #f)
						(selector #f))
					    (if (memq (car test) '(null? not eof-object?))
						(begin
						  (set! key (case (car test) ((not) #f) ((eof-object?) #<eof>) (else ())))
						  (set! selector (cadr test)))
						(begin
						  (set! selector ((if (unquoted-pair? (cadr test)) cadr caddr) test))
						  (set! key ((if (code-constant? (cadr test)) cadr caddr) test))
						  (if (pair? key) (set! key (cadr key)))))

					    (if (or (member false test)
						    (and (len=2? false)
							 (member (cadr false) test)))
						`(case ,selector
						   ((,key) ,true)
						   (else ,@(if (member false test) () (list '=> (car false)))))
						`(case ,selector
						   ((,key) ,@(if (member true test) () (list '=> (car true))))
						   (else ,false))))))))

	  ;;-------- if->or/begin --------
	  (define (if->or/begin caller form expr true false env)
	    ;; cond version of this gets no hits
	    ;; moving the test after the first statement assumes the statement won't affect the test.
	    ;;   this seems to be always the case -- a check could involve outvars or
	    ;;     (if (and (pair? true) (eq? (car true) 'set!) (tree-memq (cadr true) expr))...)
	    ;;   but it gets no hits.  The same problem probably affects move-false-outward etc.
	    (unless (side-effect? expr env)
	      (if (and (len>2? false)
		       (memq (car false) '(or begin))
		       (equal? true (cadr false)))
		  (lint-format "perhaps ~A" caller            ; (if A (f x) (or (f x) ...)) -> (or (f x) (and (not A) (or ...)))
			       (lists->string form
					      (if (eq? (car false) 'or)
						  `(or ,true (and ,(simplify-boolean (list 'not expr) () () env)
								  ,(if (pair? (cdddr false))
								       (cons 'or (cddr false))
								       (caddr false))))
						  `(begin ,true (when ,(simplify-boolean (list 'not expr) () () env)
								  ,@(cddr false))))))
		  (if (and (len>2? true)
			   (memq (car true) '(or begin))      ; (if A (or (f x)...) (f x)) -> (or (f x) (and A (or ...)))
			   (equal? false (cadr true)))
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  (if (eq? (car true) 'or)
						      `(or ,false (and ,expr
								       ,(if (pair? (cdddr true))
									    (cons 'or (cddr true))
									    (caddr true))))
						      `(begin ,false (when ,expr ,@(cddr true))))))))))

	  ;; -------- if-walker --------
	  (define (if-walker caller form env)
	   (let ((len (length form)))
	     (if (> len 4)
		 (lint-format "if has too many clauses: ~A" caller (truncated-list->string form))
		 (if (< len 3)
		     (lint-format "if has too few clauses: ~A" caller (truncated-list->string form))
		     (let ((true (caddr form))
			   (false (if (= len 4) (cadddr form) 'no-false))
			   (expr (simplify-boolean (cadr form) () () env)))

		       (let ((test (cadr form)))
			 (unless (equal? expr test)             ; (or (not (pair? x)) (not (pair? z))) -> (not (and (pair? x) (pair? z)))
					                        ; (and (equal? (car x) (car orig)) (equal? (cdr x) (cdr orig))) -> (equal? x orig)
			   (lint-format "perhaps ~A" caller (lists->string test expr)))
			 ;; (if (cond...)...) doesn't happen much and is tricky to rewrite

			 ;; (if ([=] x y) (f x) (f y)) gets only 2 hits, (if ([=] x y) x y) gets 1 hit -- are these so dumb we can't ignore them?
			 ;; (if (not (null? o)) o '()) gets 2 hits
			 ;; (if (and x...) ... (if (and (not x)...)...)) gets 6 hits and 1 is caught (and mangled)

			 (let ((suggestion made-suggestion))
			   (sensible-if? caller form test true false env)
			   (move-if-inward caller form expr true false env)
			   (unless (= last-if-line-number line-number)     ; avoid recursive (redundant) call
			     (easy-if->cond caller form))
			   (if->or/and caller form test true false env)
			   (if->when caller form test expr true false) ; test is for a reasonable line number, expr is for a corrected test (sigh)
			   (move-cond-outward caller form expr true false env)

			 (when (= len 4)
			   (combine-if caller form expr true false env)
			   (when (pair? false)
			     (move-false-outward caller form expr true false env)))

			 (when (and (eq? false 'no-false)
				    (pair? true))
			   (if->min/max caller form test true)
			   (if+if->and caller form expr true env))
			 (inverted-if->min/max caller form test true false)
			 (if->bool caller form expr true false env (= suggestion made-suggestion))

			 (when (pair? true)
			   (repeated-test->cond caller form expr true false)
			   (simplify-if+ifs caller form expr true false env))
#|
			   ;; (if (not (eq? x y)) (set! x y)) -> (set! x y)
			   (if (and (= len 3)
				    (pair? test)
				    (eq? (car test) 'not)
				    (eq? (car true) 'set!)
				    (pair? (cadr test))
				    (eq? (caadr test) 'eq?)
				    (equal? (cdadr test) (cdr true)))
			       (lint-format "perhaps ~A -> ~A" caller (truncated-list->string form) true))
			   ;; (if (eq? x y) (not (eq? x y)))
			   (if (and (eq? (car true) 'not)
				    (pair? (cdr true))
				    (pair? (cadr true))
				    (equal? test (cadr true)))
			       (lint-format "perhaps ~A -> ~A" caller (truncated-list->string form) `(if ,test #f))))
|#

			 (when (and (= suggestion made-suggestion)
				    (not (= line-number last-if-line-number)))
			   (if->cond caller form env)
			   (if+let->when caller form expr true false)

			   (when (= len 4)
			     (when (= suggestion made-suggestion) ; not redundant (if->cond above)
			       (shorter-branch-first caller form expr true false env))
			     (if->case-else caller form test true false)
			     (unrepeat-test caller form expr true false)
			     (evert-if caller form expr true false env)
			     (when (and (= suggestion made-suggestion)      ; not redundant -- this will repeat the earlier suggestion in many cases
					(not (= line-number last-if-line-number)))
			       (flip-out-not caller form expr true false))
			     (move-let-outward caller form expr true false env)
			     (if->or/begin caller form expr true false env)))))

		       ;; walking the if's with true/false lists for simplify-boolean found only 1 case of a collapsible test
		       (if (symbol? expr)
			   (set-ref expr caller form env)
			   (lint-walk caller expr env))
		       (if (symbol? true)
			   (set-ref true caller form env)
			   (set! env (lint-walk caller true env)))
		       (if (symbol? false)
			   (if (not (eq? false 'no-false))
			       (set-ref false caller form env))
			   (set! env (lint-walk caller false env))))))
	     env))
	 (hash-walker 'if if-walker))


	;; -------- when, unless --------
	(let ()
	  (define (when-walker caller form env)
	    (if (< (length form) 3)
		(begin
		  (lint-format "~A is messed up: ~A" caller (car form) (truncated-list->string form))
		  env)
		(let ((test (cadr form))
		      (head (car form)))

		  ;; when->unless and vice versa
		  (when (pair? test)
		    (cond ((eq? (car test) 'not)           ; (when (not a) (set! x y)) -> (unless a (set! x y))
			   (lint-format "perhaps ~A"
					caller
					(truncated-lists->string form
								 (cons (if (eq? head 'when) 'unless 'when)
								       (cons (cadr test)
									     (cddr form))))))
			  ((and (eq? head 'unless)
				(hash-table-ref notables (car test))) ; (unless (< y 3) ...) -> (when (>= y 3) ...)
			   (lint-format "perhaps ~A"
					caller
					(truncated-lists->string form
								 (cons 'when
								       (cons (cons (hash-table-ref notables (car test)) (cdr test))
									     (cddr form))))))))
		  (if (never-false test)
		      (lint-format "~A test is never false: ~A" caller head (truncated-list->string form))
		      (if (never-true test)       ; (unless #f...)
			  (lint-format "~A test is never true: ~A" caller head (truncated-list->string form))))

		  (if (symbol? test)
		      (begin
			(set-ref test caller form env)
			(if (and (eq? head 'when)
				 (pair? (cddr form))
				 (pair? (caddr form)))
			    (if (memq test (caddr form))
				(and-incomplete form head test (caddr form) env)
				(do ((p (caddr form) (cdr p)))
				    ((or (not (pair? p))
					 (and (pair? (car p))
					      (memq test (car p))))
				     (if (pair? p)
					 (and-incomplete form head test (car p) env)))))))
		      (when (pair? test)
			(if (and (eq? (car test) 'and)
				 (len=3? test))
			    (let ((arg1 (cadr test))
				  (arg2 (caddr test)))
			      (if (or (and (pair? arg1)
					   (eq? (car arg1) 'not))
				      (and (pair? arg2)
					   (eq? (car arg2) 'not)))
				  (if (eq? head 'unless)
				      ;; (unless (and x (not y)) (display z)) -> (when (or (not x) y) ...)
				      (lint-format "perhaps ~A" caller
						   (lists->string form (list 'when (simplify-boolean (list 'not test) () () env) '...)))
				      (if (and (pair? arg1)
					       (eq? (car arg1) 'not)
					       (pair? arg2)
					       (eq? (car arg2) 'not))
					  ;; (when (and (not x) (not y)) (display z)) -> (unless (or x y) ...)
					  (lint-format "perhaps ~A" caller
						       (lists->string form (list 'unless (list 'or (cadr arg1) (cadr arg2)) '...))))))))
			(lint-walk caller test env)))

		  (when (and (len=1? (cddr form))           ; (when t1 (if t2 A)) -> (when (and t1 t2) A)
			     (len>1? (caddr form)))
		    (let ((body (caddr form)))
		      (if (and (eq? (car body) 'cond)       ; (when test (cond ...)) -> (cond ...)
			       (proper-list? body))
			  (lint-format "perhaps ~A" caller
				       (truncated-lists->string form
								(cons 'cond
								      (cons (list (if (eq? (car form) 'when)
										      (simplify-boolean (list 'not (cadr form)) () () env)
										      (cadr form))
										  #<unspecified>) ; was #f 12-Oct-21
									    (cdr body)))))
			  (when (or (memq (car body) '(when unless))
				    (and (eq? (car body) 'if)
					 (len=3? body)))
			    (let ((new-test (let ((inner-test (if (eq? (car body) 'unless)
								  (list 'not (cadr body))
								  (cadr body)))
						  (outer-test (if (eq? head 'unless)
								  (list 'not test)
								  test)))
					      (simplify-boolean (list 'and outer-test inner-test) () () env))))
			      ;; (when (and (< x 1) y) (if z (display z))) -> (when (and (< x 1) y z) (display z))
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  (if (and (len=2? new-test)
								   (eq? (car new-test) 'not))
							      (cons 'unless (cons (cadr new-test) (cddr body)))
							      (cons 'when (cons new-test (cddr body)))))))))))
		  (lint-walk-open-body caller head (cddr form) env))))
	  (hash-walker 'when when-walker)
	  (hash-walker 'unless when-walker))


	;; -------- check-results --------
	;; called in cond, case, and do for => primarily
	(define (check-results caller syn clause sequel env)
	  (cond ((not (pair? sequel))
		 (if (not (null? sequel))  ; (not (null?...)) here is correct -- we're looking for stray dots
		     (lint-format "~A clause is messed up: ~A" caller syn (truncated-list->string clause))))

		((not (eq? (car sequel) '=>))
		 (lint-walk-open-body caller syn sequel env))

		((or (not (pair? (cdr sequel)))
		     (pair? (cddr sequel)))
		 ;; (cond (x =>))
		 (lint-format "~A => target is messed up: ~A" caller syn (truncated-list->string clause)))

		(else (let ((f (cadr sequel)))
			(if (symbol? f)
			    (let ((val (symbol->value f *e*)))
			      (when (procedure? val)
				(if (not (aritable? val 1)) ; here values might be in test expr
				    ;; (cond (x => expt))
				    (lint-format "=> target (~A) may be unhappy: ~A" caller f clause))
				(let ((sig (signature val)))
				  (if (len>1? sig)
				      (let ((from-type (->lint-type ((if (or (memq syn '(cond do-result))
									     (not (pair? (car clause))))
									 car caar) clause)))
					    (to-type (cadr sig)))
					(if (not (or (memq from-type '(#f #t values))
						     (memq to-type '(#f #t values))
						     (any-compatible? to-type from-type)))
					    ;; (cond ((> x 0) => abs) (else y))
					    (lint-format "in ~A, ~A returns a ~A, but ~A expects ~A" caller
							 (truncated-list->string clause)
							 (car clause) (prettify-checker-unq from-type)
							 f to-type)))))))
			    (if (and (len>1? f)
				     (eq? (car f) 'lambda)
				     (pair? (cadr f))
				     (not (= (length (cadr f)) 1)))
				(lint-format "=> target (~A) may be unhappy: ~A" caller f clause)))
			(lint-walk caller f env)))))

	;; ---------------- cond ----------------
	(let ()
	  ;; in s7, (cond (A) (B) (else #f)) is (or A B), but it probably never happens

	  ;; -------- cond->header+cond+trailer --------
	  (define (cond->header+cond+trailer caller form len env)
	    ;; (cond (A (and B C)) (else (and B D))) et al never happens
	    ;;    also (cond (A C) (B C)) -> (if (or A B) C) [non-pair C]
	    ;; ----------------
	    ;; if regular cond + else
	    ;;   scan all return blocks
	    ;;   if all one form, and either header or trailer always match,
	    ;;   rewrite as header + cond|if + trailer
	    ;; given values and the presence of else, every possibility is covered
	    ;; at least (car result) has to match across all
	    (when (and (> len 1) ; (cond (else ...)) is handled elsewhere
		       (pair? (cdr form))
		       (not (tree-set-memq '(unquote list-values) form)))
	      (let ((first-clause (cadr form))
		    (else-clause (list-ref form len)))
		(when (and (len=1? (cdr first-clause))
			   (pair? (cadr first-clause))
			   (not (eq? (caadr first-clause) 'list))) ; (list (cond ...values...)) is worse than (cond ...list...)
		  (let ((first-result (cadr first-clause))
			(first-func (caadr first-clause)))
		    (if (and (memq (car else-clause) '(#t else))
			     (pair? (cdr else-clause))
			     (pair? (cadr else-clause))
			     (or (equal? first-func (caadr else-clause)) ; there's some hope we'll match
				 (escape? (cadr else-clause) env)))
			(let ((else-error (escape? (cadr else-clause) env)))
			  (when (and (pair? (cdr first-result))
				     (not (eq? first-func 'values))
				     (or (not (hash-table-ref syntaces first-func))
					 (eq? first-func 'set!))
				     (lint-every? (lambda (c)
						    (and (len=2? c)
							 (len>1? (cadr c))
							 (or (equal? first-func (caadr c))
							     (and (eq? c else-clause)
								  else-error))))
						  (cddr form)))
			    ((lambda (header-len trailer-len result-min-len)
			       (when (and (>= header-len 0)
					  (>= trailer-len 0)
					  (or (not (eq? first-func 'set!))
					      (> header-len 1))
					  (or (not (eq? first-func '/))
					      (> header-len 1)
					      (> trailer-len 0)))
				 (let ((header (copy first-result (make-list header-len)))
				       (trailer (copy first-result (make-list trailer-len) (- (length first-result) trailer-len))))
				   (if (= len 2)
				       (unless (equal? first-result (cadr else-clause)) ; handled elsewhere (all results equal -> result)
					 ;; (cond (x (for-each (lambda (x) (display (+ x a))) (f y))) (else (for-each... ->
					 ;;    (for-each (lambda (x) (display (+ x a))) (if x (f y) (g y)))
					 (lint-format "perhaps ~A" caller
						      (let ((else-result (cadr else-clause)))
							(let ((first-mid-len (- (length first-result) header-len trailer-len))
							      (else-mid-len (- (length else-result) header-len trailer-len)))
							  (let ((fmid (if (= first-mid-len 1)
									  (list-ref first-result header-len)
									  (cons 'values (copy first-result (make-list first-mid-len) header-len))))
								(emid (if else-error
									  else-result
									  (if (= else-mid-len 1)
									      (list-ref else-result header-len)
									      (cons 'values (copy else-result (make-list else-mid-len) header-len))))))
							    (lists->string form (append header (cons (list 'if (car first-clause) fmid emid) trailer))))))))
				       ;; len > 2 so use cond in the revision
				       (let ((middle (map (lambda (c)
							    (if (and else-error
								     (eq? c else-clause))
								else-clause
								(let ((test (car c))
								      (result (cadr c)))
								  (let ((mid-len (- (length result) header-len trailer-len)))
								    (list test (if (= mid-len 1)
										   (list-ref result header-len)
										   (cons 'values (copy result (make-list mid-len) header-len))))))))
							  (cdr form))))
					 ;; (cond ((< x 1) (+ x 1)) ((< y 1) (+ x 3)) (else (+ x 2))) -> (+ x (cond ((< x 1) 1) ((< y 1) 3) (else 2)))
					 (lint-format "perhaps ~A" caller
						      (lists->string form (append header (cons (cons 'cond middle) trailer)))))))))
			     (partition-form (cdr form) (if else-error (- len 1) len)))))

			;; not escaping else here because the trailing args might be evaluated first
			(when (and (not (hash-table-ref syntaces (car first-result)))
				   (lint-every? (lambda (c)
						  (and (len=2? c)
						       (pair? (cadr c))
						       (not (hash-table-ref syntaces (caadr c)))
						       (equal? (cdadr c) (cdr first-result))))
						(cddr form)))
			  (if (lint-every? (lambda (c)
					(eq? first-func (caadr c)))         ; all result clauses are the same!?
				      (cddr form))                          ; possibly no else, so not always a duplicate message
			      ;; (cond (X (f y z)) (Y (f y z)) (Z (f y z))) -> (if (or X Y Z) (f y z))
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  (list 'if (cons 'or (map car (cdr form))) first-result)))
			      ;; here we need an else clause else (apply #<unspecified> args)
			      (if (memq (car else-clause) '(#t else))
				  ;; (cond (X (f y z)) (else (g y z))) -> ((cond (X f) (else g)) y z)
				  (lint-format "perhaps ~A" caller
					       (lists->string form
							      (cons (cons 'cond (map (lambda (c)
										       (list (car c) (caadr c)))
										     (cdr form)))
								    (cdr first-result)))))))))))))

	  ;; -------- cond->or --------
	  (define (cond->or caller form all-eqv eqv-select simplifications env)
	    (do ((new-clauses ())
		 (current-clauses ())
		 (clauses (cdr form) (cdr clauses)))
		((null? clauses)
		 (let ((len2 (= (length new-clauses) 2)))
		   (unless (and len2         ; i.e. don't go to check-bool-cond
				(check-bool-cond caller form (cadr new-clauses) (car new-clauses) env))
		     ;; (cond ((= x 3) 3) ((= x 2) 4) ((= x 1) 4)) -> (case x ((3) 3) ((2 1) 4))
		     (lint-format "perhaps ~A" caller
				  (lists->string
				   form
				   (cond (all-eqv
					  (cond->case eqv-select (reverse new-clauses)))
					 ((not (and len2
						    (memq (caar new-clauses) '(else #t))
						    (len=1? (cadr new-clauses))
						    (pair? (caadr new-clauses))
						    (eq? (caaadr new-clauses) 'or)))
					  (cons 'cond (reverse new-clauses)))
					 ((null? (cddar new-clauses))  ; (cond (A) (B) (else C)) -> (or A B C)
					  `(or ,@(cdaadr new-clauses) ,(cadar new-clauses)))
					 (else `(or ,@(cdaadr new-clauses) (begin ,@(cdar new-clauses))))))))))

	      (let* ((clause (car clauses))
		     (result (cdr clause))) ; can be null in which case the test is the result
		(cond ((and (pair? simplifications)
			    (assq clause simplifications))
		       => (lambda (e)
			    (set! clause (cons (cdr e) result)))))
		(if (and (pair? (cdr clauses))
			 (equal? result (cdadr clauses)))
		    (set! current-clauses (cons clause current-clauses))
		    (if (pair? current-clauses)
			(begin
			  (set! current-clauses (cons clause current-clauses))
			  (set! new-clauses (cons
					     (cons (simplify-boolean (cons 'or (map car (reverse current-clauses))) () () env)
						   result)
					     new-clauses))
			  (set! current-clauses ()))
			(set! new-clauses (cons clause new-clauses)))))))

	  ;; -------- cond->assoc --------
	  (define (cond->assoc caller form simplifications)
	    ;; look for repeated ((op x c1) c2) -> ((assoc x '((c1 . c2)...)) => cdr) anywhere in the clause list
	    (let ((nc ())
		  (op #f)
		  (sym-access #f)
		  (start #f)
		  (changed #f))

	      ;; extending this to memx possibilities got only 1 hit and involved ca. 20 lines

	      (define (car-with-expr cls)
		(cond ((and (pair? simplifications)
			    (assq cls simplifications))
		       => (lambda (e)
			    (set! changed #t)
			    (cons (cdr e) (cdr cls))))
		      (else cls)))

	      (define (start-search clauses test)
		(if (code-constant? (cadr test))
		    (if (memq (car test) '(= string=? string-ci=? eq? eqv? equal? char=? char-ci=?))
			(set! sym-access caddr))
		    (if (code-constant? (caddr test))
			(set! sym-access cadr)))
		(if sym-access
		    (begin
		      (set! start clauses)
		      (set! op (car test)))
		    (set! nc (cons (car-with-expr (car clauses)) nc))))

	      (do ((clauses (cdr form) (cdr clauses)))
		  ((null? clauses)
		   (if (and changed
			    (null? clauses))
		       ;; (cond ((< x 2) 3) ((> x 0) 4) ((< x 2) 5)) -> (cond ((< x 2) 3) ((> x 0) 4))
		       (lint-format "perhaps ~A" caller
				    (lists->string form (cons 'cond (reverse (map (lambda (c)
										    (if (not (car c)) (values) c))
										  nc)))))))
		(let ((test (caar clauses)))
		  (let ((ok-but-at-end #f)
			(looks-ok (let ((result (cdar clauses)))
				    (and (len=3? test)
					 (len=1? result)
					 (not (symbol? (car result)))
					 (or (not (pair? (car result))) ; quoted lists look bad in this context
					     (and (len=2? (car result))
						  (eq? (caar result) 'quote)
						  (not (pair? (cadar result)))))))))
		    (if (not start)
			(if (and looks-ok
				 (not (null? (cdr clauses))))
			    (start-search clauses test)
			    (set! nc (cons (car-with-expr (car clauses)) nc)))

			(unless (and looks-ok
				     (eq? (car test) op)
				     (equal? (sym-access test) (sym-access (caar start)))
				     (code-constant? ((if (eq? sym-access cadr) caddr cadr) test))
				     (not (set! ok-but-at-end (null? (cdr clauses)))))

			  (if (eq? (cdr start) clauses) ; only one item in the block, or two but it's just two at the end
			      (begin
				(set! nc (cons (car start) nc))
				(if (and looks-ok
					 (not (null? (cdr clauses))))
				    (start-search clauses test)
				    (begin
				      (set! start #f)
				      (set! nc (cons (car-with-expr (car clauses)) nc)))))

			      ;; multiple hits -- can we combine them?
			      (let ((alist ())
				    (cc (if (eq? sym-access cadr) caddr cadr)))
				(set! changed #t)
				(do ((sc start (cdr sc)))
				    ((if ok-but-at-end
					 (null? sc)
					 (eq? sc clauses))
				     (case op
				       ((eq?)
					(set! nc (cons `((assq ,(sym-access (caar start)) ',(reverse alist)) => cdr) nc)))

				       ((eqv? char=?)
					(set! nc (cons `((assv ,(sym-access (caar start)) ',(reverse alist)) => cdr) nc)))

				       ((equal?)
					(set! nc (cons `((assoc ,(sym-access (caar start)) ',(reverse alist)) => cdr) nc)))

				       ((string=?)
					;; this is probably faster than assoc + string=?, but it creates symbols
					(let ((nlst (map (lambda (c)
							   (cons (string->symbol (car c)) (cdr c)))
							 alist)))
					  (set! nc (cons `((assq (string->symbol ,(sym-access (caar start))) ',(reverse nlst)) => cdr) nc))))

				       (else
					(set! nc (cons `((assoc ,(sym-access (caar start)) ',(reverse alist) ,op) => cdr) nc)))))

				  (set! alist (cons (cons (unquoted (cc (caar sc))) (unquoted (cadar sc))) alist)))

				(if (and looks-ok
					 (not (null? (cdr clauses))))
				    (start-search clauses test)
				    (begin
				      (set! start #f)
				      (if (not ok-but-at-end)
					  (set! nc (cons (car-with-expr (car clauses)) nc))))))))))))))

	  ;; -------- cond-remove-not --------
	  (define (cond-remove-not caller form env)
	    (let ((c1 (cadr form))
		  (c2 (caddr form)))
	      (cond ((equal? (simplify-boolean (car c1) () () env)
			     (simplify-boolean (list 'not (car c2)) () () env))
		     (lint-format "perhaps ~A" caller  ; (cond ((x) y) ((not (x)) z)) -> (cond ((x) y) (else z))
				  (lists->string form (list 'cond c1 (cons 'else (cdr c2))))))
		    ((and (pair? (cdr c2))
			  (not (pair? (cadr c2)))
			  (not (memq (car c2) '(else #t)))
			  (equal? (cdr c1) (cdr c2)))
		     (lint-format "perhaps ~A" caller
				  (lists->string form
						 `(if (or ,(car c1) ,(car c2))
						      ,(if (null? (cddr c1))
							   (cadr c1)
							   (cons 'begin (cdr c1)))))))
		    ((and (len>1? (car c1))        ; (cond ((not x) y) (else z)) -> (cond (x z) (else y))
			  (pair? (cdr c1))         ;    null case is handled elsewhere
			  (eq? (caar c1) 'not)
			  (memq (car c2) '(else #t)))
		     (let ((c1-len (tree-leaves (cdr c1))) ; try to avoid the dangling short case as in if
			   (c2-len (tree-leaves (cdr c2))))
		       (when (and (< (+ c1-len c2-len) 100)
				  (> (* c1-len 4) c2-len))   ; maybe 4 is too much
			 (lint-format "perhaps ~A" caller
				      (lists->string form
						     (if (or (pair? (cddr c1))
							     (pair? (cddr c2)))
							 `(cond (,(cadar c1) ,@(cdr c2)) (else ,@(cdr c1)))
							 (list 'if (cadar c1) (cadr c2) (cadr c1)))))))))))

	  ;; -------- simplify-cond --------
	  (define (simplify-cond caller form env)
	    (let ((first-clause (cadr form))
		  (last-clause (caddr form))             ; next-to-last actually
		  (else-clause (cdr (list-ref form 3)))) ; len = 3, might be junk following else=last-clause

	      (when (and (pair? else-clause)
			 (proper-list? last-clause))
		(when (and (or (null? (cdr first-clause))
			       (and (len=1? (cdr first-clause))
				    (boolean? (cadr first-clause))))
			   (or (null? (cdr last-clause))
			       (null? (cddr last-clause))))

		  (if (and (pair? (cdr first-clause))
			   (not (cadr first-clause))            ; (cond (A #f) (B #t) (else C)) -> (and (not A) (or B C))
			   (or (null? (cdr last-clause))
			       (eq? (cadr last-clause) #t)))
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  (simplify-boolean
						   `(and (not ,(car first-clause))
							 (or ,(car last-clause)
							     ,@(if (null? (cdr else-clause))
								   else-clause
								   (cons 'begin else-clause))))
						   () () env)))
		      (if (and (or (null? (cdr first-clause))   ; (cond (A #t) (B C) (else #f)) -> (or A (and B C))
				   (eq? (cadr first-clause) #t))
			       (not (car else-clause))
			       (null? (cdr else-clause)))
			  (lint-format "perhaps ~A" caller
				       (lists->string form
						      `(or ,(car first-clause)
							   (and ,@last-clause)))))))

		(when (and (proper-list? else-clause)
			   (equal? (cdr first-clause) else-clause) ; a = else result
			   (pair? (cdr last-clause))               ; b does exist
			   (not (eq? (cadr last-clause) '=>)))     ; no => in b
		  ;; (cond (A a) (B b) (else a)) -> (if (or A (not B)) a b)
		  (lint-format "perhaps ~A" caller
			       (lists->string form
					      (let ((A (car first-clause))
						    (a (cdr first-clause))
						    (B (car last-clause))
						    (b (cdr last-clause)))
						(let ((nexpr (simplify-boolean `(or ,A (not ,B)) () () env)))
						  (cond ((not (and (null? (cdr a))
								   (null? (cdr b))))
							 `(cond (,nexpr ,@a) (else ,@b)))

							((eq? (car a) #t)
							 (if (not (car b))
							     nexpr
							     (simplify-boolean (list 'or nexpr (car b)) () () env)))

							((car a) ; i.e a is not #f
							 (list 'if nexpr (car a) (car b)))

							((eq? (car b) #t)
							 (simplify-boolean (list 'not nexpr) () () env))

							(else (simplify-boolean `(and (not ,nexpr) ,(car b)) () () env)))))))))))

	  ;; -------- simple-cond->if --------
	  (define (simple-cond->if caller form suggest)
	    (let ((clause (cadr form)))       ; (cond (a)) -> a, (cond (a b)) -> (if a b) etc
	      (if (null? (cdr clause))
		  (lint-format "perhaps ~A" caller (lists->string form (car clause)))
		  (if (and (pair? (cdr clause))
			   (not (eq? (cadr clause) '=>))
			   (or (pair? (cddr clause))
			       (= suggest made-suggestion)))
		      ;; (cond ((= x 1) 32)) -> (if (= x 1) 32)
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  (if (null? (cddr clause))
						      (list 'if (car clause) (cadr clause))
						      (if (and (pair? (car clause))
							       (eq? (caar clause) 'not))
							  (cons 'unless (append (cdar clause) (cdr clause)))
							  (cons 'when clause)))))))))

	  ;; -------- cond-repeated-else --------
	  (define (cond-repeated-else caller form len env)
	    ;; this is not ideal
	    (let ((e (list-ref form len))      ; (cond (W X) (A B) (C D) (else B)) -> (cond (W X) ((or A (not C)) B) (else D))
		  (b (list-ref form (- len 1)))
		  (a (list-ref form (- len 2))))
	      (if (and (len>1? a)              ; is (else) a legal cond clause? -- yes, it returns else...
		       (proper-list? a)
		       (equal? (cdr a) (cdr e))
		       (len>1? b)
		       (proper-list? b)
		       (not (eq? (cadr b) '=>)))
		  (let ((expr (simplify-boolean `(or ,(car a) (not ,(car b))) () () env)))
		    (lint-format "perhaps ~A" caller
				 (lists->string form `(cond ,(if (> len 4) '... (cadr form))
							    (,expr ,@(cdr a))
							    (else ,@(cdr b)))))))))

	  ;; -------- combine-repeated-tests --------
	  (define (combine-repeated-tests caller form env)
	    ;; repeated test exprs handled once
	    (let ((exprs ())
		  (reps ())
		  (ctr 0)
		  (pos 0)
		  (head-len 0)
		  (else-leaves 0)
		  (else-result #f))
	      (for-each (lambda (c)
			  (set! pos (+ pos 1))
			  (cond ((memq (car c) '(#t else))
				 (set! else-result (cdr c))
				 (set! else-leaves (tree-leaves else-result)))

				((not (and (pair? (car c))
					   (or (eq? (caar c) 'and)
					       (member (car c) reps))))
				 (set! exprs ())
				 (set! reps ())
				 (set! ctr 0))

				((null? exprs)
				 (set! head-len pos)
				 (set! exprs (cdar c))
				 (set! reps exprs)
				 (set! ctr 1))

				(else
				 (set! ctr (+ ctr 1))
				 (set! reps (remove-if (lambda (rc)
							 (not (or (equal? rc (car c))
								  (member rc (cdar c)))))
						       reps)))))
			(cdr form))
	      (when (and (pair? reps)
			 (> ctr 1)
			 (< else-leaves (* ctr (length reps) 3)))
		;; (cond ((pair? z) 32) ((and (pair? x) (pair? w)) 12) ((pair? x) 2) (else 0)) ->
		;;    (cond ((pair? z) 32) ((not (pair? x)) 0) ((pair? w) 12) (else 2))
		(lint-format "perhaps ~A" caller
			     (lists->string form
					    (let ((not-reps
						   (simplify-boolean (list 'not (if (null? (cdr reps))
										    (car reps)
										    (cons 'and reps)))
								     () () env)))
					      `(,@(copy form (make-list head-len))
						(,not-reps
						 ,@(or else-result '(#<unspecified>)))
						,@(let mapper ((clauses (list-tail form head-len))
							       (lst ()))
						    (if (null? clauses)
							(reverse lst)
							(let ((new-clause
							       (let ((c (car clauses)))
								 (if (memq (car c) '(else #t))
								     c
								     (cons (if (member (car c) reps)
									       'else
									       (remove-if (lambda (rc)
											    (member rc reps))
											  (car c)))
									   (cdr c))))))
							  (if (and (len=2? (car new-clause))
								   (eq? (caar new-clause) 'and))
							      (set-car! new-clause (cadar new-clause)))
							  (if (memq (car new-clause) '(else #t))
							      (reverse (cons new-clause lst))
							      (mapper (cdr clauses) (cons new-clause lst)))))))))))))

	  ;; -------- cond->case-at-end --------
	  (define (cond->case-at-end caller form len has-else)
	    ;; look for case at end (case in the middle is tricky due to #f handling)
	    (let ((rform (reverse form))
		  (eqv-select #f)
		  (elen (if has-else (- len 1) len)))
	      (if has-else (set! rform (cdr rform)))
	      (set! eqv-select (eqv-selector (caar rform)))
	      (when eqv-select
		(do ((clauses rform (cdr clauses))
		     (ctr 0 (+ ctr 1)))
		    ((or (null? clauses)
			 (let ((clause (car clauses)))
			   (or (and (pair? (cdr clause))
				    (eq? (cadr clause) '=>)) ; case sends selector, but cond sends test result
			       (not (cond-eqv? (car clause) eqv-select #t)))))
		     (when (and (pair? clauses)
				(> ctr 1))
		       ;; (cond ((pair? x) 3) ((eq? x 'a) z) ((eq? x 'b) (* 2 z)) ((eq? x 'c)... ->
		       ;;    (if (pair? x) 3 (case x ((a) z) ((b) (* 2 z)) ((c) (display z))))
		       (lint-format "possibly use case at the end: ~A" caller
				    (lists->string form
						   (let ((else-case (cond->case eqv-select  ; cond->case will handle the else branch
										(list-tail (cdr form) (- elen ctr)))))
						     (if (= (- elen ctr) 1)
							 (if (equal? (cdadr form) '(#f))
							     `(and (not ,(caadr form)) ,else-case)
							     `(if ,@(cadr form) ,else-case))
							 `(cond ,@(copy (cdr form) (make-list (- elen ctr)))
								(else ,else-case))))))))))))

	  ;; -------- combine-conds --------
	  (define (combine-conds caller form has-else len env)
	    (let ((last-clause (list-ref form (if has-else (- len 1) len)))) ; not the else branch! -- just before it.
	      (if (and (len=2? last-clause)          ; (cond ... (A (cond ...)) (else B)) -> (cond ... ((not A) B) ...)
		       (pair? (cadr last-clause))
		       (memq (caadr last-clause) '(if cond)))
		  (let ((new-test (simplify-boolean (list 'not (car last-clause)) () () env))
			(new-result (if has-else
					(cdr (list-ref form len))
					(if (eq? form lint-mid-form)
					    ()
					    (list #<unspecified>)))))
		    (if (and (eq? (caadr last-clause) 'cond)
			     (proper-list? (cdadr last-clause)))
			;; (cond (A (cond (B c) (else D))) (else E)) -> (cond ((not A) E) (B c) (else D))
			(lint-format "perhaps ~A" caller
				     (lists->string form
						    `(cond ,@(copy (cdr form) (make-list (- len (if has-else 2 1))))
							   (,new-test ,@new-result)
							   ,@(cdadr last-clause))))
			(if (= (length (cadr last-clause)) 4)
			    (let ((if-form (cdadr last-clause)))
			      ;; (cond (A B) (C (if D d E)) (else F)) -> (cond (A B) ((not C) F) (D d) (else E))
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  `(cond ,@(copy (cdr form) (make-list (- len (if has-else 2 1))))
								 (,new-test ,@new-result)
								 (,(car if-form) ,@(unbegin (cadr if-form)))
								 (else ,@(unbegin (caddr if-form))))))))))
		  (when (> len 2)                           ; rewrite nested conds as one cond
		    (let ((lim (if has-else (- len 2) len))
			  (tlen (tree-leaves form)))
		      (when (< tlen 200)
			(set! tlen (/ tlen 4))
			(do ((i 0 (+ i 1))
			     (k (+ lim 1) (- k 1))
			     (p (cdr form) (cdr p)))
			    ((or (not (pair? p))
				 (= i lim)))
			  (let ((nc (car p)))
			    (if (and (len=2? nc)
				     (pair? (cadr nc))
				     (eq? (caadr nc) 'cond)
				     (>= (length (cdadr nc)) (* 2 k))
				     (> (tree-leaves nc) tlen))
				(let ((new-test (simplify-boolean (list 'not (car nc)) () () env))
				      (new-result (if (and has-else
							   (= i (- lim 1))
							   (null? (cddadr p))
							   (pair? (cdaddr p)) ;(else)
							   (null? (cddr (caddr p))))
						      (list 'if (caadr p) (cadadr p) (cadr (caddr p)))
						      (cons 'cond (cdr p)))))
				  ;; (cond ((= x 0) 1) ((= x 3) (cond ((not y) 1) ((pair? y) 2) ((eq? y 'a) 3) (else 4))) ((< x 200) 2) (else 5)) ->
				  ;;   (cond ((= x 0) 1) ((not (= x 3)) (if (< x 200) 2 5)) ((not y) 1) ((pair? y) 2) ((eq? y 'a) 3) (else 4))
				  (lint-format "perhaps ~A" caller
					       (lists->string form
							      `(cond ,@(copy (cdr form) (make-list i))
								     (,new-test ,new-result)
								     ,@(cdadr nc))))))))))))))

	  ;; -------- cond-one-result --------
	  (define (cond-one-result caller form last-clause len env)
	    (let ((result (last-ref (cadr form)))
		  (else-clause (cdr (list-ref form len))))
	      (when (lint-every? (lambda (c)
				   (and (len>1? c)
					(equal? result (last-ref c))))
				 (cddr form))
		;;  (cond ((and (display x) x) 32) (#t 32)) -> (begin (and (display x) x) 32)
		(lint-format "perhaps ~A" caller
			     (lists->string form
					    (if (= len 2)       ; one is else -- this case is very common
						(let* ((c1-len (length (cdr last-clause)))
						       (new-c1 (case c1-len
								 ((1) #f)
								 ((2) (cadr last-clause))
								 (else (cons 'begin (copy (cdr last-clause) (make-list (- c1-len 1)))))))
						       (else-len (length else-clause))
						       (new-else (case else-len
								   ((1) #f)
								   ((2) (car else-clause))
								   (else (cons 'begin (copy else-clause (make-list (- else-len 1))))))))
						  `(begin
						     ,(if (= c1-len 1)
							  (if new-else
							      `(if (not ,(car last-clause)) ,new-else)
							      (car last-clause))
							  (if (= else-len 1)
							      (if new-c1
								  `(if ,(car last-clause) ,new-c1)
								  (car last-clause))
							      `(if ,(car last-clause) ,new-c1 ,new-else)))
						     ,result))
						`(begin          ; this almost never happens
						   (cond ,@(map (lambda (c)
								  (let ((len (length c)))
								    (if (= len 2)
									(if (or (memq (car c) '(else #t))
										(not (side-effect? (car c) env)))
									    (values)
									    (car c))
									(copy c (make-list (- len 1))))))
								(cdr form)))
						   ,result)))))))

 	  ;; -------- cond-partial-test-repeat --------
 	  (define (cond-partial-test-repeat caller form)
 	    (let ((arg1 (cadr form))
 		  (arg2 (caddr form)))
 	      (when (and (len=2? arg1)
 			 (len>1? (car arg1))
 			 (len=2? arg2)
 			 (eq? (caar arg1) 'and)
 			 (member (car arg2) (cdar arg1))
 			 (len=2? (cdar arg1)))
 		;; (cond ((and A B) c) (B d) (else e)) -> (cond (B (if A c d)) (else e))
 		(lint-format "perhaps ~A" caller
 			     (lists->string form
 					    (cons 'cond
 						  (cons (list (car arg2)
 							      (list 'if
 								    ((if (equal? (car arg2) (cadar arg1)) caddar cadar) arg1)
 								    (cadr arg1)
 								    (cadr arg2)))
 							(cdddr form))))))))

	  ;; -------- cond-combine-into-else --------
	  (define (cond-combine-into-else caller form last-clause len)
	    (if (and (len=1? last-clause)   ; (cond ... ((or ...)) (else ...)) -> (cond ... (else (or ... ...)))
		     (pair? (car last-clause))
		     (eq? (caar last-clause) 'or))
		(let ((else-clause (let ((e (cdr (list-ref form len))))
				     (if (null? (cdr e))
					 (car e)
					 (cons 'begin e)))))
		  ;; (cond ((A) B) ((or C D)) (else E)) -> (cond ((A) B) (else (or C D E)))
		  (lint-format "perhaps ~A" caller
			       (lists->string form
					      `(cond ,@(copy (cdr form) (make-list (- len 2)))
						     (else (or ,@(cdar last-clause) ,else-clause)))))))
	    ;; a few dozen hits here
	    ;;   the 'case parallel gets 2 hits, complex selectors
	    ;;   len = (- (length form) 1) = number of clauses
	    ;; (cond ((= x y) 2) ((= x 2) #f) (else #t)) -> (cond ((= x y) 2) (else (not (= x 2))))
	    ;; (cond ((= x y) 2) ((= x 2) #t) (else #f)) -> (cond ((= x y) 2) (else (= x 2)))
	    (when (and (> len 2)
		       (or (null? (cdr last-clause))
			   (and (len=1? (cdr last-clause))
				(boolean? (cadr last-clause)))))
	      (let ((else-clause (cdr (list-ref form len)))
		    (next-clause (cdr (list-ref form (- len 2)))))
		(when (and (len=1? else-clause)
			   (boolean? (car else-clause))
			   (not (equal? (cdr last-clause) else-clause))
			   (len=1? next-clause)
			   (not (boolean? (car next-clause))))
		  (lint-format "perhaps ~A" caller
			       (lists->string form
					      (append (copy form (make-list (- len 1)))
						      (list (list 'else (if (car else-clause)
									    (list 'not (car last-clause))
									    (car last-clause)))))))))))

	  ;; -------- cond-scan-clauses --------
	  (define (cond-scan-clauses caller form len env)
	    (let ((ctr 0)
		  (result :unset)
		  (has-else #f)
		  (has-combinations #f)
		  (simplifications ())
		  (all-eqv #t)
		  (eqv-select #f)
		  (exprs ())
		  (falses ())
		  (trues ())
		  (prev-bool #f)
		  (prev-clause #f)
		  (eqv-change 0))
	      (for-each
	       (lambda (clause)
		 (set! ctr (+ ctr 1))
		 (when all-eqv
		   (unless eqv-select
		     (set! eqv-select (eqv-selector (car clause))))
		   (set! all-eqv (and eqv-select
				      (not (and (pair? (cdr clause))
						(eq? (cadr clause) '=>))) ; case sends selector, but cond sends test result
				      (cond-eqv? (car clause) eqv-select #t)))
		   (when (and (not all-eqv)
			      eqv-select
			      (pair? (car clause))
			      (eq? (caar clause) 'not)
			      (pair? (cdar clause))
			      (pair? (cadar clause))
			      (cond-eqv? (cadar clause) eqv-select #t))
		     (set! eqv-change ctr)))

		 ;; look for successive clause tests where the earlier includes the current (number? followed by integer? etc)
		 ;;   slightly sloppy I guess -- the arg could be self-modifying!
		 (if (not (and (len>1? (car clause))
			       (hash-table-ref bools (caar clause))))
		     (set! prev-bool #f)
		     (begin
		       (if (and prev-bool
				(equal? (cadar prev-clause) (cadar clause))  ; args match
				(subsumes? prev-bool (caar clause)))         ; previous test already included this case
			   (lint-format "~A makes ~A pointless in ~A~A" caller
					(car prev-clause)
					(car clause)
					(truncated-list->string form)
					(if (eq? prev-bool 'list?)
					    (format #f "~%~NC(r5rs list? is proper-list? in s7)" (+ lint-left-margin 4) #\space)
					    "")))
		       (set! prev-bool (caar clause))))

		 (if (and (pair? prev-clause) ; i.e. not #f
			  (not has-combinations)
			  (> len 2)
			  (equal? (cdr clause) (cdr prev-clause)))
		     (if (memq (car clause) '(else #t))        ; (cond ... (x z) (else z)) -> (cond ... (else z))
			 (unless (side-effect? (car prev-clause) env)
			   ;; (cond (x y) (z 32) (else 32))
			   (lint-format "this clause could be omitted: ~A" caller (truncated-list->string prev-clause)))
			 (set! has-combinations #t)))          ; handle these later
		 (set! prev-clause clause)

		 (let ((expr (simplify-boolean (car clause) trues falses env))
		       (test (car clause))
		       (sequel (cdr clause))
		       (first-sequel (and (pair? (cdr clause)) (cadr clause))))
		   (if (not (equal? expr test))
		       (set! simplifications (cons (cons clause expr) simplifications)))

		   (when (and (pair? falses)          ; (cond ((not x) y) ((string=? x z)...))
			      (pair? (car falses))
			      (eq? (caar falses) 'not)
			      (symbol? (cadar falses))
			      (pair? test)
			      (memq (cadar falses) test))
		     (and-incomplete form 'if2 (cadar falses) test env))

		   (if (symbol? test)
		       (if (and (not (eq? test 'else))
				(pair? first-sequel))
			   (if (memq test first-sequel)
			       (and-incomplete form 'cond test first-sequel env)
			       (do ((p first-sequel (cdr p)))
				   ((or (not (pair? p))
					(and (pair? (car p))
					     (memq test (car p))))
				    (if (pair? p)
					(and-incomplete form 'cond test (car p) env))))))
		       (when (and (len>1? test)
				  (pair? first-sequel)
				  (hash-table-ref bools (car test)))
			 (if (member (cadr test) first-sequel)
			     (and-forgetful form 'cond test first-sequel env)
			     (do ((p first-sequel (cdr p)))
				 ((or (not (pair? p))
				      (and (pair? (car p))
					   (member (cadr test) (car p))))
				  (if (pair? p)
				      (and-forgetful form 'cond test (car p) env)))))))
		   ;; code here to check every arg against its use in the sequel found no problems?!?

		   (if (and (len>1? sequel)
			    (memq '=> (cdr sequel)))
		       (lint-format "'=> has no effect here: ~A~%" caller (truncated-list->string clause)))
		   ;; args>1 never happens so no need for mismatch check

		   (cond ((memq test '(else #t))
			  (set! has-else #t)

			  (when (pair? sequel)
			    (if (eq? first-sequel #<unspecified>)    ; (cond ((= x y) z) (else #<unspecified>)
				(lint-format "this #<unspecified> is redundant: ~A" caller clause))

			    (when (and (pair? first-sequel)   ; (cond (a A) (else (cond ...))) -> (cond (a A) ...)
				       (memq (car first-sequel) '(if cond when unless)))

			      (if (null? (cdr sequel))
				  (case (car first-sequel)
				    ((cond)
				     ;; (cond ((< x 1) 2) (else (cond ((< y 3) 2) (#t 4))))
				     (lint-format "else clause could be folded into the outer cond: ~A" caller
						  (lists->string form (append (copy form (make-list ctr))
									      (cdr first-sequel)))))
				    ((if)
				     ;; (cond (a A) (else (if b B)))
				     (when (and (len>1? (cdr first-sequel))
						(proper-list? first-sequel))
				       (lint-format "else clause could be folded into the outer cond: ~A" caller
						    (lists->string form
								   (append (copy form (make-list ctr))
									   (if (= (length first-sequel) 3)
									       (list (cdr first-sequel))
									       `((,(cadr first-sequel) ,@(unbegin (caddr first-sequel)))
										 (else ,@(unbegin (cadddr first-sequel))))))))))
				    ((when unless)
				     ;; (cond (a A) (else (when b B)))
				     (when (> (length first-sequel) 2)
				       (lint-format "else clause could be folded into the outer cond: ~A" caller
						    (lists->string form
								   (append (copy form (make-list ctr))
									   (if (eq? (car first-sequel) 'when)
									       `((,(cadr first-sequel) ,@(cddr first-sequel)))
									       `(((not ,(cadr first-sequel)) ,@(cddr first-sequel))))))))))

				  ;; combine else -> cond if the trailing result is very simple (it will be repeated)
				  (when (and (pair? (cdr sequel))
					     (null? (cddr sequel))
					     (< (tree-leaves (cadr sequel)) 7))
				    (let ((result (cadr sequel)))
				      (case (car first-sequel)
					((cond)
					 ;; (cond (A a) (B b) (else (cond (C c) (D d)) #t)) -> (cond (A a) (B b) (C c #t) (D d #t) (else #t))
					 (lint-format "else clause could be folded into the outer cond: ~A" caller
						      (lists->string form (append (copy form (make-list ctr))
										  (map (lambda (c)
											 (append c (list result)))
										       (cdr first-sequel))
										  (if (memq (car (last-ref first-sequel)) '(else #t))
										      ()
										      (list (list 'else result)))))))
					((if)
					 ;; (cond (A a) (B b) (else (if C c d) #t)) -> (cond (A a) (B b) (C c #t) (else d #t)
					 (when (and (len>1? (cdr first-sequel))
						    (proper-list? first-sequel))
					   (lint-format "else clause could be folded into the outer cond: ~A" caller
							(lists->string form
								       (append (copy form (make-list ctr))
									       (if (= (length first-sequel) 3)
										   `((,@(cdr first-sequel) ,result)
										     (else ,result))
										   `((,(cadr first-sequel) ,@(unbegin (caddr first-sequel)) ,result)
										     (else ,@(unbegin (cadddr first-sequel)) ,result))))))))
					((when unless)
					 ;; (cond (A a) (B b) (else (unless C c d) #t)) -> (cond (A a) (B b) ((not C) c d #t) (else #t))
					 (when (> (length first-sequel) 2)
					   (lint-format "else clause could be folded into the outer cond: ~A" caller
							(lists->string form
								       (append (copy form (make-list ctr))
									       (if (eq? (car first-sequel) 'when)
										   `((,(cadr first-sequel) ,@(cddr first-sequel) ,result)
										     (else ,result))
										   `(((not ,(cadr first-sequel)) ,@(cddr first-sequel) ,result)
										     (else ,result)))))))))))))))
			 ((not (= ctr len)))

			 ((equal? test ''else)
			  ;; (cond (x y) ('else z))
			  (lint-format "odd cond clause test: is 'else supposed to be else? ~A" caller
				       (truncated-list->string clause)))

			 ((and (eq? test 't)
			       (not (var-member 't env)))
			  ;; (cond ((= x 1) 1) (t 2)
			  (lint-format "odd cond clause test: is t supposed to be #t? ~A" caller
				       (truncated-list->string clause))))

		   (if (never-false expr)
		       (if (not (= ctr len))
			   ;; (cond ((getenv s) x) ((= y z) w))
			   (lint-format "cond test ~A is never false: ~A" caller (car clause) (truncated-list->string form))
			   (if (not (or (memq expr '(#t else))
					(side-effect? test env)))
			       (lint-format "cond last test could be #t: ~A" caller form)))
		       (if (never-true expr)
			   ;; (cond ((< 3 1) 2))
			   (lint-format "cond test ~A is never true: ~A" caller (car clause) (truncated-list->string form))))

		   (unless (side-effect? test env)
		     (cond ((or (memq test '(else #t))
				(not (pair? sequel))
				(pair? (cdr sequel))))

			   ((equal? test first-sequel)
			    ;; (cond ((= x 0) x) ((= x 1) (= x 1)))
			    (lint-format "no need to repeat the test: ~A" caller (lists->string clause (list test))))

			   ((and (len=2? first-sequel)
				 (equal? test (cadr first-sequel)))
			    (if (eq? (car first-sequel) 'not)
				;; (cond ((> x 2) (not (> x 2))))
				(lint-format "perhaps replace ~A with #f" caller first-sequel)
				;; (cond (x (abs x)))
				(lint-format "perhaps use => here: ~A" caller
					     (lists->string clause (list test '=> (car first-sequel))))))

			   ((and (eq? first-sequel #t)
				 (pair? test)
				 (not (memq (car test) '(or and)))
				 (eq? (return-type (car test) env) 'boolean?))
			    ;; (cond ((null? x) #t) (else y))
			    (lint-format "this #t could be omitted: ~A" caller (truncated-list->string clause))))

		     (if (member test exprs)
			 ;; (cond ((< x 2) 3) ((> x 0) 4) ((< x 2) 5))
			 (lint-format "cond test repeated: ~A" caller (truncated-list->string clause))
			 (set! exprs (cons test exprs))))

		   (if (boolean? expr)
		       (if (not expr)
			   ;; (cond ((< 3 1) 2))
			   (lint-format "cond test is always false: ~A" caller (truncated-list->string clause))
			   (if (not (= ctr len))
			       ;; (cond (#t 2) (x 3))
			       (lint-format "cond #t clause is not the last: ~A" caller (truncated-list->string form))))
		       (if (eq? test 'else)
			   (if (not (= ctr len))
			       ;; (cond (else 2) (x 3))
			       (lint-format "cond else clause is not the last: ~A" caller (truncated-list->string form)))
			   (lint-walk caller test env)))

		   (if (and (symbol? expr)
			    (not (var-member expr env))
			    (procedure? (symbol->value expr *e*)))
		       ;; (cond (< x 1) (else 1))
		       (lint-format "strange cond test: ~A in ~A is a procedure" caller expr clause))

		   (if (eq? result :unset)
		       (set! result sequel)
		       (if (not (equal? result sequel))
			   (set! result :unequal)))

		   (check-results caller 'cond clause sequel env)

		   (if (side-effect? expr env)
		       (begin
			 (set! falses ())
			 (set! trues ())
			 (set! result :unequal))
		       (begin
			 (if (not (member expr falses))
			     (set! falses (cons expr falses)))
			 (when (len>1? expr)
			   (if (and (eq? (car expr) 'not)
				    (not (member (cadr expr) trues)))
			       (set! trues (cons (cadr expr) trues)))
			   (if (eq? (car expr) 'or)
			       (for-each (lambda (p)
					   (if (not (member p falses))
					       (set! falses (cons p falses))))
					 (cdr expr))))))))
	       (cdr form)) ; for-each clause

	      (if has-else
		  (if (pair? result) ; all result clauses are the same (and not implicit)
		      ;; (cond (x #t) (else #t)) -> #t
		      (lint-format "perhaps ~A" caller (lists->string form
								      (if (null? (cdr result))
									  (car result)
									  (cons 'begin result)))))
		  (let* ((last-clause (and (> len 1)
					   (list-ref form len)))
			 (last-res (let ((clen (length last-clause)))
				     (and (integer? clen)
					  (> clen 1)
					  (list-ref last-clause (- clen 1))))))
		    (if (and (pair? last-res)
			     (memq (car last-res) '(#t else)))
			;; (cond (x y) (y z (else 3)))
			(lint-format "perhaps cond else clause is misplaced: ~A in ~A" caller last-res last-clause))))

	      (values has-else has-combinations simplifications all-eqv eqv-select eqv-change)))


	  ;; -------- cond-walker --------

	  (define (cond-walker-1 caller form len suggest env has-else has-combinations simplifications all-eqv eqv-select eqv-change)

	    (when (and (= len 2)
		       (not (check-bool-cond caller form (cadr form) (caddr form) env)))
	      (cond-remove-not caller form env))
	    ;; not+repeat got 1 hit

	    (when has-combinations
	      (cond->or caller form all-eqv eqv-select simplifications env)
	      (set! simplifications ())
	      (set! all-eqv #f))

	    ;; cond -> case
	    (if all-eqv
		(if (> len (if has-else 2 1)) ; (cond (x y)) -- kinda dumb, but (if x y) isn't much shorter
		    ;; (cond ((= x 0) x) ((= x 1) (= x 1))) -> (case x ((0) x) ((1) (= x 1)))
		    (lint-format "perhaps use case instead of cond: ~A" caller
				 (lists->string form (cond->case eqv-select (cdr form)))))

		(when (and eqv-select
			   (> len 1))
		  (if (= len eqv-change)
		      ;; (cond ((= x 0) 0) ((not (= x 1)) 1)) -> (case x ((0) 0) ((1) #<unspecified>) (else 1))
		      (if (not (equal? (car (list-ref form (- len 1))) (cadar (list-ref form len))))
			  (let ((new-cond (make-list (+ len 2))))
			    (copy form new-cond 0 eqv-change)
			    (list-set! new-cond len
				       (list (cadar (list-ref form len)) #<unspecified>))
			    (list-set! new-cond (+ len 1)
				       (list 'else (cadr (list-ref form len))))
			    (lint-format "perhaps use case instead of cond: ~A" caller
					 (lists->string form
							(cond->case eqv-select (cdr new-cond))))))
		      (if (= len (+ eqv-change 1))
			  ;; (cond ((= x 0) 0) ((not (= x 1)) 1) (else 2)) -> (case x ((0) 0) ((1) 2) (else 1))
			  (let ((new-cond (make-list (+ len 1)))
				(last-clause (list-ref form len))
				(next-to-last-clause (list-ref form (- len 1))))
			    (copy form new-cond 0 eqv-change)
			    (list-set! new-cond (- len 1)
				       (list (cadar next-to-last-clause)
					     (if (memq (car last-clause) '(#t else))
						 (cadr last-clause)
						 (if (or (equal? (cadar next-to-last-clause) (car last-clause)) ; ((not (= x 1))...) ((= x 1)...)
							 (and (eq? (caar last-clause) 'not)
							      (not (member (cadar last-clause) new-cond))))     ; ((not (= x 1))...) ((not (= x 2))...)
						     (cadr (list-ref form len))))))
			    ;; (cond ((= x 0) 0) ((not (= x 1)) 1) ((= x 2) 2)) -> (case x ((0) 0) ((1) #<unspecified>) (else 1))
			    ;; (cond ((= x 0) 0) ((not (= x 1)) 1) ((= x 1) 2)) -> (case x ((0) 0) ((1) 2) (else 1))
			    ;; (cond ((= x 1) 1) ((not (= x 2)) 2) ((not (= x 3)) 3)) -> (case x ((1) 1) ((2) 3) (else 2))
			    (list-set! new-cond len
				       (list 'else (cadr (list-ref form (- len 1)))))
			    (lint-format "perhaps use case instead of cond: ~A" caller
					 (lists->string form
							(cond->case eqv-select (cdr new-cond)))))))))
	    (if (and (= len 2)
		     has-else
		     (null? (cdadr form)))
		(let ((else-clause (if (null? (cdaddr form))
				       'else
				       (if (null? (cddr (caddr form)))
					   (cadr (caddr form))
					   (cons 'begin (cdaddr form))))))
		  ;; (cond ((a)) (else A)) -> (or (a) A)
		  ;;    but these two are not currently rewritten using if: (cond (A B) (else)) (cond (A B) (else C))
		  (lint-format "perhaps ~A" caller (lists->string form `(or ,(caadr form) ,else-clause)))))

	    (unless (or has-combinations all-eqv)
	      (cond->assoc caller form simplifications))

	    (when (and (> len 3)
		       (= suggest made-suggestion))
	      (cond->case-at-end caller form len has-else))

	    (combine-repeated-tests caller form env)
	    (if (= len 1)
		(simple-cond->if caller form suggest)
		(when has-else                                   ; len > 1 here
		  (let ((last-clause (list-ref form (- len 1)))) ; not the else branch! -- just before it.

		    (when (and (= suggest made-suggestion) ; look for all results the same
			       (len>1? (cadr form)))
		      (cond-one-result caller form last-clause len env))

		    (when (= len 3)
		      (simplify-cond caller form env))

		    (when (> len 3)
		      (cond-repeated-else caller form len env))

 		    (cond-partial-test-repeat caller form)
		    (cond-combine-into-else caller form last-clause len))))

	      (combine-conds caller form has-else len env))

	  (define (cond-walker caller form env)
	    (let ((len (- (length form) 1))
		  (suggest made-suggestion))
	      (if (or (< len 1)
		      (not (just-pairs? (cdr form))))
		  (lint-format "cond is messed up: ~A" caller (truncated-list->string form))
		  (begin
		    (cond->header+cond+trailer caller form len env) ; obviously out-of-place...
		    (cond-walker-1 caller form len suggest env (cond-scan-clauses caller form len env))))
	      env))

	  (hash-walker 'cond cond-walker))


	;; ---------------- case ----------------
	(let ()

	  ;; -------- case->header+case+trailer --------
	  (define (case->header+case+trailer caller form len env)
	    ;; if start/end args match (including func), move outside case (case sets whatever differs, using values if necessary)
	    (let ((first-clause (caddr form))
		  (else-clause (list-ref form (+ len 1))))
	      (when (and (len=1? (cdr first-clause))
			 (pair? (cadr first-clause))
			 (len>1? else-clause)
			 (eq? (car else-clause) 'else)
			 (pair? (cadr else-clause))
			 (or (equal? (caadr first-clause) (caadr else-clause)) ; there's some hope we'll match
			     (escape? (cadr else-clause) env)))
		(let ((first-result (cadr first-clause))
		      (first-func (caadr first-clause))
		      (else-error (escape? (cadr else-clause) env)))
		  (when (and (pair? (cdr first-result))
			     (not (eq? first-func 'values))
			     (or (not (hash-table-ref syntaces first-func))
				 (eq? first-func 'set!))
			     (lint-every? (lambda (c)
					    (and (len=2? c)
						 (len>1? (cadr c))
						 (or (equal? first-func (caadr c))
						     (and (eq? c else-clause)
							  else-error))))
					  (cdddr form)))

		    ((lambda (header-len trailer-len result-mid-len)
		       (when (and (>= header-len 0)
				  (>= trailer-len 0)
				  (or (not (eq? first-func 'set!))
				      (> header-len 1))
				  (or (not (eq? first-func '/))
				      (> header-len 1)
				      (> trailer-len 0)))
			 (let ((header (copy first-result (make-list header-len)))
			       (trailer (copy first-result (make-list trailer-len) (- (length first-result) trailer-len))))
			   (if (= len 2)
			       (unless (equal? first-result (cadr else-clause)) ; handled elsewhere (all results equal -> result)
				 ;; (case x ((1) (+ x 1)) (else (+ x 3))) -> (+ x (if (eqv? x 1) 1 3))
				 (lint-format "perhaps ~A" caller
					      (let ((else-result (cadr else-clause)))
						(let ((first-mid-len (- (length first-result) header-len trailer-len))
						      (else-mid-len (- (length else-result) header-len trailer-len)))
						  (let* ((fmid (if (= first-mid-len 1)
								   (list-ref first-result header-len)
								   (cons 'values (copy first-result (make-list first-mid-len) header-len))))
							 (emid (if else-error
								   else-result
								   (if (= else-mid-len 1)
								       (list-ref else-result header-len)
								       (cons 'values (copy else-result (make-list else-mid-len) header-len)))))
							 (middle (if (len=1? (car first-clause))
								     (list 'eqv? (cadr form) (caar first-clause))
								     `(memv ,(cadr form) ',(car first-clause)))))
						    (lists->string form (append header (cons (list 'if middle fmid emid) trailer))))))))
			       ;; len > 2 so use case in the revision
			       (let ((midctr 0)
				     (valctr 0))
				 (let ((middle
					(map (lambda (c)
					       (if (and else-error
							(eq? c else-clause))
						   else-clause
						   (let ((test (car c))
							 (result (cadr c)))
						     (let ((mid-len (- (length result) header-len trailer-len)))
						       (if (= mid-len 1)
							   (set! midctr (+ midctr 1))
							   (set! valctr (+ valctr 1)))
						       (list test (if (= mid-len 1)
								      (list-ref result header-len)
								      (cons 'values (copy result (make-list mid-len) header-len))))))))
					     (cddr form))))
				   ;; (case x ((0) (log x 2)) ((1) (log x 3)) (else (error 'oops))) -> (log x (case x ((0) 2) ((1) 3) (else (error 'oops))))
				   (if (> midctr valctr)
				       (lint-format "perhaps ~A" caller
						    (lists->string form `(,@header (case ,(cadr form) ,@middle) ,@trailer))))))))))
		     (partition-form (cddr form) (if else-error (- len 1) len))))))))

	  ;; -------- simplify-case --------
	  (define (simplify-case caller form selector env)
	    ;; case->simpler expr (eq? memq etc)
	    (let ((clauses (cddr form)))            ; (case x ((a) #t) (else #f)) -> (eq? x 'a) -- this stuff actually happens!
	      (if (null? (cdr clauses))
		  (let ((clause (car clauses)))
		    (when (and (len>1? clause)
			       (pair? (car clause)))
		      (let ((keys (car clause)))
			;; (case 3 ((0) #t)) -> (if (eqv? 3 0) #t)
			;; (case x ((#(0)) 2)) -> (if (eqv? x #(0)) 2)
			(lint-format "perhaps ~A" caller
				     (lists->string form
						    (let ((test (cond ((pair? (cdr keys))
								       `(memv ,(cadr form) ',keys))

								      ((and (symbol? (car keys))
									    (not (keyword? (car keys))))
								       `(eq? ,(cadr form) ',(car keys)))

								      ((or (keyword? (car keys))
									   (null? (car keys)))
								       (list 'eq? (cadr form) (car keys)))

								      ((not (boolean? (car keys)))
								       (list 'eqv? (cadr form) (car keys)))

								      ((car keys)
								       (cadr form))

								      (else (list 'not (cadr form)))))

							  (op (if (len>1? (cdr clause))
								  'when 'if)))
						      (cons op (cons test (cdr clause)))))))))
		  (when (and (null? (cddr clauses))
			     (len=2? (car clauses))
			     (len=2? (cadr clauses))
			     (eq? (caadr clauses) 'else)
			     (not (equal? (cadadr clauses) (cadar clauses)))
			     (pair? (caar clauses)))
		    (let* ((akey (null? (cdaar clauses)))
			   (keylist ((if akey caaar caar) clauses))
			   (quoted (or (not akey) (symbol? keylist)))
			   (op (if (just-symbols? (caar clauses))
				   (if akey 'eq? 'memq)
				   (if akey 'eqv? 'memv))))
		      ;; can't use '= or 'char=? here because the selector may return anything
		      ;; (case x ((#\a) 3) (else 4)) -> (if (eqv? x #\a) 3 4)
		      ;; (case x ((a) #t) (else #f)) -> (eq? x 'a)
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  (cond ((and (boolean? (cadar clauses))
							      (boolean? (cadadr clauses)))
							 (if (cadadr clauses)
							     (list 'not (list op selector (if quoted (list 'quote keylist) keylist)))
							     (list op selector (if quoted (list 'quote keylist) keylist))))

							((not (cadadr clauses)) ; (else #f) happens a few times
							 (simplify-boolean
							  (if quoted
							      `(and (,op ,selector ',keylist) ,(cadar clauses))
							      `(and (,op ,selector ,keylist) ,(cadar clauses)))
							  () () env))

							(quoted
							 `(if (,op ,selector ',keylist) ,(cadar clauses) ,(cadadr clauses)))

							(else
							 (let ((select-expr (if (and (eq? op 'eqv?)
										     (boolean? keylist)
										     (or (and (symbol? selector)
											      (not keylist))
											 (and (pair? selector)
											      (symbol? (car selector))
											      (let ((sig (arg-signature (car selector) env)))
												(and (pair? sig)
												     (eq? (car sig) 'boolean?))))))
										(if keylist selector (list 'not selector))
										(list op selector keylist))))
							   (list 'if select-expr (cadar clauses) (cadadr clauses))))))))))))

	  ;; -------- check-keys --------
	  (define (check-keys caller form selector sel-type env)
	    ;; combine repeated results, check for repeated or unmatchable keys
	    (let ((all-keys ())
		  (result :unset)
		  (exprs-repeated #f)
		  (else-foldable #f)
		  (has-else #f))
	      (let ((all-exprs ())
		    (ctr 0)
		    (len (length (cddr form))))
		(for-each
		 (lambda (clause)
		   (set! ctr (+ ctr 1))
		   (let ((keys (car clause))
			 (exprs (cdr clause)))
		     ;; exprs can be null (4-Jan-17)
		     (if (eq? result :unset)
			 (set! result exprs)
			 (if (not (equal? result exprs))
			     (set! result :unequal)))

		     (if (and (len>1? exprs)           ; this gets no hits -- it paralells a similar bug in cond: (test expr => expr)
			      (memq '=> (cdr exprs)))
			 (lint-format "'=> has no effect here: ~A~%" caller (truncated-list->string clause)))

		     (if (member exprs all-exprs)
			 (set! exprs-repeated exprs)
			 (set! all-exprs (cons exprs all-exprs)))

		     (when (len=1? exprs)
		       (if (or (equal? (car exprs) (cadr form))       ; (case x ((0 1) x) (else #f))
			       (and (pair? keys)
				    (null? (cdr keys))
				    (code-constant? (car exprs))
				    (eqv? (car keys) (car exprs))))
			   (lint-format "in ~A, the result can be ~Aomitted" caller
					clause
					(if (and (pair? (car exprs))  ; all this paranoia for one hit
						 (equal? (car exprs) (cadr form))
						 (side-effect? (car exprs) env))
					    "probably " "")))

		       (when (and (len=2? (car exprs))
				  (equal? selector (cadar exprs)))
			 (if (and (pair? keys)                        ; (case x ((0) (f x)) ((1) (not x)))
				  (eq? (caar exprs) 'not)
				  (not (memq #f keys)))
			     (lint-format "in ~A, perhaps replace ~A with #f" caller clause (car exprs))
			     (if (eq? keys 'else)
				 (lint-format "perhaps use => here: ~A" caller
					      (lists->string clause (list 'else '=> (caar exprs))))))))

		     (if (pair? keys)
			 (if (not (proper-list? keys))
			     ;; (case x ((0) 1) ((1) 2) ((3 . 0) 4))
			     (lint-format (if (null? keys)
					      "null case key list: ~A"
					      "stray dot in case case key list: ~A")
					  caller (truncated-list->string clause))
			     (for-each
			      (lambda (key)
				;; TODO: (or type1...) -> memq (type-of..)..
				(if (and (number? key)
					 (nan? key))
				    (lint-format "case key ~S in ~S is unlikely to work" caller key clause)
				    (if (or (and (sequence? key)
						 (not (null? key)))
					    (memq (type-of key) '(procedure? macro? iterator? c-object? c-pointer? syntax? input-port? output-port? random-state?)))
					;; (case x ((#(0)) 2)) or (apply case ...)
					(lint-format "case key ~S in ~S is unlikely to work (case uses eqv? but ~S is a ~A)" caller
						     key clause key
						     (type-of key))))
				(if (memv key all-keys)
				    ;; (case x ((0) 1) ((1) 2) ((3 0) 4)) but not (case x ((0 0.0))...)
				    (lint-format "repeated case key ~S in ~S" caller key clause)
				    (set! all-keys (cons key all-keys)))
				;; unintentional quote here, as in (case x ('a b)...) never happens and
				;;   is hard to distinguish from (case x ((quote a) b)...) which happens a lot

				(if (not (compatible? sel-type (if (symbol? key) 'symbol? (->lint-type key))))
				    ;; (case (string->symbol x) ((a) 1) ((2 3) 3))
				    (lint-format "case key ~S in ~S is pointless" caller key clause)))
			      keys))

			 (if (not (eq? keys 'else))
			     ;; (case ((1) 1) (t 2))
			     (lint-format "bad case key ~S in ~S" caller keys clause)
			     (begin
			       (set! has-else clause)
			       ;; exprs: (res) or if case, ((case ...)...)
			       (if (not (= ctr len))
				   ;; (case x (else 2) ((0) 1))
				   (lint-format "case else clause is not the last: ~A"
						caller
						(truncated-list->string (cddr form)))
				   (when (and (len=1? exprs)
					      (len>1? (car exprs)))
				     (let ((expr (car exprs)))
				       (case (car expr)
					 ((case)                     ; just the case statement in the else clause
					  (when (and (equal? selector (cadr expr))
						     (not (side-effect? selector env)))
					    (set! else-foldable (cddr expr))))
					 ((if)                       ; just if -- if foldable, make it look like it came from case
					  (when (and (equal? selector (eqv-selector (cadr expr)))
						     (cond-eqv? (cadr expr) selector #t)
						     (not (side-effect? selector env)))
					    ;; else-foldable as (((keys-from-test) true-branch) (else false-branch))
					    (set! else-foldable
						  (if (pair? (cdddr expr))
						      (list (case-branch (cadr expr) selector (list (caddr expr)))
							    (list 'else (cadddr expr)))
						      (list (case-branch (cadr expr) selector (cddr expr))))))))))))))

		     (check-results caller (car form) clause exprs env))) ; walk the result exprs
		 (cddr form)))

	      (let ((key-phrase
		     (let ((keylen (length all-keys)))
		       (cond ((< keylen 20))
			     ((lint-every? char? all-keys)
			      "vector (indexed by char->integer)")
			     ((lint-every? (lambda (k) (and (integer? k) (<= 0 k 1000))) all-keys)
			      "vector")
			     ((> keylen 40)
			      "hash-table")))))
		(when (string? key-phrase)
		  (lint-format "perhaps use a ~A rather than a case statement:~%~NC~A" caller
			       key-phrase
			       (+ lint-left-margin 4) #\space
			       (truncated-list->string form))))

	      (when (and has-else
			 (pair? result)
			 (not else-foldable))
		;; (case x (else (case x (else 1)))) -> 1
		(lint-format "perhaps ~A" caller (lists->string form
								(if (null? (cdr result))
								    (car result)
								    (cons 'begin result))))
		(set! exprs-repeated #f))
	      ;; repeated result (but not all completely equal) and with else never happens

	      (when (or exprs-repeated else-foldable)
		(let ((new-keys-and-exprs ())
		      (mergers ())
		      (else-clause (if else-foldable
				       (call-with-exit
					(lambda (return)
					  (for-each (lambda (c) (if (eq? (car c) 'else) (return c))) else-foldable)
					  ()))
				       (or has-else ()))))

		  (let ((merge-case-keys
			 (let ((else-exprs (and (pair? else-clause) (cdr else-clause)))
			       (a-few (lambda (lst)
					(if (> (length lst) 3)
					    (copy lst (make-list 4 '...) 0 3)
					    lst))))
			   (lambda (clause)
			     (when (len>1? clause)             ; ignore clauses that are messed up
			       (let ((keys (car clause))
				     (exprs (cdr clause)))
				 (unless (or (not (pair? keys)) ; else clause or buggy case key list
					     (equal? exprs else-exprs))
				   (let ((prev (member exprs new-keys-and-exprs (lambda (a b) (equal? a (cdr b))))))
				     (if prev
					 (let* ((cur-clause (car prev))
						(cur-keys (car cur-clause)))
					   (when (pair? cur-keys)
					     (set! mergers (cons (list (a-few keys) (a-few cur-keys)) mergers))
					     (set-car! cur-clause
						       (append cur-keys
							       (map (lambda (key)
								      (if (memv key cur-keys) (values) key))
								    keys)))))
					 (set! new-keys-and-exprs (cons (cons (copy (car clause))
									      (cdr clause))
									new-keys-and-exprs)))))))))))

		    (for-each merge-case-keys (cddr form))
		    (if (pair? else-foldable)
			(for-each merge-case-keys else-foldable)))

		  (if (null? new-keys-and-exprs)
		      (lint-format "perhaps ~A" caller
				   ;; (case x (else (case x (else 1)))) -> 1
				   (lists->string form
						  (if (or (null? else-clause)    ; this can happen...
							  (null? (cdr else-clause)))
						      ()
						      (if (null? (cddr else-clause))
							  (cadr else-clause)
							  (cons 'begin (cdr else-clause))))))
		      (begin
			;; (null? (cdr new-keys-and-exprs)) is rare and kinda dumb -- cases look like test suite entries
			(for-each
			 (lambda (clause)
			   (if (len>1? (car clause))
			       (if (just-integers? (car clause))
				   (set-car! clause (sort! (car clause) <))
				   (if (lint-every? char? (car clause))
				       (set-car! clause (sort! (car clause) char<?))))))
			 new-keys-and-exprs)
			(let ((new-form (if (pair? else-clause)
					    `(case ,(cadr form) ,@(reverse new-keys-and-exprs) ,else-clause)
					    `(case ,(cadr form) ,@(reverse new-keys-and-exprs)))))
			  ;; (case x ((0) 32) ((1) 32)) -> (case x ((0 1) 32))
			  (lint-format "perhaps ~A" caller
				       (if (pair? mergers)
					   (format #f "merge keys ~{~{~A with ~A~}~^, ~}: ~A"
						   (reverse mergers)
						   (lists->string form new-form))
					   (lists->string form new-form))))))))))

	  ;; -------- case->symbol->value --------
	  (define (case->symbol->value caller form) ; (case x ((abs) abs) ((expt) expt)...) -> (symbol->value x)
	    (do ((selector (cadr form))
		 (svs ())
		 (others #f)
		 (p (cddr form) (cdr p)))
		((null? p)
		 (when (pair? svs)
		   (let-temporarily ((made-suggestion 0))
		     (set! svs (reverse svs))
		     (lint-format "perhaps use => here: ~A" caller
				  (lists->string form
						 `(case ,selector
						    ,@(if (memv (car (caaddr form)) (cdar svs)) () '(...))
						    ,@(map (lambda (sv)
							     (list (reverse (cdr sv)) '=> (case (car sv)
											    ((list-values) 'list)
											    (else))))
							   svs)
						    ,@(if others '(...) ())))))))
	      (let ((c (car p)))
		(cond ((not (and (len=2? c)
				 (pair? (car c))))
		       (set! others #t))

		      ((and (null? (cdar c))                 ; ((a) a)
			    (symbol? (caar c))
			    (eq? (caar c) (cadr c)))         ; the quoted case happens only in test suites
		       (cond ((assq 'symbol->value svs)
			      => (lambda (sv-data)
				   (set-cdr! sv-data (cons (caar c) (cdr sv-data)))))
			     (else (set! svs (cons (list 'symbol->value (caar c)) svs)))))

		      ((and (just-symbols? (car c))         ; ((a b c) (eval selector))
			    (len=2? (cadr c))
			    (memq (caadr c) '(eval symbol->value))
			    (equal? (cadadr c) selector))
		       (cond ((assq 'symbol->value svs)
			      => (lambda (sv-data)
				   (set-cdr! sv-data (append (reverse (car c)) (cdr sv-data)))))
			     (else (set! svs (cons (cons 'symbol->value (reverse (car c))) svs)))))

		      ((and (null? (cdar c))                 ; ((a) (f a))
			    (len=2? (cadr c))
			    (eqv? (caar c) (cadadr c))
			    (not (memq (caadr c) '(quote values))))
		       (cond ((assq (caadr c) svs)
			      => (lambda (func-data)
				   (set-cdr! func-data (cons (caar c) (cdr func-data)))))
			     (else (set! svs (cons (list (caadr c) (caar c)) svs)))))

		      ((not (and (len=2? (cadr c))
				 (equal? (cadadr c) selector)))
		       (set! others #t))

		      ((assq (caadr c) svs)
		       => (lambda (func-data)  ; ((1 b #f) (func selector))
			    (set-cdr! func-data (append (reverse (car c)) (cdr func-data)))))

		      (else
		       (set! svs (cons (cons (caadr c) (reverse (car c))) svs)))))))


	  ;; -------- case-walker --------
	  (define case-walker
	    (let ((selector-types '(#t symbol? char? boolean? integer? rational? real? complex? number? null? eof-object?)))

	      (define (case->case+args caller form len)
		;; if all args match, move outside case (case sets func)
		(let ((first-clause (caddr form))
		      (else-clause (list-ref form (+ len 1))))
		  (when (and (eq? (car else-clause) 'else)
			     (len=1? (cdr first-clause))
			     (len>1? (cadr first-clause))
			     (not (hash-table-ref syntaces (caadr first-clause)))
			     (lint-every? (lambda (c)
					    (and (len=2? c)
						 (pair? (cadr c))
						 (not (hash-table-ref syntaces (caadr c)))
						 (equal? (cdadr first-clause) (cdadr c))))
					  (cdddr form)))
		    ;; (case x ((a) (f y z)) (else (g y z))) -> ((if (eq? x 'a) f g) y z)
		    (lint-format "perhaps ~A" caller      ; all results share trailing args
				 (lists->string form
						(if (and (= len 2)
							 (symbol? (caar first-clause))
							 (null? (cdar first-clause)))
						    `((if (eq? ,(cadr form) ',(caar first-clause))
							  ,(caadr first-clause)
							  ,(caadr else-clause))
						      ,@(cdadr first-clause))
						    `((case ,(cadr form)
							,@(map (lambda (c)
								 (list (car c) (caadr c)))
							       (cddr form)))
						      ,@(cdadr first-clause))))))))

	      (lambda (caller form env)
		;; here the keys are not evaluated, so we might have a list like (letrec define ...)
		;; also unlike cond, only 'else marks a default branch (not #t)

		(if (or (< (length form) 3)
			(not (just-pairs? (cddr form))))    ; (case 3)
		    (lint-format "case is messed up: ~A" caller (truncated-list->string form))
		    ;; perhaps also (lint-every? (lambda (c) (or (pair? c) (eq? c 'else))) (car clause)) above
		    (let ((suggest made-suggestion))

		      ;; if regular case + else, focus case on diff
		      (let ((len (- (length form) 2))) ; number of clauses
			(when (and (> len 1)                 ; (case x (else ...)) is handled elsewhere
				   (len>1? (cdr form))
				   (not (tree-set-memq '(unquote list-values) form)))
			  (case->case+args caller form len)
			  (case->header+case+trailer caller form len env)))
		      (case->symbol->value caller form)

		      (let ((selector (cadr form)))
			(when (= suggest made-suggestion)
			  (simplify-case caller form selector env))

			(if (and (not (pair? selector))
				 (constant? selector))                ; (case 3 ((0) #t))
			    (lint-format "case selector is a constant: ~A" caller (truncated-list->string form))
			    (if (symbol? selector)
				(set-ref selector caller form env)
				(lint-walk caller selector env)))

			(let ((sel-type (and (pair? selector)
					     (symbol? (car selector))
					     (return-type (car selector) env))))
			  (if (and (symbol? sel-type)                ; (case (list 1) ((0) #t))
				   (not (memq sel-type selector-types)))
			      (lint-format "case selector may not work with eqv: ~A" caller (truncated-list->string selector)))
			  (check-keys caller form selector sel-type env))))) ; calls lint-walk-open-body on each result
		env)))
	  (hash-walker 'case case-walker))


	;; ---------------- do ----------------
	(let ()
	  (define (car-subst sym new-sym tree)
	    (cond ((not (unquoted-pair? tree))
		   tree)
		  ((not (and (symbol? (car tree))
			     (len=1? (cdr tree))
			     (eq? sym (cadr tree))))
		   (cons (car-subst sym new-sym (car tree))
			 (car-subst sym new-sym (cdr tree))))

		   ((hash-table-ref cxars (car tree)) => (lambda (f) (if (symbol? f) (list f new-sym) new-sym)))
		   (else tree)))

	  (define (cadr-subst sym new-sym tree)
	    (cond ((not (unquoted-pair? tree))
		   tree)
		  ((and (memq (car tree) '(vector-ref string-ref list-ref))
			(len=3? tree)
			(equal? sym (cadr tree)))
		   new-sym)
		  (else
		   (cons (cadr-subst sym new-sym (car tree))
			 (cadr-subst sym new-sym (cdr tree))))))

	  (define (var-step v) ((cdr v) 'step))

	  ;; -------- pointless-do --------
	  (define (pointless-do caller form)
	    ;; called only if no side-effects in form
	    ;; a much more permissive check here (allowing sets of locals etc) got only a half-dozen hits
	    (let ((end+result (caddr form)))
	      (if (pair? end+result)
		  (if (null? (cdr end+result))                ; (do ((i 0 (+ i 1))) ((= i 1)))
		      (if (not (car end+result))
			  (lint-format "infinite loop: ~A" caller form)
			  (lint-format "this do-loop could probably be replaced by the end test in a let: ~A" caller (truncated-list->string form)))
		      (if (and (null? (cddr end+result))
			       (code-constant? (cadr end+result))) ; (begin (z 1) (do ((i 0 (+ i 1))) ((= i n) 32))): 32
			  (lint-format "this do-loop could be replaced by ~A: ~A" caller (cadr end+result) (truncated-list->string form))))
		  (if (and (null? end+result)          ; (do () ()) -- need more of these
			   (null? (cadr form)))
		      (lint-format "infinite loop: ~A" caller form)))))

	  ;; -------- walk-do-inits --------
	  (define (walk-do-inits caller form env)
	    ;; walk the init forms before adding the step vars to env
	    (do ((vars ())
		 (bindings (cadr form) (cdr bindings)))
		((not (pair? bindings))
		 (if (not (null? bindings))
		     (lint-format "do variable list is not a proper list? ~S" caller (cadr form)))
		 vars)
	      (when (binding-ok? caller 'do (car bindings) env #f)
		(for-each (lambda (v)
			    (if (not (or (eq? (var-initial-value v) (var-name v))
					 (not (tree-memq (var-name v) (cadar bindings)))
					 (hash-table-ref built-in-functions (var-name v))
					 (tree-set-memq binders (cadar bindings))))
				(if (not (var-member (var-name v) env))
				    ;; (let ((xx 0)) (do ((x 1 (+ x 1)) (y x (- y 1))) ((= x 3) xx) (display y)): x
				    (lint-format "~A in ~A does not appear to be defined in the calling environment" caller
						 (var-name v) (car bindings))
				    ;; (let ((x 0)) (do ((x 1 (+ x 1)) (y x (- y 1))) ((= x 3)) (display y))): y
				    (lint-format "~A in ~A refers to the caller's ~A, not the do-loop variable" caller
						 (var-name v) (car bindings) (var-name v)))))
			  vars)
		(lint-walk caller (cadar bindings) env)
		(let ((new-var (let ((v (make-lint-var (caar bindings) (cadar bindings) 'do)))
				 (let ((stepper (and (pair? (cddar bindings)) (caddar bindings))))
				   (varlet (cdr v) :step stepper)
				   (when stepper
				     (set! (var-history v) (cons (list 'set! (caar bindings) stepper) (var-history v)))
				     (set! (var-refenv v) env)))
				 v)))
		  (set! vars (cons new-var vars))))))

	  ;; -------- walk-do-steps --------
	  (define (walk-do-steps caller form vars inner-env env)
	    ;; walk the step exprs
	    (let ((baddies ()) ; these are step vars (with step exprs) used within other step vars step expressions
		  (step-vars (cadr form)))
	      (for-each (lambda (stepper)
			  (when (and (binding-ok? caller 'do stepper env #t)
				     (pair? (cddr stepper)))
			    (let ((data (var-member (car stepper) vars)))
			      (let ((old-ref (var-ref data)))
				(lint-walk caller (caddr stepper) inner-env)
				(set! (var-ref data) old-ref))
			      (if (eq? (car stepper) (caddr stepper))
				  (lint-format "perhaps ~A" caller (lists->string stepper (list (car stepper) (cadr stepper)))))
			      (set! (var-set data) (+ (var-set data) 1)))
			    (when (and (pair? (caddr stepper))
				       (not (eq? (car stepper) (cadr stepper)))
				       (eq? (caaddr stepper) 'cdr)
				       (eq? (cadr stepper) (cadr (caddr stepper))))
			      (lint-format "this looks suspicious: ~A" caller stepper))
			    (let ((step-name (car stepper))
				  (step-step (caddr stepper)))
			      (for-each (lambda (v)
					  (if (and ;(var-step v)  ; a toss-up
						   (not (eq? (var-name v) step-name))
						   (or (eq? (var-name v) step-step)
						       (and (pair? step-step)
							    (tree-memq (var-name v) step-step))))
					      (set! baddies (cons step-name baddies))))
					vars))))
			step-vars)

	      (check-unordered-exprs caller form (map var-initial-value vars) env)

	      (when (pair? baddies)
		;; (do ((i 0 j) (j ...))...) is unreadable -- which (binding of) j is i set to?
		;;    but this is tricky if there is more than one such variable -- if cross links, we'll need named let
		;;    and if no step expr, there's no confusion.
		;;    (do ((i 0 j) (j 1 i) (k 0 (+ k 1))) ((= k 4)) (format *stderr* "~A ~A~%" i j))
		;;    (let <1> ((i 0) (j 1) (k 0)) (if (= k 4) () (begin (format *stderr* "~A ~A~%" i j) (<1> j i (+ k 1)))))
		(let ((new-steppers (map (lambda (stepper)
					   (if (memq (car stepper) baddies)
					       (list (car stepper) (cadr stepper))
					       stepper))
					 step-vars))
		      (new-sets (map (lambda (stepper)
				       (if (memq (car stepper) baddies)
					   (list 'set! (car stepper) (caddr stepper))
					   (values)))
				     step-vars)))
		  (if (or (null? (cdr baddies))
			  (let ((trails new-sets))
			    (not (lint-any? (lambda (v)     ; for each baddy, is it used in any following lint-set!?
					      (and (pair? (cdr trails))
						   (set! trails (cdr trails))
						   (tree-memq v trails)))
					    (reverse baddies)))))
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  `(do ,new-steppers
						       ,(caddr form)
						     ,@(cdddr form)
						     ,@new-sets)))
		      ;; (do ((i 0 (+ i j)) (j 0 (+ k 1)) (k 1)) ((= i 10)) (display (+ i j k))) ->
		      ;;    (do ((i 0) (j 0 (+ k 1)) (k 1)) ((= i 10)) (display (+ i j k)) (set! i (+ i j)))
		      (let* ((loop (find-unique-name form))
			     (new-body (let ((let-loop (cons loop (map (lambda (s)
									 ((if (pair? (cddr s)) caddr car) s))
								       step-vars))))
					 (if (pair? (cdddr form))
					     `(begin ,@(cdddr form) ,let-loop)
					     let-loop))))
			(let ((test (if (pair? (caddr form))
					(caaddr form)
					()))
			      (result (if (not (len>1? (caddr form)))
					  ()
					  (if (null? (cdr (cdaddr form)))
					      (car (cdaddr form))
					      (cons 'begin (cdaddr form))))))
			  ;; (do ((i 0 j) (j 1 i) (k 0 (+ k 1))) ((= k 5) (set! x k) (+ k 1)) (display (+ i j)) -> use named let
			  (lint-format "this do loop is unreadable; perhaps ~A" caller
				       (lists->string form
						      `(let ,loop ,(map (lambda (s)
									  (list (car s) (cadr s)))
									step-vars)
							    (if ,test ,result ,new-body)))))))))))

	  ;; -------- walk-do-end+result --------
	  (define (walk-do-end+result caller form vars inner-env env)
	    ;; walk the body and end stuff (it's too tricky to find infinite do loops)
	    (when (and (pair? (caddr form))
		       (or (null? (cadr form))
			   (and (proper-list? (cadr form))
				(pair? (caadr form)))))
	      (let ((end+result (caddr form)))
		(when (pair? end+result)
		  (let ((end (car end+result)))
		    (lint-walk caller end inner-env) ; this will call simplify-boolean
		    (when (pair? (cdr end+result))
		      (check-results caller 'do-result end+result (cdr end+result) inner-env)
		      (if (null? (cddr end+result))
			  (let ((end (car end+result))
				(result (cadr end+result)))
			    (if (or (equal? end result)
				    (and (eq? result #t)
					 (pair? end)
					 (let ((sig (arg-signature (car end) env)))
					   (and (pair? sig)
						(eq? (car sig) 'boolean?)))))
				;; (do ((i 0 (+ i 1))) ((= i 3) ()) (display i))
				(lint-format "return value is redundant: ~A" caller end+result)
				(if (and (len=2? result)
					 (equal? end (cadr result)))
				    (lint-format "perhaps use => here: ~A" caller
						 (lists->string end+result
								(list end '=> (car result)))))))))

		    (if (memq end '(= > < >= <= null? not))
			;; (do ((i 0 (+ i 1))) (= i 10) (display i))
			(lint-format "perhaps missing parens: ~A" caller end+result))

		    (cond ((never-false end)
			   ;; (do ((i 0 (+ i 1))) ((+ i 10) i))
			   (lint-format "end test is never false: ~A" caller end))

			  (end ; it's not #f
			   (if (never-true end)
			       (lint-format "end test is never true: ~A" caller end)
			       (begin

				 ;; (do ((i 0 (+ i 1))) ((= i 0)) ...)
				 (when (and (not (tree-memq 'eof-object? end))
					    (lint-every? (lambda (stepper)
							   (and (pair? stepper)         ; (do ((i 0) ())...)
								(pair? (cdr stepper))   ; (do ((i))...)
								(not (and (symbol? (cadr stepper))
									  (tree-memq (car stepper) end)))))
							 (cadr form))
					    (let walk ((tree end))
					      (if (pair? tree)
						  (and (walk (car tree))
						       (walk (cdr tree)))
						  (or (constant? tree)
						      (hash-table-ref no-side-effect-functions tree)
						      (and (symbol? tree)
							   (var-member tree vars))))))
				   (let* ((new-end `(let (,@(map (lambda (stepper) ; -> (let ((i 0)) (= i 0)) using the example above
								   (if (tree-memq (car stepper) end)
								       (list (car stepper) (cadr stepper))
								       (values)))
								 (cadr form)))
						      ,end))
					  (val (and (not (tree-set-memq '(read-char read-line read-string read) new-end))
						    ;; if new-end has (for example) read-char, eval here will hang waiting on *stdin*
						    ;; TODO: here and below, protect against any attempt at IO
						    (catch #t
						      (lambda ()
							(car (list (eval new-end (inlet))))) ; catch multiple-values, if any
						      (lambda args
							:eval-error)))))
				     (if (and val (not (eq? val :eval-error)))
					 (lint-format "do is a no-op because ~A is true at the start: ~A" caller end form)
					 (let* ((step-end `(let (,@(map (lambda (stepper)   ; -> (let ((i (+ 0 1))) (= i 0)) as above
									  (if (tree-memq (car stepper) end)
									      (if (pair? (cddr stepper))
										  (list (car stepper)
											(tree-subst (cadr stepper) (car stepper) (caddr stepper))) ; new old tree
										  stepper) ; was (cadr stepper) which can't be right
									      (values)))
									(cadr form)))
							     ,end))
						(val (and (not (tree-set-memq '(read-char read-line read-string read) step-end))
							  (catch #t
							    (lambda ()
							      (car (list (eval step-end (inlet))))) ; catch multiple-values, if any
							    (lambda args
							      :eval-error)))))
					   (if (and val (not (eq? val :eval-error)))
					       (lint-format "do is unnecessary: ~A" caller (truncated-list->string form)))))))

				 ;; if found, v is the var info
				 (let ((v (and (len>1? end)
					       (memq (car end) '(< > <= >=))
					       (symbol? (cadr end))
					       (var-member (cadr end) vars))))
				   (when (pair? v)
				     (let ((step (var-step v)))
				       (when (pair? step)
					 (let ((inc (and (memq (car step) '(+ -))
							 (len>1? (cdr step))
							 (or (and (real? (cadr step)) (cadr step))
							     (and (real? (caddr step)) (caddr step))))))
					   (when (and (real? inc)
						      (case (car step)
							((+) (and (positive? inc)
								  (memq (car end) '(< <=))))
							((-) (and (positive? inc)
								  (memq (car end) '(> >=))))
							(else #f)))
					     ;; (do ((i 0 (+ i 1))) ((< i len)) (display i)
					     ;; (do ((i 0 (- i 1))) ((> i len)) (display i))
					     (lint-format "do step looks like it doesn't match end test: ~A" caller
							  (lists->string step end)))))))))))
			  ((pair? (cdr end+result))
			   ;; (do ((i 0 (+ i 1))) (#f i))
			   (lint-format "result is unreachable: ~A" caller end+result)))

		    (if (and (symbol? end)
			     (not (var-member end env))
			     (procedure? (symbol->value end *e*)))
			;; (do ((i 0 (+ i 1))) (abs i) (display i))
			(lint-format "strange do end-test: ~A in ~A is a procedure" caller end end+result)))))))

	  ;; -------- walk-do-body --------
	  (define (walk-do-body caller form vars inner-env env)
	    (lint-walk-body caller 'do (cdddr form) (cons (make-lint-var :do form 'do)
							  inner-env))

	    (when (and *report-constant-expressions-in-do*
		       (pair? (cdr form)))
	      (let ((local-vars (let ((lv ()))
				  (for-each (lambda (p)
					      (if (pair? p) ; s7test (messed-up do)
						  (set! lv (cons (car p) lv))))
					    (cadr form))
				  lv)))
		(call-with-exit
		 (lambda (quit)
		   (let walker ((code (cdddr form))) ; get anything that changes in the loop
		     (when (pair? code)
		       (if (memq (car code) binders)
			   (cond ((and (memq (car code) '(define* define-macro define-macro* define-bacro define-bacro* define-expansion))
				       (pair? (cdr code))             ; lg hits this
				       (pair? (cadr code)))
				  (set! local-vars (cons (caadr code) local-vars)))

				 ((memq (car code) '(load eval eval-string require provide quote))
				  (quit))

				 ((and (memq (car code) '(define define-constant))
				       (pair? (cdr code)))    ; lg
				  (set! local-vars (cons ((if (pair? (cadr code)) caadr cadr) code) local-vars))))

			   (if (and (memq (car code) '(set! vector-set! list-set! hash-table-set! float-vector-set! int-vector-set!
						       string-set! let-set! fill! string-fill! list-fill! vector-fill!
						       reverse! sort! set-car! set-cdr!))
				    (pair? (cdr code)))            ; lg again
			       (set! local-vars (cons (cadr code) local-vars))
			       (begin
				 (walker (car code))
				 (walker (cdr code)))))))
		   (let walker ((code (cdddr form)))  ; look for exprs (or portions thereof) that don't change
		     (unless (or (not (pair? code))
				 (and (null? (cdr code)) (symbol? (car code))) ; symbol in arg list etc, (do vars end (func eqv?))
				 (memq (car code) binders)
				 (memq (car code) '(quote case))
				 (hash-table-ref makers (car code)))
		       (when (hash-table-ref no-side-effect-functions (car code))
			 (if (memq (car code) '(* + - /))
			     (when (> (length code) 3) ; counting '* etc
			       (let ((cs 0))
				 (for-each (lambda (p)
					     (if (or (code-constant? p)
						     (and (symbol? p)
							  (not (memq p local-vars)))
						     (and (pair? p)
							  (not (tree-set-memq local-vars p))
							  (not (side-effect? p env))
							  (not (eq? 'random (car p)))))
						 (set! cs (+ cs 1))))
					   (cdr code))
				 (when (> cs 1)
				   (let ((new-code
					  (let ((expr (if (memq (car code) '(+ *))
							  (list (car code))
							  (if (not (or (memq (cadr code) local-vars)
								       (and (pair? (cadr code))
									    (or (tree-set-memq local-vars (cadr code))
										(side-effect? (cadr code) env)))))
							      (list (car code))
							      (list (if (eq? (car code) '-) '+ '*))))))
					    (do ((p (cdr code) (cdr p)))
						((null? p)
						 (reverse! expr))
					      (unless (or (memq (car p) local-vars)
							  (and (pair? (car p))
							       (or (tree-set-memq local-vars (car p))
								   (side-effect? (car p) env))))
						(set! expr (cons (car p) expr)))))))
				     (if (equal? new-code code)
					 (lint-format "~S is constant in the do loop" caller code)
					 (lint-format "~S in ~S is constant in the do loop" caller new-code code))))))
#|
			     ;; this messes up all the time
			     (when (and (not (memq (car code) local-vars)) ; also need (not (dilambda (symbol->value (car code)))) here I think
					(not (eq? (car code) 'quote))
					(lint-every? (lambda (v)
						       (or (code-constant? v)
							   (and (symbol? v)
								(not (memq v local-vars)))))
						     (cdr code)))
			       (lint-format "~S could be moved out of the do loop" caller code))
|#
			     ))
		       (walker (car code))
		       (walker (cdr code))))))))

	    ;; before report-usage, check for unused variables, and don't complain about them if
	    ;;   they are referenced in an earlier step expr.
	    (do ((v vars (cdr v)))
		((null? v))
	      (let ((var (car v)))
		(when (zero? (var-ref var))
		  ;; var was not seen in the end+result/body or any subsequent step exprs
		  ;;   vars is reversed order, so we need only scan var-step of the rest
		  (if (side-effect? (var-step var) env)
		      (set! (var-ref var) (+ (var-ref var) 1))
		      (for-each
		       (lambda (nv)
			 (if (or (eq? (var-name var) (var-step nv))
				 (and (pair? (var-step nv))
				      (tree-memq (var-name var) (var-step nv))))
			     (set! (var-ref var) (+ (var-ref var) 1))))
		       (cdr v))))))

	    ;; try to catch step-var sets at the body end and move to the step expr
	    ;;   (do ((i 0 (+ i 1)) (j 1)) ((= i 3)) (display (+ i j)) (set! j (+ j 1))), move (+ j 1)
	    (let ((last-expr (last-ref (cdddr form))))
	      (when (and (len=3? last-expr)
			 (eq? (car last-expr) 'set!))
		(let ((var (cadr last-expr))
		      (val (caddr last-expr)))
		  (cond ((var-member var vars)
			 => (lambda (v)
			      (if (and (var-step v)
				       (not (tree-memq (var-name v) (var-step v))))
				  (if (side-effect? val env)
				      (lint-format "this set! is pointless: ~A; perhaps replace it with ~A" caller
						   (truncated-list->string last-expr)
						   (truncated-list->string val))
				      (lint-format "this set! is pointless: ~A" caller
						   (truncated-list->string last-expr)))
				  (if (and (or (not (var-step v))
					       (= (tree-count (var-name v) (var-step v) 2) 1))
					   ;; don't move if val contains ref to other step vars
					   (not (tree-set-memq (remove-one (var-name v) (map var-name vars)) val))
					   ;; don't move if var is referred to in any other step expr
					   (not (lint-any? (lambda (binding)
							     (and (not (eq? (car binding) var))
								  (pair? (cddr binding))
								  (tree-memq var (caddr binding))))
							   (cadr form))))
				      (lint-format "perhaps move ~A to ~A's step expression: ~A" caller
						   (truncated-list->string last-expr)
						   (var-name v)
						   (list (var-name v)
							 (var-initial-value v)
							 (if (not (var-step v))
							     val
							     (let ((expr (tree-subst val (var-name v) (var-step v))))
							       (cond ((not (pair? expr))
								      expr)

								     ((hash-table-ref numeric-ops (car expr))
								      (simplify-numerics expr env))

								     ((memq (car expr) '(and or not))
								      (simplify-boolean expr () () env))

								     (else expr))))))))))))))
	    (report-usage caller 'do vars inner-env))

	  ;; -------- simplify-do --------
	  (define (simplify-do caller form vars env)
	    (let ((body (cdddr form)))
	      (when (and (len=1? body)
			 (pair? (car body)))
		;; do+let
		;;   no hits for define here
		;;   this is tricky: make-index.scm (do () ((>= i len)) (let ((c (string-ref scheme-name i))) ...)) can't move the let upwards
		(if (and (eq? (caar body) 'let)
			 (len>1? (cdar body))         ; body not ((let))!
			 (not (symbol? (cadar body))) ; not named let
			 (or (null? (cadar body))
			     (not (tree-set-memq definers (cddar body)))) ; no let capture
			 (let ((varset (map car vars))
			       (endset (if (and (pair? (caddr form))
						(pair? (caaddr form)))
					   (map (lambda (x)
						  (if (symbol? x) x (values))) ; overkill obviously
						(caaddr form))
					   ())))
			   (lint-every? (lambda (c)
					  (and (len>1? c)
					       (not (memq (car c) varset)) ; no shadowing
					       (or (code-constant? (cadr c))
						   (not (or (side-effect? (cadr c) env) ; might change end-test calc?
							    (tree-set-memq varset (cadr c))
							    (tree-set-memq endset (cadr c)))))))
					(cadar body))))
		    ;; (do ((i 0 (+ i 1))) ((= i 3)) (let ((a 12)) (set! a (+ a i)) (display a))) ->
		    ;;    (do ((i 0 (+ i 1)) (a 12 12)) ((= i 3)) (set! a (+ a i)) ...)
		    ;; maybe a better suggestion: declare in do vars, then use set! in the body
		    (lint-format "perhaps ~A" caller
				 (lists->string form
						`(do (,@(cadr form)
						      ,@(map (lambda (c)
							       (list (car c) (cadr c) (cadr c)))
							     (cadar body)))
						     ,(caddr form)
						   ,@(one-call-and-dots (cddar body))))))
		;; do+lambda in body with stepper as free var never happens
		(let ((v (var-member (caar body) env)))
		  (when (and v
			     (memq (var-ftype v) '(define lambda)))
		    (let* ((vfunc (var-initial-value v))
			   (vbody (cddr vfunc)))
		      ;; we already detect a do body with no side-effects (walk-body)
		      (when (and (proper-list? ((if (eq? (var-ftype v) 'define) cdadr cadr) vfunc))
				 (null? (cdr vbody))
				 (< (tree-leaves vbody) 16))
			(do ((pp (var-arglist v) (cdr pp)))
			    ((or (null? pp)
				 (> (tree-count (car pp) vbody 3) 1))
			     (when (null? pp)
			       (let ((new-body (copy vbody)))
				 (for-each (lambda (par arg)
					     (if (not (eq? par arg))
						 (set! new-body (tree-subst arg par new-body))))
					   (var-arglist v)
					   (cdar body))
				 ;; (do ((i 0 (+ i 1))) ((= i 10)) (f i)) -> (do ((i 0 (+ i 1))) ((= i 10)) (abs (* 2 i)))
				 (lint-format "perhaps ~A" caller
					      (lists->string form
							     `(do ,(cadr form)
								  ,(caddr form)
								,@new-body))))))))))))))
	  ;; -------- do->for-each --------
	  (define (do->for-each caller form env)
	    (let ((step-vars (cadr form)))
	      (when (len=1? step-vars)
		(let ((var (car step-vars)))
		  (when (and (len>2? var)
			     (len>1? (caddr var))
			     (len=1? (caddr form))
			     (pair? (caaddr form))
			     (eq? (car var) (cadr (caddr var))))
		    (let ((vname (car var))
			  (end (caaddr form)))
		      (case (caaddr var)
			((cdr)
			 (when (and (case (car end)
				      ((null?)
				       (eq? (cadr end) vname))
				      ((not)
				       (and (pair? (cadr end))
					    (eq? (caadr end) 'pair?)
					    (eq? (cadadr end) vname)))
				      (else #f))
				    (not (let walker ((tree (cdddr form))) ; since only (cxar sym) is accepted, surely sym can't be shadowed?
					   (or (eq? tree vname)
					       (and (pair? tree)
						    (or (and (match-cxr 'cdr (car tree))
							     (pair? (cdr tree))
							     (eq? vname (cadr tree)))
							(and (not (hash-table-ref cxars (car tree)))
							     (or (walker (car tree))
								 (walker (cdr tree))))))))))
			   ;; this assumes slightly more than the do-loop if (not (pair? var)) is the end-test
			   ;;   for-each wants a sequence, but the do loop checks that in advance.
			   ;; (do ((p lst (cdr p))) ((null? p)) (display (car p))) -> (for-each (lambda ([p]) (display [p])) lst)
			   (lint-format "perhaps ~A" caller
					(lists->string form
						       (let ((new-sym (symbol "[" (symbol->string vname) "]")))
							 `(for-each (lambda (,new-sym)
								      ,@(car-subst vname new-sym (cdddr form)))
								    ,(cadr var)))))))
			((+)
			 (when (and (eqv? (cadr var) 0)
				    (len=1? (cddr (caddr var)))
				    (eqv? (caddr (caddr var)) 1)
				    (len=3? end))
			   (let ((end-var ((if (eq? vname (cadr end)) caddr cadr) end)))
			     (if (and (pair? end-var)
				      (memq (car end-var) '(length string-length vector-length)))
				 (set! end-var (cadr end-var))
				 (let ((v (var-member end-var env)))
				   (if (and v
					    (pair? (var-initial-value v))
					    (memq (car (var-initial-value v)) '(length string-length vector-length)))
				       (set! end-var (cadr (var-initial-value v))))))
			     (when (and (memq (car end) '(= >=))
					(memq vname end)
					(tree-memq vname (cdddr form))
					(not (let walker ((tree (cdddr form)))
					       (if (and (pair? tree)
							(memq vname tree)
							(memq (car tree) '(string-ref list-ref vector-ref))
							(eq? (caddr tree) vname))
						   (not (equal? (cadr tree) end-var))
						   (or (eq? tree vname)
						       (and (pair? tree)
							    (if (memq vname tree)
								(not (and (memq (car tree) '(string-ref list-ref vector-ref))
									  (len>1? (cdr tree))
									  (eq? (caddr tree) vname)))
								(or (walker (car tree))
								    (walker (cdr tree))))))))))
			       ;; (do ((i 0 (+ i 1))) ((= i (vector-length x))) (find (vector-ref x i))) ->
			       ;;    (for-each (lambda ([x]) (find [x])) x)
			       (lint-format "perhaps ~A" caller
					    (lists->string form
							   (let ((new-sym (symbol "[" (symbol->string (if (symbol? end-var) end-var (car end-var))) "]")))
							     `(for-each (lambda (,new-sym)
									  ,@(cadr-subst end-var new-sym (cdddr form)))
									,end-var)))))))))))))))
	  ;; -------- do->copy --------
	  (define (do->copy caller form vars)
	    ;; check for do-loop as copy/fill! stand-in and other similar cases
	    (if (len=1? vars)
		(let ((step-vars (cadr form)))
		  (let ((end-test (and (pair? (caddr form)) (caaddr form)))
			(first-var (car step-vars))
			(body (cdddr form))
			(setv #f))
		    (when (and (pair? end-test)
			       (len=1? body)
			       (pair? (car body))
			       (memq (car end-test) '(>= = < negative?)))
		      (set! body (car body))
		      (when (memq (car body) '(vector-set! float-vector-set! int-vector-set! list-set! string-set!))
			(let ((vname (car first-var))
			      (start (cadr first-var))
			      (step (and (pair? (cddr first-var))
					 (caddr first-var)))
			      (end (if (eq? (car end-test) 'negative?) 0 (caddr end-test))))
			  (when (and (eq? (caddr body) vname)
				     (pair? step)
				     (case (car step)
				       ((+)
					(and (memq vname step)
					     (memv 1 step)
					     (null? (cdddr step))           ; (+ v 1) or (+ 1 v)
					     (memq (car end-test) '(>= =)))) ; (= ...) or (>= ...)
				       ((-)
					(and (eq? (cadr step) vname)
					     (eqv? (caddr step) 1)          ; (- v 1)
					     (case (car end-test)
					       ((< = <=)                    ; (< ...) or (negative? ...)
						(null? (cdddr end-test)))
					       ((negative?)
						(null? (cddr end-test)))
					       (else #f))))
				       (else #f))
				     (or (eq? (cadr end-test) vname)
					 (and (eq? (car end-test) '=)
					      (eq? (caddr end-test) vname)
					      (set! end (cadr end-test)))))
			    ;; we have (do ((v start (+ v 1)|(+ 1 v))) ((= v end)|(= end v)|(>= v end)) one-statement)
			    ;;      or (do ((v start (- v 1)))         ((= v end)|(= end v)|(negative? v)|(< v end)) one-statement)
			    ;; write-char is the only other common case here -> write-string in a few cases
			    ;; integer type check here isn't needed because we're using this as an index below
			    ;;   the type error will be seen in report-usage if not earlier
			    (let ((val (cadddr body)))
			      (set! setv val)

			      (when (eq? (car step) '-)
				(let ((tmp start))
				  (set! start end)
				  (set! end tmp))
				(if (and (len=3? end)
					 (eq? (car end) '-)
					 (eqv? (caddr end) 1))
				    (set! end (cadr end))
				    (set! end `(+ ,end 1))))

			      (when (or (code-constant? val)  ; fill!
					(and (pair? val)      ; copy
					     (memq (car val) '(vector-ref float-vector-ref int-vector-ref list-ref string-ref))
					     (eq? (caddr val) vname)))
				;; (do ((i 2 (+ i 1))) ((= i len)) (string-set! s i #\a)) -> (fill! s #\a 2 len)
				(lint-format "perhaps ~A" caller
					     (lists->string form
							    (if (code-constant? setv)
								(list 'fill! (cadr body) (cadddr body) start end)
								;; (do ((i 0 (+ i 1))) ((= i (length str)) s) (string-set! s i (string-ref str i))) -> (copy str s)
								;; it is a bother to squeeze string-up|downcase into this block, and I doubt it would ever be hit
								(if (and (eqv? start 0)
									 (pair? end)
									 (memq (car end) '(length vector-length string-length))
									 (eq? (cadr end) (cadr setv)))
								    (list 'copy (cadr setv) (cadr body))
								    (list 'copy (cadr setv) (cadr body) start end)))))))))))))
		(when (and (len=2? vars)
			   (null? (cdddr form))) ; no body
		  ;; (do ((i 0 (+ i 1)) (lst () (cons 1 lst))) ((= i 10) lst)) -> (make-list 10 1)
		  (let ((var1 (caadr form)))
		    (if (and (len=2? (cdr var1))
			     (pair? (caddr var1))
			     (eq? (caaddr var1) 'cons))
			(do->make-list caller form form (cadadr form) var1)
			(let ((var2 (cadadr form)))
			  (if (and (len=2? (cdr var2))
				   (pair? (caddr var2))
				   (eq? (caaddr var2) 'cons))
			      (do->make-list caller form form var1 var2))))))))

	  ;; -------- do-walker --------
	  (define (do-walker caller form env)
	    (if (not (and (>= (length form) 3)
			  (proper-list? (cadr form))
			  (proper-list? (caddr form))))
		(lint-format "do is messed up: ~A" caller (truncated-list->string form))
		(begin
		  (unless (side-effect? form env)
		    (pointless-do caller form))
		  ;; pointless-var equivalent here gets about 4 hits

		  (let* ((vars (walk-do-inits caller form env))
			 (inner-env (append vars env)))
		    (walk-do-steps caller form vars inner-env env)
		    (walk-do-end+result caller form vars inner-env env)
		    (walk-do-body caller form vars inner-env env)
		    (simplify-do caller form vars env)
		    (do->for-each caller form env)
		    (do->copy caller form vars))))
	    env)

	  (hash-walker 'do do-walker))


	;; ---------------- let, let*, letrec ----------------
	(let ()
	  (define (unsafe-definer? form)
	    (tree-set-memq
	     '(define define* define-constant
		curlet require load eval eval-string
		define-macro define-macro* define-bacro define-bacro* define-expansion
		definstrument define-animal define-envelope defgenerator
		define-values define-module define-method
		define-syntax define-public define-inlinable define-integrable define^
		call/cc call-with-current-continuation)
	     form))

	  ;; -------- walk-letx-body --------
	  (define (walk-letx-body caller form body vars env)
	    (let* ((cur-env (cons (make-lint-var :let form 'let)
				  (append vars env)))
		   (e (lint-walk-body caller (car form) body cur-env)))

	      (when (pair? vars)
		(case (car form)
		  ((let)
		   (for-each (lambda (v)
			       (if (null? (var-env v))
				   (set! (var-env v) cur-env)))
			     vars))
		  ((let*)                                  ; could also scan vars (stored backwards here) for any not ref'd later in vals
		   (set! (var-env (car vars)) cur-env))))  ;   but this only gets 3 hits

	      (let ((nvars (and (not (eq? e cur-env))
				(env-difference caller e cur-env ()))))
		(if (pair? nvars)
		    (if (memq (var-name (car nvars)) '(:lambda :dilambda))
			(begin
			  (set! env (cons (car nvars) env))
			  (set! nvars (cdr nvars)))
			(set! vars (append nvars vars)))))
	      (report-usage caller (car form) vars e)
	      (cons vars env)))

	  ;; --------declare-named-let --------
	  (define (declare-named-let form env)	    ; used in let-walker and let*-walker
	    (let ((named-let (and (symbol? (cadr form)) (cadr form))))
	      (if (or (not named-let)
		      (keyword? named-let)
		      (not (or (null? (caddr form))
			       (and (proper-list? (caddr form))
				    (just-pairs? (caddr form))))))
		  ()
		  (let ((vars (map car (caddr form))))
		    (if (memq named-let vars)
			(lint-format "perhaps ~S -> ~S" 'let form (cons 'let (cddr form))))
		    (list (make-fvar named-let (car form) vars form env))))))


	  ;; -------- remove-null-let --------
	  (define (remove-null-let caller form env)
	    (if (and (null? (cadr form)) ; this can be fooled by macros that define things
		     (not (tree-set-memq open-definers (cddr form)))) ; somewhat too restrictive but hard to improve
		;; (begin (let () (display x)) y)
		(if (or (eq? form lint-current-form) ; i.e. we're in a body?
			(null? (cdddr form)))
		    (lint-format "pointless let: ~A" caller (truncated-list->string form))
		    (if (lint-every? (lambda (p)
				       (or (not (pair? p))
					   (hash-table-ref built-in-functions (car p))
					   (hash-table-ref syntaces (car p))
					   (not (side-effect? p env))))
				     (cddr form))
			(lint-format "let could be begin: ~A" caller
				     (truncated-lists->string form (cons 'begin (cddr form))))))
		(let ((body (cddr form)))
		  (when (and (null? (cdr body))
			     (len>1? (car body)))
		    (if (memq (caar body) '(let let*))
			(if (null? (cadr form))
			    ;; (let () (let ((a x)) (+ a 1)))
			    (lint-format "pointless let: ~A" caller (lists->string form (car body)))
			    (if (null? (cadar body))
				;; (let ((a x)) (let () (+ a 1)))
				(lint-format "pointless let: ~A" caller (lists->string form (cons 'let (cons (cadr form) (cddar body)))))))
			(if (and (memq (caar body) '(lambda lambda*)) ; or any definer?
				 (null? (cadr form)))
			    ;; (let () (lambda (a b) (if (positive? a) (+ a b) b))) -> (lambda (a b) (if (positive? a) (+ a b) b))
			    (lint-format "pointless let: ~A" caller (lists->string form (car body)))))))))

	  ;; -------- walk-let-vars --------
	  (define (walk-let-vars caller form varlist vars env)
	    (let ((named-let (and (symbol? (cadr form)) (cadr form))))
	      (do ((bindings varlist (cdr bindings)))
		  ((null? bindings))
		(when (binding-ok? caller 'let (car bindings) env #f)
		  (let ((val (cadar bindings)))
		    (if (and (pair? val)
			     (eq? 'lambda (car val))
			     (tree-car-member (caar bindings) val)
			     (not (var-member (caar bindings) env))) ; (let ((x (lambda (a) (x 1)))) x)
			(lint-format "let variable ~A is called in its binding?  Perhaps let should be letrec: ~A"
				     caller (caar bindings)
				     (truncated-list->string bindings))
			(unless named-let
			  (for-each (lambda (v)
				      (if (and (tree-memq (var-name v) (cadar bindings))
					       (not (hash-table-ref built-in-functions (var-name v)))
					       (not (tree-set-memq binders (cadar bindings))))
					  (if (not (var-member (var-name v) env))
					      ;; (let ((x 1) (y x)) (+ x y)): x in (y x)
					      (lint-format "~A in ~A does not appear to be defined in the calling environment" caller
							   (var-name v) (car bindings))
					      ;; (let ((x 3)) (+ x (let ((x 1) (y x)) (+ x y)))): x in (y x)
					      (lint-format "~A in ~A refers to the caller's ~A, not the let variable" caller
							   (var-name v) (car bindings) (var-name v)))))
				    vars)))
		    ;; escape as value got no hits
		    (let ((e (if (symbol? val)
				 (set-ref val caller form env)
				 (lint-walk caller val env))))
		      (if (and (pair? e)
			       (not (eq? e env))
			       (memq (var-name (car e)) '(:lambda :dilambda)))
			  (let ((ldata (cdar e)))
			    (set! (var-name (car e)) (caar bindings))
			    (let-set! ldata 'initial-value val)
			    (set! vars (cons (car e) vars)))
			  (set! vars (cons (make-lint-var (caar bindings) val (if named-let 'named-let 'let))
					   vars)))))))

	      (check-unordered-exprs caller form
				     (map (if (not named-let)
					      var-initial-value
					      (lambda (v)
						(if (eq? (var-name v) named-let)
						    (values)
						    (var-initial-value v))))
					  vars)
				     env)
	      vars))

	  ;; -------- move-let-into-if --------
	  (define (move-let-into-if caller form env)
	    ;; move let in:
	    ;;   (let ((a (car x))) (if b (+ a (f a)) (display c))) -> (if b (let ((a (car x))) (+ a (f a))) (display c))
	    ;;   let* version gets only 3 hits
	    (unless (lint-any? (lambda (c)
				 (not (and (len>1? c)
					   (symbol? (car c))
					   (not (side-effect? (cadr c) env)))))
			       (cadr form))
	      (let ((first-expr (caddr form)))
		(case (car first-expr)
		  ((if)
		   (when (pair? (cddr first-expr))
		     (let ((test (cadr first-expr))
			   (true (caddr first-expr))
			   (false (and (pair? (cdddr first-expr)) (cadddr first-expr)))
			   (vars (map car (cadr form)))
			   (false-let #f))
		       (when (and (not (memq test vars))
				  (not (tree-set-memq vars test))
				  (or (and (not (memq true vars))
					   (not (tree-set-memq vars true))
					   (set! false-let #t))
				      (not false)
				      (not (or (memq false vars)
					       (tree-set-memq vars false))))
				  (tree-set-memq vars (cddr form))) ; otherwise we'll complain elsewhere about unused variables
			 (lint-format "perhaps move the let to the ~A branch: ~A" caller
				      (if false-let "false" "true")
				      (lists->string form
						     (let ((true-dots (if (> (tree-leaves true) 30) '... true))
							   (false-dots (if (and (pair? false) (> (tree-leaves false) 30)) '... false)))
						       (if false-let
							   `(if ,test ,true-dots (let ,(cadr form) ,@(unbegin false-dots)))
							   (if (pair? (cdddr (caddr form)))
							       `(if ,test (let ,(cadr form) ,@(unbegin true-dots)) ,false-dots)
							       `(if ,test (let ,(cadr form) ,@(unbegin true-dots))))))))))))
		  ((cond)
		   ;; happens about a dozen times
		   (let ((vars (map car (cadr form))))
		     (when (tree-set-memq vars (cdr first-expr))
		       (call-with-exit
			(lambda (quit)
			  (let ((branch-let #f))
			    (for-each (lambda (c)
					(if (and (not branch-let)
						 (pair? c)
						 (side-effect? (car c) env))
					    (quit))
					(when (and (pair? c)
						   (tree-set-memq vars c))
					  (if branch-let (quit))
					  (set! branch-let c)))
				      (cdr first-expr))
			    (when (and branch-let
				       (not (memq (car branch-let) vars))
				       (not (tree-set-memq vars (car branch-let))))
			      (lint-format "perhaps move the let into the '~A branch: ~A" caller
					   (truncated-list->string branch-let)
					   (lists->string form
							  (if (eq? '=> (cadr branch-let))
							      (if (eq? branch-let (cadr first-expr))
								  `(cond (,(car branch-let) => (let ,(cadr form) ,@(cddr branch-let))) ...)
								  `(cond ... (,(car branch-let) => (let ,(cadr form) ,@(cddr branch-let))) ...))
							      (if (eq? branch-let (cadr first-expr))
								  `(cond (,(car branch-let) (let ,(cadr form) ,@(cdr branch-let))) ...)
								  `(cond ... (,(car branch-let) (let ,(cadr form) ,@(cdr branch-let))) ...))))))))))))
		  ((case)
		   (let ((vars (map car (cadr form)))
			 (test (cadr first-expr)))
		     (when (and (not (memq test vars))
				(not (tree-set-memq vars test))
				(tree-set-memq vars (cddr first-expr)))
		       (call-with-exit
			(lambda (quit)
			  (let ((branch-let #f))
			    (for-each (lambda (c)
					(when (and (pair? c)
						   (tree-set-memq vars (cdr c)))
					  (if branch-let (quit))
					  (set! branch-let c)))
				      (cddr first-expr))
			    (when (proper-list? branch-let)
			      (lint-format "perhaps move the let into the '~A branch: ~A" caller
					   (truncated-list->string branch-let)
					   (lists->string form
							  (if (eq? '=> (cadr branch-let))
							      (if (eq? branch-let (caddr first-expr))
								  `(case ,test (,(car branch-let) => (let ,(cadr form) ,@(cddr branch-let))) ...)
								  `(case ,test ... (,(car branch-let) => (let ,(cadr form) ,@(cddr branch-let))) ...))
							      (if (eq? branch-let (caddr first-expr))
								  `(case ,test (,(car branch-let) (let ,(cadr form) ,@(cdr branch-let))) ...)
								  `(case ,test ... (,(car branch-let) (let ,(cadr form) ,@(cdr branch-let))) ...))))))))))))
		  ((when unless) ; no hits -- maybe someday?
		   (let ((test (cadr first-expr))
			 (vars (map car (cadr form))))
		     (unless (or (memq test vars)
				 (tree-set-memq vars test)
				 (side-effect? test env)
				 (not (proper-list? (cddr first-expr))))
		       (lint-format "perhaps move the let inside the ~A: ~A" caller
				    (car first-expr)
				    (truncated-lists->string form `(,(car first-expr) ,test (let ,(cadr form) ,@(cddr first-expr))))))))))))

	  ;; -------- let-body->value --------
	  (define (let-body->value caller form vars env)
	    (let ((named-let (and (symbol? (cadr form)) (cadr form))))
	      (let ((body ((if named-let cdddr cddr) form))
		    (varlist ((if named-let  caddr cadr) form)))
		(case (caar body)
		  ((set!)
		   (let ((settee (cadar body))
			 (setval (caddar body))
			 (vals-ok #f))
		     (if (and (not named-let)           ; (let ((x 0)...) (set! x 1)...) -> (let ((x 1)...)...)
			      (not (tree-memq 'curlet setval))
			      (cond ((assq settee vars)
				     => (lambda (v)
					  (or (set! vals-ok (and (code-constant? (var-initial-value v))
								 (code-constant? setval)))
					      (and (<= (tree-count settee setval 3) 1)
						   (not (lint-any? (lambda (v1)
								     (or (tree-memq settee (cadr v1))
									 (and (not (eq? (car v1) settee))
									      (or (tree-memq (car v1) setval)
										  (side-effect? (cadr v1) env)))))
								   varlist))))))
				    (else #f)))
			 (begin
			   (if (not vals-ok)
			       (set! setval (let replace ((tree setval))
					      (if (eq? tree settee)
						  (var-initial-value (assq settee vars))
						  (if (not (pair? tree))
						      tree
						      (cons (replace (car tree))
							    (replace (cdr tree))))))))
			   (lint-format "perhaps ~A" caller  ;  (let ((a 1)) (set! a 2)) -> 2
					(lists->string form
						       (if (null? (cdr body)) ; this only happens in test suites...
							   (if (null? (cdr varlist))
							       setval
							       (list 'let (map (lambda (v) (if (eq? (car v) settee) (values) v)) varlist)
								     setval))
							   (cons 'let
								 (cons (map (lambda (v)
									      (if (eq? (car v) settee)  ; (let ((x 0)) (set! x 1)...) -> (let ((x 1)) ...)
										  (list (car v) setval) ; replace initial with set! value
										  v))
									    varlist)
								       (if (null? (cddr body))
									   (cdr body)
									   (list (cadr body) '...))))))))
			 ;; repetition for the moment
			 (when (and (pair? varlist)
				    (assq settee vars)           ; settee is a local var
				    (not (eq? settee named-let)) ; (let loop () (set! loop 3))!
				    (or (null? (cdr body))
					(and (null? (cddr body))
					     (eq? settee (cadr body))))) ; (let... (set! local val) local)
			   (lint-format "perhaps ~A" caller
					(lists->string form
						       (if (or (tree-memq settee setval)
							       (side-effect? (cadr (assq settee varlist)) env))
							   (list 'let varlist setval)
							   (if (null? (cdr varlist))
							       setval
							       (list 'let (remove-if (lambda (v)
										       (eq? (car v) settee))
										     varlist)
								     setval)))))))))

		  ((define)
		   (unless named-let
		     (let ((f (cdar body)))
		       (when (and (len=2? f)
				  (symbol? (car f))
				  (not (assq (car f) varlist))           ; this (let ((x ...)) (set! x ...)) is handled elsewhere
				  (or (code-constant? (cadr f))
				      (not (or (tree-memq 'lambda (cadr f))  ; else we have to scan forward for pending refs
					       (and (pair? varlist)
						    (or (side-effect? (cadr f) env)   ; might be depending on the let var calcs
							(tree-set-memq (map car varlist) (cadr f))))))))
			 (lint-format "perhaps ~A" caller
				      (lists->string form
						     `(let (,@varlist
							    ,f)
							...)))))))

		  ;; display et al here happen a lot, but only a few are rewritable or collapsible
		  ;; *-set! happen a couple dozen times, but not in ways we can rewrite

		  ((fill! string-fill! vector-fill!) ; (let ((x (make-vector 3))) (fill! x 1) ...) -> (let ((x (make-vector 3 1))) ...)
		   (cond ((assq (cadar body) vars) =>
			  (lambda (v)
			    (let ((new-init (let ((init (var-initial-value v)))
					      (if (and (code-constant? init)
						       (code-constant? (caddar body))
						       (sequence? init))
						  (let ((i1 (copy init)))  ; (fill! #(0 1 2) 3)??
						    (catch #t
						      (lambda ()
							(fill! i1 (caddar body))
							i1)
						      (lambda args :none)))
						  (if (and (pair? init)
							   (memq (car init) '(make-string make-list make-vector
											  make-int-vector make-float-vector make-byte-vector))
							   (let ((ninit (caddar body))
								 (local-vars (map car vars)))
							     ;; watch out for (let ((g (make-oscil)) (v (make-vector 3))) (fill! v g) ...)
							     (not (or (tree-set-memq local-vars ninit)
								      (memq ninit local-vars)))))
						      (list (car init) (cadr init) (caddar body))
						      :none)))))
			      (if (not (eq? new-init :none))
				  (lint-format "perhaps ~A" caller
					       (lists->string form
							      (if (null? (cdr body))
								  (list 'let (map (lambda (v)
										    (if (eq? (car v) (cadar body))
											(values)
											v))
										  varlist)
									(caddar body))
								  (cons 'let
									(cons (map (lambda (v)
										     (if (eq? (car v) (cadar body))
											 (list (car v) new-init)
											 v))
										   varlist)
									      (if (null? (cddr body))
										  (cdr body)
										  (list (cadr body) '...)))))))))))))))))

	  ;; -------- normal-let->do --------
	  (define (normal-let->do caller form env)
	    (let ((varlist (cadr form))
		  (body (cddr form)))
	      (when (lint-every? len=2? varlist)
		(when (and (null? (cdr body))  ; removing this restriction gets only 3 hits
			   (pair? (cdar body))
			   (pair? (cadar body))
			   (just-len>1? (cadar body)))
		  (let ((inits (map cadr (cadar body))))
		    (when (lint-every? (lambda (v)
					 (and (= (tree-count (car v) (car body) 2) 1)
					      (tree-memq (car v) inits)))
				       varlist)
		      (let ((new-cadr (copy (cadar body))))
			(for-each (lambda (v)
				    (set! new-cadr (tree-subst (cadr v) (car v) new-cadr)))
				  varlist)
			;; (let ((a 1)) (do ((i a (+ i 1))) ((= i 3)) (display i))) -> (do ((i 1 (+ i 1))) ...)
			(lint-format "perhaps ~A" caller
				     (lists->string form (list 'do new-cadr '...)))))))

		;; let->do -- sometimes a bad idea, set *report-combinable-lets* to #f to disable this.
		;;   (the main objection is that the s7/clm optimizer can't handle it, and
		;;   instruments using it look kinda dumb -- the power of habit or something)
		(when (and *report-combinable-lets*
			   (not (len>1? (cdr body)))
			   ;; moving more than one expr here is usually ugly -- the only exception I've
			   ;;   seen is where the do body is enormous and the end stuff very short, and
			   ;;   it (the end stuff) refers to the let/do variables -- in the unedited case,
			   ;;   the result is hard to see.
			   (<= (tree-leaves (cdr body)) *max-cdr-len*))
		  (let ((inits (if (and (pair? (cdar body))
					(pair? (cadar body))
					(just-len>1? (cadar body)))
				   (map cadr (cadar body))
				   ()))
			(locals (if (and (pair? (cdar body))
					 (pair? (cadar body))
					 (just-pairs? (cadar body)))
				    (map car (cadar body))
				    ())))
		    (unless (and (pair? inits)
				 (lint-any? (lambda (v)
					      (or (memq (car v) locals) ; shadowing
						  (tree-memq (car v) inits)
						  (side-effect? (cadr v) env))) ; let var opens *stdin*, do stepper reads it at init
					    varlist))
		      ;; (let ((xx 0)) (do ((x 1 (+ x 1)) (y x (- y 1))) ((= x 3) xx) (display y))) ->
		      ;;    (do ((xx 0) (x 1 (+ x 1)) (y x (- y 1))) ...)
		      (let ((do-form (cdar body)))
			(if (pair? do-form)
			    (lint-format "perhaps ~A" caller
					 (lists->string form
							(if (null? (cdr body))    ; do is only expr in let
							    (list 'do (append varlist (car do-form))
								  '...)
							    `(do ,(append varlist (car do-form))
								 (,(and (pair? (cadr do-form)) (caadr do-form))
								  ,@(if (side-effect? (cdadr do-form) env) (cdadr do-form) ())
								  ,@(cdr body))   ; include rest of let as do return value
							       ...))))))))))))

	  ;; -------- split-let --------
	  (define (split-let caller form body vars env)
	    ;; look for splittable lets and let-temporarily possibilities
	    (for-each
	     (lambda (local-var)
	       (let ((vname (var-name local-var)))

		 ;; ideally we'd collect vars that fit into one let etc
		 (when (> (length body) (* 5 (var-set local-var)) 0)
		   (do ((i 0 (+ i 1))
			(preref #f)
			(p body (cdr p)))
		       ((or (not (pair? (cdr p)))
			    (and (pair? (car p))
				 (eq? (caar p) 'set!)
				 (eq? (cadar p) vname)
				 (> i 5)
				 (begin
				   (if (or preref
					   (side-effect? (var-initial-value local-var) env))
				       ;; (let ((x 32)) (display x) (set! y (f x)) (g (+ x 1) y) (a y) (f y) (g y) (h y) (i y) (set! x 3) (display x) (h y x))
				       ;;     (let ... (let ((x 3)) ...))
				       (lint-format "perhaps add a new binding for ~A to replace ~A: ~A" caller
						    vname
						    (truncated-list->string (car p))
						    (lists->string form
								   `(let ...
								      (let ((,vname ,(caddar p)))
									...))))
				       ;; (let ((x 32)) (set! y (f 1)) (a y) (f y) (g y) (h y) (i y) (set! x (+ x... -> (let () ... (let ((x (+ 32 1))) ...))
				       (lint-format "perhaps move the ~A binding to replace ~A: ~A" caller
						    vname
						    (truncated-list->string (car p))
						    (let ((new-value (if (tree-memq vname (caddar p))
									 (tree-subst (var-initial-value local-var) vname (copy (caddar p)))
									 (caddar p))))
						      (lists->string form
								     `(let ,(let rewrite ((lst (cadr form)))
									      (cond ((null? lst) ())
										    ((and (pair? (car lst))
											  (eq? (caar lst) vname))
										     (rewrite (cdr lst)))
										    (else (cons (if (< (tree-leaves (cadar lst)) 30)
												    (car lst)
												    (list (caar lst) '...))
												(rewrite (cdr lst))))))
									...
									(let ((,vname ,new-value))
									  ...))))))
				   #t))))
		     (if (tree-memq vname (car p))
			 (set! preref i))))

		 (when (and (zero? (var-set local-var))
			    (= (var-ref local-var) 2)) ; initial value and set!'s value
		   (do ((saved-name (var-initial-value local-var))
			(p body (cdr p))
			(last-pos #f)
			(first-pos #f))
		       ((not (pair? p))
			(when (and (pair? last-pos)
				   (not (eq? first-pos last-pos))
				   (not (tree-equal-member saved-name (cdr last-pos))))
			  ;; (let ((old-x x)) (set! x 12) (display (log x)) (set! x 1) (set! x old-x)) ->
			  ;;    (let-temporarily ((x 12)) (display (log x)) (set! x 1))
			  ;; the pattern (set! x y) ... (set! y x) happens a few times (say 5 to 10)
			  (lint-format "perhaps use let-temporarily here: ~A" caller
				       (lists->string form
						      (let ((new-let (cons 'let-temporarily
									   (cons (list (list saved-name
											     (if (pair? first-pos)
												 (caddar first-pos)
												 saved-name)))
										 (map (lambda (expr)
											(if (or (and (pair? first-pos)
												     (eq? expr (car first-pos)))
												(eq? expr (car last-pos)))
											    (values)
											    expr))
										      body)))))
							(if (null? (cdr vars)) ; we know vars is a pair, want len=1
							    new-let
							    (list 'let (map (lambda (v)
									      (if (eq? (car v) vname)
										  (values)
										  v))
									    (cadr form))
								  new-let)))))))
		     ;; someday maybe look for additional saved vars, but this happens only in snd-test
		     ;;   also the let-temp could be reduced to the set locations (so the tree-equal-member
		     ;;   check above would be unneeded).
		     (let ((expr (car p)))
		       (when (and (len>2? expr)
				  (eq? (car expr) 'set!)
				  (equal? (cadr expr) saved-name))
			 (if (not first-pos)
			     (set! first-pos p))
			 (if (eq? (caddr expr) vname)
			     (set! last-pos p))))))))
	     vars))

	  ;; -------- let-var->body --------
	  (define (let-var->body caller form body varlist)
	    (when (len=1? varlist)
	      (if (and (len=1? body)                ; (let ((x y)) x) -> y, named let is possible here
		       (eq? (car body) (caar varlist))
		       (pair? (cdar varlist)))     ; (let ((a))...)
		  (lint-format "perhaps ~A" caller (lists->string form (cadar varlist))))
	      ;; also (let ((x ...)) (let ((y x)...))) happens but it looks like automatically generated code or test suite junk

	      ;; copied from letrec below -- happens about a dozen times
	      (when (and (list? (cadr form)) ; not named let
			 (len=1? (cddr form))
			 (pair? (caddr form)))
		(let ((body (caddr form))
		      (sym (caar varlist))
		      (lform (and (pair? (caadr form))
				  (pair? (cdaadr form))
				  (cadar (cadr form)))))
		  (if (and (len>1? lform)
			   (eq? (car lform) 'lambda)
			   (proper-list? (cadr lform)))
		      ;; unlike in letrec, here there can't be recursion (ref to same name is ref to outer env)
		      (if (eq? sym (car body))
			  (if (not (tree-memq sym (cdr body)))
			      ;; (let ((x (lambda (y) (+ y (x (- y 1)))))) (x 2)) -> (let ((y 2)) (+ y (x (- y 1))))
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  (cons 'let
								(cons (map list (cadr lform) (cdr body))
								      (cddr lform))))))
			  (if (= (tree-count sym body 2) 1)
			      (let ((call (find-call sym body)))
				(when (pair? call)
				  (let ((new-call (cons 'let
							(cons (map list (cadr lform) (cdr call))
							      (cddr lform)))))
				    ;; (let ((f60 (lambda (x) (* 2 x)))) (+ 1 (f60 y))) -> (+ 1 (let ((x y)) (* 2 x)))
				    (lint-format "perhaps ~A" caller
						 (lists->string form (tree-subst new-call call body)))))))))))))

	  ;; -------- combine-lets --------
	  (define combine-lets
	    (let ()
	      (define (letstar env . lets)
		(let loop ((vars (list 'curlet)) (forms lets))
		  (and (pair? forms)
		       (or (and (pair? (car forms))
				(or (tree-set-memq vars (car forms))
				    (lint-any? (lambda (a)
						 (or (not (pair? a))
						     (not (pair? (cdr a)))
						     (side-effect? (cadr a) env)))
					       (car forms))))
			   (loop (append (map car (car forms)) vars)
				 (cdr forms))))))

	      (lambda (caller form varlist env)
		(when (and (pair? (cadr form))
			   (len=1? (cddr form))
			   (pair? (caddr form)))
		  (let ((inner (caddr form))  ; the inner let
			(outer-vars (cadr form)))

		    (when (len>1? (cdr inner))
		      (let ((inner-vars (cadr inner)))
			(when (and (eq? (car inner) 'let)
				   (symbol? inner-vars))
			  (let ((named-body (cdddr inner))
				(named-args (caddr inner)))
			    (unless (lint-any? (lambda (v)
						 (or (not (= (tree-count (car v) named-args 2) 1))
						     (tree-memq (car v) named-body)))
					       varlist)
			      (let ((new-args (copy named-args)))
				(for-each (lambda (v)
					    (set! new-args (tree-subst (cadr v) (car v) new-args)))
					  varlist)
				;; (let ((x 1) (y (f g 2))) (let loop ((a (+ x 1)) (b y)) (loop a b))) -> (let loop ((a (+ 1 1)) (b (f g 2))) (loop a b))
				(lint-format "perhaps ~A" caller
					     (lists->string form
							    (cons 'let
								  (cons inner-vars
									(cons new-args named-body)))))))))

			;; maybe more code than this is worth -- combine lets
			(when (and (memq (car inner) '(let let*))
				   (pair? inner-vars))

			  (cond ((and (null? (cdadr form))   ; let(1) + let* -> let*
				      (eq? (car inner) 'let*)
				      (not (symbol? inner-vars))) ; not named let*
				 ;; (let ((a 1)) (let* ((b (+ a 1)) (c (* b 2))) (display (+ a b c)))) -> (let* ((a 1) (b (+ a 1)) (c (* b 2))) (display (+ a b c)))
				 (lint-format "perhaps ~A" caller
					      (lists->string form
							     (cons 'let*
								   (cons (append outer-vars inner-vars)
									 (one-call-and-dots (cddr inner)))))))
				((and (len=1? (cddr inner))
				      (len>1? (caddr inner))
				      (eq? (caaddr inner) 'let)
				      (pair? (cadr (caddr inner))))
				 (let* ((inner1 (cdaddr inner))
					(inner1-vars (car inner1)))
				   (if (and (len=1? (cdr inner1))
					    (len>1? (cadr inner1))
					    (eq? (caadr inner1) 'let)
					    (pair? (cadadr inner1)))
				       (let* ((inner2 (cdadr inner1))
					      (inner2-vars (car inner2)))
					 (if (not (letstar env outer-vars
							   inner-vars
							   inner1-vars
							   inner2-vars))
					     ;; (let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (+ a b c d))))) -> (let ((a 1) (b 2) (c 3) (d 4)) (+ a b c d))
					     (lint-format "perhaps ~A" caller
							  (lists->string form
									 (cons 'let
									       (cons (append outer-vars inner-vars inner1-vars inner2-vars)
										     (one-call-and-dots (cdr inner2))))))))
				       (if (not (letstar env outer-vars
							 inner-vars
							 inner1-vars))
					   ;; (let ((b 2)) (let ((c 3)) (let ((d 4)) (+ a b c d)))) -> (let ((b 2) (c 3) (d 4)) (+ a b c d))
					   (lint-format "perhaps ~A" caller
							(lists->string form
								       (cons 'let
									     (cons (append outer-vars inner-vars inner1-vars)
										   (one-call-and-dots (cdr inner1))))))))))
				((not (letstar env outer-vars
					       inner-vars))
				 ;; (let ((c 3)) (let ((d 4)) (+ a b c d))) -> (let ((c 3) (d 4)) (+ a b c d))
				 (lint-format "perhaps ~A" caller
					      (lists->string form
							     (cons 'let
								   (cons (append outer-vars inner-vars)
									 (one-call-and-dots (cddr inner)))))))

				((and (null? (cdadr form))   ; 1 outer var
				      (pair? inner-vars)
				      (null? (cdadr inner))) ; 1 inner var, dependent on outer
				 ;; (let ((x 0)) (let ((y (g 0))) (+ x y))) -> (let* ((x 0) (y (g 0))) (+ x y))
				 (lint-format "perhaps ~A" caller
					      (lists->string form
							     (cons 'let*
								   (cons (append outer-vars inner-vars)
									 (one-call-and-dots (cddr inner))))))))))))))))

	  ;; -------- tighten-let --------
	  (define (tighten-let caller form vars env)
	    (let ((body (cddr form))
		  (varlist (cadr form)))
	      (when (and (> (length body) 3)  ; setting this to 1 did not catch anything new
			 (not (tree-set-memq open-definers body)))
		;; define et al are like a continuation of the let bindings, so we can't restrict them by accident
		;;   (let ((x 1)) (define y x) ...)
		(let ((last-refs (map (lambda (v)
					(vector (var-name v) #f 0 v))
				      vars))
		      (got-lambdas (tree-set-car-member '(lambda lambda*) body)))
		  ;; (let ((x #f) (y #t)) (set! x (lambda () y)) (set! y 5) (x))
		  (do ((p body (cdr p))
		       (i 0 (+ i 1)))
		      ((null? p)
		       (let ((end 0))
			 (for-each (lambda (v)
				     (set! end (max end (v 2))))
				   last-refs)
			 (if (and (< end (/ i lint-let-reduction-factor))
				  (eq? form lint-current-form)
				  (< (tree-leaves (car body)) 100))
			     (let ((old-start (let ((old-pp (lint-pp-funclet '*pretty-print-left-margin*)))
						(set! (lint-pp-funclet '*pretty-print-left-margin*) (+ lint-left-margin 4))
						(let ((res (lint-pp (cons 'let
									  (cons (cadr form)
										(copy body (make-list (+ end 1))))))))
						  (set! (lint-pp-funclet '*pretty-print-left-margin*) old-pp)
						  res))))
			       (lint-format "this let could be tightened:~%~NC~A ->~%~NC~A~%~NC~A ..." caller
					    (+ lint-left-margin 4) #\space
					    (truncated-list->string form)
					    (+ lint-left-margin 4) #\space
					    old-start
					    (+ lint-left-margin 4) #\space
					    (lint-pp (list-ref body (+ end 1)))))
			     (begin
			       ;; look for bindings that can be severely localized
			       (let ((locals (map (lambda (v)
						    (if (and (integer? (v 1))
							     (< (- (v 2) (v 1)) 2)
							     (code-constant? (var-initial-value (v 3))))
							v
							(values)))
						  last-refs)))
				 ;; should this omit cases where most of the let is in one or two lines?
				 (when (pair? locals)
				   (set! locals (sort! locals (lambda (a b)
								(or (< (a 1) (b 1))
								    (< (a 2) (b 2))))))
				   (do ((lv locals (cdr lv)))
				       ((null? lv))
				     (let* ((v (car lv))
					    (cur-line (v 1)))
				       (let gather ((pv lv) (cur-vars ()) (max-line (v 2)))
					 (if (or (null? (cdr pv))
						 (not (= cur-line ((cadr pv) 1))))
					     (begin
					       (set! cur-vars (reverse (cons (car pv) cur-vars)))
					       (set! max-line (max max-line ((car pv) 2)))
					       (set! lv pv)
					       (lint-format "~{~A~^, ~} ~A only used in expression~A (of ~A),~%~NC~A~A of~%~NC~A" caller
							    (map (lambda (v) (v 0)) cur-vars)
							    (if (null? (cdr cur-vars)) "is" "are")
							    (format #f (if (= cur-line max-line)
									   (values " ~D" (+ cur-line 1))
									   (values "s ~D and ~D" (+ cur-line 1) (+ max-line 1))))
							    (length body)
							    (+ lint-left-margin 6) #\space
							    (truncated-list->string (list-ref body cur-line))
							    (if (= cur-line max-line)
								""
								(format #f "~%~NC~A"
									(+ lint-left-margin 6) #\space
									(truncated-list->string (list-ref body max-line))))
							    (+ lint-left-margin 4) #\space
							    (truncated-list->string form)))
					     (gather (cdr pv)
						     (cons (car pv) cur-vars)
						     (max max-line ((car pv) 2)))))))))
			       (let ((mnv ())
				     (cur-end i))
				 (for-each (lambda (v)
					     (when (and (or (null? mnv)
							    (<= (v 2) cur-end))
							(positive? (var-ref (v 3)))
							(let ((expr (var-initial-value (v 3))))
							  (not (lint-any? (lambda (ov) ; watch out for shadowed vars
									    (tree-memq (car ov) expr))
									  varlist))))
					       (set! mnv (cons v (if (= (v 2) cur-end) mnv ())))
					       (set! cur-end (v 2))))
					   last-refs)

				 ;; look for vars used only at the start of the let
				 (when (and (pair? mnv)
					    (< cur-end (/ i lint-let-reduction-factor))
					    (> (- i cur-end) 3))
				   ;; mnv is in the right order because last-refs is reversed
				   (lint-format "the scope of ~{~A~^, ~} could be reduced: ~A" caller
						(map (lambda (v) (v 0)) mnv)
						(lists->string form
							       `(let ,(map (lambda (v)
									     (if (member (car v) mnv (lambda (a b) (eq? a (b 0))))
										 (values)
										 v))
									   varlist)
								  (let ,(map (lambda (v)
									       (list (v 0) (var-initial-value (v 3))))
									     mnv)
								    ,@(copy body (make-list (+ cur-end 1))))
								  ,(list-ref body (+ cur-end 1))
								  ...)))))))))

		    ;; body of do loop above
		    (if (and (not got-lambdas)
			     (pair? (car p))
			     (pair? (cdr p))
			     (eq? (caar p) 'set!)
			     (var-member (cadar p) vars)
			     (not (tree-memq (cadar p) (cdr p))))
			(if (not (side-effect? (caddar p) env))     ;  (set! v0 (channel->vct 1000 100)) -> (channel->vct 1000 100)
			    (lint-format "~A in ~A could be omitted" caller (car p) (truncated-list->string form))
			    (lint-format "perhaps ~A" caller (lists->string (car p) (caddar p)))))
		    ;; 1 use in cadr and none thereafter happens a few times, but looks like set-as-documentation mostly

		    (for-each (lambda (v)
				(when (tree-memq (v 0) (car p))
				  (set! (v 2) i)
				  (if (not (v 1)) (set! (v 1) i))))
			      last-refs))))))

	  ;; -------- let-ends-in-set --------
	  (define (let-ends-in-set caller form)
	    (let ((body (cddr form))
		  (varlist (cadr form)))
	      ;; if last is (set! local-var...) and no complications, complain
	      (let ((last (last-ref body)))
		(when (and (len>2? last)
			   (eq? (car last) 'set!)
			   (symbol? (cadr last))
			   (assq (cadr last) varlist)       ; (let ((a 1) (b (display 2))) (set! a 2))
			   ;; this is overly restrictive:
			   (not (tree-set-memq '(call/cc call-with-current-continuation curlet lambda lambda*) form)))
		  (lint-format "set! is pointless in ~A: use ~A" caller
			       last (caddr last))))))      ; could add definers here, especially define

	  ;; -------- embed-let --------
	  (define (embed-let caller form env)
	    (let ((varlist (cadr form))
		  (body (cddr form)))
	      ;; (let ((x 1) (y 2)) (+ x y)) -> (+ 1 2)
	      ;; this happens a lot, but it often looks like a form of documentation
	      (when (and (< (length varlist) 8)
			 (not (or (memq (caar body) '(lambda lambda* define define* define-macro))
				  (and (eq? (caar body) 'set!)
				       (lint-any? (lambda (v) (eq? (car v) (cadar body))) varlist))
				  (any-macro? (caar body) env)
				  (lint-any? (lambda (p)
					       (and (unquoted-pair? p)
						    (or (not (hash-table-ref no-side-effect-functions (car p)))
							(any-pairs? (cdr p)))))
					     (cdar body))))
			 (lint-every? (lambda (v)
					(and (len>1? v)
					     (< (tree-leaves (cadr v)) 8)
					     (= (tree-count (car v) body 2) 1)))
				      varlist))

		(let ((new-body (copy (car body)))
		      (bool-arg? #f))
		  (for-each (lambda (v)
			      (if (not bool-arg?)
				  (let tree-walk ((tree body))
				    (if (pair? tree)
					(if (and (memq (car tree) '(or and))
						 (memq (car v) (cdr tree)))
					    (set! bool-arg? #t)
					    (begin
					      (tree-walk (car tree))
					      (tree-walk (cdr tree)))))))
			      (set! new-body (tree-subst (cadr v) (car v) new-body)))
			    varlist)
		  (lint-format (if bool-arg?
				   "perhaps, ignoring short-circuit issues, ~A"
				   "perhaps ~A")
			       caller (lists->string form new-body))))))


	  ;; -------- useless-let --------
	  (define (useless-let caller form env)
	    (when (and (pair? (cadr form))         ; (let ((x x)) (+ x 1)) -> (+ x 1), (let ((x x))...) does not copy x if x is a sequence
		       (lint-every? (lambda (c)
				      (and (len>1? c)  ; the usual... (let binding might be messed up)
					   (eq? (car c) (cadr c))))
				    (cadr form)))
	      (let ((vs (map car (cadr form))))
		(unless (lint-any? (lambda (p)
				     (and (pair? p)
					  (memq (cadr p) vs)
					  (or (eq? (car p) 'set!)
					      (lint-set!? p env))))
				   (cddr form))
		  (lint-format "perhaps omit this useless let: ~A" caller
			       (truncated-lists->string form
							(if (null? (cdddr form))
							    (caddr form)
							    (cons 'begin (cddr form)))))))))

	  ;; -------- let->cond --------
	  (define let->cond                                ; not named-let here
	    ;; (let ((x (A))) (if x (f x) B)) -> (cond ((A) => f) (else B)
	    (let ((wrap-new-form
		   (lambda (header new-form trailer)
		     (if (pair? trailer)
			 `(let ,header ,new-form ,@(if (< (tree-leaves trailer) 20) trailer '(...)))
			 (if (pair? header)
			     `(let ,header ,new-form)
			     new-form)))))
	      (lambda (caller form env)
		(let ((body (cddr form)))
		  (let ((p (car body))
			(trailer (cdr body))
			(vname #f)
			(vvalue #f)
			(header ()))
		    (when (let ((varlist (cadr form)))
			    (lint-any? (lambda (v)
					 (and (len=2? v)
					      (< 0 (tree-count (car v) p 5) 4)
					      (not (tree-memq (car v) trailer))
					      (begin
						(set! vname (car v))
						(set! vvalue (cadr v))
						(set! header (map (lambda (p)
								    (if (eq? v p)
									(values)
									p))
								  varlist)))))
				       varlist))
		      (let ((suggest made-suggestion)
			    (pargs (cdr p))
			    (first-arg (cadr p))
			    (next-args (cddr p)))

			;; (let ((x (assq a y))) (set! z (if x (cadr x) 0))) -> (set! z (cond ((assq a y) => cadr) (else 0)))
			(when (and (not (memq (car p) '(if cond))) ; handled separately below
				   (= (tree-count vname p 3) 2))
			  (do ((i 0 (+ i 1))
			       (bp pargs (cdr bp)))
			      ((or (not (pair? bp))
				   (let ((b (car bp)))
				     (and (len>2? b)
					  (eq? (car b) 'if)
					  (= (tree-count vname b 3) 2)
					  (eq? vname (cadr b))
					  (len=2? (caddr b))
					  (eq? vname (cadr (caddr b))))))
			       (if (pair? bp)
				   (let ((else-clause (if (pair? (cdddar bp)) (list (cons 'else (cdddar bp))) ())))
				     (lint-format "perhaps ~A" caller
						  (lists->string form
								 (wrap-new-form
								  header
								  `(,@(copy p (make-list (+ i 1)))
								    (cond (,vvalue => ,(caaddr (car bp))) ,@else-clause)
								    ,@(cdr bp))
								  trailer))))))))

			(when (and (eq? (car p) 'cond) ; (let ((x (f y))) (cond (x (g x)) ...)) -> (cond ((f y) => g) ...)
				   (len=2? first-arg)
				   (eq? (car first-arg) vname)
				   (or (and (len=2? (cadr first-arg))  ; one arg to func
					    (eq? vname (cadadr first-arg)))
				       (eq? vname (cadr first-arg)))
				   (or (null? next-args)
				       (not (tree-memq vname next-args))))
			  (lint-format "perhaps ~A" caller
				       (lists->string form
						      (wrap-new-form
						       header
						       (if (eq? vname (cadr first-arg))
							   (list 'or vvalue
								 (if (and (pair? next-args)
									  (len>1? (car next-args))
									  (memq (caar next-args) '(else #t t)))
								     (if (null? (cddar next-args))
									 (cadar next-args)
									 (cons 'begin (cdar next-args)))
								     (cons 'cond next-args)))
							   (cons 'cond
								 (cons (list vvalue '=> (caadr first-arg))
								       next-args)))
						       trailer))))

			(when (and (null? next-args)         ; (let ((x (+ y 1))) (abs x)) -> (abs (+ y 1))
				   (eq? vname first-arg))      ;   not tree-subst or trailing (pair) args: the let might be forcing evaluation order

			  (if (or (hash-table-ref built-in-functions (car p))
				  (let ((v (var-member (car p) env)))
				    (and v (memq (var-ftype v) '(define define* lambda lambda*))))) ; was definer??
			      (lint-format "perhaps ~A" caller (lists->string form
									      (wrap-new-form header (list (car p) vvalue) trailer)))
			      (if (not (or (any-macro? vname env)
					   (tree-unquoted-member vname (car p))))
				  (lint-format "perhaps, assuming ~A is not a macro, ~A" caller (car p)
					       (lists->string form
							      (wrap-new-form header (list (car p) vvalue) trailer))))))
			(when (pair? next-args)
			  (when (and (eq? (car p) 'if)
				     (pair? (cdr next-args)))
			    (let ((if-true (car next-args))
				  (if-false (cadr next-args)))

			      (when (and (eq? first-arg vname) ; (let ((x (g y))) (if x #t #f)) -> (g y)
					 (boolean? if-true)
					 (boolean? if-false)
					 (not (eq? if-true if-false)))
				(lint-format "perhaps ~A" caller
					     (lists->string form
							    (wrap-new-form header (if if-true vvalue (list 'not vvalue)) trailer))))

			      (when (and (len>1? first-arg) ; (let ((x (f y))) (if (not x) B (g x))) -> (cond ((f y) => g) (else B))
					 (eq? (car first-arg) 'not)
					 (eq? (cadr first-arg) vname)
					 (len=2? if-false)
					 (eq? vname (cadr if-false)))
				(let ((else-clause (if (eq? if-true vname)
						       (list (list 'else #f))
						       (if (and (pair? if-true)
								(tree-memq vname if-true))
							   :oops! ; if the let var appears in the else portion, we can't do anything with =>
							   (list (list 'else if-true))))))
				  (unless (eq? else-clause :oops!)
				    (lint-format "perhaps ~A" caller
						 (lists->string form
								(wrap-new-form
								 header
								 (cons 'cond (cons (list vvalue '=> (car if-false)) else-clause))
								 trailer))))))))
			  (let ((crf #f))
			    ;; all this stuff still misses (cond ((not x)...)) and (set! y (if x (cdr x)...)) i.e. need embedding in this case
			    (when (and (or (and (memq (car p) '(if and))          ; (let ((x (f y))) (and x (g x))) -> (cond ((f y) => g) (else #f))
						(eq? first-arg vname))
					   (and (eq? (car p) 'or)
						(equal? first-arg (list 'not vname)))
					   (and (pair? vvalue)
						(memq (car vvalue) '(assoc assv assq member memv memq))
						(len>1? first-arg)                 ; (let ((x (memq z y))) (if (pair? x) (g x))) -> (cond ((memq z y) => g))
						(or (eq? (car first-arg) 'pair?)
						    (and (eq? (car first-arg) 'list?)
							 (lint-format "in ~A, ~A can't be null so pair? might be better" caller p vname)
							 #t)
						    (and (eq? (car first-arg) 'null?)   ; (let ((x (assoc y z))) (if (null? x) (g x)))
							 (lint-format "in ~A, ~A can't be null because ~A in ~A only returns #f or a pair"
								      caller p vname (car vvalue) (truncated-list->string (list vname vvalue)))
							 #f))
						(eq? (cadr first-arg) vname))
					   (and (pair? vvalue)
						(memq (car vvalue) '(char-position string-position string->number length arity)) ; length|arity only in s7
						(or (eq? first-arg vname)
						    (and (len>1? first-arg)
							 (or (memq (car first-arg) '(number? complex?))
							     (and (not (eq? (car vvalue) 'string->number))
								  (eq? (car first-arg) 'integer?)))
							 (eq? (cadr first-arg) vname)))))

				       (or (and (len=2? (car next-args))   ; one func arg
						(or (eq? vname (cadar next-args))
						    (and (hash-table-ref combinable-cxrs (caar next-args))
							 ((lambda* (cr arg) ; lambda* not lambda because combine-cxrs might return just #f
							    (and cr
								 (< (length cr) 5)
								 (eq? vname arg)
								 (set! crf (symbol "c" cr "r"))))
							  (combine-cxrs (car next-args))))))
					   (and (eq? (car p) 'if)
						(eq? (car next-args) vname)
						(not (tree-unquoted-member vname (cdr next-args))) ; (let ((x (g y))) (if x x (g z))) -> (or (g y) (g z))
						(lint-format "perhaps ~A" caller
							     (lists->string form
									    (wrap-new-form
									     header
									     (if (null? (cdr next-args))
										 vvalue
										 (list 'or vvalue (cadr next-args)))
									     trailer)))
						#f))
				       (pair? (car next-args))
				       (or (eq? (car p) 'if)
					   (null? (cdr next-args))))
			      (let ((else-clause (if (pair? (cdr next-args))
						     (if (eq? (cadr next-args) vname)
							 (list (list 'else #f)) ; this stands in for the local var
							 (if (and (pair? (cadr next-args))
								  (tree-memq vname (cadr next-args)))
							     :oops!   ; if the let var appears in the else portion, we can't do anything with =>
							     (list (list 'else (cadr next-args)))))
						     (case (car p)
						       ((and) '((else #f)))
						       ((or)  '((else #t)))
						       (else  ())))))
				(unless (eq? else-clause :oops!)     ; (let ((x (assoc y z))) (if x (cdr x))) -> (cond ((assoc y z) => cdr))
				  (lint-format "perhaps ~A" caller
					       (lists->string form
							      (wrap-new-form
							       header
							       (cons 'cond
								     (cons (list vvalue '=> (or crf (caar next-args)))
									   else-clause))
							       trailer))))))))
			(when (and (= suggest made-suggestion)
				   (< (tree-leaves vvalue) 20))
			  ;; also need to be sure let is not blocking defines or keep (let ()...)? -- never happens I think
			  (case (car p)
			    ((cond)
			     (when (and (pair? pargs)
					(pair? first-arg)
					(eq? vname (car first-arg))
					(= (tree-count vname body 2) 1))
			       (lint-format "perhaps ~A" caller
					    (lists->string form
							   (wrap-new-form header `(cond (,vvalue ,@(cdr first-arg)) ,@next-args) trailer)))))
			    ((when unless)
			     (if (and (eq? first-arg vname)
				      (= (tree-count vname body 2) 1)) ; 2 if we can use cond => (remember cdr)
				 (lint-format "perhaps ~A" caller
					      (lists->string form
							     (wrap-new-form header (tree-subst vvalue vname p) trailer)))))
			    ((if)
			     (when (len=3? p)
			       (if (eq? first-arg vname)
				   (let ((calls (tree-count vname body 3)))
				     (if (= calls 1)
					 (lint-format "perhaps ~A" caller
						      (lists->string form
								     (wrap-new-form header (tree-subst vvalue vname p) trailer)))
					 (let ((true (and (pair? next-args) (car next-args))))
					   (if (and (= calls 2)
						    (len=2? true)
						    (eq? first-arg (cadr true)))
					       (lint-format "perhaps ~A" caller
							    (lists->string form
									   (wrap-new-form header (list 'cond (list vvalue '=> (car true))) trailer)))))))
				   (if (and (len=2? first-arg)
					    (eq? (car first-arg) 'not)
					    (eq? (cadr first-arg) vname)
					    (= (tree-count vname body 2) 1))
				       (lint-format "perhaps ~A" caller
						    (lists->string form
								   (wrap-new-form header (tree-subst vvalue vname p) trailer))))))))))))))))

	  ;; -------- let->for-each --------
	  (define (let->for-each caller form varlist body)
	    (when (and (len>2? body)
		       (null? (cdr varlist)) ; allowing extra vars gets no additional hits
		       (pair? (car varlist))
		       (pair? (cdar varlist))
		       (pair? (cadar varlist)))
	      (let ((name (caar varlist))
		    (value (cadar varlist)))
		(when (and (eq? (car value) 'lambda)
			   (pair? (cadr value))
			   (< 0 (length (cadr value)) 3))
		  (do ((arg1 ())
		       (arg2 ())
		       (p body (cdr p))
		       (i 0 (+ i 1)))
		      ((or (null? p)
			   (not (and (pair? (car p))
				     (eq? name (caar p)))))
		       (if (and (>= i 3)
				(not (tree-memq name p))) ; we could split the body into for-each sections, but that would repeat the lambda
			   (lint-format "perhaps ~A" caller
					(lists->string form
						       (let ((fe (let ((a1 (if (just-code-constants? arg1)
									       (list 'quote (map unquoted (reverse arg1)))
									       (cons 'list (reverse arg1))))
								       (a2 (if (not (pair? arg2))
									       ()
									       (if (just-code-constants? arg2)
										   (list 'quote (map unquoted (reverse arg2)))
										   (cons 'list (reverse arg2))))))
								   (if (pair? arg2)
								       `(for-each ,value ,a1 ,a2)
								       `(for-each ,value ,a1)))))
							 (if (null? p)
							     fe
							     `(let () ,fe ...)))))))
		    (set! arg1 (cons (cadar p) arg1))
		    (if (pair? (cddar p))
			(set! arg2 (cons (caddar p) arg2))))))))

	  ;; -------- combine set+one-use --------
	  (define (combine-set+one-use caller body varlist env)
	    (do ((hits ())
		 (prev ())
		 (p body (cdr p))) ; or start at (cdr body)??
		((not (pair? p))
		 (for-each (lambda (h)
			     (lint-format "perhaps combine these two lines:~%~NC~A~%~NC~A" caller
					  (+ lint-left-margin 4) #\space
					  (truncated-list->string (car h))
					  (+ lint-left-margin 4) #\space
					  (truncated-list->string (caadr h))))
			   hits))
	      (let ((call (car p)))
		(if (not (pair? call))
		    (set! prev ())
		    (if (and (eq? (car call) 'set!)
			     (assq (cadr call) varlist))
			(set! prev call)
			(begin
			  (if (and (pair? prev)
				   (= (tree-count (cadr prev) call 2) 1)
				   (not (or (tree-memq (cadr prev) (cdr p))
					    (hash-table-ref definers-table (car call))
					    (side-effect? (caddr prev) env) ; this is needed -- let* in effect
					    (any-macro? (car call) env)
					    (memq (car call) '(map for-each list-values))
					    (and (eq? (car call) 'let)
						 (symbol? (cadr call))))) ; i.e. not named-let
				   (or (not (eq? (car call) 'do))
				       (and (pair? (caddr call))
					    (tree-memq (cadr prev) (caddr call)))  ; in the end+result section
				       (and (pair? (cadr call))
					    (member (cadr prev) (cadr call) (lambda (a b) (eq? a (cadr b))))))) ; initial value
			      (set! hits (cons (list prev p) hits)))
			  (set! prev ())))))))

	  ;; --------- rewrite-funcs --------
	  (define (rewrite-funcs f)
	    (let ((def (cdr f)))
	      (if (symbol? (cadr def))
		  (cdr def)
		  (list (caadr def)
			(cons (if (eq? (car def) 'define*)
				  'lambda*
				  'lambda)
			      (cons (cdadr def)
				    (if (< (tree-leaves (cddr def)) local-function-context)
					(cddr def)
					'(...))))))))

	  ;; -------- let-local-funcs->closure --------
	  (define (let-local-funcs->closure caller form body largs)
	    (let ((named-let (symbol? (cadr form))))
	      (if named-let (set! largs (cons (cadr form) largs)))
	      (let ((ok-funcs (local-movable-funcs body largs)))
		(if (eq? body last-lambda-let)       ; remove funcs already moved to the enclosing function's closure
		    (set! ok-funcs (map (lambda (f)
					  (if (assq (car f) last-lambda-let-funcs)
					      (values)
					      f))
					ok-funcs)))
		(when (pair? ok-funcs)
		  (let* ((func-names (map car ok-funcs))
			 (letrec? (lint-any? (lambda (f)
					       (tree-set-memq func-names (cdddr f)))
					     ok-funcs))
			 (old-vars (if (< (tree-leaves (cadr form)) local-function-context)
				       (cadr form)
				       (list (if (< (tree-leaves (caadr form)) local-function-context)
						 (caadr form)
						 (list (caaadr form) '...))
					     '...))))

		    ;; the letrec has to be added (we can't combine let+letrec) because
		    ;;   (let ((x 1)) (letrec ((x (+ x 1))) x)) is an error ("+ argument 1, #<undefined>...")
		    ;;   so the original (let ((x (+ x 1))) (define ...)...) has to be
		    ;;   (let ((x (+ x 1))) (letrec ...) ...) or the reverse: (let ((x 1)) (letrec (...) (let ((x (+ x 1))) x)))
		    ;;   there's no straightforward way to say "these entities are independent" if one is a recursive function
		    ;;     and another shadows but depends on the outer environment!  Also, it's not impossible that letrec here should
		    ;;     be letrec* in r7rs -- the example below may be incorrect in r7rs.
		    ;;   I think I'll put the functions on the outside.
		    ;; (letrec ((f1 () (lambda () (f2 1))) (f2 (lambda () (f1 2))))...) is happy in s7.

		    (lint-format "the inner function~A ~{~A~^, ~} could be moved ~A: ~A" caller
				 (if (null? (cdr ok-funcs)) "" "s")
				 func-names
				 (if named-let
				     (format #f "out to ~A's closure" (cadr form))
				     (if letrec?
					 "to an outer letrec"
					 "into the let"))
				 (lists->string form
						(if named-let
						    `(,(if letrec? 'letrec 'let) ,(map rewrite-funcs ok-funcs)
						      (let ,old-vars ...))
						    (if letrec?
							`(letrec ,(map rewrite-funcs ok-funcs)
							   (let ,old-vars ...))
							`(let (,@old-vars
							       ,@(map rewrite-funcs ok-funcs))
							   ...))))))))))

	  ;; -------- let->case-else --------
	  (define (let->case-else caller form var selector body) ; (let ((q (caddr x))) (if (eq? q 'a) b q)) -> (case (caddr x) ((a) b) (else))
	    (let ((expr (car body))                             ;   this function also called by let*-walker
		  (test #f)
		  (true #f)
		  (false #f))
	      ;; scan of multiple vars for useful one got no hits
	      (when (case (car expr)
		      ((if) (and (= (length expr) 4)
				 (pair? (cadr expr))))
		      ((cond) (and (= (length expr) 3)
				   (len=2? (cadr expr))
				   (len=2? (caddr expr))
				   (pair? (caadr expr))
				   (eq? (caaddr expr) 'else)))
		      (else #f))
		(if (eq? (car expr) 'if)
		    (begin
		      (set! test (cadr expr))
		      (set! true (caddr expr))
		      (set! false (cadddr expr)))
		    (begin
		      (set! test (caadr expr))
		      (set! true (cadadr expr))
		      (set! false (cadr (caddr expr)))))
		  (when (and (memq var test)
			     (or (and (len=2? test)
				      (memq (car test) '(null? not eof-object?)))   ; memx/charx got no hits
				 (and (len=2? (cdr test))
				      (memq (car test) '(eq? eqv? =))
				      (lint-any? code-constant? test)))
			     (or (and (or (eq? false var)
					  (and (len=2? false)
					       (eq? var (cadr false))))
				      (not (tree-memq var true)))  ; shadowing happens very rarely
				 (and (or (eq? true var)
					  (and (len=2? true)
					       (eq? var (cadr true))))
				      (not (tree-memq var false)))))
		    (set! last-let->case-line-number line-number)
		    (lint-format "perhaps use case: ~A" caller
				 (let ((new-form (let ((key #f))
						   (if (memq (car test) '(null? not eof-object?))
						       (set! key (case (car test) ((not) #f) ((eof-object?) #<eof>) (else ())))
						       (begin
							 (set! key ((if (eq? (cadr test) var) caddr cadr) test))
							 (if (pair? key) (set! key (cadr key)))))
						   (if (or (eq? false var)
							   (and (len=2? false)
								(eq? var (cadr false))))
						       `(case ,selector
							  ((,key) ,true)
							  (else ,@(if (eq? false var) () (list '=> (car false)))))
						       `(case ,selector
							  ((,key) ,@(if (eq? true var) () (list '=> (car true))))
							  (else ,false))))))
				   (lists->string form
						  (if (or (eq? (car form) 'let)
							  (null? (cdadr form)))
						      new-form
						      (if (null? (cddadr form))
							  `(let (,(caadr form))
							     ,new-form)
							  `(let* ,(copy (cadr form) (make-list (- (length (cadr form)) 1)))
							     ,new-form))))))))))

	  ;; -------- let-walker --------

	  (define let-walker
	    (let ((input-side-effect-functions '(;set-current-input-port close-input-port open-input-file open-input-string
						 read-char peek-char read-byte read-line read-string read))
		  (output-side-effect-functions '(;set-current-output-port close-output-port flush-output-port open-output-file open-output-string
						  ;get-output-string call-with-output-string call-with-output-file with-output-to-file
						  newline write-char write-string display write write-byte format)))

	      ;; need port extractors for each of these
	      (define (extract-port init)
		(if (pair? (cdr init))
		    (cadr init)
		    ()))

	      (lambda (caller form env)
		(if (or (< (length form) 3)             ; (let ((a 1) (set! a 2)))
			(not (or (symbol? (cadr form))
				 (list? (cadr form)))))
		    (lint-format "let is messed up: ~A" caller (truncated-list->string form))

		    (let ((named-let (and (symbol? (cadr form)) (cadr form))))
		      (if (keyword? named-let)          ; (let :x ((i y)) (x i))
			  (lint-format "bad let name: ~A" caller named-let))

		      (if named-let
			  (if *report-shadowed-variables*
			      (report-shadower caller 'let 'named-let-function-name named-let named-let env))
			  (remove-null-let caller form env))

		      (let ((vars (declare-named-let form env))
			    (varlist ((if named-let caddr cadr) form))
			    (body ((if named-let cdddr cddr) form)))

			(if (not (and (proper-list? varlist)
				      (just-pairs? varlist)))
			    (lint-format "let is messed up: ~A" caller (truncated-list->string form))
			    (begin
			      (if (and (null? varlist)
				       (len=1? body)
				       (not (side-effect? (car body) env))) ; (let xx () z)
				  (lint-format "perhaps ~A" caller (lists->string form (car body))))

			      (set! vars (walk-let-vars caller form varlist vars env))

			      (when (and (pair? body)
					 (pair? vars)
					 (func-definer? (car body)))
				(let-local-funcs->closure caller form body (map var-name vars)))
			      ;; here we could check '+signature+ and others, but it is tricky to tell that
			      ;;   var-initial-value is messed up

			      (let ((suggest made-suggestion))
				(unless named-let
				  (when (and (pair? varlist)
					     (pair? body)
					     (len>1? (car body)))
				    (let->cond caller form env)
				    (when (null? (cdr body))
				      (if (and ;(= suggest made-suggestion)
					   (null? (cdr varlist))
					   (pair? (cdar varlist)))
					  (let->case-else caller form (caar varlist) (cadar varlist) body))
				      (move-let-into-if caller form env)
				      (when (= suggest made-suggestion)
					(embed-let caller form env))))
				  (when (= suggest made-suggestion)
				    (useless-let caller form env)))

				;; let ((x <init1>) (y <init2)) -> varlist: '((x <init1>) (y <init2>))
				;;   if init1 and init2 have side-effect contention, suggest let*
				;;   (let ((x (read-char)) (y (peek-char)) ...) -> (let* ...)
				;; see also check-unordered-exprs above (sigh) -- I missed it when I wrote this code
				(when (and (pair? varlist)
					   (pair? (cdr varlist)))
				  (let ((hits (map (lambda (var)
						     (if (and (pair? (cdr var))
							      (pair? (cadr var)))
							 (if (memq (caadr var) input-side-effect-functions)
							     (list :input var)
							     (if (memq (caadr var) output-side-effect-functions)
								 (list :output var)
								 (values)))
							 (values)))
						   varlist)))
				    (when (and (pair? hits)
					       (pair? (cdr hits)))
				      (let loop1 ((cur-var (car hits)) (rest (cdr hits)))
					(let* ((cur-init (cadadr cur-var))
					       (cur-port (extract-port cur-init)) ; can be #f
					       (got-hit #f))
					  (let loop2 ((rst-var rest))
					    (when (pair? rst-var)
					      (if (and (eq? (car cur-var) (caar rst-var))
						       cur-port ; not #f
						       (equal? cur-port (extract-port (cadr (cadar rst-var)))))
						  ;; or equal? one to () and the other to current-input|output-port
						  (begin
						    (lint-format "in ~S, let should be let*" caller (truncated-list->string form))
						    (set! got-hit #t))
						  (loop2 (cdr rst-var)))))
					  (unless (or got-hit (null? rest))
					    (loop1 (car rest) (cdr rest))))))))

				(let ((es (walk-letx-body caller form body vars env)))
				  (set! vars (car es))
				  (set! env (cdr es)))

				(when (pair? vars)
				  (unless (or named-let
					      (not *report-combinable-lets*))
				    (evert-function-locals form vars env))
				  (when (and (pair? (cadr form))
					     (pair? (caadr form)))
				    (split-let caller form body vars env)))

				(let-var->body caller form body varlist)

				(when (pair? body)
				  (when (len>2? (car body))
				    (let-body->value caller form vars env))
				  (when (and (pair? varlist)
					     (not named-let))
				    (let->for-each caller form varlist body)
				    (let-ends-in-set caller form)
				    (when (and (pair? (car body))
					       (eq? (caar body) 'do))
				      (normal-let->do caller form env))
				    (tighten-let caller form vars env)
				    (if (= suggest made-suggestion)
					(combine-set+one-use caller body varlist env)))))

			      (combine-lets caller form varlist env))))))
		env)))
	  (hash-walker 'let let-walker)


	  ;; -------- let*->let+do --------
	  (define (let*->let+do caller form env)
	    (let ((body (cddr form))
		  (varlist (cadr form)))
	      ;; let*->do (could go further down)
	      (when (and *report-combinable-lets*
			 (pair? body)
			 (pair? (car body))
			 (eq? (caar body) 'do)
			 (len>2? (car body))
			 (just-len>1? (cadar body))
			 (< (tree-leaves (cdr body)) *max-cdr-len*))
		(let ((inits (if (pair? (cadar body))
				 (map cadr (cadar body))
				 ()))
		      (locals (if (pair? (cadar body))
				  (map car (cadar body))
				  ()))
		      (lv (list-ref varlist (- (length varlist) 1))))
		  (unless (and (pair? inits)
			       (or (memq (car lv) locals) ; shadowing
				   (tree-memq (car lv) inits)
				   (not (pair? (cdr lv)))
				   (side-effect? (cadr lv) env)))
		    ;; (let* ((x (log z))) (do ((i 0 (+ x z))) ((= i 3)) (display x))) -> (do ((x (log z)) (i 0 (+ x z))) ...)
		    (lint-format "perhaps ~A" caller
				 (lists->string form
						(let ((new-do (let ((do-form (cdar body)))
								(if (null? (cdr body))
								    (list 'do (cons lv (car do-form))
									  '...)
								    `(do ,(cons lv (car do-form))
									 (,(and (pair? (cadr do-form)) (caadr do-form))
									  ,@(if (side-effect? (cdadr do-form) env) (cdadr do-form) ())
									  ,@(cdr body))   ; include rest of let as do return value
								       ...)))))
						  (case (length varlist)
						    ((1) new-do)
						    ((2) (list 'let (list (car varlist)) new-do))
						    (else (list 'let* (copy varlist (make-list (- (length varlist) 1)))
								new-do)))))))))))

	  ;; -------- walk-let*-vars --------
	  (define (walk-let*-vars caller form vars env)
	    (let* ((named-let (and (symbol? (cadr form)) (cadr form)))
		   (varlist ((if named-let caddr cadr) form)))
	      (do ((side-effects #f)
		   (bindings varlist (cdr bindings)))
		  ((null? bindings)
		   (if (or (null? varlist)
			   (len=1? varlist)                  ; (let* ((x (log y))) x)
			   (not (or side-effects             ; (let* ((x (log y)) (z 32))...)
				    (lint-any? (lambda (v) (positive? (var-ref v))) vars))))
		       (lint-format "let* could be let: ~A" caller (truncated-list->string form))))
		;; in s7, let evaluates var values top down, so this message is correct
		;;   even in cases like (let ((ind (open-sound...)) (mx (maxamp))) ...)
		;; in r7rs, the order is not specified (section 4.2.2 of the spec), so
		;;   here we would restrict this message to cases where there is only
		;;   one variable, or where subsequent values are known to be independent.
		;; if each function could tell us what globals it depends on or affects,
		;;   we could make this work in all cases.

		(when (binding-ok? caller 'let* (car bindings) env #f)
		  (let ((expr (cadar bindings))
			(side (side-effect? (cadar bindings) env)))
		    (if (not (or (eq? bindings varlist)
				 ;; first var side-effect is innocuous (especially if it's the only one!)
				 ;;    does this need to protect against a side-effect that the next var accesses?
				 ;;    I think we're ok -- the accessed var must be exterior, and we go down in order
				 side-effects))
			(set! side-effects side))
		    (let ((e (lint-walk caller expr (append vars env))))
		      (if (and (pair? e)
			       (not (eq? e env))
			       (memq (var-name (car e)) '(:lambda :dilambda)))
			  (let ((ldata (cdar e)))
			    (set! (var-name (car e)) (caar bindings))
			    (let-set! ldata 'initial-value expr)
			    (set! vars (cons (car e) vars)))
			  (set! vars (cons (make-lint-var (caar bindings) expr (if named-let 'named-let* 'let*))
					   vars))))

		    ;; look for duplicate values
		    ;;   someday protect against any shadows if included in any expr
		    (unless (or side
				(not (pair? expr))
				(code-constant? expr)
				(maker? expr))
		      (let ((name (caar bindings)))
			(let dup-check ((vs (cdr vars)))
			  (if (and (pair? vs)
				   (pair? (car vs))
				   (not (eq? name (caar vs)))
				   (not (tree-memq (caar vs) expr)))
			      ;; perhaps also not side-effect of car vs initial-value (char-ready? + read + char-ready? again)
			      (if (equal? expr (var-initial-value (car vs)))
				  ;; (let* ((x (log y 2)) (y (log y 2)) (z (f x))) (+ x y z z))
				  (lint-format "~A's value ~S could be ~S" caller
					       name expr (caar vs))
				  (dup-check (cdr vs))))))))))
	      vars))

	  ;; -------- let*->let+let --------
	  (define (let*->let+let caller form vars env)
	    ;; if var is not used except in other var bindings, it can be moved out of this let*
	    ;;   collect vars not in body, used in only one binding, gather these cases, and rewrite the let*
	    ;;   repeated names are possible here
	    ;;   also cascading dependencies: (let* ((z 1) (y (+ z 2)) (x (< y 3))) (if x (f x)))
	    ;;                                (let ((x (let ((y (let ((z 1))) (+ z 2))) (< y 3)))) ...) ??
	    ;;                      new-vars: ((z y) (y x))
	    (let ((body (cddr form))
		  (new-vars ())
		  (vs-pos vars)
		  (no-repeats (do ((p vars (cdr p)))
				  ((or (null? p)
				       (var-member (var-name (car p)) (cdr p)))
				   (null? p)))))
	      (for-each (lambda (v)
			  (let ((vname (var-name v))
				(vvalue #f))
			    (if (not (tree-memq vname body))
				(let walker ((vs vars))
				  (if (not (pair? vs))
				      (if (and vvalue
					       (or (not (side-effect? (var-initial-value v) env))
						   (eq? vvalue (var-name (car vs-pos)))))
					  (set! new-vars (cons (list vvalue vname (var-initial-value v)) new-vars)))
				      (let ((b (car vs)))
					(if (or (eq? (var-name b) vname)
						(not (tree-memq vname (var-initial-value b)))) ; tree-memq matches the bare symbol (tree-member doesn't)
					    (walker (cdr vs))
					    (unless vvalue
					      (set! vvalue (var-name b))
					      (walker (cdr vs))))))))
			    (set! vs-pos (cdr vs-pos))))
			(cdr vars)) ; vars is reversed from code order, new-vars is in code order

	      (when (pair? new-vars)
		(define (gather-dependencies var val env)
		  (let ((deps ()))
		    (for-each (lambda (nv)
				(if (and (eq? (car nv) var)
					 (or no-repeats
					     (tree-memq (cadr nv) val)))
				    (set! deps (cons (list (cadr nv)
							   (gather-dependencies (cadr nv) (caddr nv) env))
						     deps))))
			      new-vars)
		    (if (> (tree-leaves val) 30)
			(set! val '...))
		    (if (pair? deps)
			(list (if (null? (cdr deps)) 'let 'let*) deps val)
			val)))

		(let ((new-let-binds (map (lambda (v)
					    (if (member (var-name v) new-vars (lambda (name lst) (eq? name (cadr lst))))
						(values)
						(list (var-name v)
						      (gather-dependencies (var-name v) (var-initial-value v) env))))
					  (reverse vars))))
		  ;; (let* ((a 1) (b 2) (c (+ a 1))) (* c 2)) -> (let* ((b 2) (c (let ((a 1)) (+ a 1)))) ...)
		  (lint-format "perhaps restrict ~{~A~^, ~} which ~A not used in the let* body ~A" caller
			       (map cadr new-vars)
			       (if (null? (cdr new-vars)) "is" "are")
			       (lists->string form
					      (list (if (null? (cdr new-let-binds)) 'let 'let*)
						    new-let-binds
						    '...)))))

	      (when (and no-repeats
			 *report-splittable-lets*
			 (len>2? vars))
		(let ((outer-vars ())
		      (inner-vars ())
		      (let-vars ())
		      (cur-vars ())
		      (varlist ((if (symbol? (cadr form)) caddr cadr) form)))
		  (do ((vs (reverse vars) (cdr vs)))
		      ((null? vs))

		    (let* ((v (car vs))
			   (vname (var-name v))
			   (value (var-initial-value v)))

		      (if (or (side-effect? value env)
			      (lint-any? (lambda (trailing-var)
					   ;; vname is possible inner let var if it is not mentioned in any trailing initial value
					   ;;   (repeated name can't happen here)
					   (tree-memq vname (var-initial-value trailing-var)))
					 (cdr vs)))
			  (set! outer-vars (cons vname outer-vars))
			  (set! inner-vars (cons vname inner-vars)))

		      (if (and (not (and (pair? value)
					 (memq (car value) cur-vars)))
			       (constant-expression? value env)
			       (do ((oldv varlist (cdr oldv)))
				   ((or (null? oldv)
					(tree-memq vname (car oldv)))
				    (and (pair? oldv)
					 (pair? (car oldv))
					 (eq? vname (caar oldv))))))
			  (set! let-vars (cons vname let-vars)))
		      (set! cur-vars (cons vname cur-vars))))

		  (when (and (pair? outer-vars)
			     (or (len>1? let-vars)
				 (len>1? inner-vars)))
		    (let ((lv ())
			  (ov ())
			  (iv ())
			  (truncate-value
			   (lambda (v)
			     (list v (let ((val (var-initial-value (var-member v vars))))
				       (if (and (pair? val)
						(> (tree-leaves val) 20))
					   (list (car val) '...)
					   val))))))
		    (when (len>1? let-vars)
		      (set! outer-vars (remq-set let-vars outer-vars))
		      (set! inner-vars (remq-set let-vars inner-vars))
		      (set! lv (map truncate-value let-vars)))

		    (if (pair? outer-vars)
			(set! ov (map truncate-value outer-vars)))

		    (if (len>1? inner-vars)
			(set! iv (map truncate-value inner-vars))
			(if (pair? inner-vars)
			    (set! ov (cons (truncate-value (car inner-vars)) ov))))

		    (set! ov (if (null? ov)
				 (list 'let (reverse iv) '...)
				 (list (if (pair? (cdr ov)) 'let* 'let)
				       (reverse ov)
				       (if (pair? iv)
					   (list 'let (reverse iv) '...)
					   '...))))
		    (set! lv (if (pair? lv)
				 (list 'let (reverse lv) ov)
				 ov))

		    (lint-format "perhaps split this let*: ~A" caller
				 (lists->string form lv))))))))


	  ;; -------- let*+let*->let*
	  (define (let*+let*->let* caller form)
	    (let ((varlist (cadr form))
		  (body (cddr form)))
	      ;; successive let*'s combined into one
	      (when (and (len>1? (car body))
			 (or (eq? (caar body) 'let*)      ; let*+let* -> let*
			     (and (eq? (caar body) 'let)  ; let*+let(1) -> let*
				  (or (null? (cadar body))
				      (and (pair? (cadar body))
					   (null? (cdadar body))))))
			 (null? (cdr body))
			 (not (symbol? (cadar body))))
		;; (let* ((a 1) (b (+ a 2))) (let* ((c (+ b 3)) (d (+ c 4))) (display a) (+ a... ->
		;;    (let* ((a 1) (b (+ a 2)) (c (+ b 3)) (d (+ c 4))) (display a) ...)
		(lint-format "perhaps ~A" caller
			     (lists->string form
					    (cons 'let*
						  (cons (append varlist (cadar body))
							(one-call-and-dots (cddar body)))))))))

	  ;; -------- remove-unneeded-let*-vars --------
	  (define (remove-unneeded-let*-vars caller form env)
	    (do ((body (cddr form))
		 (changes ())
		 (vs (cadr form) (cdr vs)))
		((null? vs)
		 (if (pair? changes)
		     (let ((new-form (copy form)))
		       (for-each
			(lambda (v)
			  (list-set! new-form 1 (remove-if (lambda (p) (equal? p v)) (cadr new-form)))
			  (set! new-form (tree-subst (cadr v) (car v) new-form)))
			changes)
		       ;; (let* ((x y) (a (* 2 x))) (+ (f a (+ a 1)) (* 3 x))) -> (let ((a (* 2 y))) (+ (f a (+ a 1)) (* 3 y)))
		       (lint-format "assuming we see all set!s, the binding~A ~{~A~^, ~} ~A pointless: perhaps ~A" caller
				    (if (pair? (cdr changes)) "s" "")
				    changes
				    (if (pair? (cdr changes)) "are" "is")
				    (lists->string form
						   (let ((header (if (len>1? (cadr new-form)) 'let* 'let)))
						     (cons header
							   (if (< (tree-leaves new-form) 200)
							       (cdr new-form)
							       (cons (cadr new-form)
								     (one-call-and-dots (cddr new-form)))))))))))
	      (let ((v (car vs)))
		(if (and (len=2? v)
			 (symbol? (cadr v))
			 (not (assq (cadr v) (cadr form))) ; value is not a local var
			 (not (set-target (car v) body env))
			 (not (set-target (cadr v) body env)))
		    (let ((data (var-member (cadr v) env)))
		      (if (and (or (not data)
				   (and (not (eq? (var-definer data) 'parameter))
					(or (null? (var-setters data))
					    (not (tree-set-memq (var-setters data) body)))))
			       (not (lint-any? (lambda (p)
						 (and (len>1? p)
						      (or (set-target (cadr v) (cdr p) env)
							  (set-target (car v) (cdr p) env)
							  (and data
							       (pair? (var-setters data))
							       (tree-set-memq (var-setters data) body)))))
					       (cdr vs))))
			  (set! changes (cons v changes))))))))

	  ;; -------- combine-let*-vars --------
	  (define (combine-let*-vars caller form vars env)
	    ;; successive vars, first used in second but nowhere else -- combine if (very!) simple-looking
	    (let ((varlist (cadr form))
		  (body (cddr form)))
	      (do ((gone-vars ())
		   (v varlist (cdr v)))
		  ((or (null? v)
		       (null? (cdr v)))

		   (when (pair? gone-vars)
		     (let ((waiter #f)
			   (new-vars ())
			   (save-vars ()))
		       (set! gone-vars (reverse gone-vars))
		       (set! new-vars (map (lambda (v)
					     (if (and (pair? gone-vars)
						      (eq? v (car gone-vars)))
						 (begin
						   (set! waiter v)
						   (set! gone-vars (cdr gone-vars))
						   (values))
						 (if (not waiter)
						     v
						     (let ((new-v (tree-subst (cadr waiter) (car waiter) v)))
						       (set! save-vars (cons (list (car waiter) (car v)) save-vars))
						       (set! waiter #f)
						       new-v))))
					   varlist))
		       ;; (let* ((y 3) (x (log y))) x) -> (let ((x (log 3))) ...)
		       (lint-format "perhaps substitute ~{~{~A into ~A~}~^, ~}: ~A" caller
				    (reverse save-vars)
				    (lists->string form
						   (list (if (null? (cdr new-vars)) 'let 'let*)
							 new-vars
							 '...))))))
		(let ((cur-var (car v))
		      (nxt-var (cadr v)))
		  (when (and (len>1? cur-var)
			     (let ((v (var-member (car cur-var) vars)))
			       (and v
				    (zero? (var-set v))))
			     (len>1? nxt-var)
			     (< (tree-leaves (cadr cur-var)) 8)
			     (not (and (len>1? (cadr nxt-var))
				       (eq? (caadr nxt-var) 'let) ; if named-let, forget it
				       (symbol? (cadadr nxt-var))))
			     (or (not (pair? (cadr nxt-var)))
				 (not (side-effect? (cadr cur-var) env))
				 (lint-every? (lambda (a)
						(or (code-constant? a)
						    (assq a varlist)))
					      (cdadr nxt-var)))
			     (= (tree-count (car cur-var) (cadr nxt-var) 2) 1)
			     (not (tree-memq (car cur-var) (cddr v)))
			     (not (tree-memq (car cur-var) body)))
		    (set! gone-vars (cons cur-var gone-vars))
		    (set! v (cdr v)))))))

	  ;; -------- combine-let*-last-var --------
	  (define (combine-let*-last-var caller form last-var env)
	    ;; if last var only occurs once in body, and timing can't be an issue, substitute its value
	    ;;   this largely copied from the let case above (but only one substitution)
	    ;;   in both cases, we're assuming that the possible last-var value's side-effect won't
	    ;;      affect other vars (in let* the local, in let something outside that might be used locally)
	    ;;      perhaps add (not (side-effect (cadr last-var) env))?
	    (let ((body (cddr form))
		  (varlist (cadr form)))
	      (let ((varlist-len (length varlist)))
		(when (and (pair? (cdr last-var))  ; varlist-len can be 1 here
			   (< (tree-leaves (cadr last-var)) 12)
			   (= (tree-count (car last-var) body 2) 1)
			   (pair? (car body))
			   (null? (cdr body))
			   (not (memq (caar body) '(lambda lambda* define define* define-macro)))
			   (not (and (eq? (caar body) 'set!)
				     (eq? (car last-var) (cadar body))))
			   (not (any-macro? (caar body) env))
			   (not (lint-any? (lambda (p)
					     (and (unquoted-pair? p)
						  (or (not (hash-table-ref no-side-effect-functions (car p)))
						      (any-pairs? (cdr p)))))
					   (cdar body))))
		  ;; (let* ((a 1) (b 2) (c (+ a 1))) (* c 2)) -> (let* ((a 1) (b 2)) (* (+ a 1) 2))
		  (lint-format "perhaps ~A" caller
			       (lists->string form (cons (if (<= varlist-len 2) 'let 'let*)
							 (cons (copy varlist (make-list (- varlist-len 1)))
							       (tree-subst (cadr last-var) (car last-var) body))))))

		(when (null? (cdr body)) ; (let* (...(x A)) (if x (f A) B)) -> (let(*) (...) (cond (A => f) (else B)))
		  (when (pair? (cdr last-var))
		    (let ((p (car body)))
		      (when (and (len>2? p)
				 (case (car p)
				   ((if and) (eq? (cadr p) (car last-var)))
				   ((or)     (equal? (cadr p) (list 'not (car last-var))))
				   (else #f))
				 (len=2? (caddr p))
				 (or (eq? (car p) 'if)
				     (null? (cdddr p)))
				 (not (eq? (caaddr p) (car last-var))) ; ! (let* (...(x A)) (if x (x x)))
				 (eq? (car last-var) (cadr (caddr p))))

			(let ((else-clause (if (pair? (cdddr p)) ; only if 'if (see above)
					       (if (eq? (cadddr p) (car last-var))
						   `((else #f)) ; this stands in for the local var
						   (if (and (pair? (cadddr p))
							    (tree-memq (car last-var) (cadddr p)))
						       :oops! ; if the let var appears in the else portion, we can't do anything with =>
						       (list (list 'else (cadddr p)))))
					       (case (car p)
						 ((and) '((else #f)))
						 ((or)  '((else #t)))
						 (else  ())))))
			  (unless (eq? else-clause :oops!)
			    ;; (let* ((x (f y))) (and x (g x))) -> (cond ((f y) => g) (else #f)
			    (lint-format "perhaps ~A" caller
					 (case varlist-len
					   ((1) (lists->string form
							       (cons 'cond
								     (cons (list (cadr last-var) '=> (caaddr p))
									   else-clause))))
					   ((2) (lists->string form
							       `(let (,(car varlist))
								  (cond (,(cadr last-var) => ,(caaddr p)) ,@else-clause))))
					   (else (lists->string form
								`(let* ,(copy varlist (make-list (- varlist-len 1)))
								   (cond (,(cadr last-var) => ,(caaddr p)) ,@else-clause)))))))))))

		  (unless (pair? (car body))
		    (if (and (eq? (car body) (caar varlist))
			     (null? (cdr varlist))
			     (pair? (cdar varlist)))       ; (let* ((a...)) a)
			;; (let* ((x (log y))) x) -> (log y)
			(lint-format "perhaps ~A" caller (lists->string form (cadar varlist)))
			(if (and (> varlist-len 1)         ; (let* (... (x y)) x) -> (let(*)(...) y)
				 (len=2? last-var)
				 (eq? (car body) (car last-var)))
			    ;; (let* ((y 3) (x (log y))) x) -> (let ((y 3)) (log y))
			    (lint-format "perhaps ~A" caller
					 (lists->string form (list (if (= varlist-len 2) 'let 'let*)
								   (copy varlist (make-list (- varlist-len 1)))
								   (cadr last-var)))))))))))

	  ;; -------- reduce-let*-scope
	  (define (reduce-let*-scope caller form vars)
	    (let ((lastref (vector (var-name (car vars)) #f 0 (car vars))) ; this is the last of the let* vars I think
		  (body (cddr form))
		  (varlist (cadr form)))
	      (do ((p body (cdr p))
		   (i 0 (+ i 1)))
		  ((null? p)
		   (let ((cur-line (lastref 1))
			 (max-line (lastref 2))
			 (vname (lastref 0)))
		     (if (and (< max-line (/ i lint-let-reduction-factor))
			      (> (- i max-line) 3))
			 (lint-format "the scope of ~A could be reduced: ~A" caller
				      vname
				      (lists->string form
						     `(,(if (> (length vars) 2) 'let* 'let)
						       ,(copy varlist (make-list (- (length vars) 1)))
						       (let (,(list vname (var-initial-value (lastref 3))))
							 ,@(copy body (make-list (+ max-line 1))))
						       ,(list-ref body (+ max-line 1))
						       ...)))
			 (when (and (integer? cur-line)
				    (< (- max-line cur-line) 2)
				    (code-constant? (var-initial-value (lastref 3))))
			   (lint-format "~A is only used in expression~A (of ~A),~%~NC~A~A of~%~NC~A" caller
					vname
					(format #f (if (= cur-line max-line)
						       (values " ~D" (+ cur-line 1))
						       (values "s ~D and ~D" (+ cur-line 1) (+ max-line 1))))
					(length body)
					(+ lint-left-margin 6) #\space
					(truncated-list->string (list-ref body cur-line))
					(if (= cur-line max-line)
					    ""
					    (format #f "~%~NC~A"
						    (+ lint-left-margin 6) #\space
						    (truncated-list->string (list-ref body max-line))))
					(+ lint-left-margin 4) #\space
					(truncated-list->string form))))) )
		(when (tree-memq (lastref 0) (car p))
		  (set! (lastref 2) i)
		  (if (not (lastref 1)) (set! (lastref 1) i))))))

	  ;; -------- let*-local-funcs->closure --------
	  (define (let*-local-funcs->closure caller form body largs)
	    (let ((ok-funcs (local-movable-funcs body largs)))
	      (when (pair? ok-funcs)
		(let* ((func-names (map car ok-funcs))
		       (letrec? (lint-any? (lambda (f)
					     (tree-set-memq func-names (cdddr f)))
					   ok-funcs)))
		  (lint-format "the inner function~A ~{~A~^, ~} could be moved out of the let*: ~A" caller
			       (if (null? (cdr ok-funcs)) "" "s")
			       func-names
			       (lists->string form
					      `(,(if letrec? 'letrec 'let) ,(map rewrite-funcs ok-funcs)
						 (let* ,(if (> (tree-leaves (cadr form)) local-function-context)
							    (list (caadr form) '...)
							    (cadr form))
						   ...))))))))

	  ;; -------- let*-walker --------
	  (define (let*-walker caller form env)
	    (if (< (length form) 3)
		(lint-format "let* is messed up: ~A" caller (truncated-list->string form))
		(let ((named-let (and (symbol? (cadr form)) (cadr form))))
		  (let ((vars (declare-named-let form env))
			(varlist ((if named-let caddr cadr) form))
			(body ((if named-let cdddr cddr) form)))

		    (if (and named-let
			     *report-shadowed-variables*)
			(report-shadower caller 'let* 'named-let*-function-name named-let named-let env))

		    (if (not (and (proper-list? varlist)
				  (just-pairs? varlist)))
			(lint-format "let* is messed up: ~A" caller (truncated-list->string form))
			(begin
			  (when (and (pair? body)
				     (pair? varlist)
				     (func-definer? (car body)))
			    (let*-local-funcs->closure caller form body
						       (if named-let
							   (cons (cadr form) (map car varlist))
							   (map car varlist))))

			  (let ((es (walk-letx-body caller form body
						    (walk-let*-vars caller form vars env)
						    env)))
			    (set! vars (car es))
			    (set! env (cdr es)))

			  (when (and (not named-let)
				     (pair? body)
				     (proper-pair? varlist))
			    (when (pair? vars)
			      (let*->let+let caller form vars env))
			    (let*+let*->let* caller form)
			    (let*->let+do caller form env)
			    ;; (define...) as first in body rarely happens in rewritable contexts

			    (unless (unsafe-definer? body)
			      (remove-unneeded-let*-vars caller form env))
			    (combine-let*-vars caller form vars env)

			    (let ((last-var (last-ref varlist)))
			      (combine-let*-last-var caller form last-var env)
			      (if (and (null? (cdr body))
				       (len>1? (car body))
				       (len=2? last-var))
				  (let->case-else caller form (car last-var) (cadr last-var) body)))
			    ;; last var -> if/cond in car(body) as in let only happens a few times (leaving aside stuff caught above)

			    (when (and (> (length body) 3)
				       (> (length vars) 1)
				       (not (tree-set-car-member '(define define* define-macro define-macro*
								    define-bacro define-bacro* define-constant define-expansion)
								 body)))
			      (reduce-let*-scope caller form vars))))))))

	    env)
	  (hash-walker 'let* let*-walker)


	  ;; -------- letrec->let --------
	  (define (letrec->let caller form vars env)
	    (let ((head (car form)))
	      (do ((bindings (cadr form) (cdr bindings)) ; if none of the local vars occurs in any of the values, no need for the "rec"
		   (vs (map var-name vars)))
		  ((or (not (and (pair? bindings)
				 (pair? (car bindings))
				 (pair? (cdar bindings))))
		       (memq (cadar bindings) vs)
		       (tree-set-memq vs (cadar bindings)))
		   (when (null? bindings)
		     (let ((letx (if (or (eq? head 'letrec)
					 (do ((p (map cadr (cadr form)) (cdr p))
					      (q (map car (cadr form)) (cdr q)))
					     ((or (null? p)
						  (side-effect? (car p) env)
						  (memq (car q) (cdr q)))
					      (null? p))))
				     'let 'let*)))
		       ;; (letrec ((f1 (lambda (a) a))) 32)
		       (lint-format "~A could be ~A: ~A" caller
				    head letx
				    (truncated-list->string form))))))

	      (when (and (eq? (car form) 'letrec*)
			 (len>1? (cadr form)) ; len=1 case handle elsewhere
			 (lint-every? (lambda (p)
					(and (len=2? p)
					     (pair? (cadr p))
					     (eq? (caadr p) 'lambda)))
				      (cadr form)))
		;; this happens only in psyntax-pp.scm (Guile)
		(lint-format "letrec* could be letrec: ~A" caller (truncated-list->string form)))

	      (when (and (null? (cdr vars))
			 (pair? (cadr form))
			 (len>1? (caadr form))
			 (len=1? (cddr form))
			 (pair? (caddr form)))
		(let ((body (caddr form))
		      (sym (var-name (car vars)))
		      (lform (cadar (cadr form))))           ; the letrec var's lambda
		  (when (and (len>1? lform)
			     (eq? (car lform) 'lambda)
			     (proper-list? (cadr lform)))    ; includes ()
		    (if (eq? sym (car body))                 ; (letrec ((x (lambda ...))) (x...)) -> (let x (...)...)
			(if (and (not (tree-memq sym (cdr body)))
				 (< (tree-leaves body) 100))
			    ;; the limit on tree-leaves is for cases where the args are long lists of data --
			    ;;   more like for-each than let, and easier to read if the code is first, I think.
			    (lint-format "perhaps ~A" caller
					 (lists->string
					  form `(let ,sym
						  ,(map list (cadr lform) (cdr body))
						  ,@(cddr lform)))))
			(if (and (not (eq? caller 'define))
				 (= (tree-count sym body 2) 1))
			    (let ((call (find-call sym body)))
			      (when (pair? call)
				(let ((new-call `(let ,sym
						   ,(map list (cadr lform) (cdr call))
						   ,@(cddr lform))))
				  (lint-format "perhaps ~A" caller
					       (lists->string form (tree-subst new-call call body)))))))))))))

	  ;; maybe (let () ...) here because (letrec ((x (lambda (y) (+ y 1)))) (x (define z 32))) needs to block z?
	  ;;   currently we get (let x ((y (define z 32))) (+ y 1))
	  ;;   and even that should be (let () (define z 32) (+ z 1)) or something similar
	  ;; lambda here is handled under define??

	  ;; -------- walk-letrec-vars --------
	  (define (walk-letrec-vars caller form env)
	    (let ((head (car form))
		  (vars ()))
	      (do ((warned (or (eq? head 'letrec*)
			       (not (pair? (cadr form)))
			       (negative? (length (cadr form))))) ; malformed letrec
		   (baddy #f)
		   (bindings (cadr form) (cdr bindings)))
		  ((not (pair? bindings))
		   (if (not (null? bindings)) ; (letrec* letrec)!
		       (lint-format "~A variable list is not a proper list? ~S" caller head (cadr form))))

		(when (and (not warned)              ; letrec -> letrec*
			   (len>1? (car bindings))
			   ;; type of current var is not important -- if used in non-function elsewhere,
			   ;;   it has to be letrec*
			   (lint-any? (lambda (b)
					(and (len>1? b)
					     (or (and (not (pair? (cadr b)))
						      (eq? (caar bindings) (cadr b)))
						 (tree-memq (caar bindings) (cadr b)))
					     (not (tree-set-memq '(lambda lambda* define define* case-lambda) (cadr b)))
					     (set! baddy b)))
				      (cdr bindings)))
		  (set! warned #t)
		  ;; (letrec ((x 32) (f1 (let ((y 1)) (lambda (z) (+ x y z)))) (f2 (f1 x))) (+ x f2))
		  (lint-format "in ~A,~%~NCletrec should be letrec* because ~A is used in ~A's value (not a function): ~A" caller
			       (truncated-list->string form)
			       (+ lint-left-margin 4) #\space
			       (caar bindings)
			       (car baddy)
			       (cadr baddy)))

		(when (binding-ok? caller head (car bindings) env #f)
		  (let ((init (if (and (eq? (caar bindings) (cadar bindings))
				       (or (eq? head 'letrec)
					   (not (var-member (caar bindings) vars))))
				  (begin ; (letrec ((x x)) x)
				    (lint-format "~A is the same as (~A #<undefined>) in ~A" caller
						 (car bindings) (caar bindings) head)
				    ;; in letrec* ((x 12) (x x)) is an error
				    #<undefined>)
				  (cadar bindings))))
		    (set! vars (cons (make-lint-var (caar bindings) init head)
				     vars)))))
	      vars))

	  ;; -------- letrec+lambda->lambda+let --------
	  (define (letrec+lambda->lambda+let caller form)
	    (when (and (len=1? (cadr form))
		       (null? (cdddr form))
		       (pair? (caddr form))
		       (eq? (caaddr form) 'lambda)
		       (pair? (caadr form))
		       (tree-car-member (caaadr form) (cddr form))
		       (= (tree-count (caaadr form) (cddr form) 2) 1)) ; this alone can give (caaadr form) passed as a function arg
	      (let ((lr-lambda (cadr (caadr form))))
		(when (and (pair? lr-lambda)
			   (eq? 'lambda (car lr-lambda)))
		  (let ((pars (cadr lr-lambda)))
		    (when (proper-list? pars)
		      (let* ((lr-name (caaadr form))
			     (lr-args (let search ((tree (caddr form)))
					(and (pair? tree)
					     (if (eq? (car tree) lr-name)
						 (cdr tree)
						 (or (search (car tree))
						     (search (cdr tree))))))))

			(call-with-exit
			 (lambda (quit)
			   (let ((vs (car (out-vars lr-name pars (cddr lr-lambda)))))
			     (when (pair? vs)
			       (for-each (lambda (v)
					   (if (shadowed? v (caddr form)) ; this never happens
					       (quit)))
					 vs)))                            ; set (cadr) appears to include args which leads to false positives here
			   (lint-format "perhaps ~A" caller
					(lists->string form
						       (tree-subst `(let ,lr-name ,(map list pars lr-args)
									 ,@(one-call-and-dots (cddr lr-lambda)))
								   (cons lr-name lr-args)
								   (caddr form)))))))))))))

	  ;; -------- letrec-walker --------
	  (define (letrec-walker caller form env)
	    (if (not (and (>= (length form) 3)                 ;  (letrec () . 1)
			  (proper-list? (cadr form))
			  (just-pairs? (cadr form))))
		(begin
		  (lint-format "~A is messed up: ~A" caller (car form) (truncated-list->string form))
		  env)
		(let ((head (car form)))

		  (cond ((null? (cadr form))        ;  (letrec () 1)
			 (lint-format "~A could be let: ~A" caller head (truncated-list->string form)))

			((and (null? (cdadr form))
			      (eq? head 'letrec*))  ;  (letrec* ((a (lambda b (a 1)))) a)
			 (lint-format "letrec* could be letrec: ~A" caller (truncated-list->string form))))

		  (let ((vars (walk-letrec-vars caller form env)))
		    ;; no hits for func-definer as car of body

		    (when (eq? head 'letrec)
		      (check-unordered-exprs caller form (map var-initial-value vars) env)
		      (letrec+lambda->lambda+let caller form))

		    ;; define backwards propagation check got no hits: (letrec ((f1 (lambda () f2))) (define f2 1) (f1))

		    (when (pair? vars)
		      (letrec->let caller form vars env))

		    (when (pair? (cadr form))
		      (let ((new-env (append vars env)))
			(for-each (lambda (binding)
				    (if (binding-ok? caller head binding env #t)
					(lint-walk caller (cadr binding) new-env)))
				  (cadr form))))

		   (cdr (walk-letx-body caller form (cddr form) vars env))))))

	  (hash-walker 'letrec letrec-walker)
	  (hash-walker 'letrec* letrec-walker))


	;; ---------------- begin ----------------
	(let ()
	  (define (begin-walker caller form env)

	    (if (not (proper-list? form))
		(begin                           ;  (begin . 1)
		  (lint-format "stray dot in begin? ~A" caller (truncated-list->string form))
		  env)
		(begin
		  (when (pair? (cdr form))
		    (if (null? (cddr form))      ;  (begin (f y))
			(lint-format "begin could be omitted: ~A" caller (truncated-list->string form))

			;; these two are questionable -- simpler, but scope enlarged
			(when (and (pair? (cadr form))
				   (len=1? (cddr form)))

			  ;; begin+do+return -> do+return
			  (if (and (eq? (caadr form) 'do)
				   (< (tree-leaves (caddr form)) 24) ; or maybe (< ... (min 24 (tree-leaves do-form)))?
				   (not (tree-set-memq (map car (cadadr form)) (caddr form))))
			      ;;  (begin (do ((i 0 (+ i 1))) ((= i 3)) (display i)) 32) -> (do ((i 0 (+ i 1))) ((= i 3) 32) (display i))
			      ;; the do loop has to end normally to go on? That is, moving the following expr into the do end section is safe?
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  (let ((do-form (cdadr form)))
							    (let ((do-test (and (pair? (cadr do-form))
										(caadr do-form)))
								  (new-end (if (len>1? (cadr do-form))
									       (append (cdadr do-form) (cddr form))
									       (cddr form))))
							      `(do ,(car do-form)
								   (,do-test ,@new-end)
								 ,@(cddr do-form))))))

			      ;; begin+letx+return -> letx+return
			      (if (and (memq (caadr form) '(let let* letrec letrec*)) ; same for begin + let + expr -- not sure about this...
				       (not (symbol? (cadadr form)))
				       (< (tree-leaves (caddr form)) 24)
				       (not (tree-set-memq (map car (cadadr form)) (caddr form))))
				  (lint-format "perhaps ~A" caller
					       (lists->string form
							      (let ((let-form (cadr form)))
								`(,(car let-form) ,(cadr let-form)
								  ,@(if (< (tree-leaves (cddr let-form)) 60)
									(cddr let-form)
									(one-call-and-dots (cddr let-form)))
								  ,(caddr form))))))))))
		  (lint-walk-open-body caller 'begin (cdr form) env))))
	  (hash-walker 'begin begin-walker))


	;; ---------------- with-baffle ----------------
	(let ()
	  (define (with-baffle-walker caller form env)
	    ;; with-baffle introduces a new frame, so we need to handle it here
	    (lint-walk-body caller 'with-baffle (cdr form)
			    (cons (make-lint-var :with-baffle form 'with-baffle)
				  env))
	    env)
	  (hash-walker 'with-baffle with-baffle-walker))


	;; ---------------- let-temporarily ----------------
	(let ()
	  (define (let-temporarily-walker caller form env)
	    (if (< (length form) 2)  ; empty body is ok here
		(lint-format "let-temporarily is messed up: ~A" caller (truncated-list->string form))
		(let ((new-env (cons (make-lint-var :let form 'let-temporarily) env)))
		  (if (null? (cadr form))
		      (lint-format "let-temporarily with no vars? ~A" caller (truncated-list->string form))
		      (lint-walk caller (cadr form) new-env))
		  (let ((e (lint-walk-body caller 'let-temporarily (cddr form) new-env)))
		    (report-usage caller 'let-temporarily
				  (if (eq? e new-env)
				      ()
				      (env-difference caller e new-env ()))
				  new-env))))
	    env)
	  (hash-walker 'let-temporarily let-temporarily-walker))


	;; -------- with-let --------
	(let ()
	  (define (with-let-walker caller form env)
	    (if (< (length form) 3)
		(lint-format "with-let is messed up: ~A" caller (truncated-list->string form))
		(let ((e (cadr form)))
		  (if (or (and (code-constant? e)
			       (not (let? e))
			       (not (eq? e '*s7*))) ; (with-let *s7* ... )
			  (and (pair? e)
			       (let ((op (return-type (car e) env)))
				 (and op
				      (not (return-type-ok? 'let? op))))))  ;  (with-let 123 123)
		      (lint-format "~A: first argument should be an environment: ~A" 'with-let caller (truncated-list->string form)))

		  (if (symbol? e)
		      (set-ref e caller form env)
		      (when (pair? e)
			(if (and (null? (cdr e))
				 (eq? (car e) 'curlet))                 ;  (with-let (curlet) x)
			    (lint-format "~A is not needed here: ~A" 'with-let caller (truncated-list->string form)))
			(lint-walk caller e (cons (make-lint-var :let form 'with-let)
						  env))))
		  (let ((lib #f)
			(new-env (cons (make-lint-var :with-let form 'with-let) env))
			(ignore-usage (memq e '(*motif* *gl* *libc* *libm* *libgdbm* *libgsl*))))
		    (if (or ignore-usage
			    (and (len>1? e)
				 (eq? (car e) 'sublet)
				 (memq (cadr e) '(*motif* *gl* *libc* *libm* *libgdbm* *libgsl*))
				 (set! e (cadr e))))
			(set! lib (if (defined? e)
				       (symbol->value e)
				       (let ((file (*autoload* e)))
					 (and (string? file)
					      (load file))))))
		    (let-temporarily ((*e* (if (let? lib) lib *e*))
				      (lint-in-with-let #t))
		      (let ((e (lint-walk-open-body caller 'with-let (cddr form) new-env)))
			(if (not (or ignore-usage
				     (tree-memq 'curlet (cddr form))))
			    (report-usage caller 'with-let
					  (if (eq? e new-env)
					      ()
					      (env-difference caller e new-env ()))
					  new-env)))))))
	    env)
	  (hash-walker 'with-let with-let-walker))


	;; ---------------- load ----------------
	(let ()
	  (define (load-walker caller form env)
	    (check-call caller 'load form env)
	    (if (and (pair? (cdr form))
		     (equal? (cadr form) ""))
		(lint-format "load needs a real file name, not the empty string: ~A" caller form))
	    (lint-walk caller (cdr form) env)
	    (if (and *report-loaded-files*
		     (string? (cadr form)))
		(catch #t
		  (lambda ()
		    (lint-file (cadr form) env))
		  (lambda args
		    env))
		env))
	  (hash-walker 'load load-walker))


	;; ---------------- require ----------------
	(let ()
	  (define (require-walker caller form env)
	    (if (not (pair? (cdr form)))                   ; (require)
		(lint-format "~A is pointless" caller form)
		(if (lint-any? string? (cdr form))              ; (require "repl.scm")
		    (lint-format "require's arguments should be symbols: ~A" caller (truncated-list->string form))))
	    (if (not *report-loaded-files*)
		env
		(let ((vars env))
		  (for-each
		   (lambda (f)
		     (let ((file (*autoload* f)))
		       (if (string? file)
			   (catch #t
			     (lambda ()
			       (set! vars (lint-file file vars)))
			     (lambda args
			       #f)))))
		   (cdr form))
		  vars)))
	  (hash-walker 'require require-walker))


	;; ---------------- call-with-input-file etc ----------------
	(let ()
	  (define (call-with-io-walker caller form env)
	    (check-call caller (car form) form env)
	    (let ((len (if (eq? (car form) 'call-with-output-string) 2 3))) ; call-with-output-string func is the first arg, not second
	      (when (= (length form) len)

		;; call-with-output-string -> object->string
		(when (and (eq? (car form) 'call-with-output-string)
			   (= (length form) 2)
			   (len>1? (cadr form))
			   (eq? (caadr form) 'lambda)
			   (pair? (cadadr form)))
		  (let ((body (cddadr form)))
		    (when (and (pair? body)
			       (len>2? (car body))
			       (memq (caar body) '(write display))
			       (eq? (caddar body) (car (cadadr form))))
		      (if (null? (cdr body))
			  (lint-format "perhaps ~A" caller
				       (lists->string form (cons 'object->string
								 (cons (cadar body)
								       (if (eq? (caar body) 'display) '(#f) ())))))
			  (if (and (len=1? (cdr body))
				   (len=1? (cadr body))
				   (eq? (caadr body) 'newline))
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  (list 'format #f (if (eq? (caar body) 'display) "~A~%" "~S~%") (cadar body)))))))))

		(let ((func (list-ref form (- len 1))))
		  (if (= len 3)
		      (lint-walk caller (cadr form) env))
		  (if (not (and (len>1? func)
				(eq? (car func) 'lambda)))
		      (let ((f (and (symbol? func)
				    (symbol->value func *e*))))
			(if (and (procedure? f)
				 (not (aritable? f 1)))
			    (lint-format "~A argument should be a function of one argument: ~A" caller (car form) func))
			(lint-walk caller func env))
		      (let ((args (cadr func)))
			(let ((body (cddr func))
			      (port (and (pair? args) (car args)))
			      (head (car form)))

			  (when *report-shadowed-variables*
			    (report-shadower caller head 'port port func env))

			  (if (or (not port)
				  (pair? (cdr args)))
			      ;; (lambda () (write args) (newline))
			      (lint-format "~A argument should be a function of one argument: ~A" caller head func)
			      (begin
				(if (and (len=1? body)
					 (len=2? (car body))
					 (eq? (cadar body) port))
				    ;; (call-with-input-file "file" (lambda (p) (read-char p))) -> (call-with-input-file "file" read-char)
				    (lint-format "perhaps ~A" caller
						 (lists->string form
								(list head (if (= len 2)
									       (caar body)
									       (values (cadr form) (caar body)))))))
				(let ((cc (make-lint-var port
							 (list (case head
								 ((call-with-input-string)  'open-input-string)
								 ((call-with-output-string) 'open-output-string)
								 ((call-with-input-file)    'open-input-file)
								 ((call-with-output-file)   'open-output-file)
								 (else 'error)))
							 head)))
				  (lint-walk-body caller head body (cons cc
									 (cons (make-lint-var :let form head)
									       env)))
				  (report-usage caller head (list cc) env))))))))))
	    env)
	  (for-each (lambda (op)
		      (hash-walker op call-with-io-walker))
		    '(call-with-input-string call-with-input-file call-with-output-file call-with-output-string)))


	;; ---------------- catch ----------------
	(let ()
	  (define (catch-walker caller form env)
	    ;; catch tag is tricky -- it is evaluated, then eq? matches at error time, so we need
	    ;;   to catch constants that can't be eq?
	    (if (not (= (length form) 4))
		(begin
		  (lint-format "catch takes 3 arguments (tag body error-handler): ~A" caller (truncated-list->string form))
		  (lint-walk caller (cdr form) env))
		(let ((tag (cadr form)))
		  (if (or (and (not (pair? tag))
			       (or (number? tag) (char? tag) (length tag)))
			  (and (pair? tag)
			       (eq? (car tag) 'quote)
			       (or (not (pair? (cdr tag)))
				   (length (cadr tag)))))
		      ;; (catch #(0) (lambda () #f) (lambda a a))
		      (lint-format "catch tag ~S is unreliable (catch uses eq? to match tags)" caller tag)
		      (if (not tag) ; (catch #f ...)
			  (lint-format "catch tag #f makes this catch a no-op" caller)))
		  (let ((body (caddr form))
			(error-handler (cadddr form)))
		    ;; empty catch+catch apparently never happens
		    (lint-walk caller body (cons (make-lint-var :let form 'catch)
						 (cons (make-lint-var :catch form 'catch)
						       env)))
		    (lint-walk caller error-handler env))))
	    env)
	  (hash-walker 'catch catch-walker))


	;; ---------------- call-with-exit etc ----------------
	(let ()
	  (define (call-with-exit-walker caller form env)
	    (check-call caller (car form) form env)
	    (let ((continuation (and (pair? (cdr form))
				     (len>2? (cadr form))
				     (eq? (caadr form) 'lambda)
				     (pair? (cadadr form))
				     (car (cadadr form)))))

	      (if (not (symbol? continuation))
		  (lint-walk caller (cdr form) env)
		  (let ((body (cddadr form))
			(head (car form)))

		    (when *report-shadowed-variables*
		      (report-shadower caller head
				       (if (eq? head 'call-with-exit) 'exit-function 'continuation)
				       continuation form env))

		    (if (not (or (eq? head 'call-with-exit)    ;   (call/cc (lambda (p) (+ x (p 1))))
				 (eq? continuation (car body)) ; and (null? (cdr) I think (call/cc (lambda (k) k)) is intended
				 (tree-sym-set-member continuation '(lambda lambda* define define* curlet apply error) body)))
			;; this checks for continuation as arg (of anything), and any of set as car
			;;   for define and define* it would be tighter to check that they aren't returned or used as an arg
			(lint-format "perhaps ~A could be call-with-exit:~%~NC~A" caller
				     head
				     (+ lint-left-margin 4) #\space
				     (truncated-list->string form)))

		    (if (not (tree-unquoted-member continuation body))    ; (call-with-exit (lambda (p) (+ x 1)))
			(lint-format "~A ~A ~A appears to be unused: ~A" caller head
				     (if (eq? head 'call-with-exit) "exit function" "continuation")
				     continuation
				     (truncated-list->string form))
			(let ((last (and (proper-list? body)
					 (last-ref body))))
			  (if (and (pair? last)
				   (eq? (car last) continuation))
			      ;; (call-with-exit (lambda (return) (display x) (return (+ x y))))
			      (lint-format "~A is redundant here: ~A" caller continuation (truncated-list->string last)))))

		    (let ((cc (make-lint-var continuation (if (eq? head 'call-with-exit) :call/exit :call/cc) head)))
		      (lint-walk-body caller head body (cons cc env))
		      (report-usage caller head (list cc) env)))))
	    env)
	  (for-each (lambda (op)
		      (hash-walker op call-with-exit-walker))
		    '(call/cc call-with-current-continuation call-with-exit)))


	;; ---------------- import etc ----------------
	(let ()
	  (define (get-repeats caller lst)
	    (do ((repeats ())
		 (p lst (cdr p)))
		((null? p)
		 (if (pair? repeats)
		     (lint-format "repeated entr~@P: ~{~A~^, ~}" caller (length repeats) (reverse repeats))))
	      (if (and (memq (car p) (cdr p))
		       (not (memq (car p) repeats)))
		  (set! repeats (cons (car p) repeats)))))

	  (define (walk-import caller form env)   ; report repeated entries in import and export lists -- this does not apply to s7
	    (if (and (> (length form) 12)
		     (just-symbols? (cdr form)))
		(get-repeats caller (cdr form)))
	    env)

	  (hash-walker 'import walk-import)
	  (hash-walker 'export walk-import)
	  (hash-walker 'define-module (lambda (caller form env)
					(let loop ((lst (cdr form)))
					  (if (pair? lst)
					      (if (eq? (car lst) :export)
						  (get-repeats caller (cadr lst))
						  (loop (cdr lst)))))
					env)))

	(hash-walker 'provide
		     (lambda (caller form env)
		       (if (not (= (length form) 2))	                     ; (provide a b c)
			   (lint-format "provide takes one argument: ~A" caller (truncated-list->string form))
			   (unless (symbol? (cadr form))
			     (let ((op (->lint-type (cadr form))))
			       (if (not (memq op '(symbol? #f #t values)))   ; (provide "test")
				   (lint-format "provide's argument should be a symbol: ~S" caller form)))))
		       env))

	(hash-walker 'module        ; module apparently has different syntax and expectations in various schemes
		     (lambda (caller form env)
		       (if (len>1? (cdr form))
			   (lint-walk 'module (cddr form) env))
		       env))

	(hash-walker 'define-syntax
		     (lambda (caller form env)
		       ;; we need to put the macro name in env with ftype=define-syntax
		       (if (and (pair? (cdr form))
				(symbol? (cadr form))
				(not (keyword? (cadr form)))) ; !! this thing is a disaster from the very start
			   (cons (make-fvar (cadr form) 'define-syntax #f #f #f) env)
			   env)))

	(hash-walker 'define-method   ; guile and mit-scheme have different syntaxes here
		     (lambda (caller form env)
		       (let ((cdr-form (cdr form)))
			 (if (not (len>1? cdr-form))
			     env
			     (if (symbol? (car cdr-form))
				 (if (keyword? (car cdr-form))
				     (lint-walk-body caller 'define-method (cddr cdr-form) env)
				     (let ((new-env (if (var-member (car cdr-form) env)
							env
							(cons (make-fvar (car cdr-form) 'define-method #f #f #f) env))))
				       (lint-walk-body caller (car cdr-form) (cddr cdr-form) new-env)))
				 (let ((new-env (if (var-member (caar cdr-form) env)
						    env
						    (cons (make-fvar (caar cdr-form) 'define-method #f #f #f) env))))
				   (lint-walk-body caller (caar cdr-form) (cdr cdr-form) new-env)))))))

	(hash-walker 'let-syntax (lambda (caller form env)
				   (lint-walk-body caller 'define-method (cddr form) env)
				   env))

	(hash-walker 'letrec-syntax (lambda (caller form env)
				      (lint-walk-body caller 'define-method (cddr form) env)
				      env))


	;; ---------------- case-lambda ----------------
	(let ()
	  (define (case-lambda-walker caller form env)
	    (when (pair? (cdr form))
	      (let ((lens ())
		    (body ((if (string? (cadr form)) cddr cdr) form)) ; might have a doc string before the clauses
		    (doc-string (and (string? (cadr form)) (cadr form))))

		(for-each
		 (lambda (choice)
		   (if (pair? choice)
		       (let ((len (length (car choice))))
			 (if (member len lens)
			     ;; (case-lambda (() 0) ((x y) x) ((x y) (+ x y)) ((x y z) (+ x y z)) (args (apply + args))
			     (lint-format "repeated parameter list? ~A in ~A" caller (car choice) form))
			 (set! lens (cons len lens))
			 (lint-walk 'case-lambda (cons 'lambda choice) env))))
		 body)

		(case (length lens)
		  ((1)
		   ;; (case-lambda (() (if #f #f))) -> (lambda () (if #f #f))
		   (lint-format "perhaps ~A" caller
				(lists->string form
					       (if doc-string
						   (list 'let (list (list '+documentation+ doc-string))
							 (cons 'lambda (car body)))
						   (cons 'lambda (car body))))))
		  ((2)
		   (when (let arglists-equal? ((args1 (caar body))
					       (args2 (caadr body)))
			   (if (null? args1)
			       (len=1? args2)
			       (and (pair? args1)
				    (if (null? args2)
					(null? (cdr args1))
					(and (pair? args2)
					     (eq? (car args1) (car args2))
					     (arglists-equal? (cdr args1) (cdr args2)))))))
		     (let* ((clause1 (car body))
			    (body1 (cdr clause1))
			    (clause2 (cadr body))
			    (body2 (cdr clause2))
			    (arglist (let ((arg1 (car clause1))
					   (arg2 (car clause2)))
				       (if (> (car lens) (cadr lens)) arg2 arg1))) ; lens is reversed
			    (arg-name (last-ref arglist))
			    (diffs (let arg->defaults ((arg arg-name)
						       (b1 body1)
						       (b2 body2)
						       (defaults ()))
				     (and defaults
					  (cond ((null? b1) (and (null? b2) defaults))
						((null? b2) (and (null? b1) defaults))
						((eq? arg b1) (cons b2 defaults))
						((eq? arg b2) (cons b1 defaults))
						((pair? b1)
						 (and (pair? b2)
						      (arg->defaults arg (car b1) (car b2) (arg->defaults arg (cdr b1) (cdr b2) defaults))))
						(else (and (equal? b1 b2) defaults)))))))
		       (when (and (len=1? diffs)
				  (code-constant? (car diffs)))
			 (let ((new-body (if (> (car lens) (cadr lens)) body2 body1))
			       (new-arglist (if (not (car diffs))
						arglist
						(if (null? (cdr arglist))
						    (list (list arg-name (car diffs)))
						    (list (car arglist) (list arg-name (car diffs)))))))
			   ;; (case-lambda (() (display x #f)) ((y) (display x y))) -> (lambda* (y) (display x y))
			   (lint-format "perhaps ~A" caller
					(lists->string form
						       (if doc-string
							   `(let ((+documentation+ ,doc-string))
							      (lambda* ,new-arglist ,@new-body))
							   (cons 'lambda* (cons new-arglist new-body)))))))))))))
	    env)
	  (hash-walker 'case-lambda case-lambda-walker))
	walker-table))

    ;; end walker-functions
    ;; ----------------------------------------

    (define (hash-fragment reduced-form leaves env func orig-form line outer-vars)
      ;; func here is either #f or an env-style entry (cons name let) as produced by make-fvar,
      ;;   the let entries accessed are initial-value, history, arglist
      (let ((old (hash-table-ref (or (vector-ref fragments leaves) (vector-set! fragments leaves (make-hash-table))) reduced-form)))
	(set! fragmin (min leaves fragmin))
	(set! fragmax (min *fragment-max-size* (max leaves fragmax)))
	(if (not (vector? old))
	    (hash-table-set! (vector-ref fragments leaves) reduced-form (vector 1 (list line) (and func (list func)) orig-form #f outer-vars))
	    ;; key = reduced-form
	    ;; value = #(list uses line-numbers fvar original-form)
	    (begin
	      (vector-set! old 0 (+ (vector-ref old 0) 1))
	      (vector-set! old 1 (cons line (vector-ref old 1)))
	      (when func
		(if (not (vector-ref old 2))
		    (vector-set! old 2 (list func))
		    (let ((caller (if (keyword? (var-name func)) 'define (var-name func)))
			  (func-name (var-name func)))
		      (let search ((vs (vector-ref old 2)))
			(when (pair? vs)
			  (let* ((v (car vs))
				 (vname (var-name v)))
			    (cond ((not (eqv? (length (var-arglist v)) (length (var-arglist func))))
				   (search (cdr vs)))

				  ((eq? (var-history v) :built-in)
				   (lint-format "~A is the same as the built-in ~A ~A" caller
						func-name
						(if (eq? (car (var-initial-value v)) 'define-macro) 'macro 'function)
						vname))

				  ((not (var-member vname env))
				   (lint-format "~A is the same as ~A" caller
						func-name
						(if (and (pair-line-number (var-initial-value v))
							 (> (pair-line-number (var-initial-value v)) 0))
						    (format #f "~A (line ~D)" vname (pair-line-number (var-initial-value v)))
						    (if (eq? func-name vname)
							(format #f "previous ~A" vname)
							vname))))

				  ((eq? vname func-name)
				   (lint-format "~A definition repeated: ~A" caller
						func-name (truncated-list->string (var-initial-value func))))

				  (else
				   (lint-format "~A could be (define ~A ~A)" caller
						func-name func-name vname))))))
		      (vector-set! old 2 (cons func (vector-ref old 2))))))))))



    (denote reduce-tree
      (let ((quit #f)
	    (outer-vars ())
	    (local-ctr 0)
	    (line 0)
	    (reduced-form ())
	    (fuvar #f))

	(denote (set-outer ovars tree)
	  (if (null? ovars)
	      (quit)
	      (if (null? (caar ovars))
		  (begin
		    (set-car! (car ovars) tree)
		    (cadar ovars))
		  (set-outer (cdr ovars) tree))))

	(denote (reduce-v v)
	  ;; this might be the first appearance of (car v)
	  (when (null? (cadr v))
	    (list-set! v 1 (symbol "<L" (number->string local-ctr) ">"))
	    (list-set! v 2 local-ctr)
	    (set! local-ctr (+ local-ctr 1)))
	  (cadr v))

	(denote (reduce-walker tree vars)
	  (cond ((pair? tree)
		 (case (car tree)
		   ((quote)
		    tree)

		   ((let let*)
		    ;; in let we need to sort locals by order of appearance in the body
		    (if (<= (length tree) 2)
			(quit))
		    (let ((locals ())
			  (body ())
			  (named-let (symbol? (cadr tree)))
			  (lvars ()))
		      (if named-let
			  (begin
			    (set! lvars (cons (list (cadr tree) (symbol "<NL" (number->string local-ctr) ">") -1) lvars))
			    (set! local-ctr (+ local-ctr 1))
			    (set! locals (caddr tree))
			    (set! body (cdddr tree)))
			  (begin
			    (set! locals (cadr tree))
			    (set! body (cddr tree))))
		      (if (not (list? locals)) (quit))

		      (let ((func (if (eq? (car tree) 'let)
				      (lambda (local)
					(if (not (len>1? local)) (quit))
					(set! lvars (cons (list (car local) () 0 (reduce-walker (cadr local) vars)) lvars)))
				      (lambda (local)
					(if (not (len>1? local)) (quit))
					(set! lvars (cons (list (car local)
								(symbol "<L" (number->string local-ctr) ">")
								local-ctr
								(reduce-walker (cadr local) (append lvars vars)))
							  lvars))
					(set! local-ctr (+ local-ctr 1))))))
			(for-each func locals))

		      ;; now walk the body, setting the reduced local name by order of encounter (in let, not let*)
		      (let ((new-body (reduce-walker body (append lvars vars))))
			(unless (pair? new-body) (set! new-body (list new-body)))
			(when (and (eq? (car tree) 'let)
				   ;; fill-in unused-var dummy names etc
				   (pair? lvars))
			  (for-each (lambda (v)
				      (when (null? (cadr v))
					(list-set! v 1 (symbol "<L" (number->string local-ctr) ">"))
					(list-set! v 2 local-ctr)
					(set! local-ctr (+ local-ctr 1))))
				    lvars))
			(set! lvars (sort! lvars (lambda (a b) (< (caddr a) (caddr b)))))

			(if named-let
			    `(,(car tree) ,(cadr (assq (cadr tree) lvars))
			      ,(map (lambda (v) (list (cadr v) (cadddr v))) (cdr lvars))
			      ,@new-body)
			    `(,(car tree)
			      ,(map (lambda (v) (list (cadr v) (cadddr v))) lvars)
			      ,@new-body)))))

		   ((if)
		    (if (not (and (len>1? (cdr tree))
				  (list? (cdddr tree))))
			(quit))
		    (let ((expr (reduce-walker (cadr tree) vars))
			  (true (reduce-walker (caddr tree) vars)))
		      (if (null? (cdddr tree))
			  (if (and (len>1? expr)
				   (eq? (car expr) 'not))
			      (cons 'unless (cons (cadr expr) (unbegin true)))
			      (cons 'when (cons expr (unbegin true))))
			  (list 'if expr true (reduce-walker (cadddr tree) vars)))))

		   ((when unless)
		    (if (not (len>1? (cdr tree)))
			(quit))
		    (cons (car tree)
			  (cons (reduce-walker (cadr tree) vars)
				(map (lambda (p) (reduce-walker p vars)) (cddr tree)))))

		   ((set!)
		    (if (not (len>1? (cdr tree))) (quit))
		    (if (symbol? (cadr tree))
			(let ((v (assq (cadr tree) vars)))
			  (if (or (not v)  ; if not a var, it's about to be an outer-var
				  (and (not fuvar)
				       (memq (cadr v) '(<1> <2> <3> <4>))))
			      (quit))
			  (when (null? (cadr v))  ; must be a previously unencountered local
			    (list-set! v 1 (symbol "<L" (number->string local-ctr) ">"))
			    (list-set! v 2 local-ctr)
			    (set! local-ctr (+ local-ctr 1)))
			  (list 'set! (cadr v) (reduce-walker (caddr tree) vars)))
			(list 'set! (reduce-walker (cadr tree) vars) (reduce-walker (caddr tree) vars))))

		   ((do)
		    (if (not (and (len>1? (cdr tree))
				  (list? (cadr tree))
				  (list? (cdddr tree))))
			(quit))
		    (let ((locals (cadr tree))
			  (end+result (caddr tree))
			  (body (cdddr tree))
			  (lvars ()))
		      (if (not (and (list? end+result)
				    (proper-list? body)))
			  (quit))
		      (for-each (lambda (local)
				  (if (not (len>1? local))
				      (quit))
				  (set! lvars (cons (list (car local)
							  () 0
							  (reduce-walker (cadr local) vars)
							  (if (pair? (cddr local))
							      (caddr local)
							      :unset))
						    lvars)))
				locals)
		      (let ((new-env (append lvars vars)))
			(let ((new-end (reduce-walker end+result new-env))
			      (new-body (reduce-walker body new-env)))
			  (unless (pair? new-body)
			    (set! new-body (list new-body)))
			  (when (pair? lvars)
			    (for-each (lambda (lv)
					(if (not (eq? (lv 4) :unset))
					    (list-set! lv 4 (reduce-walker (lv 4) new-env))))
				      lvars)
			    (for-each (lambda (v)
					(when (null? (cadr v))
					  (list-set! v 1 (symbol "<L" (number->string local-ctr) ">"))
					  (list-set! v 2 local-ctr)
					  (set! local-ctr (+ local-ctr 1))))
				      lvars)
			    (set! lvars (sort! lvars (lambda (a b) (< (caddr a) (caddr b))))))

			  `(do ,(map (lambda (v)
				       (map v (if (eq? (v 4) :unset) '(1 3) '(1 3 4))))
				     lvars)
			       ,new-end
			     ,@new-body)))))

		   ((lambda)
		    (if (not (proper-pair? (cdr tree)))
			(quit))
		    (let* ((lvars (map (lambda (a)
					 (let ((res (list a (symbol "<A" (number->string local-ctr) ">") local-ctr)))
					   (set! local-ctr (+ local-ctr 1))
					   res))
				       (let ((args (args->proper-list (cadr tree))))
					 (if (pair? args) args (quit)))))
			   (new-body (let ((new-vars (append lvars vars)))
				       (map (lambda (p) (reduce-walker p new-vars)) (cddr tree))))
			   (new-args (if (symbol? (cadr tree))
					 (cadar lvars)
					 (if (proper-list? (cadr tree))
					     (map cadr lvars)
					     (let ((lst (map cadr lvars)))
					       (append (copy lst (make-list (- (length lst) 1)))
						       (last-ref lst)))))))
		      (cons 'lambda (cons new-args new-body))))

		   ((case)
		    (if (not (and (len>1? (cdr tree))
				  (pair? (caddr tree))))
			(quit))
		    (list 'case
			  (reduce-walker (cadr tree) vars)
			  (map (lambda (c)
				 (if (not (len>1? c))
				     (quit))
				 (cons (car c)
				       (map (lambda (p) (reduce-walker p vars)) (cdr c))))
			       (cddr tree))))

		   ((letrec letrec*)
		    (if (not (pair? (cdr tree))) (quit))
		    (let ((locals (cadr tree))
			  (body (cddr tree))
			  (lvars ()))
		      (if (not (and (list? locals) (pair? body))) (quit))
		      (for-each (lambda (local)
				  (if (not (len>1? local))
				      (quit))
				  (set! lvars (cons (list (car local)
							  (symbol "<L" (number->string local-ctr) ">")
							  local-ctr ())
						    lvars))
				  (set! local-ctr (+ local-ctr 1)))
				locals)
		      (for-each (lambda (local lv)
				  (list-set! lv 3 (reduce-walker (cadr local) lvars)))
				locals lvars)
		      (cons (car tree)
			    (cons (map (lambda (v) (list (cadr v) (cadddr v))) lvars)
				  (reduce-walker body (append lvars vars))))))

		   ((lambda*)
		    (if (not (and (proper-pair? (cdr tree))
				  (or (symbol? (cadr tree))
				      (proper-list? (cadr tree)))))
			(quit))
		    (let ((old-args (args->proper-list (cadr tree))))
		      (if (or (not (pair? old-args))
			      (just-keywords? old-args))  ; (:allow-other-keys)
			  (quit))
		      (let* ((lvars (map (lambda (a)
					   (if (memq a '(:rest :allow-other-keys))
					       (values)
					       (let ((res (list (if (pair? a) (car a) a)
								(symbol "<A" (number->string local-ctr) ">") local-ctr)))
						 (set! local-ctr (+ local-ctr 1))
						 res)))
					 old-args))
			     (new-body (let ((new-vars (append lvars vars)))
					 (map (lambda (p) (reduce-walker p new-vars)) (cddr tree))))
			     (new-args (if (symbol? (cadr tree))
					   (cadar lvars)
					   (map (lambda (a)
						  (cond ((keyword? a) a)
							((symbol? a) (cadr (assq a lvars)))
							((len>1? a)
							 (list (assq a lvars) (cadr a)))
							(else (quit))))
						(cadr tree)))))
			(cons 'lambda* (cons new-args new-body)))))

		   (else ; still (pair? tree) but (car tree) not hit above
		    (cons (cond ((pair? (car tree))
				 (reduce-walker (car tree) vars))
				((assq (car tree) vars) => reduce-v)
				(else (car tree)))
			  (if (pair? (cdr tree))
			      (map (lambda (p)
				     (reduce-walker p vars))
				   (cdr tree))
			      (cdr tree))))))

		;; (pair? tree) far far above

		((or (not (symbol? tree))
		     (keyword? tree))
		 tree)

		((assq tree vars) => reduce-v) ; replace in-tree symbol with its reduction (this includes any outer-var once set below)

		(fuvar (quit))

		(else
		 (set-outer outer-vars tree))))

	(lambda (new-form leaves env fvar orig-form)
	  (unless (tree-set-memq '(define define*
				    ;; these propagate backwards and we're not returning the new env in this loop,
				    ;; lvars can be null, so splicing a new local into vars is a mess,
				    ;; but if the defined name is not reduced, it can occur later as itself (not via car),
				    ;; so without lots of effort (a dummy var if null lvars, etc), we can only handle
				    ;; functions within a function (fvar not #f).
				    ;; but adding that possibility got no hits
				    list-values apply-values append quasiquote unquote
				    define-constant define-macro define-macro* define-expansion
				    define-syntax let-syntax letrec-syntax match syntax-rules
				    require import module cond-expand reader-cond while case-lambda
				    call-with-values let-values define-values let*-values multiple-value-bind)
				 new-form)
	    (call-with-exit
	     (lambda (q)
	       (set! quit q)
	       (set! fuvar fvar)
	       (set! outer-vars (if fvar
				    (do ((e (list (list (var-name fvar) '<F> 0 ())))
					 (i 1 (+ i 1))
					 (args (args->proper-list (var-arglist fvar)) (cdr args)))
					((not (pair? args)) e)
				      (set! e (cons (list (car args) (symbol "<" (number->string i) ">") i ()) e)))
				    (list (list () '<1>) (list () '<2>) (list () '<3>) (list () '<4>)))) ; avoid cyclic-tree check in copy+:readable
	       (set! local-ctr 0)
	       (set! line (pair-line-number orig-form))
	       (set! reduced-form (reduce-walker new-form outer-vars))
	       (unless line
		 (set! line (let search ((tree orig-form))
			      (and (pair? tree)
				   (or (pair-line-number tree)
				       (search (car tree))
				       (search (cdr tree))))))
		 (if (not line) (set! line 0)))

	       (set! leaves (tree-leaves reduced-form))        ; if->when, for example, so tree length might change
	       (if (and (<= *fragment-min-size* leaves)
			(< leaves *fragment-max-size*))
		   (hash-fragment reduced-form leaves env fvar orig-form line outer-vars))
	       (when fvar (quit))

	       (unless (and (pair? lint-function-body)
			    (equal? new-form (car lint-function-body)))
		 (let ((fvars (let ((fcase (and (< leaves *fragment-max-size*)
						(vector-ref fragments leaves)
						(hash-table-ref (vector-ref fragments leaves) (list reduced-form)))))
				(and (vector? fcase)
				     (vector-ref fcase 2)))))
		   (when (pair? fvars)
		     (call-with-exit
		      (lambda (ok)
			(for-each (lambda (fv)
				    (when (var-member (var-name fv) env)
				      (out-format outport "~NCperhaps ~A -> (~A~{ ~A~})~%" lint-left-margin #\space
					      (truncated-list->string new-form)
					      (var-name fv)
					      (map (lambda (a) (case (car a) ((()) (values)) (else))) outer-vars))
				      (ok)))
				  fvars)
			(out-format outport "~NCif '~A were in scope, perhaps ~A -> (~A~{ ~A~})~%" lint-left-margin #\space
				(var-name (car fvars))
				(truncated-list->string new-form)
				(var-name (car fvars))
				(map (lambda (a) (case (car a) ((()) (values)) (else))) outer-vars)))))))))))))

    (define (lint-fragment form env)
      (let ((leaves (tree-leaves form)))
	(when (< *fragment-min-size* leaves *fragment-max-size*)
	  (reduce-tree form leaves env #f form))))

    (define (reduce-function-tree fvar env)
      (let ((definition (cond ((var-initial-value fvar) => cddr))))
	(when (pair? definition)
	  (let* ((form (if (and (string? (car definition))
				(pair? (cdr definition)))
			   (cdr definition)
			   definition))
		 (leaves (tree-leaves form)))
	    (when (< *fragment-min-size* leaves *fragment-max-size*)
	      (reduce-tree form leaves env (and (not (keyword? (var-name fvar))) fvar) form))))))
    ;; ----------------------------------------


    (denote lint-walk-pair
      (let ()
	(denote (walk-rest caller form env)
	  (let ((vars env))
	    (for-each
	     (lambda (f)
	       (set! vars (lint-walk caller f vars)))
	     form))
	  env)

	;; -------- walk head=symbol --------
	(denote walk-symbol
	  (letrec ((unsafe-makers '(sublet inlet copy cons list append subvector vector hash-table
				    make-hash-table make-hook list-values append gentemp or and not))

		   (equal-ignoring-constants?
		    (lambda (a b)
		      (or (equivalent? a b)
			  (and (symbol? a)
			       (constant? a)
			       (equivalent? (symbol->value a) b))
			  (and (symbol? b)
			       (constant? b)
			       (equivalent? (symbol->value b) a))
			  (and (pair? a)
			       (pair? b)
			       (equal-ignoring-constants? (car a) (car b))
			       (equal-ignoring-constants? (cdr a) (cdr b))))))
		   (constable? (lambda (cp)
				 (and (len>1? cp)
				      (memq (car cp) '(list vector int-vector float-vector byte-vector))
				      (lint-every? (lambda (inp)
						     (and (or (not (symbol? inp)) ; leave (list pi *stderr*) unrewritten
							      (keyword? inp))
							  (or (code-constant? inp)
							      (constable? inp))))
						   (cdr cp))))))
	    (lambda (caller head form env)
	      (let ((v (var-member head env)))
		(when (and v (not (memq form (var-history v))))
		  (set! (var-history v) (cons form (var-history v)))
		  (set! (var-refenv v) env))
		(check-call caller head form env)

		;; look for one huge argument leaving lonely trailing arguments somewhere off the screen
		;;   (it needs to be one arg, not a call on values)
		(let ((branches (length form)))

		  (when (and (= branches 2)
			     (any-procedure? head v)
			     (not (eq? head 'unquote)))
		    (let ((arg (cadr form)))
		      ;; begin=(car arg) happens very rarely
		      (when (len>2? arg)
			(when (and (memq (car arg) '(let let*))
				   (list? (cadr arg))
				   (not (or (null? (cddr arg))
					    (and (pair? (cddr arg))
						 (pair? (caddr arg))
						 (eq? 'lambda (caaddr arg)))
					    (assq head (cadr arg)))))
			  (let* ((body (cddr arg))
				 (len (- (length body) 1)))
			    (when (>= len 0)
			      ;; (string->symbol (let ((s (copy vstr))) (set! (s (+ pos 1)) #\s) s)) ->
			      ;;    (let ((s (copy vstr))) (set! (s (+ pos 1)) #\s) (string->symbol s))")
			      (lint-format "perhaps~%~NC~A ->~%~NC~A" caller
					   (+ lint-left-margin 4) #\space
					   (truncated-list->string form)
					   (+ lint-left-margin 4) #\space
					   (let ((str (object->string `(,(car arg) ,(cadr arg)
									,@(copy body (make-list len))
									(,head ,(list-ref body len))))))
					     (if (<= (length str) target-line-length)
						 str
						 (format #f "(~A ... (~A ~A))"
							 (car arg) head
							 (truncated-list->string (list-ref body len)))))))))
			(when (and (eq? (car arg) 'or)
				   (proper-list? arg))
			  (let ((else-clause (let ((last-clause (last-ref arg)))
					       (if (and (pair? last-clause)
							(memq (car last-clause) '(error throw)))
						   last-clause
						   (if (or (not (code-constant? last-clause))
							   (side-effect? (list head last-clause) env))
						       :checked-eval-error
						       (let ((res (checked-eval (list head last-clause))))
							 (if (or (and (symbol? res)
								      (not (eq? res :checked-eval-error)))
								 (pair? res))
							     (list 'quote res)
							     res)))))))
			    (unless (eq? else-clause :checked-eval-error)
			      (set! last-rewritten-internal-define form)
			      ;; (string->number (or (f x) "4")) -> (cond ((f x) => string->number) (else 4))
			      (lint-format "perhaps ~A" caller
					   (lists->string form
							  `(cond (,(if (or (null? (cddr arg))
									   (null? (cdddr arg)))
								       (cadr arg)
								       (copy arg (make-list (- (length arg) 1))))
								  => ,head)
								 (else ,else-clause))))))))))
		  (unless (or (<= branches 2)
			      (any-macro? head env)
			      (memq head '(for-each map list-values * + - /)))
		    (let ((leaves (tree-leaves form)))
		      (when (> leaves (max *report-bloated-arg* (* branches 3)))
			(do ((p (cdr form) (cdr p))
			     (i 1 (+ i 1)))
			    ((or (not (pair? p)) ; end of arg list
				 (null? (cdr p))
				 (and (pair? (car p))
				      (symbol? (caar p))
				      (not (memq (caar p) '(lambda quote call/cc list vector match-lambda values)))
				      (> (tree-leaves (car p)) (- leaves (* branches 2)))
				      (or (not (memq head '(or and)))
					  (= i 1))
				      (not (tree-memq 'values (car p)))
				      (let ((header (copy form (make-list i)))
					    (trailer (copy form (make-list (- branches i 1)) (+ i 1)))
					    (disclaimer (if (or (any-procedure? head v)
								(hash-table-ref no-side-effect-functions head))
							    ""
							    (format #f ", assuming ~A is not a macro," head))))
					;; begin=(caar p) here is almost entirely as macro arg
					;; (apply env-channel (make-env ...) args) -> (let ((<1> (make-env ...))) (apply env-channel <1> args))
					(lint-format "perhaps~A~%~NC~A ->~%~NC~A" caller
						     disclaimer
						     (+ lint-left-margin 4) #\space
						     (lint-pp (append header (cons (one-call-and-dots (car p)) trailer)))
						     (+ lint-left-margin 4) #\space
						     (if (and (memq (caar p) '(let let*))
							      (list? (cadar p))
							      (not (assq head (cadar p)))) ; actually not intersection header+trailer (map car cadr)
							 (let ((last (last-ref (cddar p))))
							   (if (< (tree-leaves last) 12)
							       (format #f "(~A ... ~A)"
								       (caar p)
								       (lint-pp (append header (cons last trailer))))
							       (lint-pp `(let ((<1> ,(one-call-and-dots (car p))))
									   (,@header <1> ,@trailer)))))
							 (lint-pp `(let ((<1> ,(one-call-and-dots (car p))))
								     (,@header <1> ,@trailer)))))
					#t)))))))))

		(when (pair? form)
		  ;; save any references to vars in their var-history (type checked later)
		  ;;   this can be fooled by macros, as everywhere else
		  (for-each (lambda (arg)
			      (when (symbol? arg)
				(let ((v (var-member arg env)))
				  (when (and v (not (memq form (var-history v))))
				    (set! (var-history v) (cons form (var-history v)))
				    (set! (var-refenv v) env)))))
			    form)

		  (if (lint-set!? form env)
		      (set-set (cadr form) caller form env)))

		(if (var? v)
		    (if (memq (var-ftype v) '(define lambda define* lambda*))
			(update-scope v caller env))
		    (begin
		      (cond ((hash-table-ref special-case-functions head)
			     => (lambda (f)
				  (f caller head form env))))

		      ;; change (list ...) to '(...) if it's safe as a constant list
		      ;;   and (vector ...) -> #(...)
		      (if (and (pair? (cdr form))
			       (hash-table-ref no-side-effect-functions head)
			       (not (memq head unsafe-makers)))
			  (for-each (lambda (p)
				      (when (constable? p)
					(let ((pval (eval/error caller p)))
					  (if (not (eq? pval :error))
					      (lint-format "perhaps ~A -> ~A~A" caller
							   (truncated-list->string p)
							   (if (eq? (car p) 'list) "'" "")
							   (object->string pval))))))
				    (cdr form)))

		      (when (and (not (= line-number last-simplify-numeric-line-number))
				 (hash-table-ref numeric-ops head)
				 (proper-tree? form))
			;; head always is (car form) here
			(let ((val (simplify-numerics form env)))
			  (unless (equal-ignoring-constants? form val)
			    (set! last-simplify-numeric-line-number line-number)
			    ;; (+ 1 2) -> 3, and many others
			    (lint-format "perhaps ~A" caller (lists->string form val)))))

		      ;; if a var is used before it is defined, the var history and ref/set
		      ;;   info needs to be saved until the definition, so other-identifiers collects it
		      (unless (defined? head (rootlet))
			(hash-table-set! other-identifiers head
					 (cons form (or (hash-table-ref other-identifiers head) ()))))))

		;; (f ... (if A B C) (if A D E) ...) -> (f ... (if A (values B D) (values C E)) ...)
		;;    these happen up to almost any number of clauses
		;;    need true+false in every case, and need to be contiguous
		;;    case/cond happen here, but very rarely in a way we can combine via values

		(unless (any-macro? head env) ; actually most macros are safe here...
		  (do ((p (cdr form) (cdr p)))
		      ((or (not (pair? p))
			   (and (len>2? (car p))
				(eq? (caar p) 'if)
				(pair? (cdddar p))))
		       (when (pair? p)
			 (do ((test (cadar p))
			      (q (cdr p) (cdr q)))
			     ((not (and (pair? q)
					(let ((x (car q)))
					  (and (len>2? x)
					       (eq? (car x) 'if)
					       (equal? (cadr x) test)
					       (pair? (cdddr x))))))
			      (unless (eq? q (cdr p))
				(let ((header (do ((i 1 (+ i 1))
						   (r (cdr form) (cdr r)))
						  ((eq? r p)
						   (copy form (make-list i)))))
				      (middle (do ((r p (cdr r))
						   (trues ())
						   (falses ()))
						  ((eq? r q)
						   (list 'if test
							 (cons 'values (reverse trues))
							 (cons 'values (reverse falses))))
						(set! trues (cons (caddar r) trues))
						(set! falses (cons (car (cdddar r)) falses)))))
				  ;; (+ (if A B C) (if A C D) y) -> (+ (if A (values B C) (values C D)) y)
				  (lint-format "perhaps~A ~A" caller
					       (if (side-effect? test env)
						   (format #f " (ignoring ~S's possible side-effects)" test)
						   "")
					       (lists->string form (append header (cons middle q)))))))))))))
	      (walk-rest caller form env))))

	;; -------- walk head=pair --------
	(denote (walk-pair caller head form env)
	  (cond ((eq? (car head) 'list)
		 (lint-format "perhaps use vector here: ~A" caller (truncated-list->string form)))

		((eq? (car head) 'if)
		 (let ((if-len (length head)))
		   (if (= if-len 3)                     ; ((if y +) 3 4)
		       (lint-format "~S better not be #f here: ~A" caller (cadr head) (truncated-list->string form))

		       (if (= if-len 4)                 ; ((if y + -) 2 3), if-len might be anything
			   (let ((true (caddr head))
				 (false (cadddr head))
				 (num-args (length (cdr form))))

			     (if (and (symbol? true)
				      (symbol? false))
				 (when (and (defined? true)
					    (defined? false))
				   (let ((op1 (symbol->value true))
					 (op2 (symbol->value false)))

				     (unless (aritable? op1 num-args)
				       (lint-format "~S in ~S can't be called with ~D argument~P: ~A"
						    caller true head num-args num-args (truncated-list->string form)))
				     (unless (aritable? op2 num-args)
				       (lint-format "~S in ~S can't be called with ~D argument~P: ~A"
						    caller false head num-args num-args (truncated-list->string form)))

				     (let ((sig1 (signature op1))
					   (sig2 (signature op2))
					   (data (var-member head env)))
				       (when (pair? sig1)
					 (check-args caller form data form (cdr sig1) env num-args))
				       (when (pair? sig2)
					 (check-args caller form data form (cdr sig2) env num-args)))))

				 (unless (or (pair? true) (pair? false))

				   (unless (aritable? true num-args)
				     (lint-format "~S in ~S can't be called with ~D argument~P: ~A"
						  caller true head num-args num-args (truncated-list->string form)))
				   (unless (aritable? false num-args)
				     (lint-format "~S in ~S can't be called with ~D argument~P: ~A"
						  caller false head num-args num-args (truncated-list->string form))))))))))

		((not (and (pair? (cdr head))
			   (memq (car head) '(lambda lambda*)))))
		;; just ((lambda|lambda* ...)...) from here down

		((and (identity? head)
		      (pair? (cdr form))) ; identity needs an argument
		 ;; ((lambda (x) x) 32) -> 32
		 (lint-format "perhaps ~A" caller (truncated-lists->string form (cadr form))))

		((and (symbol? (cadr head)) ; ((lambda x x) 1 2 3) -> (list 1 2 3)
		      (len=1? (cddr head))
		      (eq? (cadr head) (caddr head)))
		 (lint-format "perhaps ~A" caller
			      (lists->string form (cons 'list (cdr form)))))

		((and (null? (cadr head))
		      (pair? (cddr head)))
		 ;; ((lambda () 32) 0) -> 32
		 (lint-format "perhaps ~A" caller
			      (truncated-lists->string
			       form
			       (if (and (null? (cdddr head))
					(not (and (pair? (caddr head))
						  (memq (caaddr head) '(define define* define-constant define-macro define-macro*)))))
				   (caddr head)
				   (cons 'let (cons () (cddr head)))))))

		((and (proper-pair? (cddr head)) ; ((lambda (...) ...) ...) -> (let ...) -- lambda here is ugly and slow
		      (not (lint-any? (lambda (a) (mv-range a env)) (cdr form))))
		 (call-with-exit
		  (lambda (quit)          ; uncountably many things can go wrong with the lambda form
		    (let ((vars ())
			  (vals ()))
		      (do ((v (cadr head) (cdr v))
			   (a (cdr form) (cdr a)))
			  ((not (and (pair? a)
				     (pair? v)))
			   (cond ((symbol? v)
				  (set! vars (cons v vars))
				  (set! vals (cons (cons 'list a) vals)))

				 ((or (not (list? v))
				      (and (null? a)
					   (pair? v)
					   (eq? (car head) 'lambda))) ; too many args
				  (quit))

				 (else
				  (for-each (lambda (p)
					      (if (pair? p)
						  (begin
						    (if (not (pair? (cdr p))) (quit))
						    (set! vars (cons (car p) vars))
						    (set! vals (cons (cadr p) vals)))
						  (begin
						    (set! vars (cons p vars))
						    (set! vals (cons #f vals)))))
					    v))))
			(set! vars (cons ((if (pair? (car v)) caar car) v) vars))
			(let ((val (car a)))
			  (when (and (pair? val)
				     (symbol? (car val)))
			    (let ((sig (arg-signature (car val) env)))
			      (when (and (pair? sig)
					 (eq? (car sig) 'values)) ; don't try to rewrite as let if values in play
				(quit))))
			  (set! vals (cons val vals))))
		      ;; ((lambda* (a b) (+ a b)) 1) -> (let ((a 1) (b #f)) (+ a b))
		      (lint-format "perhaps ~A" caller
				   (lists->string form
						  (cons (if (or (eq? (car head) 'lambda)
								(not (pair? (cadr head)))
								(null? (cdadr head)))
							    'let 'let*)
							(cons (map list (reverse vars) (reverse vals))
							      (cddr head))))))))))
	  (walk-rest caller form env))

	;; -------- walk head=quasiquote (aimed at ,@x primarily) --------
	(denote walk-qq
	  (let ((qq-form #f))

	    (define (safe-av? p)
	      (and (pair? p)
		   (eq? (car p) 'apply-values)
		   (not (tree-set-memq '(apply-values list-values append unquote) (cdr p)))))

	    (lambda (caller head form env)
	      (for-each (lambda (p)
			  (let ((sym (and (symbol? p) p)))
			    (cond ((not sym)
				   #f)
				  ((var-member sym env)
				   (set-ref sym caller form env))
				  ((not (defined? sym (rootlet)))
				   (hash-table-set! other-identifiers sym
						    (cons form (or (hash-table-ref other-identifiers sym) ())))))))
			(cdr form))

	      (let ((old-current-form lint-current-form))
		;; maybe put these on a switch -- some days I think they're good, and others...
		(unless (and qq-form
			     (eq? lint-current-form qq-form))
		  (if (not lint-current-form) (set! lint-current-form form))
		  (set! qq-form lint-current-form) ; only interested in simplest cases here
		  (if (eq? head 'append)                   ; `(f . g) -> (cons f g) ignoring quotes etc
		      (if (and (= (length form) 3)          ; `(f . (g . h)) -> (cons f (cons g h))
			       (pair? (cadr form))
			       (eq? (caadr form) 'list-values)
			       (not (tree-set-memq '(apply-values append unquote) (cdr form))))
			  (let ((lst (unlist-values (cadr form)))
				(rest (caddr form)))
			    (if (pair? rest) (set! rest (unlist-values rest)))
			    (let ((lst-len (length lst)))
			      (case lst-len
				((2) (lint-format "perhaps ~A" caller (lists->string form `(cons ,(cadr lst) ,rest))))
				((3) (lint-format "perhaps ~A" caller
						  (lists->string form
								 `(cons ,(cadr lst)
									(cons ,(caddr lst) ,rest)))))))))

		      (when (eq? head 'list-values)
			(case (length form)
			  ((1) #f)  ; this never happens
			  ((2)
			   (let ((arg1 (cadr form)))
			     (cond ((and (pair? arg1)
					 (eq? (car arg1) 'apply-values))  ; `(,@x) -> (copy x)
				    (if (not (pair? (cdr arg1)))
					(lint-format "apply-values needs an argument: ~A" caller form)
					(if (not (qq-tree? (cadr arg1)))
					    (lint-format "perhaps ~A" caller
							 (lists->string form
									(unlist-values (if (pair? (cadr arg1))
											   (cadr arg1)
											   (list 'copy (cadr arg1)))))))))
				   ((or (symbol? arg1)
					(quoted-symbol? arg1))
				    (lint-format "perhaps ~A" caller      ; `(,x) -> (list x)
						 (lists->string form (list 'list arg1))))

				   ((and (pair? arg1)                     ; `((a ,b)) -> (list (list 'a b))
					 (not (tree-set-memq '(apply-values unquote) arg1)))
				    (lint-format "perhaps ~A" caller
						 ((if (< (tree-leaves form) 50) lists->string truncated-lists->string)
						  form
						  (unlist-values form)))))))

			  ((3)
			   (let ((arg1 (cadr form))
				 (arg2 (caddr form)))

			     (if (and (len=2? arg1)           ; `(begin (abs ,x)) -> `(abs ,x), this is a separate issue from the rewrites below
				      (eq? (car arg1) 'quote)
				      (eq? (cadr arg1) 'begin)
				      (not (and (pair? arg2)
						(eq? (car arg2) 'apply-values)))) ; no other way to splice here, I hope
				 (lint-format "pointless begin: ~A" caller
					      (lists->string form (caddr form))))

			     (cond ((and (len=1? arg2)
					 (eq? (car arg2) 'apply-values))
				    (lint-format "apply-values takes one argument: ~A" caller form))

				   ((not (or (and (pair? arg1)
						  (tree-set-memq '(apply-values append unquote) arg1))
					     (and (pair? arg2)
						  (or (tree-set-memq '(append unquote) arg2)
						      (tree-set-memq '(list-values apply-values) (cdr arg2))))))
				    (lint-format "perhaps ~A" caller        ; `(f ,(map g x)) -> (list 'f (map g x))
						 (lists->string form        ; `(f ,@(map g x)) -> (cons 'f (map g x))
								(if (pair? arg2)
								    (case (car arg2)
								      ((apply-values)
								       (list 'cons (unlist-values arg1) (cadr arg2)))
								      ((list-values)
								       (list 'list (unlist-values arg1) (cons 'list (cdr arg2))))
								      (else
								       (list 'list (unlist-values arg1) arg2)))
								    (list 'list (unlist-values arg1) arg2)))))
				   ((and (len=2? arg1)
					 (eq? (car arg1) 'apply-values)
					 (not (qq-tree? (cadr arg1))))
				    (if (and (len=2? arg2)
					     (not (qq-tree? (cadr arg2)))
					     (eq? (car arg2) 'apply-values))  ; `(,@x ,@y) -> (append x y)
					(lint-format "perhaps ~A" caller
						     (lists->string form
								    (list 'append
									  (unlist-values (cadr arg1))
									  (unlist-values (cadr arg2)))))
					(if (not (and (pair? arg2)
						      (tree-set-memq '(apply-values append unquote) arg2)))
					    (lint-format "perhaps ~A" caller     ; `(,@x ,y) -> (append x (list y))
							 (lists->string form
									(list 'append
									      (unlist-values (cadr arg1))
									      (list 'list (unlist-values arg2))))))))

				   ((and (len=3? arg1)                        ; `((a . b) (c . d)) -> (list (cons a b) (cons c d))
					 (eq? (car arg1) 'append)            ; `((a . (b . c))...) -> (list (cons a (cons b c)) ...)
					 (pair? (cadr arg1))
					 (eq? (caadr arg1) 'list-values) ; was memq+list
					 (len=3? arg2)
					 (eq? (car arg2) 'append)
					 (pair? (cadr arg2))
					 (eq? (caadr arg2) 'list-values)) ; same
				    (let ((ca1 (cadr arg1))
					  (ca2 (cadr arg2)))
				      (let ((len1 (length ca1))
					    (len2 (length ca2)))
					(let ((pa1 (case len1
						     ((2) (list 'cons (cadr ca1) (caddr arg1)))
						     ((3) (list 'cons (cadr ca1) (list 'cons (caddr ca1) (caddr arg1))))
						     (else 'error)))
					      (pa2 (case len2
						     ((2) (list 'cons (cadr ca2) (caddr arg2)))
						     ((3) (list 'cons (cadr ca2) (list 'cons (caddr ca2) (caddr arg2))))
						     (else 'error))))
					  (if (and (pair? pa1)
						   (pair? pa2))
					      (lint-format "perhaps ~A" caller
							   (lists->string form (list 'list pa1 pa2))))))))

				   ((not (tree-set-memq '(apply-values unquote) (cdr form)))
				    (lint-format "perhaps ~A" caller
						 ((if (< (tree-leaves form) 100) lists->string truncated-lists->string)
						  form
						  (unlist-values form)))))))

			  (else                  ; checked already that form is a proper-list, so the length here is > 3
			   (let ((args (cdr form)))      ; car is list-values
			     (cond ((lint-every? (lambda (p)              ; `((f . ,a) (g . ,b)...) -> (list (cons f a) (cons g b) ...)
						   (and (pair? p)         ;     from (append (list x) y) -> (cons x y)
							(eq? (car p) 'append)
							(len=2? (cdr p))
							(len=2? (cadr p))
							(eq? (caadr p) 'list-values)))
						 args)
				    (lint-format "perhaps ~A" caller
						 (truncated-lists->string form
									  `(list (cons ,(cadadr (car args)) ,(caddar args))
										 (cons ,(cadadr (cadr args)) ,(caddr (cadr args)))
										 ...))))

				   ((lint-any? (lambda (p)
						 (and (len=1? p)
						      (eq? (car p) 'apply-values)))
					       args)
				    (lint-format "apply-values needs an argument: ~A" caller form))

				   ((not (lint-every? safe-av? (cddr args)))
				    (if (and (len=3? args)
					     (safe-av? (car args))   ; `(,@x ,@y ,z) -> (append x y (list z)) etc
					     (safe-av? (cadr args))
					     (not (and (pair? (caddr args))
						       (memq (caaddr args) '(apply-values append unquote)))))
					(lint-format "perhaps ~A" caller
						     (lists->string form
								    (list 'append (cadar args) (cadadr args)
									  (list 'list (unlist-values (caddr args))))))))

				   ;; `(+ ,y ,@(map f x)) -> (cons '+ (cons y (map f x)))
				   ;; `(+ ,y ,@x ,@z etc) -> (cons '+ (cons y (append x z ...)))
				   ;; `(f ,@x ,@y etc) -> (cons 'f (append x y ...))
				   ;; `(,@x ,@y etc) -> (append x y ...)
				   ((safe-av? (cadr args))
				    (if (safe-av? (car args))
					(lint-format "perhaps ~A" caller
						     (lists->string form
								    (cons 'append (map cadr args))))
					(if (not (tree-set-memq '(apply-values append unquote) (car args)))
					    (lint-format "perhaps ~A" caller
							 (lists->string form
									`(cons ,(unlist-values (car args)) (append ,@(map cadr (cdr args)))))))))

				   ((not (or (tree-set-memq '(apply-values append unquote) (car args))
					     (tree-set-memq '(apply-values append unquote) (cadr args))))
				    (lint-format "perhaps ~A" caller
						 (lists->string form
								`(cons ,(unlist-values (car args))
								       (cons ,(unlist-values (cadr args))
									     ,(if (null? (cdddr args))
										  (cadr (caddr args))
										  (cons 'append (map cadr (cddr args)))))))))
				   ((and (len=3? args)
					 (safe-av? (car args))   ; `(,@x ,y ,@z) -> (append x (cons y z))
					 (not (tree-set-memq '(apply-values append unquote) (cadr args))))
				    (lint-format "perhaps ~A" caller
						 (lists->string form
								(list 'append (cadar args)
								      (list 'cons (unlist-values (cadr args)) (cadr (caddr args))))))))))))))

		(let ((e (walk-rest caller form env)))
		  (set! lint-current-form old-current-form)
		  e)))))

	;; -------- lint-walk-pair --------
	(lambda (caller form env)
	  (let ((head (car form)))
	    (set! line-number (or (pair-line-number form) (max line-number 0)))

	    (if *report-repeated-code-fragments*
		(lint-fragment form env))

	    ;; (error...) as arg happens very rarely (a half-dozen hits, one: (values (error...))!

	    (cond
	     ((hash-table-ref walker-functions head)
	      => (lambda (walker)
		   (walker caller form env)))

	     ((not (proper-list? form))
	      ;; these appear to be primarily macro/match arguments or stuff like (+ . 1)
	      ;;   other cases (not list) have already been dealt with above
	      (if (and (pair? form)
		       (symbol? head)
		       (or (procedure? (symbol->value head *e*))
			   (memq head '(or and))))
		  (lint-format "unexpected dot: ~A" caller (truncated-list->string form)))
	      env)

	     ((and *report-quasiquote-rewrites*
		   (memq head '(list-values apply-values)))
	      (walk-qq caller head form env))

	     ((symbol? head)
	      (walk-symbol caller head form env))

	     ((pair? head)
	      (walk-pair caller head form env))

	     (else (walk-rest caller form env)))))))

    ;; -------- lint-walk --------
    (denote (lint-walk caller form env)
      (cond ((symbol? form)
	     ;(set-ref form caller #f env) ; returns env
	     ;         name caller form env)
	     (let ((data (var-member form env)))
	       (if data
		   (begin
		     (set! (var-ref data) (+ (var-ref data) 1))
		     (update-scope data caller env))
		   (if (not (defined? form (rootlet)))
		       (let ((old (hash-table-ref other-identifiers form)))
			 (check-for-bad-variable-name caller form)
#|
			 (when *report-undefined-identifiers*
			   ;; this is easily fooled by macros (including ,form = gensym), defines by a different name (denote),
			   ;;   and functions from elsewhere that lint can't see
			   (cond ((assq form recent-names) =>
				  (lambda (data)
				    (lint-format "~S might be undefined, but it was defined recently (via a ~S broadly speaking) to be ~S"
						 caller form (cadr data) (caddr data))))))
|#
			 (hash-table-set! other-identifiers form (cons #f (or old ()))))))
	       env))

	    ((pair? form)
	     (lint-walk-pair caller form env))

	    ((string? form)
	     (let ((len (length form)))
	       (when (and (> len 8)               ; "*****************************" -> (format #f "~NC" 29 #\*)
			  (string=? form (make-string len (string-ref form 0))))
		 (lint-format "perhaps ~S -> ~A" caller form `(format #f "~NC" ,len ,(string-ref form 0)))))
	     env)

	    ((vector? form)
	     (unless (or (int-vector? form)
			 (byte-vector? form)
			 (float-vector? form))
	       (let ((len (length form)))
		 (when (positive? len)
		   (if (integer? (form 0))
		       (do ((i 1 (+ i 1)))
			   ((or (= i len)
				(not (integer? (vector-ref form i))))
			    (if (= i len)
				(lint-format "~A could be ~A" caller
					     (let-temporarily (((*s7* 'print-length) 8))
					       (values (object->string form)
						       (object->string (copy form (make-int-vector len)))))))))
		       (if (float? (form 0))
			   (do ((i 1 (+ i 1)))
			       ((or (= i len)
				    (not (float? (vector-ref form i))))
				(if (= i len)
				    (lint-format "~A could be ~A" caller
						 (let-temporarily (((*s7* 'print-length) 8))
						   (values (object->string form)
							   (object->string (copy form (make-float-vector len)))))))))))
		   (let ((happy #t))
		     (for-each
		      (lambda (x)
			(when (and (pair? x)
				   (eq? (car x) 'unquote))
			  (lint-walk caller (cadr x) env) ; register refs
			  (set! happy #f)))
		      form)
		     ;; (begin (define x 1) `#(,x))
		     (if (not happy)
			 (lint-format "quasiquoted vectors are not supported: ~A~%~NCperhaps use `(vector ...) rather than `#(...)" caller
				      (truncated-list->string form)
				      (+ lint-left-margin 4) #\space))))))
	           ;; `(x #(,x)) for example will not work in s7, but `(,x ,(vector x)) will
	     env)

	    (else
	     env)))


    ;; -------- lint-file --------
    (define *report-input* #t)
    ;; lint-file is called via load etc above and it's a pain to thread this variable all the way down the call chain

    (define (lint-file-1 file env)
      (set! linted-files (cons file linted-files))
      (let ((fp (if (input-port? file)
		    file
		    (begin
		      (set! *current-file* file)
		      (catch #t
			(lambda ()
			  (let ((p (open-input-file file)))
			    (when *report-input*
			      (out-format outport
				      (if (and (output-port? outport)
					       (not (member outport (list *stderr* *stdout*))))
					  (values "~%~NC~%;~A~%" (+ lint-left-margin 16) #\-)
					  ";~A~%")
				      file))
			    p))
			(lambda (type info)
			  (out-format outport "~NCcan't open ~S: ~A~%" lint-left-margin #\space file (apply format #f info))
			  #f))))))

	(if (not (input-port? fp))
	    env
	    (do ((vars env)
		 (line 0)
		 (last-form #f)
		 (last-line-number -1)
		 (form (read fp) (read fp)))
		((eof-object? form)

		 (if (not (input-port? file))
		     (close-input-port fp))

		 (when (and *report-repeated-code-fragments*
			    (or (not *report-loaded-files*)
				(= lint-left-margin 1)))
		   (let ((reportables ())
			 (size-cutoff (+ *fragment-min-size* 6))
			 (score-cutoff (if (integer? *report-repeated-code-fragments*) *report-repeated-code-fragments* 130)))
		     (do ((i fragmin (+ i 1)))
			 ((> i fragmax))
		       (when (and (vector-ref fragments i)
				  (> (hash-table-entries (vector-ref fragments i)) 0))
			 (for-each (lambda (kv)
				     (let ((vals (cdr kv)))
				       (when (> (vals 0) 1) ; more than 1 use of fragment
					 (let ((score (* i (vals 0) (vals 0))))
					   (when (and (> score score-cutoff)
						      (or (> i size-cutoff)
							  (let ((count 0))
							    (let counter ((tree (car kv)))
							      (if (pair? tree)
								  (begin
								    (counter (car tree))
								    (counter (cdr tree)))
								  (if (memq tree '(<1> <2> <3> <4>))
								      (set! count (+ count 1)))))
							    (> (- i count) *fragment-min-size*))))
					     (vector-set! vals 1 (map (lambda (b) ; line numbers of use points
									(if (< 0 b 100000)
									    b
									    (values)))
								      (reverse (vector-ref vals 1))))
					     (set! reportables (cons (list score i kv) reportables)))))))
				   (vector-ref fragments i))))
		     (let ((reported-lines ())
			   (reported #f)
			   (reports 0))
		       (for-each (lambda (rp)
				   (let ((size (cadr rp))
					 (keyval (caddr rp)))
				     (let ((val (cdr keyval)))
				       (unless (or (>= reports 20)
						   (and (pair? (val 1))
						    (memv (car (val 1)) reported-lines)))
					 (if (pair? (val 1))
					     (set! reported-lines (cons (car (val 1)) reported-lines)))
					 (unless reported
					   (set! reported #t)
					   (out-format outport "~%~NCrepeated code fragments:~%" lint-left-margin #\space))
					 (set! reports (+ reports 1))
					 (out-format outport
						 (if (equal? (val 3) (car keyval))
						     (values "~NCsize: ~A, uses: ~A, lines: '~A:~%~NCexpression: ~A~%"
							     lint-left-margin #\space
							     size (val 0) (val 1)
							     (+ lint-left-margin 2) #\space
							     (let-temporarily ((target-line-length 120))
							       (truncated-list->string (car keyval))))
						     (values "~NCsize: ~A, uses: ~A, lines: '~A:~%~NCpattern: ~A~%~NCexample: ~A~A~%"
							     lint-left-margin #\space
							     size (val 0) (val 1)
							     (+ lint-left-margin 2) #\space
							     (let-temporarily ((target-line-length 120))
							       (truncated-list->string (car keyval)))
							     (+ lint-left-margin 2) #\space
							     (let-temporarily ((target-line-length 120))
							       (truncated-list->string (val 3)))
							     (if (not (and (> size 10)
									   (val 5)))
								 ""
								 (let ((vars (map (lambda (v) (case (car v) ((()) (values)) (else))) (val 5))))
								   (if (pair? vars)
								       (format #f "~%~NCwith var~P: ~{~A ~}"
									       (+ lint-left-margin 4) #\space (length vars) vars)
								       ""))))))))))
				 (sort! reportables                              ; the sort needs to be stable across calls so diff works on the output
					(lambda (kv1 kv2)
					  (or (> (car kv1) (car kv2))            ; first sort by (size * uses * uses)
					      (and (= (car kv1) (car kv2))
						   (let ((a (cdaddr kv1))
							 (b (cdaddr kv2)))
						     (or (> (a 0) (b 0))         ; then sort by size
							 (and (= (a 0) (b 0))    ; then length of (example) original form as a string
							      (or (string<? (or (a 4) (set! (a 4) (object->string (a 3))))
									    (or (b 4) (set! (b 4) (object->string (b 3)))))
								  (and (string=? (a 4) (b 4)) ; finally by reduced form as a string
								       (string<? (object->string (car kv1))
										 (object->string (car kv2))))))))))))))))
		 vars) ; lint-file-1 should return the environment

	      (if (pair? form)
		  (set! line (max line (or (pair-line-number form) 0))))

	      (if (not (or (= last-line-number -1)
			   (side-effect? last-form vars)))
		  (out-format outport "~NCtop-level (line ~D): this has no effect: ~A~%"
			  lint-left-margin #\space last-line-number
			  (truncated-list->string last-form)))
	      (set! last-form form)
	      (set! last-line-number line)

	      (if (and (len>1? form)
		       (hash-table-ref definers-table (car form))    ; set! case is handled elsewhere
		       (not (memq (car form) '(eval eval-string load require)))  ; (eval-string|load (string-append...)) (eval (string->symbol...))
		       (or (pair? (cadr form))
			   (symbol? (cadr form))))
		  (let ((f ((if (pair? (cadr form)) caadr cadr) form)))
		    (if (and (symbol? f)
			     (hash-table-ref built-in-functions f))
			(out-format outport "~NCtop-level ~Aredefinition of built-in function ~A: ~A~%"
				lint-left-margin #\space
				(if (and (pair-line-number form)
					 (> (pair-line-number form) 0))
				    (format #f "(line ~D) " (pair-line-number form))
				    "")
				f (truncated-list->string form)))))

	      (set! vars (lint-walk (if (symbol? form)
					form
					(and (pair? form)
					     (car form)))
				    form
				    vars))))))


    (define (lint-file file env)
      ;; (if (string? file) (format *stderr* "lint ~S~%" file))

      (if (member file linted-files)
	  env
	  (let ((old-current-file *current-file*)
		(old-pp-left-margin pp-left-margin)
		(old-lint-left-margin lint-left-margin)
		(old-load-path *load-path*))

	    (dynamic-wind
		(lambda ()
		  (set! pp-left-margin (+ pp-left-margin 4))
		  (set! lint-left-margin (+ lint-left-margin 4))
		  (when (and (string? file)
			     (char=? (file 0) #\/))
		    (let ((last-pos 0))
		      (do ((pos (char-position #\/ file (+ last-pos 1)) (char-position #\/ file (+ last-pos 1))))
			  ((not pos)
			   (if (> last-pos 0)
			       (set! *load-path* (cons (substring file 0 last-pos) *load-path*))))
			(set! last-pos pos)))))

		(lambda ()
		  (lint-file-1 file env))

		(lambda ()
		  (set! pp-left-margin old-pp-left-margin)
		  (set! lint-left-margin old-lint-left-margin)
		  (set! *current-file* old-current-file)
		  (set! *load-path* old-load-path)
		  (if (positive? (length *current-file*))
		      (newline outport)))))))


    ;;; --------------------------------------------------------------------------------'
    ;;; lint itself
    ;;;
    (let ((+documentation+ "(lint file port) looks for infelicities in file's scheme code")
	  (+signature+ '(#t (string? input-port?) (output-port? null? not) boolean?)) ; not list! we want a list of symbols
	  (readers
	   (list (cons #\e (lambda (str)
			     (unless (string=? str "e")
			       (let ((num (string->number (substring str 1))))
				 (cond ((not num))
				       ((rational? num)
					(out-format outport "~NCthis #e is dumb, #~A -> ~A~%" lint-left-margin #\space str (substring str 1)))
				       ((not (real? num))
					(out-format outport "~NC#e can't handle complex numbers, #~A -> ~A~%" lint-left-margin #\space str num))
				       ((= num (floor num))
					(out-format outport "~NCperhaps #~A -> ~A~%" lint-left-margin #\space str (floor num))))))
			     #f))
		 (cons #\i (lambda (str)
			     (unless (string=? str "i")
			       (let ((num (string->number (substring str 1))))
				 (when num
				   (out-format outport
					   (if (not (rational? num))
					       (values "~NCthis #i is dumb, #~A -> ~A~%" lint-left-margin #\space str (substring str 1))
					       (values "~NCperhaps #~A -> ~A~%" lint-left-margin #\space str (* 1.0 num)))))))
			     #f))
		 (cons #\d (lambda (str)
			     (if (and (not (string=? str "d"))
				      (string->number (substring str 1)))
				 (out-format outport "~NC#d is pointless, #~A -> ~A~%" lint-left-margin #\space str (substring str 1)))
			     #f))

		 (cons #\' (lambda (str)                      ; for Guile (and syntax-rules, I think)
			     (list 'syntax (if (string=? str "'") (read) (string->symbol (substring str 1))))))

		 (cons #\` (lambda (str)                      ; for Guile (sigh)
			     (list 'quasisyntax (if (string=? str "`") (read) (string->symbol (substring str 1))))))

		 (cons #\, (lambda (str)                      ; the same, the last is #,@ -> unsyntax-splicing -- right.
			     (list 'unsyntax (if (string=? str ",") (read) (string->symbol (substring str 1))))))

		 (cons #\& (lambda (str)                      ; ancient Guile code
			     (and (> (length str) 1)
				  (string->keyword (substring str 1)))))

		 (cons #\\ (lambda (str)
			     (cond ((assoc str '(("\\x0"        . #\null)
						 ("\\x7"        . #\alarm)
						 ("\\x8"        . #\backspace)
						 ("\\x9"        . #\tab)
						 ("\\xd"        . #\return)
						 ("\\xa"        . #\newline)
						 ("\\1b"        . #\escape)
						 ("\\x20"       . #\space)
						 ("\\x7f"       . #\delete)))
				    => (lambda (c)
					 (out-format outport "~NC#\\~A is ~W~%" lint-left-margin #\space (substring str 1) (cdr c)))))
			     #f))

		 (cons #\! (lambda (str)
			     (if (member str '("!optional" "!default" "!rest" "!key" "!aux" "!false" "!true" "!r6rs") string-ci=?) ; for MIT-scheme
				 (string->keyword (substring str 1))
				 (if (string=? str "!eof") ; Bigloo? or Chicken? Guile writes it as #<eof> but can't read it
				     (begin
				       (out-format outport "~NC#!eof should probably be #<eof>~%" lint-left-margin #\space)
				       #<eof>)
				     (let ((lc (str 0)))   ; s7 should handle this, but...
				       (do ((c (read-char) (read-char)))
					   ((or (and (eof-object? c)
						     (or (out-format outport "~NCunclosed block comment~%" lint-left-margin #\space)
							 #t))
						(and (char=? lc #\!)
						     (char=? c #\#)))
					    #f)
					 (set! lc c)))))))))
	  (read-hooks
	   ;; try to get past all the # and \ stuff in other Schemes
	   ;;   main remaining problem: [] used as parentheses (Gauche and Chicken for example)
	   (list (lambda (h)
		   (let ((data (h 'data))
			 (line (port-line-number)))
		     (if (not (h 'type))
			 (begin
			   (out-format outport "~NCreader~A: unknown \\ usage: \\~C~%" lint-left-margin #\space (if (zero? line) "" (format #f "[~A]" line)) data)
			   (set! (h 'result) data))
			 (begin
			   (out-format outport "~NCreader~A: unknown # object: #~A~%" lint-left-margin #\space (if (zero? line) "" (format #f "[~A]" line)) data)
			   (set! (h 'result)
				 (catch #t
				   (lambda ()
				     (case (data 0)
				       ((#\;) (read) (values))

				       ((#\T)
					(and (string=? data "T")
					     (out-format outport "#T should be #t~%")
					     #t))

				       ((#\F)
					(and (string=? data "F")
					     (out-format outport "#F should be #f~%")
					     ''#f))

				       ((#\X #\B #\O #\D)
					(let ((num (string->number (substring data 1) (case (data 0) ((#\X) 16) ((#\O) 8) ((#\B) 2) ((#\D) 10)))))
					  (if (number? num)
					      (begin
						(out-format outport "~NCuse #~A~A not #~A~%"
							lint-left-margin #\space
							(char-downcase (data 0)) (substring data 1) data)
						num)
					      (string->symbol data))))

				       ((#\i)
					(out-format outport "#i is used for int-vectors, not numbers.~%")
					(cond ((string->number (substring data 1)) => exact->inexact) (else #f)))

				       ((#\r)
					(out-format outport "#r is used for float-vectors, not numbers.~%")
					#f)

				       ((#\l #\z)
					(let ((num (string->number (substring data 1)))) ; Bigloo (also has #ex #lx #z and on and on)
					  (if (number? num)
					      (begin
						(out-format outport "~NCjust omit this silly #~C!~%" lint-left-margin #\space (data 0))
						num)
					      (string->symbol data))))

				       ((#\u) ; for Bigloo
					(if (string=? data "unspecified")
					    (out-format outport "~NCuse #<unspecified>, not #unspecified~%" lint-left-margin #\space))
					;; #<unspecified> seems to hit the no-values check?
					(string->symbol data))
				       ;; Bigloo also seems to use #" for here-doc concatenation??

				       ((#\v) ; r6rs byte-vectors?
					(if (string=? data "vu8")
					    (out-format outport "~NCuse #u, not #vu8~%" lint-left-margin #\space))
					(string->symbol data))

				       ((#\>) ; for Chicken, apparently #>...<# encloses in-place C code
					(do ((last #\#)
					     (c (read-char) (read-char)))
					    ((and (char=? last #\<)
						  (char=? c #\#))
					     (values))
					  (if (char=? c #\newline)
					      (set! (port-line-number) (+ (port-line-number) 1)))
					  (set! last c)))

				       ((#\<) ; Chicken also, #<<EOF -> EOF
					(if (string=? data "<undef>") ; #<undef> chibi et al
					    #<undefined>
					    (if (and (char=? (data 1) #\<)
						     (> (length data) 2))
						(do ((end (substring data 2))
						     (c (read-line) (read-line)))
						    ((string-position end c)
						     (values)))
						(string->symbol data))))

				       ((#\\)
					(cond ((assoc data '(("\\newline"   . #\newline)
							     ("\\return"    . #\return)
							     ("\\space"     . #\space)
							     ("\\tab"       . #\tab)
							     ("\\null"      . #\null)
							     ("\\nul"       . #\null)
							     ("\\linefeed"  . #\linefeed)
							     ("\\alarm"     . #\alarm)
							     ("\\esc"       . #\escape)
							     ("\\escape"    . #\escape)
							     ("\\rubout"    . #\delete)
							     ("\\delete"    . #\delete)
							     ("\\backspace" . #\backspace)
							     ("\\page"      . #\xc)
							     ("\\altmode"   . #\escape)
							     ("\\bel"       . #\alarm) ; #\x07
							     ("\\sub"       . #\x1a)
							     ("\\soh"       . #\x01)

							     ;; these are for Guile
							     ("\\vt"        . #\xb)
							     ("\\bs"        . #\backspace)
							     ("\\cr"        . #\newline)
							     ("\\sp"        . #\space)
							     ("\\lf"        . #\linefeed)
							     ("\\nl"        . #\null)
							     ("\\ht"        . #\tab)
							     ("\\ff"        . #\xc)
							     ("\\np"        . #\xc))
						      string-ci=?)
					       => (lambda (c)
						    (out-format outport "~NCperhaps use ~W instead~%" (+ lint-left-margin 4) #\space (cdr c))
						    (cdr c)))
					      (else
					       (string->symbol (substring data 1)))))
				       (else
					(string->symbol data))))
				   (lambda args #f))))))))))

      (lambda* (file (outp *output-port*) (report-input #t))
	(if (null? outp)
	    (set! outp (current-output-port))
	    (if (and outp (not (output-port? outp)))
		(error 'wrong-type-arg (format #f "~S should be an output port" outp))))
	(set! outport outp)
	(set! other-identifiers (make-hash-table))
	(set! linted-files ())
	(fill! other-names-counts 0)

	(set! fragmax (min fragmax (- *fragment-max-size* 1)))
	(fill! fragments #f)

	(set! last-simplify-boolean-line-number -1)
	(set! last-simplify-numeric-line-number -1)
	(set! last-simplify-cxr-line-number -1)
	(set! last-checker-line-number -1)
	(set! last-cons-line-number -1)
	(set! last-if-line-number -1)
	(set! last-if->case-line-number -1)
	(set! last-let->case-line-number -1)
	(set! last-rewritten-internal-define #f)
	(set! last-lambda-let #f)
	(set! last-lambda-let-funcs #f)
	(set! line-number -1)
	(set! quote-warnings 0)
	(set! pp-left-margin 0)
	(set! lint-left-margin -3) ; lint-file above adds 4
	(set! big-constants (make-hash-table))
	(set! fragmin *fragment-max-size*)
	(set! fragmax 0)

	(set! *report-input* report-input)
	(set! *report-nested-if* (if (integer? *report-nested-if*) (max 3 *report-nested-if*) 4))
	(set! *report-short-branch* (if (integer? *report-short-branch*) (max 0 *report-short-branch*) 12))
	(set! *#readers* readers)
	(unless (defined? 'lint-no-read-error)
	  (set! (hook-functions *read-error-hook*) read-hooks))

	;; preset list-tail and list-ref
	(vector-set! fragments 10 (make-hash-table))
	(hash-table-set! (vector-ref fragments 10) '((if (zero? <2>) <1> (<F> (cdr <1>) (- <2> 1))))
			 (vector 0 ()
				 (list (cons 'list-tail
					     (inlet 'initial-value '(define (list-tail x k) (if (zero? k) x (list-tail (cdr x) (- k 1))))
						    'arglist '(x k)
						    'history :built-in)))
				 '(define (list-tail x k) (if (zero? k) x (list-tail (cdr x) (- k 1))))
				 #f))
	(vector-set! fragments 12 (make-hash-table))
	(hash-table-set! (vector-ref fragments 12) '((if (= <2> 0) (car <1>) (<F> (cdr <1>) (- <2> 1))))
			 (vector 0 ()
				 (list (cons 'list-ref (inlet 'initial-value '(define (list-ref items n) (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))
							      'arglist '(items n)
							      'history :built-in)))
				 '(define (list-ref items n) (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))
				 #f))


	;; -------- call lint --------

	(let ((vars (let-temporarily (((*s7* 'expansions?) #f)
				      ((lint-pp-funclet '*pretty-print-cycles*) #f))
		      (lint-file file ()))))
	  (set! lint-left-margin (max lint-left-margin 1))

	  (when (pair? vars)
	    (if *report-multiply-defined-top-level-functions*
		(for-each
		 (lambda (var)
		   (let ((var-file (hash-table-ref *top-level-objects* (car var))))
		     (if (not var-file)
			 (hash-table-set! *top-level-objects* (car var) *current-file*)
			 (if (and (string? *current-file*)
				  (not (string=? var-file *current-file*)))
			     (out-format outport "~NC~S is defined at the top level in ~S and ~S~%"
				     lint-left-margin #\space
				     (car var) var-file *current-file*)))))
		 vars))

	    (if (string? file)
		(report-usage top-level: "" vars vars))))

	(for-each
	 (lambda (p)
	   (if (> (cdr p) 3)
	       (out-format outport "'~A occurs ~D times~%"
		       (truncated-list->string (car p)) (cdr p))))
	 big-constants)

	;(format *stderr* "~W~%" other-identifiers)
	(if (and *report-undefined-identifiers*
		 (positive? (hash-table-entries other-identifiers)))
	    (let ((lst (sort! (map car other-identifiers)
			      (lambda (a b)
				(string<? (symbol->string a) (symbol->string b))))))
	      (out-format outport "~NCth~A identifier~A not defined~A: ~{~S~^ ~}~%"
		      lint-left-margin #\space
		      (if (= (hash-table-entries other-identifiers) 1)
			  (values "is" " was")
			  (values "e following" "s were"))
		      (if (string? file) (format #f " in ~A" file) "")
		      lst)
	      (fill! other-identifiers #f)))))))



;;; --------------------------------------------------------------------------------
;;; this reads an HTML file, finds likely-looking scheme code, and runs lint over it.
;;;    called on all snd files in hg.scm

(define html-lint
  (letrec ((remove-markups
	    (lambda (str)
	      (let ((tpos (string-position "<b>" str)))
		(if tpos
		    (let ((epos (string-position "</b>" str)))
		      (remove-markups (string-append (substring str 0 tpos)
						     (substring str (+ tpos 3) epos)
						     (substring str (+ epos 4)))))
		    (let ((apos (string-position "<a " str))
			  (epos (string-position "<em " str)))
		      (if (not (or apos epos))
			  str
			  (let* ((pos ((if (and apos epos) min or) apos epos))
				 (bpos (+ (char-position #\> str (+ pos 1)) 1))
				 (epos (string-position (if (and apos (= pos apos)) "</a>" "</em>") str bpos)))
			    (string-append (substring str 0 pos)
					   (substring str bpos epos)
					   (remove-markups (substring str (+ epos (if (and apos (= apos pos)) 4 5))))))))))))
	   (fixup-html
	    (lambda (str)
	      (let ((pos (char-position #\& str)))
		(if (not pos)
		    str
		    (string-append (substring str 0 pos)
				   (let* ((epos (char-position #\; str pos))
					  (substr (substring str (+ pos 1) epos)))
				     (string-append (cond ((assoc substr '(("gt"    . ">")
									   ("lt"    . "<")
									   ("mdash" . "-")
									   ("amp"   . "&"))
								  string=?) => cdr)
							  (else (format #t "unknown: ~A~%" substr)))
						    (fixup-html (substring str (+ epos 1)))))))))))
    (lambda (file)
      (call-with-input-file file
	(lambda (f)
	  (do ((line-num 0 (+ line-num 1))
	       (line (read-line f #t) (read-line f #t)))
	      ((eof-object? line))

	    ;; look for <pre , gather everything until </pre>
	    ;;   decide if it is scheme code (first char is #\()
	    ;;   if so, clean out html markup stuff, call lint on that

	    (when (string-position "<pre" line)
	      (let ((code (substring line (+ (char-position #\> line) 1))))
		(do ((cline (read-line f #t) (read-line f #t))
		     (rline 1 (+ rline 1)))
		    ((string-position "</pre>" cline)
		     (set! line-num (+ line-num rline)))
		  (set! code (string-append code cline)))

		;; is first non-whitespace char #\(? ignoring comments
		(do ((len (length code))
		     (i 0 (+ i 1)))
		    ((>= i len))
		  (let ((c (string-ref code i)))
		    (unless (char-whitespace? c)
		      (if (char=? c #\;)
			  (set! i (char-position #\newline code i))
			  (begin
			    (set! i (+ len 1))
			    (when (char=? c #\()
			      (catch #t
				(lambda ()
				  (let ((outstr (call-with-output-string
						 (lambda (op)
						   (call-with-input-string
						       (object->string (with-input-from-string
									   (fixup-html (remove-markups code))
									 read)
								       #t) ; write, not display
						     (lambda (ip)
						       (let-temporarily ((*report-shadowed-variables* #t))
							 (lint ip op #f))))))))
				    (if (> (length outstr) 1)               ; possible newline at end
					(format () ";~A ~D: ~A~%" file line-num outstr))))
				(lambda args
				  (format () ";~A ~D, error in read: ~A ~A~%" file line-num args
					  (fixup-html (remove-markups code)))))))))))))))))))


;;; --------------------------------------------------------------------------------
;;; and this reads C code looking for s7_eval_c_string. No attempt here to
;;;   handle weird cases.

(define (C-lint file)
  (call-with-input-file file
    (lambda (f)
      (do ((line-num 0 (+ line-num 1))
	   (line (read-line f #t) (read-line f #t)))
	  ((eof-object? line))

	;; look for s7_eval_c_string, get string arg without backslashes, call lint
	(let ((pos (string-position "s7_eval_c_string(sc, \"(" line)))
	  (when pos
	    (let ((code (substring line (+ pos 22)))) ; (length "s7_eval_c_string(sc, \"")
	      (if (not (string-position "\");" code))
		  (do ((cline (read-line f #t) (read-line f #t))
		       (rline 1 (+ rline 1)))
		      ((string-position "\");" cline)
		       (set! code (string-append code cline))
		       (set! line-num (+ line-num rline)))
		    (set! code (string-append code cline))))

	      (let ((len (string-position "\");" code)))
		(set! code (substring code 0 len))

		;; clean out backslashes
		(do ((i 0 (+ i 1)))
		    ((>= i (- len 3)))
		  (cond ((not (char=? (code i) #\\)))

			((char=? (code (+ i 1)) #\n)
			 (set! (code i) #\space)
			 (set! (code (+ i 1)) #\space))

			((memv (code (+ i 1)) '(#\newline #\"))
			 (set! (code i) #\space))

			((and (char=? (code (+ i 1)) #\\)
			      (char=? (code (- i 1)) #\#))
			 (set! (code (- i 1)) #\space)
			 (set! (code i) #\#)))))
	      (catch #t
		(lambda ()
		  (let ((outstr (call-with-output-string
				 (lambda (op)
				   (call-with-input-string code
				     (lambda (ip)
				       (let-temporarily ((*report-shadowed-variables* #t))
					 (lint ip op #f))))))))
		    (if (> (length outstr) 1) ; possible newline at end
			(format () ";~A ~D: ~A~%" file line-num outstr))))
		(lambda args
		  (format () ";~A ~D, error in read: ~A ~A~%" file line-num args code))))))))))


;;; --------------------------------------------------------------------------------
#|
;;; external use of lint contents (see also snd-lint.scm):
(for-each (lambda (f)
            (if (not (hash-table-ref (*lint* 'no-side-effect-functions) (car f)))
                (format *stderr* "~A " (car f))))
          (*lint* 'built-in-functions))

;;; get rid of []'s! (using Snd)
(define (edit file)
  (let* ((str (file->string file))
	 (len (length str)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (case (str i)
	((#\]) (set! (str i) #\)))
	((#\[) (set! (str i) #\())))
    (call-with-output-file file
      (lambda (p)
	(display str p)))
    #f))
|#

;;; 54 896368, 53 874874, 52 871075, 54 889347
