;; from lint.scm

;; (set! (hook-functions *read-error-hook*) read-hooks))

(define read-hooks
  ;; try to get past all the # and \ stuff in other Schemes
  ;;   main remaining problem: [] used as parentheses (Gauche and Chicken for example)
  (list (lambda (h)
	  (let ((data (h 'data))
		(line (port-line-number)))
	    (if (not (h 'type))
		(begin
		  (format outport "~NCreader~A: unknown \\ usage: \\~C~%" lint-left-margin #\space (if (zero? line) "" (format #f "[~A]" line)) data)
		  (set! (h 'result) data))
		(begin
		  (format outport "~NCreader~A: unknown # object: #~A~%" lint-left-margin #\space (if (zero? line) "" (format #f "[~A]" line)) data)
		  (set! (h 'result)
			(catch #t
			       (lambda ()
				 (case (data 0)
				   ((#\;) (read) (values))

				   ((#\T)
				    (and (string=? data "T")
					 (format outport "#T should be #t~%")
					 #t))

				   ((#\F)
				    (and (string=? data "F")
					 (format outport "#F should be #f~%")
					 ''#f))

				   ((#\X #\B #\O #\D)
				    (let ((num (string->number (substring data 1) (case (data 0) ((#\X) 16) ((#\O) 8) ((#\B) 2) ((#\D) 10)))))
				      (if (number? num)
					  (begin
					    (format outport "~NCuse #~A~A not #~A~%"
						    lint-left-margin #\space
						    (char-downcase (data 0)) (substring data 1) data)
					    num)
					  (string->symbol data))))

				   ((#\i)
				    (format outport "#i is used for int-vectors, not numbers.~%")
				    (cond ((string->number (substring data 1)) => exact->inexact) (else #f)))

				   ((#\r)
				    (format outport "#r is used for float-vectors, not numbers.~%")
				    #f)

				   ((#\l #\z)
				    (let ((num (string->number (substring data 1)))) ; Bigloo (also has #ex #lx #z and on and on)
				      (if (number? num)
					  (begin
					    (format outport "~NCjust omit this silly #~C!~%" lint-left-margin #\space (data 0))
					    num)
					  (string->symbol data))))

				   ((#\u) ; for Bigloo
				    (if (string=? data "unspecified")
					(format outport "~NCuse #<unspecified>, not #unspecified~%" lint-left-margin #\space))
				    ;; #<unspecified> seems to hit the no-values check?
				    (string->symbol data))
				   ;; Bigloo also seems to use #" for here-doc concatenation??

				   ((#\v) ; r6rs byte-vectors?
				    (if (string=? data "vu8")
					(format outport "~NCuse #u, not #vu8~%" lint-left-margin #\space))
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
						(format outport "~NCperhaps use ~W instead~%" (+ lint-left-margin 4) #\space (cdr c))
						(cdr c)))
					  (else
					   (string->symbol (substring data 1)))))
				   (else
				    (string->symbol data))))
			       (lambda args #f)))))))))
