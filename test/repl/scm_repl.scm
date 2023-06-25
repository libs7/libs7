 ;; "test/data/example.scm"
(load "alist.scm")

;; NB: s7 string and file inports are indistinguishable
;; open-input-file returns #<input-string-port>
(define test (open-input-file "test/batch/dune/mwe/dune"))
test ;; => #<input-string-port>
(close-input-port test)
test ;; => #<input-string-port:closed>

(define test (open-input-string "foo bar"))
test ;; => #<input-string-port>
(close-input-port test)
test ;; => #<input-string-port:closed>

;; use length to get file size from port
(define test (open-input-file "test/batch/dune/mwe/dune"))
(length test) ;; => 21
(close-input-port test)

;; ditto for string ports
(define test (open-input-string "foo bar, buz!"))
(length test) ;; => 13
(close-input-port test)

;; then use read-string to read entire thing
(define test (open-input-file "test/batch/dune/mwe/dune"))
(length test) ;; => 21
(read-string 21 test) ;; => "(alias (name \"mwe\"))
(close-input-port test)

;; ditto for string ports
(define test (open-input-string "foo bar, buz!"))
(length test) ;; => 13
(read-string 13 test) ;; => "foo bar, buz!"
(close-input-port test)

(define datafile "test/batch/dune/async_ssl/src/dune")
(define m (call-with-input-file datafile sexp:read))
;; (define m (with-input-from-file datafile sexp:read))
m
(alist? m)
(car m)
(alist? (cdr m))

;; (set! *sexp:expand-includes* #f)
;; (set! *sexp:expand-includes* #t)
;; *sexp:expand-includes*

;; include w/o baddot
(define datafile "test/batch/dune/include/case001/dune")
(define m (call-with-input-file datafile sexp:read))
;; (define m (with-input-from-file datafile sexp:read))

;; include with baddot
(define datafile "test/batch/dune/include/case002/dune")
(define m (call-with-input-file datafile sexp:read))
;; (define m (with-input-from-file datafile sexp:read))

(define datafile "test/batch/dune/mwe/dune")
(define m (call-with-input-file datafile sexp:read))
;; (define m (with-input-from-file datafile sexp:read))

(define datafile "test/batch/dune/empty/dune")
(define m (with-input-from-file datafile sexp:read))

(define datafile "test/batch/dune/baddot/dune")
(define m (with-input-from-file datafile sexp:read))

;; (let* ((datafile "test/batch/dune/baddot/dune") (m (with-input-from-file datafile sexp:read))) m)


(define datafile "test/batch/dune/strings/eol/dune")
(define m (with-input-from-file datafile sexp:read))

m
(assoc 'library m)
(assoc 'rule m)
(length m)
alist->hash-table


;; (let ((m (with-input-from-file datafile sexp:read))) (format #t "FOOP"))

m

;; (define m (with-input-from-file datafile (lambda() (format #t "~A~^%" (stacktrace)) (format #t "EOST~%")  (sexp:read))))
m
(assoc 'library m)

;; opens inport on datafile, passes to sexp:read
(define m (call-with-input-file datafile sexp:read))



(let ((data (with-input-from-file
                datafile reader))
      (template (call-with-input-file template
                  (lambda (p)
                    (let ((tlen (length p)))
                      (read-string tlen p))))))
  (mustache:render #f template data 0))



(with-input-from-string "t = { i = 1, s = \"Hello\" }" toml:read)
(toml:read "t = { i = 1, s = \"Hello\" }")

(with-input-from-string "{ \"t\": { \"i\": 1, \"s\": \"Hello\"}}" json:read)
(json:read "{ \"t\": { \"i\": 1, \"s\": \"Hello\"}}")

;; (call-with-input-file "config.toml"
;;   (lambda (p)
;;     (let f ((x (read p)))
;;       (if (eof-object? x)
;;           '()
;;           (cons x (f (read p)))))))

(call-with-input-file "test/libtoml/data/example.toml"
  (lambda (port) (let ((toml (toml:read port)))
                   toml)))

(call-with-input-file "test/libjson/data/example.json" (lambda (port) (let ((json (json:read port))) json)))



*json:version*

(define root (json:read "{
    \"m\": { \"truthy\": true,
         \"falsey\": false,
         \"nil\": null,
         \"s\": \"Hello!\",
         \"i\": 1,
         \"pi\": 3.14,
         \"subm\": { \"m1\": 1 },
         \"v\": [0, 1, 2]
         }}"))
(define m (root "m"))
(define ident (lambda (x) x))

(map ident m)
(m "v")
(define xform (lambda (x) (if (number? x) (* 2 x) x)))

(map xform m)

(load "srfi.scm")


(define numbers (lambda (x) (number

(define datafile "test/batch/dune/mwe/dune")
(define m (call-with-input-file datafile sexp:read))
;; (define m (with-input-from-file datafile sexp:read))

(define dune->mibl (load "mibl.scm"))
(dune->mibl m)
