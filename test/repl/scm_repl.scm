 ;; "test/data/example.scm"
(load "alist.scm")

(define datafile "test/batch/dune/baddot/dune")

(define datafile "test/batch/dune/async_ssl/src/dune")
(define m (with-input-from-file datafile sexp:read))
m
(car m)
(alist? (cdr m))

(define m (call-with-input-file datafile sexp:read))


;; include w/o baddot
(define datafile "test/batch/dune/include/case001/dune")
(define m (with-input-from-file datafile sexp:read))

;; include with baddot
(define datafile "test/batch/dune/include/case002/dune")
(define m (with-input-from-file datafile sexp:read))

;; (define m (call-with-input-file datafile sexp:read))

(define datafile "test/batch/dune/mwe/dune")
(define m (with-input-from-file datafile sexp:read))

(define datafile "test/batch/dune/empty/dune")
(define m (with-input-from-file datafile sexp:read))

(define datafile "test/batch/dune/baddot/dune")
(define m (with-input-from-file datafile sexp:read))

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


