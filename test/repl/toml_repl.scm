(define toml-str "m = { a = 0, b = \"Hi\" }")
(define tt (with-input-from-string toml-str toml:read))
tt
(toml:map? tt)
(toml:map-ref tt "m")
(tt "m")
(toml:map-ref tt 'm)
(tt 'm)
(toml:map-ref tt :m)
(tt :m)

(define tht (toml:map->hash-table tt))
tht
(hash-table? tht)
;;(tht #(:m :a))

(with-input-from-string "t = { i = 1, s = \"Hello\" }" toml:read)
(toml:read "t = { i = 1, s = \"Hello\" }")

(define tt (with-input-from-file "test/data/spec-example-1.toml" toml:read))

;;(define tt (with-input-from-file "test/data/example.toml" toml:read))
(toml:map? tt)
(define ht (toml:map->hash-table tt))
(hash-table? ht)
tt

(call-with-input-file "test/libtoml/data/example.toml"
  (lambda (port) (let ((toml (toml:read port)))
                   toml)))

(define tt (call-with-input-file "test/data/spec-example-1.toml" (lambda (port) (let ((toml (toml:read port))) toml))))
tt

(load "string.scm")

(define s (string-join '("[m]" "truthy = true" "falsey = false" "s      = \"Hello!\"" "i      = 1" "pi     = 3.14" "subm   = { m1 = 1 }" "v      = [0, 1, 2]") :delim "\n"))
s
(define root (toml:read s))

(define m (root :m))
(define ident (lambda (x) x))

(partition odd? '(7 4 2 8 3))
(load "srfi.scm")
(receive (odds evens) (values '(7 3) '(4 2 8)) (display odds) (display " and ") (display evens))

(load "utils.scm")
(load "srfi/srfi-19.scm")

(map ident m)
(m "v")
(define xform (lambda (x) (if (number? x) (* 2 x) x)))

(map xform m)

(load "srfi.scm")


(define numbers (lambda (x) (number


