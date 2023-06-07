(define toml-str "m = { a = 0, b = \"Hi\" }")
(define tt (with-input-from-string toml-str toml:read))

(toml:map-ref tt "m")
(toml:map-ref tt 'm)
(toml:map-ref tt :m)

(with-input-from-string "t = { i = 1, s = \"Hello\" }" toml:read)
(toml:read "t = { i = 1, s = \"Hello\" }")

(define tt (with-input-from-file "test/libtoml/data/spec-example-1.toml" toml:read))

(call-with-input-file "test/libtoml/data/example.toml"
  (lambda (port) (let ((toml (toml:read port)))
                   toml)))

(define tt (call-with-input-file "test/libtoml/data/spec-example-1.toml" (lambda (port) (let ((toml (toml:read port))) toml))))

(load "string.scm")

(define root (toml:read (string-join
                         '("[m]" "truthy = true" "falsey = false" "s      = \"Hello!\"" "i      = 1" "pi     = 3.14" "subm   = { m1 = 1 }" "v      = [0, 1, 2]") :delim "\n")))

(define m (root "m"))
(define ident
  (lambda (x) x))

(partition odd? '(7 4 2 8 3))
(load "srfi.scm")
(receive (odds evens) (values '(7 3) '(4 2 3)) (display odds) (display " and ") (display evens) (newline))

(load "utils.scm")
(load "srfi/srfi-19.scm")

(map ident m)
(m "v")
(define xform (lambda (x) (if (number? x) (* 2 x) x)))

(map xform m)

(load "srfi.scm")


(define numbers (lambda (x) (number


