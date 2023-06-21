*json:version*

(with-input-from-string "{ \"t\": { \"i\": 1, \"s\": \"Hello\"}}" json:read)
(define jo (json:read "{ \"t\": { \"i\": 1, \"s\": \"Hello\"}}"))
jo
(json:map? jo)
(json:map-ref jo "t")
(jo "t")
(json:map-ref jo 't)
(jo 't)
(json:map-ref jo :t)
(jo :t)
;; key not found returns #f
(jo :foo)

(define jm (with-input-from-file "test/data/example.json" json:read))
jm

(define jm (with-input-from-file "test/data/nested.json" json:read))
(define t "{{#blist}}hi {{name}}{{?}}, {{/?}}{{$}}!{{/$}}{{/blist}}")
(mustache:render #t t jm 0)
(define ht (json:map->hash-table jm))
(mustache:render #t t ht 0)

(call-with-input-file "test/data/example.json" (lambda (port) (let ((json (json:read port))) json)))

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


