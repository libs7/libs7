(define m (with-input-from-file "test/data/example.scm" read))


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


