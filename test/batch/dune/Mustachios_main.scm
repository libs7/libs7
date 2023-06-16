;; (format *stderr* "Reader: ~A~%" reader)
;; (format *stderr* "Datafile: ~A~%" datafile)
;; (format *stderr* "Template: ~A~%" template)

(let ((data (with-input-from-file
                datafile reader))
      (template (call-with-input-file template
                  (lambda (p)
                    (let ((tlen (length p)))
                      (read-string tlen p))))))
  (mustache:render #f template data 0))
