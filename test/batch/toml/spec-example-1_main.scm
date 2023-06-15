;; (format *stderr* "Reader: ~A~%" reader)
;; (format *stderr* "Datafile: ~A~%" datafile)
;; (format *stderr* "Template: ~A~%" template)

(let ((data (with-input-from-file
                datafile reader))
      (template (call-with-input-file template
                  (lambda (p) (read-string 1000 p)))))
  ;; (format *stderr* "data: ~A~%" data)
  (mustache:render #f template data 0))
