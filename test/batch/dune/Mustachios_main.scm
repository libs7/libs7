;; (format *stderr* "Reader: ~A~%" reader)
;; (format *stderr* "Datafile: ~A~%" datafile)
;; (format *stderr* "Template: ~A~%" template)

(let* ((data (with-input-from-file
                datafile reader))
      (template (call-with-input-file template
                  (lambda (p)
                    (let ((tlen (length p)))
                      (read-string tlen p))))))
  (format *stderr* "DATA:~%~A~%" data)
  (call-with-output-file outfile
    (lambda (p)
      (write data p)
      #;(mustache:render p template data 0)))
  )

      ;; (format p "Hello~%")
      ;; (write data p)
      ;; (newline p)
      ;; (write mibl p)))
