(format #t "load path: ~A~%" *load-path*)
(load "scm/alist.scm")
;; (format *stderr* "Reader: ~A~%" reader)
;; (format *stderr* "Datafile: ~A~%" datafile)
;; (format *stderr* "Template: ~A~%" template)

(let* (
       (data (with-input-from-file datafile reader))
       ;; (data (call-with-input-file datafile reader))

       ;; (data (with-input-from-string
       ;;           "(alias (name \"mwe\"))
       ;;         (alias (name \"foo\"))
       ;;         (alias (name \"bar\"))"
       ;;         reader))

       ;; (data (reader "(alias (name \"mwe\"))
       ;;         (alias (name \"foo\"))
       ;;         (alias (name \"bar\"))"))

       ;; (data (apply reader (list
       ;;                      "(alias (name \"mwe\"))
       ;;         (alias (name \"foo\"))
       ;;         (alias (name \"bar\"))")))

      (template (call-with-input-file template
                  (lambda (p)
                    (let ((tlen (length p)))
                      (read-string tlen p)))))
       (xdata (alist->hash-table data))
      )
  (format *stderr* "DATA: '~A'~%" xdata)
  (call-with-output-file outfile
    (lambda (p)
      ;;(write xdata p)
      (mustache:render p template xdata 0)))
  )

      ;; (format p "Hello~%")
      ;; (write data p)
      ;; (newline p)
      ;; (write mibl p)))
