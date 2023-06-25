(format *stderr* "load path: ~A~%" *load-path*)
(set! *load-path* (cons "scm" *load-path*))
      ;; (cons "../mustachios.runfiles/libs7/scm"
      ;;       (cons "libs7"
(format *stderr* "load path: ~A~%" *load-path*)

(load "scm/mibl.scm")
;; (format *stderr* "Reader: ~A~%" reader)
;; (format *stderr* "Datafile: ~A~%" datafile)
;; (format *stderr* "Template: ~A~%" template)
(format *stderr* "Outfile: ~A~%" outfile)

(let* ((data (with-input-from-file
                datafile reader))
      (mibl (dune->mibl data)))
  (format *stderr* "DATA:~%~A~%" data)
  (format *stderr* "MIBL:~%~A~%" mibl)
  (call-with-output-file outfile
    (lambda (p)
      (format p "Hello~%")
      (write data p)
      (newline p)
      (write mibl p))))
