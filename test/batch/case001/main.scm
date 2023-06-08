(let ((data (with-input-from-file "test/batch/case001/colors.json" json:read))
      (template (call-with-input-file "test/batch/case001/colors.mustache"
                  (lambda (p) (read-string 1000 p)))))
  (mustache:render #f template data 0))
