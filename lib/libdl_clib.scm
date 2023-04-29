;;; libdl_clib.scm
;;;
;;; generate libdl_s7.c, s7 bindings for libdl (dlopen etc.)

(require clibgen.scm) ;; cload.scm)
(provide 'libdl_clib.scm)

;; if loading from a different directory, pass that info to C
(let ((directory (let ((current-file (port-filename)))
		   (and (memv (current-file 0) '(#\/ #\~))
			(substring current-file 0 (- (length current-file) 9))))))
  (when (and directory (not (member directory *load-path*)))
    (set! *load-path* (cons directory *load-path*))))


;; FIXME: dlopen, dlsym return null ptr on error, so must be implemented in-C

(if (not (defined? '*libdl*))
    (define *libdl*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libdl.scm" (curlet)) *libraries*))
	(set! *cload-library-name* "*libdl*")
	(c-define '((void* dlopen (char* int))
		    (int dlclose (void*))
		    (void* dlsym (void* char*))
                    (C-macro (void* (RTLD_DEFAULT RTLD_NEXT RTLD_SELF RTLD_MAIN_ONLY))) ;; dlsym handles
		    (char* dlerror (void))
		    (C-macro (int (RTLD_LAZY RTLD_NOW RTLD_BINDING_MASK RTLD_NOLOAD RTLD_DEEPBIND RTLD_GLOBAL RTLD_LOCAL RTLD_NODELETE))))
		  "libdl" "" "dlfcn.h" "" "" "libdl_s7")
	(curlet))))

*libdl*
;; the loader will return *libdl*
