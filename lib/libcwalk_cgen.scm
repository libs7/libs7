;;; libcwalk_cgen.scm
;;;
;;; generate libcwalk_s7.c, s7 bindings for libcwalk
;;; https://github.com/likle/cwalk

(require cgen.scm) ;; cload.scm)
(provide 'libcwalk_cgen.scm)

;; if loading from a different directory, pass that info to C
(let ((directory (let ((current-file (port-filename)))
		   (and (memv (current-file 0) '(#\/ #\~))
			(substring current-file 0 (- (length current-file) 9))))))
  (when (and directory (not (member directory *load-path*)))
    (set! *load-path* (cons directory *load-path*))))


(if (not (defined? '*libdl*))
    (define *libdl*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libdl.scm" (curlet)) *libraries*))
	(set! *cload-library-name* "*libdl*")
	(c-define '((void* dlopen (char* int))
		    (int dlclose (void*))
		    (void* dlsym (void* char*))
		    (char* dlerror (void))
		    (C-macro (int (RTLD_LAZY RTLD_NOW RTLD_BINDING_MASK RTLD_NOLOAD RTLD_DEEPBIND RTLD_GLOBAL RTLD_LOCAL RTLD_NODELETE))))
		  "" "dlfcn.h" "" "" "libdl_s7")
	(curlet))))

*libdl*
;; the loader will return *libdl*
