;; libm.scm

(format #t "loading libm.scm")

;; clib_sinit(s7, libm_s7_init, "libm");

(load ;; "libm_s7.dylib"
      "external/libs7/lib/shared/libm_s7.dylib"
      (inlet 'init_func 'libm_s7_init))

