;;; libtoml_clib.scm
;;;
;;; generate libtoml_s7.c, s7 bindings for libtoml
;;; https://github.com/likle/toml

(require clibgen.scm) ;; cload.scm)
(provide 'libtoml_clibgen.scm)

;; if loading from a different directory, pass that info to C
(let ((directory (let ((current-file (port-filename)))
		   (and (memv (current-file 0) '(#\/ #\~))
			(substring current-file 0 (- (length current-file) 9))))))
  (when (and directory (not (member directory *load-path*)))
    (set! *load-path* (cons directory *load-path*))))


(if (not (defined? '*libtoml*))
    ;; (define *libtoml*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libtoml.scm" (curlet)) *libraries*))
        ;; IMPORTANT! if *cload-library-name* is defined, then clibgen will
	;; (set! *cload-library-name* "*libtoml*")
	(c-define

         '(
           (toml_table_t* toml_parse_file (FILE* char* int))

           ;; (toml_table_t* toml_parse(char* char* int))
	   (in-C "static s7_pointer g_toml_parse(s7_scheme *sc, s7_pointer args)
{
    s7_pointer p, arg;
    char* toml_toml_parse_0;
    p = args;
    arg = s7_car(p);
    if (s7_is_string(arg))
       toml_toml_parse_0 = (char*)s7_string(arg);
    else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, \"toml:parse\", 10), 1, arg, string_string));
    char errbuff[200];
    return(s7_make_c_pointer_with_type(sc, (void*)toml_parse(toml_toml_parse_0, errbuff, sizeof(errbuff)), toml_table_t__symbol, s7_f(sc)));
}
                 ")
	   (C-function ("toml:parse" g_toml_parse "" 1))

           (void toml_free (toml_table_t*))

           ;; struct toml_timestamp_t { };
           ;; struct toml_datum_t { };

           ;; on arrays:
           ;; ... retrieve size of array.
           (int toml_array_nelem (toml_array_t*))
           ;; ... retrieve values using index.
           (toml_datum_t toml_string_at (toml_array_t* int))
           (toml_datum_t toml_bool_at (toml_array_t* int))

           (toml_datum_t toml_int_at (toml_array_t* int))
           (toml_datum_t toml_double_at (toml_array_t* int))
           (toml_datum_t toml_timestamp_at (toml_array_t* int))
           ;; ... retrieve array or table using index.
           (toml_array_t* toml_array_at (toml_array_t* int))
           (toml_table_t* toml_table_at (toml_array_t* int))

           ;; on tables:
           ;; ... retrieve the key in table at keyidx. Return 0 if out of range.
           (char* toml_key_in (toml_table_t* int))
           ;; ... returns 1 if key exists in tab, 0 otherwise
           (int toml_key_exists (toml_table_t* char*))
           ;; ... retrieve values using key.
           (toml_datum_t* toml_string_in (toml_table_t* char*))
           (toml_datum_t toml_bool_in (toml_table_t* char*))

           ;; (toml_datum_t toml_int_in (toml_table_t* char*))
	   (in-C "static s7_pointer g_toml_int_in(s7_scheme *sc, s7_pointer args)
{
 s7_pointer p, arg;
 toml_table_t* toml_toml_int_in_0;
 char* toml_toml_int_in_1;
 p = args;
 arg = s7_car(p);
 toml_toml_int_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
 p = s7_cdr(p);
 arg = s7_car(p);
 if (s7_is_string(arg))
   toml_toml_int_in_1 = (char*)s7_string(arg);
 else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, \"toml:int-in\", 11), 2, arg, string_string));
 toml_datum_t int_datum = toml_int_in(toml_toml_int_in_0, toml_toml_int_in_1);

// malloc and copy returned value, so we can return a ptr? what about gc?

 return(s7_make_c_pointer_with_type(sc, (void*)toml_parse(toml_toml_parse_0, errbuff, sizeof(errbuff)), toml_table_t__symbol, s7_f(sc)));
}
                 ")
	   (C-function ("toml:int-in" g_toml_int_in "" 1))


           (toml_datum_t toml_double_in (toml_table_t* char*))
           (toml_datum_t toml_timestamp_in (toml_table_t* char*))
           ;; .. retrieve array or table using key.
           (toml_array_t* toml_array_in (toml_table_t* char*))
           (toml_table_t* toml_table_in (toml_table_t* char*))

           ;; lesser used
           ;; Return the array kind: 't'able, 'a'rray, 'v'alue, 'm'ixed
           ;; WARNING: return type is char, but that throws an error
           (int toml_array_kind (toml_array_t*))

           ;; For array kind 'v'alue, return the type of values
           ;; i:int, d:double, b:bool, s:string, t:time, D:date, T:timestamp, 'm'ixed
           ;; 0 if unknown
           ;; WARNING: return type is char, but that throws an error
           (int toml_array_type (toml_array_t*))

           ;; Return the key of an array
           (char* toml_array_key (toml_array_t*))

           ;; Return the number of key-values in a table
           (int toml_table_nkval (toml_table_t*))

           ;; Return the number of arrays in a table
           (int toml_table_narr (toml_table_t*))

           ;; Return the number of sub-tables in a table
           (int toml_table_ntab (toml_table_t*))

           ;; Return the key of a table
           (char* toml_table_key (toml_table_t*))

           ;;;;;;;;;;;;;;;;;;
           ;; misc
           (int toml_utf8_to_ucs (char* int int64_t*))
           (int toml_ucs_to_utf8 (int64_t char*)) ;; char buf[6])
           ;; (in-C "
           ;; ;; (void toml_set_memutil (void *(*xxmalloc)(size_t),
           ;; ;;                        void (*xxfree)(void *));
           ;;  ")
           )

	 "toml" ;; prefix to add
         "toml_"    ;; strip-prefix
         "toml.h"
         ;; "" ""
         "libtoml_s7")
	(curlet)))

;; *libtoml*
;; the loader will return *libtoml*
