;;; libregex_clibgen.scm
;;;
;;; generate s7 bindings for regex.h
(require clibgen.scm)
(provide 'libregex.scm)

;; if loading from a different directory, pass that info to C
(let ((directory (let ((current-file (port-filename)))
		   (and (memv (current-file 0) '(#\/ #\~))
			(substring current-file 0 (- (length current-file) 9))))))
  (when (and directory (not (member directory *load-path*)))
    (set! *load-path* (cons directory *load-path*))))

(unless (defined? '*libregex*)
  (define *libregex*
      (with-let (unlet)
	(set! *libraries* (cons (cons "libregex.scm" (curlet)) *libraries*))

	(c-define
	   ;; -------- regex.h --------

	   '((C-macro (int (REG_NOTBOL REG_NOTEOL REG_NOMATCH REG_ESPACE REG_BADBR REG_BADPAT REG_BADRPT REG_ECOLLATE REG_ECTYPE REG_EESCAPE
			  REG_ESUBREG REG_EBRACK REG_EPAREN REG_EBRACE REG_ERANGE REG_EXTENDED REG_ICASE REG_NOSUB REG_NEWLINE)))

	   (in-C "
                  static s7_pointer g_regcomp(s7_scheme *sc, s7_pointer args)
                  {
                    int res, flags;
                    regex_t *regexp;
                    const char *str;
                    if (!s7_is_string(s7_cadr(args)))
                       return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, \"(*libc* 'regcomp)\", 17), 2, s7_cadr(args), string_string));
                    if (!s7_is_integer(s7_caddr(args)))
                       return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, \"(*libc* 'regcomp)\", 17), 3, s7_caddr(args), integer_string));
                    regexp = (regex_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, \"regex_t*\"), __func__, 1);
                    str = (const char *)s7_string(s7_cadr(args));
                    flags = s7_integer(s7_caddr(args));
                    res = regcomp(regexp, str, flags);
                    return(s7_make_integer(sc, res));
                  }

                  static s7_pointer g_regexec(s7_scheme *sc, s7_pointer args)
                  {
                    int i, res, flags, nmatches;
                    regex_t *regexp;
                    const char *str;
                    regmatch_t *matches;
                    s7_pointer subs;
                    s7_int *els;

                    if (!s7_is_string(s7_cadr(args)))
                       return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, \"(*libc* 'regexec)\", 17), 2, s7_cadr(args), string_string));
                    if (!s7_is_integer(s7_caddr(args)))
                       return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, \"(*libc* 'regexec)\", 17), 3, s7_caddr(args), integer_string));
                    if (!s7_is_integer(s7_cadddr(args)))
                       return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, \"(*libc* 'regexec)\", 17), 4, s7_cadddr(args), integer_string));
                    regexp = (regex_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, \"regex_t*\"), __func__, 1);
                    str = (const char *)s7_string(s7_cadr(args));
                    nmatches = s7_integer(s7_caddr(args));
                    flags = s7_integer(s7_cadddr(args));
                    if (nmatches == 0)
                       return(s7_make_integer(sc, regexec(regexp, str, 0, NULL, flags)));
                    matches = (regmatch_t *)calloc(nmatches, sizeof(regmatch_t));
                    res = regexec(regexp, str, nmatches, matches, flags);
                    if (res != 0)
                       {
                         free(matches);
                         return(s7_make_integer(sc, res));
                       }
                    subs = s7_make_int_vector(sc, nmatches * 2, 1, NULL);
                    els = s7_int_vector_elements(subs);
                    for (i = 0; i < nmatches; i++)
                      {
                        els[i * 2] = matches[i].rm_so;
                        els[i * 2 + 1] = matches[i].rm_eo;
                      }
                    free(matches);
                    return(subs);
                  }

                  static s7_pointer g_regex_make(s7_scheme *sc, s7_pointer args)
                  {
                    return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(regex_t)), s7_make_symbol(sc, \"regex_t*\"), s7_f(sc)));
                  }

                  static s7_pointer g_regfree(s7_scheme *sc, s7_pointer args)
                  {
                    regfree((regex_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, \"regex_t*\"), __func__, 1));
                    return(s7_f(sc));
                  }

                  static s7_pointer g_regex_free(s7_scheme *sc, s7_pointer args)
                  {
                    free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, \"regex_t*\"), __func__, 1));
                    return(s7_f(sc));
                  }

                  static s7_pointer g_regerror(s7_scheme *sc, s7_pointer args)
                  {
                    size_t len;
                    int errcode;
                    regex_t *regexp;
                    char *err;
                    s7_pointer result;
                    errcode = s7_integer(s7_car(args));
                    regexp = (regex_t *)s7_c_pointer_with_type(sc, s7_cadr(args), s7_make_symbol(sc, \"regex_t*\"), __func__, 1);
                    len = regerror (errcode, regexp, NULL, 0);
                    err = (char *)malloc(len);
                    regerror(errcode, regexp, err, len);
                    result = s7_make_string_with_length(sc, err, len - 1);
                    free(err);
                    return(result);
                  }
                 ")
	   (C-function ("libc:regex.make" g_regex_make "" 0))
	   (C-function ("libc:regex.free" g_regex_free "" 1))
	   (C-function ("libc:regfree" g_regfree "" 1))
	   (C-function ("libc:regcomp" g_regcomp "" 3))
	   (C-function ("libc:regexec" g_regexec "" 4)) ; (regexec regex string nmatches flags)
	   (C-function ("libc:regerror" g_regerror "" 2))
	   )

	 "rgx" ;; prefix
         ""     ;; strip-prefix
	 (list "regex.h")
	 ;; "" ;; cflags
	 ;; (if (provided? 'linux) "-lrt"
	 ;;     (if (provided? 'openbsd) "-pthread" "")) ;; lflags
	 "libregex_s7" ;; output-name
         )

	(curlet))))

;; (format #t "LOADED libc_clibgen.scm~%")
*libregex*
