#include <regex.h>

static s7_pointer g_regcomp(s7_scheme *sc, s7_pointer args)
                  {
                    int res, flags;
                    regex_t *regexp;
                    const char *str;
                    regexp = (regex_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "regex_t*"), __func__, 1);
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

                    regexp = (regex_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "regex_t*"), __func__, 1);
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
                    return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(regex_t)), s7_make_symbol(sc, "regex_t*"), s7_f(sc)));
                  }

                  static s7_pointer g_regfree(s7_scheme *sc, s7_pointer args)
                  {
                    regfree((regex_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "regex_t*"), __func__, 1));
                    return(s7_f(sc));
                  }

                  static s7_pointer g_regex_free(s7_scheme *sc, s7_pointer args)
                  {
                    free((void *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "regex_t*"), __func__, 1));
                    return(s7_f(sc));
                  }

                  static s7_pointer g_regerror(s7_scheme *sc, s7_pointer args)
                  {
                    size_t len;
                    int errcode;
                    regex_t *regexp;
                    char *err;
                    errcode = s7_integer(s7_car(args));
                    regexp = (regex_t *)s7_c_pointer_with_type(sc, s7_cadr(args), s7_make_symbol(sc, "regex_t*"), __func__, 1);
                    len = regerror (errcode, regexp, NULL, 0);
                    err = (char *)malloc(len);
                    regerror(errcode, regexp, err, len);
                    return(s7_make_string_with_length(sc, err, len - 1));
                  }

#ifdef _SC_REGEX_VERSION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_REGEX_VERSION"), s7_make_integer(sc, (s7_int)_SC_REGEX_VERSION));
#endif
#ifdef _SC_REGEXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_REGEXP"), s7_make_integer(sc, (s7_int)_SC_REGEXP));
#endif

#ifdef _POSIX_REGEXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_REGEXP"), s7_make_integer(sc, (s7_int)_POSIX_REGEXP));
#endif

#ifdef REG_NEWLINE
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_NEWLINE"), s7_make_integer(sc, (s7_int)REG_NEWLINE));
#endif
#ifdef REG_NOSUB
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_NOSUB"), s7_make_integer(sc, (s7_int)REG_NOSUB));
#endif
#ifdef REG_ICASE
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_ICASE"), s7_make_integer(sc, (s7_int)REG_ICASE));
#endif
#ifdef REG_EXTENDED
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_EXTENDED"), s7_make_integer(sc, (s7_int)REG_EXTENDED));
#endif
#ifdef REG_ERANGE
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_ERANGE"), s7_make_integer(sc, (s7_int)REG_ERANGE));
#endif
#ifdef REG_EBRACE
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_EBRACE"), s7_make_integer(sc, (s7_int)REG_EBRACE));
#endif
#ifdef REG_EPAREN
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_EPAREN"), s7_make_integer(sc, (s7_int)REG_EPAREN));
#endif
#ifdef REG_EBRACK
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_EBRACK"), s7_make_integer(sc, (s7_int)REG_EBRACK));
#endif
#ifdef REG_ESUBREG
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_ESUBREG"), s7_make_integer(sc, (s7_int)REG_ESUBREG));
#endif
#ifdef REG_EESCAPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_EESCAPE"), s7_make_integer(sc, (s7_int)REG_EESCAPE));
#endif
#ifdef REG_ECTYPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_ECTYPE"), s7_make_integer(sc, (s7_int)REG_ECTYPE));
#endif
#ifdef REG_ECOLLATE
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_ECOLLATE"), s7_make_integer(sc, (s7_int)REG_ECOLLATE));
#endif
#ifdef REG_BADRPT
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_BADRPT"), s7_make_integer(sc, (s7_int)REG_BADRPT));
#endif
#ifdef REG_BADPAT
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_BADPAT"), s7_make_integer(sc, (s7_int)REG_BADPAT));
#endif
#ifdef REG_BADBR
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_BADBR"), s7_make_integer(sc, (s7_int)REG_BADBR));
#endif
#ifdef REG_ESPACE
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_ESPACE"), s7_make_integer(sc, (s7_int)REG_ESPACE));
#endif
#ifdef REG_NOMATCH
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_NOMATCH"), s7_make_integer(sc, (s7_int)REG_NOMATCH));
#endif
#ifdef REG_NOTEOL
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_NOTEOL"), s7_make_integer(sc, (s7_int)REG_NOTEOL));
#endif
#ifdef REG_NOTBOL
  s7_define(sc, cur_env, s7_make_symbol(sc, "REG_NOTBOL"), s7_make_integer(sc, (s7_int)REG_NOTBOL));
#endif

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "regexec"),
            s7_make_typed_function(sc, "regexec", g_regexec, 4, 0, false, "regexec", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "regcomp"),
            s7_make_typed_function(sc, "regcomp", g_regcomp, 3, 0, false, "regcomp", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "regfree"),
            s7_make_typed_function(sc, "regfree", g_regfree, 1, 0, false, "regfree", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "regex.free"),
            s7_make_typed_function(sc, "regex.free", g_regex_free, 1, 0, false, "regex.free", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "regex.make"),
            s7_make_typed_function(sc, "regex.make", g_regex_make, 0, 0, false, "regex.make", NULL));

