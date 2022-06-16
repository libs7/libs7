#include <glob.h>
#include <stdio.h>
#include <stdlib.h>

#include "s7.h"

// #include "glob.h"

static s7_pointer s7__globfree(s7_scheme *sc, s7_pointer arg)
{
  glob_t* s7__globfree_0;
  s7__globfree_0 = (glob_t*)s7_c_pointer_with_type(sc, s7_car(arg), s7_make_symbol(sc, "glob_t*"), __func__, 0);
  globfree(s7__globfree_0);
  return(s7_unspecified(sc));
}

/* alloc a glob_t* to pass to glob */
static s7_pointer g_glob_make(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc,
                                     (void *)calloc(1, sizeof(glob_t)),
                                     s7_make_symbol(sc, "glob_t*"),
                                     s7_f(sc)));
}

static s7_pointer g_glob(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc,
                         glob(s7_string(s7_car(args)),
                              s7_integer(s7_cadr(args)), // flags
                              NULL, // *errfunc
                              (glob_t *)s7_c_pointer(s7_caddr(args)))));
}

static s7_pointer g_glob_gl_pathv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p;
  int i;
  glob_t *g;
  g = (glob_t *)s7_c_pointer(s7_car(args));
  p = s7_nil(sc);
  for (i = 0; i < g->gl_pathc; i++)
    p = s7_cons(sc, s7_make_string(sc, g->gl_pathv[i]), p);
  return(p);
}

#define GLOB_MAKE_DOCSTRING "(glob.make) returns a glob_t* object to pass to glob"
#define GLOB_DOCSTRING "Wrapper on BSD glob functions, see manpage glob(3)\n(glob pattern flags g)\nwhere g is returned by (glob.make)\nflags: see glob(3), esp. GLOB_BRACE, GLOB_MARK, GLOB_TILDE\nto obtain results: (glob.gl_pathv g)\nthen (globfree g)"

void init_glob(s7_scheme *sc)
{
    /* printf("init_glob\n"); */
    s7_pointer cur_env, pl_tx;
    s7_int gc_loc;

    /* cur_env = s7_inlet(sc, s7_nil(sc)); */
    cur_env = s7_curlet(sc);
    gc_loc = s7_gc_protect(sc, cur_env);

#ifdef GLOB_NOSYS
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_NOSYS"), s7_make_integer(sc, (s7_int)GLOB_NOSYS));
#endif
#ifdef GLOB_NOMATCH
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_NOMATCH"), s7_make_integer(sc, (s7_int)GLOB_NOMATCH));
#endif
#ifdef GLOB_ABORTED
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_ABORTED"), s7_make_integer(sc, (s7_int)GLOB_ABORTED));
#endif
#ifdef GLOB_NOSPACE
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_NOSPACE"), s7_make_integer(sc, (s7_int)GLOB_NOSPACE));
#endif
#ifdef GLOB_TILDE_CHECK
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_TILDE_CHECK"), s7_make_integer(sc, (s7_int)GLOB_TILDE_CHECK));
#endif
#ifdef GLOB_ONLYDIR
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_ONLYDIR"), s7_make_integer(sc, (s7_int)GLOB_ONLYDIR));
#endif
#ifdef GLOB_TILDE
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_TILDE"), s7_make_integer(sc, (s7_int)GLOB_TILDE));
#endif
#ifdef GLOB_NOMAGIC
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_NOMAGIC"), s7_make_integer(sc, (s7_int)GLOB_NOMAGIC));
#endif
#ifdef GLOB_BRACE
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_BRACE"), s7_make_integer(sc, (s7_int)GLOB_BRACE));
#endif
#ifdef GLOB_ALTDIRFUNC
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_ALTDIRFUNC"), s7_make_integer(sc, (s7_int)GLOB_ALTDIRFUNC));
#endif
#ifdef GLOB_MAGCHAR
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_MAGCHAR"), s7_make_integer(sc, (s7_int)GLOB_MAGCHAR));
#endif
#ifdef GLOB_PERIOD
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_PERIOD"), s7_make_integer(sc, (s7_int)GLOB_PERIOD));
#endif
#ifdef GLOB_NOESCAPE
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_NOESCAPE"), s7_make_integer(sc, (s7_int)GLOB_NOESCAPE));
#endif
#ifdef GLOB_APPEND
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_APPEND"), s7_make_integer(sc, (s7_int)GLOB_APPEND));
#endif
#ifdef GLOB_NOCHECK
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_NOCHECK"), s7_make_integer(sc, (s7_int)GLOB_NOCHECK));
#endif
#ifdef GLOB_DOOFFS
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_DOOFFS"), s7_make_integer(sc, (s7_int)GLOB_DOOFFS));
#endif
#ifdef GLOB_NOSORT
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_NOSORT"), s7_make_integer(sc, (s7_int)GLOB_NOSORT));
#endif
#ifdef GLOB_MARK
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_MARK"), s7_make_integer(sc, (s7_int)GLOB_MARK));
#endif
#ifdef GLOB_ERR
    s7_define(sc, cur_env, s7_make_symbol(sc, "GLOB_ERR"), s7_make_integer(sc, (s7_int)GLOB_ERR));
#endif


    s7_define(sc, cur_env,
              s7_make_symbol(sc, "glob.make"),
              s7_make_typed_function(sc, "glob.make", g_glob_make,
                                     0, /* required args */
                                     0, /* optional args */
                                     false, /* rest */
                                     GLOB_MAKE_DOCSTRING, /* docstring */
                                     NULL));      /* signature */

    s7_pointer t, x;
    t = s7_t(sc);
    x = s7_make_symbol(sc, "c-pointer?");

    pl_tx = s7_make_signature(sc, 2, t, x);

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "globfree"),
              s7_make_typed_function(sc, "globfree", s7__globfree, 1, 0, false, "void globfree(glob_t*)", pl_tx));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "glob"),
              s7_make_typed_function(sc, "glob", g_glob,
                                     3, /* pattern, flags, globobj */
                                     0,
                                     false,
                                     GLOB_DOCSTRING,
                                     NULL));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "glob.gl_pathv"),
              s7_make_typed_function(sc, "glob.gl_pathv", g_glob_gl_pathv, 1, 0, false, "glob.gl_pathv", NULL));

    s7_gc_unprotect_at(sc, gc_loc);

}
