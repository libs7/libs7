#include <fnmatch.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <wordexp.h>

#include "s7.h"

static s7_pointer wordexp_t__symbol;

/* -------- wordexp -------- */
static s7_pointer s7__wordexp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__wordexp_0;
  wordexp_t* s7__wordexp_1;
  int s7__wordexp_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__wordexp_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__wordexp_1 = (wordexp_t*)s7_c_pointer_with_type(sc, arg, wordexp_t__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__wordexp_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)wordexp(s7__wordexp_0, s7__wordexp_1, s7__wordexp_2)));
}


/* -------- wordfree -------- */
static s7_pointer s7__wordfree(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  wordexp_t* s7__wordfree_0;
  p = args;
  arg = s7_car(p);
  s7__wordfree_0 = (wordexp_t*)s7_c_pointer_with_type(sc, arg, wordexp_t__symbol, __func__, 0);
  wordfree(s7__wordfree_0);
  return(s7_unspecified(sc));
}

static s7_pointer g_wordexp_make(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(wordexp_t)), s7_make_symbol(sc, "wordexp_t*"), s7_f(sc)));}
                           static s7_pointer g_wordexp_we_wordc(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_integer(sc, ((wordexp_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "wordexp_t*"), __func__, 1))->we_wordc));}
                           static s7_pointer g_wordexp_we_wordv(s7_scheme *sc, s7_pointer args)
                           {
                             s7_pointer p;
                             size_t i;
                             wordexp_t *g;
                             g = (wordexp_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "wordexp_t*"), __func__, 1);
                             p = s7_nil(sc);
                             for (i = 0; i < g->we_wordc; i++)
                               p = s7_cons(sc, s7_make_string(sc, g->we_wordv[i]), p);
                             return(p);
                           }

void init_shell_api(s7_scheme *sc)
{
    s7_pointer cur_env, pl_isxi, pl_tx;
    /* s7_int gc_loc; */

    s7_pointer t, s,i,x;
    t = s7_t(sc);
    s = s7_make_symbol(sc, "string?");
    i = s7_make_symbol(sc, "integer?");
    x = s7_make_symbol(sc, "c-pointer?");

    pl_tx = s7_make_signature(sc, 2, t, x);
    pl_isxi = s7_make_signature(sc, 4, i, s, x, i);

    cur_env = s7_curlet(sc);

    wordexp_t__symbol = s7_make_symbol(sc, "wordexp_t*");

    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_SYNTAX"), s7_make_integer(sc, (s7_int)WRDE_SYNTAX));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_CMDSUB"), s7_make_integer(sc, (s7_int)WRDE_CMDSUB));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_BADVAL"), s7_make_integer(sc, (s7_int)WRDE_BADVAL));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_BADCHAR"), s7_make_integer(sc, (s7_int)WRDE_BADCHAR));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_NOSPACE"), s7_make_integer(sc, (s7_int)WRDE_NOSPACE));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_UNDEF"), s7_make_integer(sc, (s7_int)WRDE_UNDEF));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_SHOWERR"), s7_make_integer(sc, (s7_int)WRDE_SHOWERR));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_REUSE"), s7_make_integer(sc, (s7_int)WRDE_REUSE));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_NOCMD"), s7_make_integer(sc, (s7_int)WRDE_NOCMD));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_APPEND"), s7_make_integer(sc, (s7_int)WRDE_APPEND));
    s7_define(sc, cur_env, s7_make_symbol(sc, "WRDE_DOOFFS"), s7_make_integer(sc, (s7_int)WRDE_DOOFFS));


    s7_define(sc, cur_env,
              s7_make_symbol(sc, "wordexp.we_wordv"),
              s7_make_typed_function(sc, "wordexp.we_wordv", g_wordexp_we_wordv, 1, 0, false, "wordexp.we_wordv", NULL));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "wordexp.we_wordc"),
              s7_make_typed_function(sc, "wordexp.we_wordc", g_wordexp_we_wordc, 1, 0, false, "wordexp.we_wordc", NULL));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "wordexp.make"),
              s7_make_typed_function(sc, "wordexp.make", g_wordexp_make, 0, 0, false, "wordexp.make", NULL));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "wordfree"),
              s7_make_typed_function(sc, "wordfree", s7__wordfree, 1, 0, false, "void wordfree(wordexp_t*)", pl_tx));

    s7_define(sc, cur_env,
              s7_make_symbol(sc, "wordexp"),
              s7_make_typed_function(sc, "wordexp", s7__wordexp, 3, 0, false, "int wordexp(char* wordexp_t* int)", pl_isxi));

}
