#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dlfcn.h>
#include "s7.h"

static s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string, integer_string;
static s7_pointer void__symbol;


/* -------- dlopen -------- */
static s7_pointer s7__dlopen(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__dlopen_0;
  int s7__dlopen_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__dlopen_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libdl* 'dlopen)", 17), 1, arg, string_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__dlopen_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libdl* 'dlopen)", 17), 2, arg, integer_string));
  return(s7_make_c_pointer_with_type(sc, (void*)dlopen(s7__dlopen_0, s7__dlopen_1), void__symbol, s7_f(sc)));
}


/* -------- dlclose -------- */
static s7_pointer s7__dlclose(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__dlclose_0;
  p = args;
  arg = s7_car(p);
  s7__dlclose_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)dlclose(s7__dlclose_0)));
}


/* -------- dlsym -------- */
static s7_pointer s7__dlsym(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__dlsym_0;
  char* s7__dlsym_1;
  p = args;
  arg = s7_car(p);
  s7__dlsym_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__dlsym_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libdl* 'dlsym)", 16), 2, arg, string_string));
  return(s7_make_c_pointer_with_type(sc, (void*)dlsym(s7__dlsym_0, s7__dlsym_1), void__symbol, s7_f(sc)));
}


/* -------- dlerror -------- */
static s7_pointer s7__dlerror(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, (char*)dlerror()));
}

void libdl_s7_init(s7_scheme *sc);
void libdl_s7_init(s7_scheme *sc)
{
  s7_pointer cur_env;
  s7_pointer pl_st, pl_ix, pl_xxs, pl_xsi;
  {
    s7_pointer t, x, s, i;
    t = s7_t(sc);
    x = s7_make_symbol(sc, "c-pointer?");
    s = s7_make_symbol(sc, "string?");
    i = s7_make_symbol(sc, "integer?");

    pl_st = s7_make_signature(sc, 2, s, t);
    pl_ix = s7_make_signature(sc, 2, i, x);
    pl_xxs = s7_make_signature(sc, 3, x, x, s);
    pl_xsi = s7_make_signature(sc, 3, x, s, i);
  }

  string_string = s7_make_semipermanent_string(sc, "a string");
  c_pointer_string = s7_make_semipermanent_string(sc, "a c-pointer");
  character_string = s7_make_semipermanent_string(sc, "a character");
  boolean_string = s7_make_semipermanent_string(sc, "a boolean");
  real_string = s7_make_semipermanent_string(sc, "a real");
  complex_string = s7_make_semipermanent_string(sc, "a complex number");
  integer_string = s7_make_semipermanent_string(sc, "an integer");
  cur_env = s7_curlet(sc);

  void__symbol = s7_make_symbol(sc, "void*");

#ifdef RTLD_NODELETE
  s7_define(sc, cur_env, s7_make_symbol(sc, "RTLD_NODELETE"), s7_make_integer(sc, (s7_int)RTLD_NODELETE));
#endif
#ifdef RTLD_LOCAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "RTLD_LOCAL"), s7_make_integer(sc, (s7_int)RTLD_LOCAL));
#endif
#ifdef RTLD_GLOBAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "RTLD_GLOBAL"), s7_make_integer(sc, (s7_int)RTLD_GLOBAL));
#endif
#ifdef RTLD_DEEPBIND
  s7_define(sc, cur_env, s7_make_symbol(sc, "RTLD_DEEPBIND"), s7_make_integer(sc, (s7_int)RTLD_DEEPBIND));
#endif
#ifdef RTLD_NOLOAD
  s7_define(sc, cur_env, s7_make_symbol(sc, "RTLD_NOLOAD"), s7_make_integer(sc, (s7_int)RTLD_NOLOAD));
#endif
#ifdef RTLD_BINDING_MASK
  s7_define(sc, cur_env, s7_make_symbol(sc, "RTLD_BINDING_MASK"), s7_make_integer(sc, (s7_int)RTLD_BINDING_MASK));
#endif
#ifdef RTLD_NOW
  s7_define(sc, cur_env, s7_make_symbol(sc, "RTLD_NOW"), s7_make_integer(sc, (s7_int)RTLD_NOW));
#endif
#ifdef RTLD_LAZY
  s7_define(sc, cur_env, s7_make_symbol(sc, "RTLD_LAZY"), s7_make_integer(sc, (s7_int)RTLD_LAZY));
#endif

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "dlerror"),
            s7_make_typed_function(sc, "dlerror", s7__dlerror, 0, 0, false, "char* dlerror(void)", pl_st));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "dlsym"),
            s7_make_typed_function(sc, "dlsym", s7__dlsym, 2, 0, false, "void* dlsym(void* char*)", pl_xxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "dlclose"),
            s7_make_typed_function(sc, "dlclose", s7__dlclose, 1, 0, false, "int dlclose(void*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "dlopen"),
            s7_make_typed_function(sc, "dlopen", s7__dlopen, 2, 0, false, "void* dlopen(char* int)", pl_xsi));
}
