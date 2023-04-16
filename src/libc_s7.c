#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <stdint.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <fenv.h>
#include <stdio.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <dirent.h>
#include <ftw.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/time.h>
#include <utime.h>
#include <termios.h>
#include <grp.h>
#include <pwd.h>
#include <fnmatch.h>
#include <glob.h>
#include <signal.h>
#include <sys/wait.h>
#include <netdb.h>
#include <sys/resource.h>
#include <regex.h>
#include <wordexp.h>
#include "s7.h"

static s7_pointer struct_msghdr__symbol, const_struct_msghdr__symbol, const_struct_sockaddr__symbol, struct_addrinfo__symbol, netent__symbol, protoent__symbol, servent__symbol, hostent__symbol, struct_rlimit__symbol, sigset_t__symbol, glob_t__symbol, wordexp_t__symbol, passwd__symbol, tm__symbol, time_t__symbol, DIR__symbol, int__symbol, fpos_t__symbol, FILE__symbol, void__symbol, fenv_t__symbol, fexcept_t__symbol;

static s7_pointer g_c_pointer_to_string(s7_scheme *sc, s7_pointer args) 
                  {return(s7_make_string_with_length(sc, (const char *)s7_c_pointer(s7_car(args)), s7_integer(s7_cadr(args))));}
                  static s7_pointer g_string_to_c_pointer(s7_scheme *sc, s7_pointer args)
                  {
                   if (s7_is_string(s7_car(args)))
                     return(s7_make_c_pointer_with_type(sc, (void *)s7_string(s7_car(args)), s7_make_symbol(sc, "void*"), s7_f(sc)));
                   return(s7_car(args));
                  }

/* -------- isalnum -------- */
static s7_pointer s7__isalnum(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__isalnum_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__isalnum_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)isalnum(s7__isalnum_0)));
}
static s7_int isalnum_i_i(s7_int i1) {return(isalnum(i1));}


/* -------- isalpha -------- */
static s7_pointer s7__isalpha(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__isalpha_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__isalpha_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)isalpha(s7__isalpha_0)));
}
static s7_int isalpha_i_i(s7_int i1) {return(isalpha(i1));}


/* -------- iscntrl -------- */
static s7_pointer s7__iscntrl(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__iscntrl_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__iscntrl_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)iscntrl(s7__iscntrl_0)));
}
static s7_int iscntrl_i_i(s7_int i1) {return(iscntrl(i1));}


/* -------- isdigit -------- */
static s7_pointer s7__isdigit(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__isdigit_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__isdigit_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)isdigit(s7__isdigit_0)));
}
static s7_int isdigit_i_i(s7_int i1) {return(isdigit(i1));}


/* -------- islower -------- */
static s7_pointer s7__islower(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__islower_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__islower_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)islower(s7__islower_0)));
}
static s7_int islower_i_i(s7_int i1) {return(islower(i1));}


/* -------- isgraph -------- */
static s7_pointer s7__isgraph(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__isgraph_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__isgraph_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)isgraph(s7__isgraph_0)));
}
static s7_int isgraph_i_i(s7_int i1) {return(isgraph(i1));}


/* -------- isprint -------- */
static s7_pointer s7__isprint(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__isprint_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__isprint_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)isprint(s7__isprint_0)));
}
static s7_int isprint_i_i(s7_int i1) {return(isprint(i1));}


/* -------- ispunct -------- */
static s7_pointer s7__ispunct(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__ispunct_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__ispunct_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)ispunct(s7__ispunct_0)));
}
static s7_int ispunct_i_i(s7_int i1) {return(ispunct(i1));}


/* -------- isspace -------- */
static s7_pointer s7__isspace(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__isspace_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__isspace_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)isspace(s7__isspace_0)));
}
static s7_int isspace_i_i(s7_int i1) {return(isspace(i1));}


/* -------- isupper -------- */
static s7_pointer s7__isupper(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__isupper_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__isupper_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)isupper(s7__isupper_0)));
}
static s7_int isupper_i_i(s7_int i1) {return(isupper(i1));}


/* -------- isxdigit -------- */
static s7_pointer s7__isxdigit(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__isxdigit_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__isxdigit_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)isxdigit(s7__isxdigit_0)));
}
static s7_int isxdigit_i_i(s7_int i1) {return(isxdigit(i1));}


/* -------- tolower -------- */
static s7_pointer s7__tolower(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__tolower_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tolower_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)tolower(s7__tolower_0)));
}
static s7_int tolower_i_i(s7_int i1) {return(tolower(i1));}


/* -------- toupper -------- */
static s7_pointer s7__toupper(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__toupper_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__toupper_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)toupper(s7__toupper_0)));
}
static s7_int toupper_i_i(s7_int i1) {return(toupper(i1));}


/* -------- fcntl -------- */
static s7_pointer s7__fcntl(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__fcntl_0;
  int s7__fcntl_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fcntl_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fcntl_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)fcntl(s7__fcntl_0, s7__fcntl_1)));
}
static s7_int fcntl_i_ii(s7_int i1, s7_int i2) {return(fcntl(i1, i2));}

static s7_pointer g_c_open(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer arg;
                    char* name;
                    int flags, mode;
                    arg = args;
                    if (s7_is_string(s7_car(arg)))
                       name = (char*)s7_string(s7_car(arg));
                    else return(s7_wrong_type_arg_error(sc, "open", 1, s7_car(arg), "string"));
                    arg = s7_cdr(arg);
                    if (s7_is_integer(s7_car(arg)))
                       flags = (int)s7_integer(s7_car(arg));
                    else return(s7_wrong_type_arg_error(sc, "open", 2, s7_car(arg), "integer"));
                    if (s7_is_pair(s7_cdr(arg)))
                      {
                        arg = s7_cdr(arg);
                        if (s7_is_integer(s7_car(arg)))
                          mode = (int)s7_integer(s7_car(arg));
                        else return(s7_wrong_type_arg_error(sc, "open", 3, s7_car(arg), "integer"));
                        return(s7_make_integer(sc, (s7_int)open(name, flags, mode)));
                       }
                     return(s7_make_integer(sc, (s7_int)open(name, flags)));
                    }

/* -------- creat -------- */
static s7_pointer s7__creat(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__creat_0;
  mode_t s7__creat_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__creat_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__creat_1 = (mode_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)creat(s7__creat_0, s7__creat_1)));
}


/* -------- lockf -------- */
static s7_pointer s7__lockf(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__lockf_0;
  int s7__lockf_1;
  int s7__lockf_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__lockf_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__lockf_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__lockf_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)lockf(s7__lockf_0, s7__lockf_1, s7__lockf_2)));
}


/* -------- feclearexcept -------- */
static s7_pointer s7__feclearexcept(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__feclearexcept_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__feclearexcept_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)feclearexcept(s7__feclearexcept_0)));
}
static s7_int feclearexcept_i_i(s7_int i1) {return(feclearexcept(i1));}


/* -------- fegetexceptflag -------- */
static s7_pointer s7__fegetexceptflag(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  fexcept_t* s7__fegetexceptflag_0;
  int s7__fegetexceptflag_1;
  p = args;
  arg = s7_car(p);
  s7__fegetexceptflag_0 = (fexcept_t*)s7_c_pointer_with_type(sc, arg, fexcept_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fegetexceptflag_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)fegetexceptflag(s7__fegetexceptflag_0, s7__fegetexceptflag_1)));
}


/* -------- feraiseexcept -------- */
static s7_pointer s7__feraiseexcept(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__feraiseexcept_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__feraiseexcept_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)feraiseexcept(s7__feraiseexcept_0)));
}
static s7_int feraiseexcept_i_i(s7_int i1) {return(feraiseexcept(i1));}


/* -------- fesetexceptflag -------- */
static s7_pointer s7__fesetexceptflag(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  fexcept_t* s7__fesetexceptflag_0;
  int s7__fesetexceptflag_1;
  p = args;
  arg = s7_car(p);
  s7__fesetexceptflag_0 = (fexcept_t*)s7_c_pointer_with_type(sc, arg, fexcept_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fesetexceptflag_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)fesetexceptflag(s7__fesetexceptflag_0, s7__fesetexceptflag_1)));
}


/* -------- fetestexcept -------- */
static s7_pointer s7__fetestexcept(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__fetestexcept_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fetestexcept_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)fetestexcept(s7__fetestexcept_0)));
}
static s7_int fetestexcept_i_i(s7_int i1) {return(fetestexcept(i1));}


/* -------- fegetround -------- */
static s7_pointer s7__fegetround(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)fegetround()));
}


/* -------- fesetround -------- */
static s7_pointer s7__fesetround(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__fesetround_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fesetround_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)fesetround(s7__fesetround_0)));
}
static s7_int fesetround_i_i(s7_int i1) {return(fesetround(i1));}


/* -------- fegetenv -------- */
static s7_pointer s7__fegetenv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  fenv_t* s7__fegetenv_0;
  p = args;
  arg = s7_car(p);
  s7__fegetenv_0 = (fenv_t*)s7_c_pointer_with_type(sc, arg, fenv_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)fegetenv(s7__fegetenv_0)));
}


/* -------- feholdexcept -------- */
static s7_pointer s7__feholdexcept(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  fenv_t* s7__feholdexcept_0;
  p = args;
  arg = s7_car(p);
  s7__feholdexcept_0 = (fenv_t*)s7_c_pointer_with_type(sc, arg, fenv_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)feholdexcept(s7__feholdexcept_0)));
}


/* -------- fesetenv -------- */
static s7_pointer s7__fesetenv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  fenv_t* s7__fesetenv_0;
  p = args;
  arg = s7_car(p);
  s7__fesetenv_0 = (fenv_t*)s7_c_pointer_with_type(sc, arg, fenv_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)fesetenv(s7__fesetenv_0)));
}


/* -------- feupdateenv -------- */
static s7_pointer s7__feupdateenv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  fenv_t* s7__feupdateenv_0;
  p = args;
  arg = s7_car(p);
  s7__feupdateenv_0 = (fenv_t*)s7_c_pointer_with_type(sc, arg, fenv_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)feupdateenv(s7__feupdateenv_0)));
}

static s7_pointer g_fenv_t_make(s7_scheme *sc, s7_pointer args) 
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(fenv_t)), s7_make_symbol(sc, "fenv_t*"), s7_f(sc)));}

/* -------- fnmatch -------- */
static s7_pointer s7__fnmatch(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__fnmatch_0;
  char* s7__fnmatch_1;
  int s7__fnmatch_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__fnmatch_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__fnmatch_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fnmatch_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)fnmatch(s7__fnmatch_0, s7__fnmatch_1, s7__fnmatch_2)));
}


/* -------- memcpy -------- */
static s7_pointer s7__memcpy(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__memcpy_0;
  void* s7__memcpy_1;
  size_t s7__memcpy_2;
  p = args;
  arg = s7_car(p);
  s7__memcpy_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__memcpy_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__memcpy_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)memcpy(s7__memcpy_0, s7__memcpy_1, s7__memcpy_2), void__symbol, s7_f(sc)));
}


/* -------- memmove -------- */
static s7_pointer s7__memmove(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__memmove_0;
  void* s7__memmove_1;
  size_t s7__memmove_2;
  p = args;
  arg = s7_car(p);
  s7__memmove_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__memmove_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__memmove_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)memmove(s7__memmove_0, s7__memmove_1, s7__memmove_2), void__symbol, s7_f(sc)));
}


/* -------- memset -------- */
static s7_pointer s7__memset(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__memset_0;
  int s7__memset_1;
  size_t s7__memset_2;
  p = args;
  arg = s7_car(p);
  s7__memset_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__memset_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__memset_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)memset(s7__memset_0, s7__memset_1, s7__memset_2), void__symbol, s7_f(sc)));
}


/* -------- memcmp -------- */
static s7_pointer s7__memcmp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__memcmp_0;
  void* s7__memcmp_1;
  size_t s7__memcmp_2;
  p = args;
  arg = s7_car(p);
  s7__memcmp_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__memcmp_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__memcmp_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)memcmp(s7__memcmp_0, s7__memcmp_1, s7__memcmp_2)));
}


/* -------- memchr -------- */
static s7_pointer s7__memchr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__memchr_0;
  int s7__memchr_1;
  size_t s7__memchr_2;
  p = args;
  arg = s7_car(p);
  s7__memchr_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__memchr_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__memchr_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)memchr(s7__memchr_0, s7__memchr_1, s7__memchr_2), void__symbol, s7_f(sc)));
}


/* -------- strcpy -------- */
static s7_pointer s7__strcpy(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strcpy_0;
  char* s7__strcpy_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcpy_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcpy_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_string(sc, (char*)strcpy(s7__strcpy_0, s7__strcpy_1)));
}


/* -------- strncpy -------- */
static s7_pointer s7__strncpy(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strncpy_0;
  char* s7__strncpy_1;
  size_t s7__strncpy_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strncpy_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strncpy_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__strncpy_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_string(sc, (char*)strncpy(s7__strncpy_0, s7__strncpy_1, s7__strncpy_2)));
}


/* -------- strcat -------- */
static s7_pointer s7__strcat(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strcat_0;
  char* s7__strcat_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcat_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcat_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_string(sc, (char*)strcat(s7__strcat_0, s7__strcat_1)));
}


/* -------- strncat -------- */
static s7_pointer s7__strncat(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strncat_0;
  char* s7__strncat_1;
  size_t s7__strncat_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strncat_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strncat_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__strncat_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_string(sc, (char*)strncat(s7__strncat_0, s7__strncat_1, s7__strncat_2)));
}


/* -------- strcmp -------- */
static s7_pointer s7__strcmp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strcmp_0;
  char* s7__strcmp_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcmp_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcmp_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_integer(sc, (s7_int)strcmp(s7__strcmp_0, s7__strcmp_1)));
}


/* -------- strncmp -------- */
static s7_pointer s7__strncmp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strncmp_0;
  char* s7__strncmp_1;
  size_t s7__strncmp_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strncmp_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strncmp_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__strncmp_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)strncmp(s7__strncmp_0, s7__strncmp_1, s7__strncmp_2)));
}


/* -------- strcoll -------- */
static s7_pointer s7__strcoll(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strcoll_0;
  char* s7__strcoll_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcoll_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcoll_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_integer(sc, (s7_int)strcoll(s7__strcoll_0, s7__strcoll_1)));
}


/* -------- strxfrm -------- */
static s7_pointer s7__strxfrm(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strxfrm_0;
  char* s7__strxfrm_1;
  size_t s7__strxfrm_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strxfrm_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strxfrm_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__strxfrm_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)strxfrm(s7__strxfrm_0, s7__strxfrm_1, s7__strxfrm_2)));
}


/* -------- strchr -------- */
static s7_pointer s7__strchr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strchr_0;
  int s7__strchr_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strchr_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__strchr_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_string(sc, (char*)strchr(s7__strchr_0, s7__strchr_1)));
}


/* -------- strrchr -------- */
static s7_pointer s7__strrchr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strrchr_0;
  int s7__strrchr_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strrchr_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__strrchr_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_string(sc, (char*)strrchr(s7__strrchr_0, s7__strrchr_1)));
}


/* -------- strcspn -------- */
static s7_pointer s7__strcspn(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strcspn_0;
  char* s7__strcspn_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcspn_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcspn_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_integer(sc, (s7_int)strcspn(s7__strcspn_0, s7__strcspn_1)));
}


/* -------- strspn -------- */
static s7_pointer s7__strspn(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strspn_0;
  char* s7__strspn_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strspn_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strspn_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_integer(sc, (s7_int)strspn(s7__strspn_0, s7__strspn_1)));
}


/* -------- strpbrk -------- */
static s7_pointer s7__strpbrk(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strpbrk_0;
  char* s7__strpbrk_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strpbrk_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strpbrk_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_string(sc, (char*)strpbrk(s7__strpbrk_0, s7__strpbrk_1)));
}


/* -------- strstr -------- */
static s7_pointer s7__strstr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strstr_0;
  char* s7__strstr_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strstr_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strstr_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_string(sc, (char*)strstr(s7__strstr_0, s7__strstr_1)));
}


/* -------- strtok -------- */
static s7_pointer s7__strtok(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strtok_0;
  char* s7__strtok_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strtok_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strtok_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_string(sc, (char*)strtok(s7__strtok_0, s7__strtok_1)));
}


/* -------- strlen -------- */
static s7_pointer s7__strlen(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strlen_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strlen_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)strlen(s7__strlen_0)));
}


/* -------- strerror -------- */
static s7_pointer s7__strerror(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__strerror_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__strerror_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_string(sc, (char*)strerror(s7__strerror_0)));
}


/* -------- strcasecmp -------- */
static s7_pointer s7__strcasecmp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strcasecmp_0;
  char* s7__strcasecmp_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcasecmp_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strcasecmp_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_integer(sc, (s7_int)strcasecmp(s7__strcasecmp_0, s7__strcasecmp_1)));
}


/* -------- strncasecmp -------- */
static s7_pointer s7__strncasecmp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__strncasecmp_0;
  char* s7__strncasecmp_1;
  size_t s7__strncasecmp_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strncasecmp_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__strncasecmp_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__strncasecmp_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)strncasecmp(s7__strncasecmp_0, s7__strncasecmp_1, s7__strncasecmp_2)));
}


/* -------- remove -------- */
static s7_pointer s7__remove(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__remove_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__remove_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)remove(s7__remove_0)));
}


/* -------- rename -------- */
static s7_pointer s7__rename(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__rename_0;
  char* s7__rename_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__rename_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__rename_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_integer(sc, (s7_int)rename(s7__rename_0, s7__rename_1)));
}


/* -------- tmpfile -------- */
static s7_pointer s7__tmpfile(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void*)tmpfile(), FILE__symbol, s7_f(sc)));
}


/* -------- fclose -------- */
static s7_pointer s7__fclose(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__fclose_0;
  p = args;
  arg = s7_car(p);
  s7__fclose_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)fclose(s7__fclose_0)));
}


/* -------- fflush -------- */
static s7_pointer s7__fflush(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__fflush_0;
  p = args;
  arg = s7_car(p);
  s7__fflush_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)fflush(s7__fflush_0)));
}


/* -------- fopen -------- */
static s7_pointer s7__fopen(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__fopen_0;
  char* s7__fopen_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__fopen_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__fopen_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)fopen(s7__fopen_0, s7__fopen_1), FILE__symbol, s7_f(sc)));
}


/* -------- freopen -------- */
static s7_pointer s7__freopen(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__freopen_0;
  char* s7__freopen_1;
  FILE* s7__freopen_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__freopen_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__freopen_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__freopen_2 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 3);
  return(s7_make_c_pointer_with_type(sc, (void*)freopen(s7__freopen_0, s7__freopen_1, s7__freopen_2), FILE__symbol, s7_f(sc)));
}


/* -------- fdopen -------- */
static s7_pointer s7__fdopen(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__fdopen_0;
  char* s7__fdopen_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fdopen_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__fdopen_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)fdopen(s7__fdopen_0, s7__fdopen_1), FILE__symbol, s7_f(sc)));
}


/* -------- setbuf -------- */
static s7_pointer s7__setbuf(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__setbuf_0;
  char* s7__setbuf_1;
  p = args;
  arg = s7_car(p);
  s7__setbuf_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__setbuf_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  setbuf(s7__setbuf_0, s7__setbuf_1);
  return(s7_unspecified(sc));
}


/* -------- setvbuf -------- */
static s7_pointer s7__setvbuf(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__setvbuf_0;
  char* s7__setvbuf_1;
  int s7__setvbuf_2;
  size_t s7__setvbuf_3;
  p = args;
  arg = s7_car(p);
  s7__setvbuf_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__setvbuf_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setvbuf_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setvbuf_3 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 4, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)setvbuf(s7__setvbuf_0, s7__setvbuf_1, s7__setvbuf_2, s7__setvbuf_3)));
}


/* -------- setlinebuf -------- */
static s7_pointer s7__setlinebuf(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__setlinebuf_0;
  p = args;
  arg = s7_car(p);
  s7__setlinebuf_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  setlinebuf(s7__setlinebuf_0);
  return(s7_unspecified(sc));
}


/* -------- fgetc -------- */
static s7_pointer s7__fgetc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__fgetc_0;
  p = args;
  arg = s7_car(p);
  s7__fgetc_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)fgetc(s7__fgetc_0)));
}


/* -------- getc -------- */
static s7_pointer s7__getc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__getc_0;
  p = args;
  arg = s7_car(p);
  s7__getc_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)getc(s7__getc_0)));
}


/* -------- getchar -------- */
static s7_pointer s7__getchar(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)getchar()));
}


/* -------- fputc -------- */
static s7_pointer s7__fputc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__fputc_0;
  FILE* s7__fputc_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fputc_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__fputc_1 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 2);
  return(s7_make_integer(sc, (s7_int)fputc(s7__fputc_0, s7__fputc_1)));
}


/* -------- putc -------- */
static s7_pointer s7__putc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__putc_0;
  FILE* s7__putc_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__putc_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__putc_1 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 2);
  return(s7_make_integer(sc, (s7_int)putc(s7__putc_0, s7__putc_1)));
}


/* -------- putchar -------- */
static s7_pointer s7__putchar(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__putchar_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__putchar_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)putchar(s7__putchar_0)));
}
static s7_int putchar_i_i(s7_int i1) {return(putchar(i1));}


/* -------- fgets -------- */
static s7_pointer s7__fgets(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__fgets_0;
  int s7__fgets_1;
  FILE* s7__fgets_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__fgets_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fgets_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__fgets_2 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 3);
  return(s7_make_string(sc, (char*)fgets(s7__fgets_0, s7__fgets_1, s7__fgets_2)));
}


/* -------- fputs -------- */
static s7_pointer s7__fputs(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__fputs_0;
  FILE* s7__fputs_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__fputs_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__fputs_1 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 2);
  return(s7_make_integer(sc, (s7_int)fputs(s7__fputs_0, s7__fputs_1)));
}


/* -------- puts -------- */
static s7_pointer s7__puts(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__puts_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__puts_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)puts(s7__puts_0)));
}


/* -------- ungetc -------- */
static s7_pointer s7__ungetc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__ungetc_0;
  FILE* s7__ungetc_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__ungetc_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__ungetc_1 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 2);
  return(s7_make_integer(sc, (s7_int)ungetc(s7__ungetc_0, s7__ungetc_1)));
}


/* -------- fread -------- */
static s7_pointer s7__fread(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__fread_0;
  size_t s7__fread_1;
  size_t s7__fread_2;
  FILE* s7__fread_3;
  p = args;
  arg = s7_car(p);
  s7__fread_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fread_1 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fread_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__fread_3 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 4);
  return(s7_make_integer(sc, (s7_int)fread(s7__fread_0, s7__fread_1, s7__fread_2, s7__fread_3)));
}


/* -------- fwrite -------- */
static s7_pointer s7__fwrite(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__fwrite_0;
  size_t s7__fwrite_1;
  size_t s7__fwrite_2;
  FILE* s7__fwrite_3;
  p = args;
  arg = s7_car(p);
  s7__fwrite_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fwrite_1 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fwrite_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__fwrite_3 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 4);
  return(s7_make_integer(sc, (s7_int)fwrite(s7__fwrite_0, s7__fwrite_1, s7__fwrite_2, s7__fwrite_3)));
}


/* -------- fseek -------- */
static s7_pointer s7__fseek(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__fseek_0;
  int s7__fseek_1;
  int s7__fseek_2;
  p = args;
  arg = s7_car(p);
  s7__fseek_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fseek_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fseek_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)fseek(s7__fseek_0, s7__fseek_1, s7__fseek_2)));
}


/* -------- ftell -------- */
static s7_pointer s7__ftell(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__ftell_0;
  p = args;
  arg = s7_car(p);
  s7__ftell_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)ftell(s7__ftell_0)));
}


/* -------- rewind -------- */
static s7_pointer s7__rewind(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__rewind_0;
  p = args;
  arg = s7_car(p);
  s7__rewind_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  rewind(s7__rewind_0);
  return(s7_unspecified(sc));
}


/* -------- fgetpos -------- */
static s7_pointer s7__fgetpos(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__fgetpos_0;
  fpos_t* s7__fgetpos_1;
  p = args;
  arg = s7_car(p);
  s7__fgetpos_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__fgetpos_1 = (fpos_t*)s7_c_pointer_with_type(sc, arg, fpos_t__symbol, __func__, 2);
  return(s7_make_integer(sc, (s7_int)fgetpos(s7__fgetpos_0, s7__fgetpos_1)));
}


/* -------- fsetpos -------- */
static s7_pointer s7__fsetpos(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__fsetpos_0;
  fpos_t* s7__fsetpos_1;
  p = args;
  arg = s7_car(p);
  s7__fsetpos_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__fsetpos_1 = (fpos_t*)s7_c_pointer_with_type(sc, arg, fpos_t__symbol, __func__, 2);
  return(s7_make_integer(sc, (s7_int)fsetpos(s7__fsetpos_0, s7__fsetpos_1)));
}


/* -------- clearerr -------- */
static s7_pointer s7__clearerr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__clearerr_0;
  p = args;
  arg = s7_car(p);
  s7__clearerr_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  clearerr(s7__clearerr_0);
  return(s7_unspecified(sc));
}


/* -------- feof -------- */
static s7_pointer s7__feof(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__feof_0;
  p = args;
  arg = s7_car(p);
  s7__feof_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)feof(s7__feof_0)));
}


/* -------- ferror -------- */
static s7_pointer s7__ferror(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__ferror_0;
  p = args;
  arg = s7_car(p);
  s7__ferror_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)ferror(s7__ferror_0)));
}


/* -------- perror -------- */
static s7_pointer s7__perror(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__perror_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__perror_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  perror(s7__perror_0);
  return(s7_unspecified(sc));
}


/* -------- fileno -------- */
static s7_pointer s7__fileno(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__fileno_0;
  p = args;
  arg = s7_car(p);
  s7__fileno_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)fileno(s7__fileno_0)));
}


/* -------- popen -------- */
static s7_pointer s7__popen(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__popen_0;
  char* s7__popen_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__popen_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__popen_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)popen(s7__popen_0, s7__popen_1), FILE__symbol, s7_f(sc)));
}


/* -------- pclose -------- */
static s7_pointer s7__pclose(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__pclose_0;
  p = args;
  arg = s7_car(p);
  s7__pclose_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)pclose(s7__pclose_0)));
}


/* -------- ctermid -------- */
static s7_pointer s7__ctermid(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__ctermid_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__ctermid_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_string(sc, (char*)ctermid(s7__ctermid_0)));
}


/* -------- flockfile -------- */
static s7_pointer s7__flockfile(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__flockfile_0;
  p = args;
  arg = s7_car(p);
  s7__flockfile_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  flockfile(s7__flockfile_0);
  return(s7_unspecified(sc));
}


/* -------- ftrylockfile -------- */
static s7_pointer s7__ftrylockfile(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__ftrylockfile_0;
  p = args;
  arg = s7_car(p);
  s7__ftrylockfile_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)ftrylockfile(s7__ftrylockfile_0)));
}


/* -------- funlockfile -------- */
static s7_pointer s7__funlockfile(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* s7__funlockfile_0;
  p = args;
  arg = s7_car(p);
  s7__funlockfile_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 0);
  funlockfile(s7__funlockfile_0);
  return(s7_unspecified(sc));
}


/* -------- atof -------- */
static s7_pointer s7__atof(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__atof_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__atof_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_real(sc, (s7_double)atof(s7__atof_0)));
}


/* -------- atoi -------- */
static s7_pointer s7__atoi(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__atoi_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__atoi_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)atoi(s7__atoi_0)));
}


/* -------- atol -------- */
static s7_pointer s7__atol(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__atol_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__atol_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)atol(s7__atol_0)));
}


/* -------- atoll -------- */
static s7_pointer s7__atoll(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__atoll_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__atoll_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)atoll(s7__atoll_0)));
}


/* -------- random -------- */
static s7_pointer s7__random(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)random()));
}


/* -------- srandom -------- */
static s7_pointer s7__srandom(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__srandom_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__srandom_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  srandom(s7__srandom_0);
  return(s7_unspecified(sc));
}


/* -------- initstate -------- */
static s7_pointer s7__initstate(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__initstate_0;
  char* s7__initstate_1;
  size_t s7__initstate_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__initstate_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__initstate_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__initstate_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_string(sc, (char*)initstate(s7__initstate_0, s7__initstate_1, s7__initstate_2)));
}


/* -------- setstate -------- */
static s7_pointer s7__setstate(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__setstate_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__setstate_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_string(sc, (char*)setstate(s7__setstate_0)));
}


/* -------- rand -------- */
static s7_pointer s7__rand(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)rand()));
}


/* -------- srand -------- */
static s7_pointer s7__srand(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__srand_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__srand_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  srand(s7__srand_0);
  return(s7_unspecified(sc));
}


/* -------- malloc -------- */
static s7_pointer s7__malloc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  size_t s7__malloc_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__malloc_0 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)malloc(s7__malloc_0), void__symbol, s7_f(sc)));
}


/* -------- calloc -------- */
static s7_pointer s7__calloc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  size_t s7__calloc_0;
  size_t s7__calloc_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__calloc_0 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__calloc_1 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)calloc(s7__calloc_0, s7__calloc_1), void__symbol, s7_f(sc)));
}


/* -------- realloc -------- */
static s7_pointer s7__realloc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__realloc_0;
  size_t s7__realloc_1;
  p = args;
  arg = s7_car(p);
  s7__realloc_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__realloc_1 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)realloc(s7__realloc_0, s7__realloc_1), void__symbol, s7_f(sc)));
}


/* -------- abort -------- */
static s7_pointer s7__abort(s7_scheme *sc, s7_pointer args)
{
  abort();
  return(s7_unspecified(sc));
}


/* -------- exit -------- */
static s7_pointer s7__exit(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__exit_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__exit_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  exit(s7__exit_0);
  return(s7_unspecified(sc));
}


/* -------- getenv -------- */
static s7_pointer s7__getenv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__getenv_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__getenv_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_string(sc, (char*)getenv(s7__getenv_0)));
}


/* -------- putenv -------- */
static s7_pointer s7__putenv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__putenv_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__putenv_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)putenv(s7__putenv_0)));
}


/* -------- setenv -------- */
static s7_pointer s7__setenv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__setenv_0;
  char* s7__setenv_1;
  int s7__setenv_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__setenv_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__setenv_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setenv_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)setenv(s7__setenv_0, s7__setenv_1, s7__setenv_2)));
}


/* -------- unsetenv -------- */
static s7_pointer s7__unsetenv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__unsetenv_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__unsetenv_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)unsetenv(s7__unsetenv_0)));
}


/* -------- mkstemp -------- */
static s7_pointer s7__mkstemp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__mkstemp_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__mkstemp_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)mkstemp(s7__mkstemp_0)));
}


/* -------- system -------- */
/* obazl avoid clash with g_system defined in s7.c (WITH_SYSTEM_EXTRAS) */
/* static s7_pointer s7__system(s7_scheme *sc, s7_pointer args) */
/* { */
/*   s7_pointer p, arg; */
/*   char* s7__system_0; */
/*   p = args; */
/*   arg = s7_car(p); */
/*   if (s7_is_string(arg)) */
/*     s7__system_0 = (char*)s7_string(arg); */
/*   else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string")); */
/*   return(s7_make_integer(sc, (s7_int)system(s7__system_0))); */
/* } */


/* -------- abs -------- */
static s7_pointer s7__abs(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__abs_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__abs_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)abs(s7__abs_0)));
}
/*obazl -Wunused-function static s7_int abs_i_i(s7_int i1) {return(llabs(i1));} */


/* -------- labs -------- */
static s7_pointer s7__labs(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__labs_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__labs_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)labs(s7__labs_0)));
}
static s7_int labs_i_i(s7_int i1) {return(labs(i1));}

static s7_pointer g_llabs(s7_scheme *sc, s7_pointer args) 
                  {
                  #if  ((__GNUC__) && ((__GNUC__ < 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ < 4))))
                    return(s7_make_integer(sc, labs(s7_integer(s7_car(args)))));
                  #else
                    return(s7_make_integer(sc, llabs(s7_integer(s7_car(args)))));
                  #endif
                  }
                 static s7_pointer g_realpath(s7_scheme *sc, s7_pointer args)
                  {
                    char *s7_dl_realpath_0, *res;
                    if (s7_is_string(s7_car(args)))
                       s7_dl_realpath_0 = (char*)s7_string(s7_car(args));
                    else return(s7_wrong_type_arg_error(sc, "realpath", 1, s7_car(args), "string"));
                    res = realpath(s7_dl_realpath_0, NULL);
                    if (res) {s7_pointer str; str = s7_make_string(sc, res); free(res); return(str);}
                    return(s7_f(sc));
                 }
                 static s7_pointer g_free(s7_scheme *sc, s7_pointer args)
                 {free(s7_c_pointer(s7_car(args))); return(s7_f(sc));}
                 static s7_pointer g_strtod(s7_scheme *sc, s7_pointer args) 
                 {return(s7_make_real(sc, strtod(s7_string(s7_car(args)), NULL)));}
                 static s7_pointer g_strtof(s7_scheme *sc, s7_pointer args) 
                 {return(s7_make_real(sc, strtof(s7_string(s7_car(args)), NULL)));}
                 static s7_pointer g_strtol(s7_scheme *sc, s7_pointer args) 
                 {return(s7_make_integer(sc, strtol(s7_string(s7_car(args)), NULL, s7_integer(s7_cadr(args)))));}
                 static s7_pointer g_strtoll(s7_scheme *sc, s7_pointer args)
                 {return(s7_make_integer(sc, strtoll(s7_string(s7_car(args)), NULL, s7_integer(s7_cadr(args)))));}
                 static s7_pointer g_ldiv(s7_scheme *sc, s7_pointer args)
                 {
                   ldiv_t d;
                   if (!s7_is_integer(s7_car(args))) return(s7_wrong_type_arg_error(sc, "ldiv", 1, s7_car(args), "integer"));
                   if (!s7_is_integer(s7_cadr(args))) return(s7_wrong_type_arg_error(sc, "ldiv", 2, s7_cadr(args), "integer"));
                   d = ldiv(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)));
                   return(s7_list(sc, 2, s7_make_integer(sc, d.quot), s7_make_integer(sc, d.rem)));
                 }
                  
static s7_pointer g_errno(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, errno));}
                  static s7_pointer g_set_errno(s7_scheme *sc, s7_pointer args) {errno = (int)s7_integer(s7_car(args)); return(s7_car(args));}

/* -------- setlocale -------- */
static s7_pointer s7__setlocale(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__setlocale_0;
  char* s7__setlocale_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setlocale_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__setlocale_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_string(sc, (char*)setlocale(s7__setlocale_0, s7__setlocale_1)));
}


             static s7_pointer g_localeconv(s7_scheme *sc, s7_pointer args)
             {
               struct lconv *lc;
               lc = localeconv();
               return(s7_inlet(sc, s7_list(sc, 36,
             		     s7_make_symbol(sc, "decimal_point"),     s7_make_string(sc, lc->decimal_point),
             		     s7_make_symbol(sc, "thousands_sep"),     s7_make_string(sc, lc->thousands_sep),
             		     s7_make_symbol(sc, "grouping"),          s7_make_string(sc, lc->grouping),
             		     s7_make_symbol(sc, "int_curr_symbol"),   s7_make_string(sc, lc->int_curr_symbol),
             		     s7_make_symbol(sc, "currency_symbol"),   s7_make_string(sc, lc->currency_symbol),
             		     s7_make_symbol(sc, "mon_decimal_point"), s7_make_string(sc, lc->mon_decimal_point),
             		     s7_make_symbol(sc, "mon_thousands_sep"), s7_make_string(sc, lc->mon_thousands_sep),
             		     s7_make_symbol(sc, "mon_grouping"),      s7_make_string(sc, lc->mon_grouping),
             		     s7_make_symbol(sc, "positive_sign"),     s7_make_string(sc, lc->positive_sign),
             		     s7_make_symbol(sc, "negative_sign"),     s7_make_string(sc, lc->negative_sign),
             		     
             		     s7_make_symbol(sc, "int_frac_digits"),   s7_make_integer(sc, lc->int_frac_digits),
             		     s7_make_symbol(sc, "frac_digits"),       s7_make_integer(sc, lc->frac_digits),
             		     s7_make_symbol(sc, "p_cs_precedes"),     s7_make_integer(sc, lc->p_cs_precedes),
             		     s7_make_symbol(sc, "p_sep_by_space"),    s7_make_integer(sc, lc->p_sep_by_space),
             		     s7_make_symbol(sc, "n_cs_precedes"),     s7_make_integer(sc, lc->n_cs_precedes),
             		     s7_make_symbol(sc, "n_sep_by_space"),    s7_make_integer(sc, lc->n_sep_by_space),
             		     s7_make_symbol(sc, "p_sign_posn"),       s7_make_integer(sc, lc->p_sign_posn),
             		     s7_make_symbol(sc, "n_sign_posn"),       s7_make_integer(sc, lc->n_sign_posn))));
              }

             static s7_pointer g_uname(s7_scheme *sc, s7_pointer args)
             {
               struct utsname buf;
               uname(&buf);
               return(s7_list(sc, 5, s7_make_string(sc, buf.sysname), 
             		        s7_make_string(sc, buf.machine), 
             		        s7_make_string(sc, buf.nodename), 
             		        s7_make_string(sc, buf.version), 
             		        s7_make_string(sc, buf.release)));
             }

/* -------- access -------- */
static s7_pointer s7__access(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__access_0;
  int s7__access_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__access_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__access_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)access(s7__access_0, s7__access_1)));
}


/* -------- lseek -------- */
static s7_pointer s7__lseek(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__lseek_0;
  int s7__lseek_1;
  int s7__lseek_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__lseek_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__lseek_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__lseek_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)lseek(s7__lseek_0, s7__lseek_1, s7__lseek_2)));
}


/* -------- close -------- */
static s7_pointer s7__close(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__close_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__close_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)close(s7__close_0)));
}
static s7_int close_i_i(s7_int i1) {return(close(i1));}


/* -------- read -------- */
static s7_pointer s7__read(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__read_0;
  void* s7__read_1;
  size_t s7__read_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__read_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__read_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__read_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)read(s7__read_0, s7__read_1, s7__read_2)));
}


/* -------- write -------- */
/* static s7_pointer s7__write(s7_scheme *sc, s7_pointer args) */
/* { */
/*   s7_pointer p, arg; */
/*   int s7__write_0; */
/*   void* s7__write_1; */
/*   size_t s7__write_2; */
/*   p = args; */
/*   arg = s7_car(p); */
/*   if (s7_is_integer(arg)) */
/*     s7__write_0 = (int)s7_integer(arg); */
/*   else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer")); */
/*   p = s7_cdr(p); */
/*   arg = s7_car(p); */
/*   s7__write_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2); */
/*   p = s7_cdr(p); */
/*   arg = s7_car(p); */
/*   if (s7_is_integer(arg)) */
/*     s7__write_2 = (size_t)s7_integer(arg); */
/*   else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer")); */
/*   return(s7_make_integer(sc, (s7_int)write(s7__write_0, s7__write_1, s7__write_2))); */
/* } */


/* -------- pread -------- */
static s7_pointer s7__pread(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__pread_0;
  void* s7__pread_1;
  size_t s7__pread_2;
  int s7__pread_3;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__pread_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__pread_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__pread_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__pread_3 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 4, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)pread(s7__pread_0, s7__pread_1, s7__pread_2, s7__pread_3)));
}


/* -------- pwrite -------- */
static s7_pointer s7__pwrite(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__pwrite_0;
  void* s7__pwrite_1;
  size_t s7__pwrite_2;
  int s7__pwrite_3;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__pwrite_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__pwrite_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__pwrite_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__pwrite_3 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 4, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)pwrite(s7__pwrite_0, s7__pwrite_1, s7__pwrite_2, s7__pwrite_3)));
}


/* -------- pipe -------- */
static s7_pointer s7__pipe(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int* s7__pipe_0;
  p = args;
  arg = s7_car(p);
  s7__pipe_0 = (int*)s7_c_pointer_with_type(sc, arg, int__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)pipe(s7__pipe_0)));
}


/* -------- alarm -------- */
static s7_pointer s7__alarm(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__alarm_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__alarm_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)alarm(s7__alarm_0)));
}
static s7_int alarm_i_i(s7_int i1) {return(alarm(i1));}


/* -------- sleep -------- */
static s7_pointer s7__sleep(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__sleep_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sleep_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)sleep(s7__sleep_0)));
}
static s7_int sleep_i_i(s7_int i1) {return(sleep(i1));}


/* -------- pause -------- */
static s7_pointer s7__pause(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)pause()));
}


/* -------- chown -------- */
static s7_pointer s7__chown(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__chown_0;
  int s7__chown_1;
  int s7__chown_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__chown_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__chown_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__chown_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)chown(s7__chown_0, s7__chown_1, s7__chown_2)));
}


/* -------- chdir -------- */
static s7_pointer s7__chdir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__chdir_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__chdir_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)chdir(s7__chdir_0)));
}


/* -------- getcwd -------- */
static s7_pointer s7__getcwd(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__getcwd_0;
  size_t s7__getcwd_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__getcwd_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getcwd_1 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_string(sc, (char*)getcwd(s7__getcwd_0, s7__getcwd_1)));
}


/* -------- dup -------- */
static s7_pointer s7__dup(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__dup_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__dup_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)dup(s7__dup_0)));
}
static s7_int dup_i_i(s7_int i1) {return(dup(i1));}


/* -------- dup2 -------- */
static s7_pointer s7__dup2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__dup2_0;
  int s7__dup2_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__dup2_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__dup2_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)dup2(s7__dup2_0, s7__dup2_1)));
}
static s7_int dup2_i_ii(s7_int i1, s7_int i2) {return(dup2(i1, i2));}


/* -------- _exit -------- */
static s7_pointer s7___exit(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7___exit_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7___exit_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  _exit(s7___exit_0);
  return(s7_unspecified(sc));
}


/* -------- pathconf -------- */
static s7_pointer s7__pathconf(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__pathconf_0;
  int s7__pathconf_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__pathconf_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__pathconf_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)pathconf(s7__pathconf_0, s7__pathconf_1)));
}


/* -------- fpathconf -------- */
static s7_pointer s7__fpathconf(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__fpathconf_0;
  int s7__fpathconf_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fpathconf_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__fpathconf_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)fpathconf(s7__fpathconf_0, s7__fpathconf_1)));
}
static s7_int fpathconf_i_ii(s7_int i1, s7_int i2) {return(fpathconf(i1, i2));}


/* -------- sysconf -------- */
static s7_pointer s7__sysconf(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__sysconf_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sysconf_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)sysconf(s7__sysconf_0)));
}
static s7_int sysconf_i_i(s7_int i1) {return(sysconf(i1));}


/* -------- confstr -------- */
static s7_pointer s7__confstr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__confstr_0;
  char* s7__confstr_1;
  size_t s7__confstr_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__confstr_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__confstr_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__confstr_2 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)confstr(s7__confstr_0, s7__confstr_1, s7__confstr_2)));
}


/* -------- getpid -------- */
static s7_pointer s7__getpid(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)getpid()));
}


/* -------- getppid -------- */
static s7_pointer s7__getppid(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)getppid()));
}


/* -------- getpgid -------- */
static s7_pointer s7__getpgid(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__getpgid_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getpgid_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)getpgid(s7__getpgid_0)));
}
static s7_int getpgid_i_i(s7_int i1) {return(getpgid(i1));}


/* -------- setpgid -------- */
static s7_pointer s7__setpgid(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__setpgid_0;
  int s7__setpgid_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setpgid_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setpgid_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)setpgid(s7__setpgid_0, s7__setpgid_1)));
}
static s7_int setpgid_i_ii(s7_int i1, s7_int i2) {return(setpgid(i1, i2));}


/* -------- setsid -------- */
static s7_pointer s7__setsid(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)setsid()));
}


/* -------- getsid -------- */
static s7_pointer s7__getsid(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__getsid_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getsid_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)getsid(s7__getsid_0)));
}
static s7_int getsid_i_i(s7_int i1) {return(getsid(i1));}


/* -------- getuid -------- */
static s7_pointer s7__getuid(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)getuid()));
}


/* -------- geteuid -------- */
static s7_pointer s7__geteuid(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)geteuid()));
}


/* -------- getgid -------- */
static s7_pointer s7__getgid(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)getgid()));
}


/* -------- getegid -------- */
static s7_pointer s7__getegid(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)getegid()));
}


/* -------- setuid -------- */
static s7_pointer s7__setuid(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__setuid_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setuid_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)setuid(s7__setuid_0)));
}
static s7_int setuid_i_i(s7_int i1) {return(setuid(i1));}


/* -------- setgid -------- */
static s7_pointer s7__setgid(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__setgid_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setgid_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)setgid(s7__setgid_0)));
}
static s7_int setgid_i_i(s7_int i1) {return(setgid(i1));}


/* -------- fork -------- */
static s7_pointer s7__fork(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)fork()));
}


/* -------- ttyname -------- */
static s7_pointer s7__ttyname(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__ttyname_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__ttyname_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_string(sc, (char*)ttyname(s7__ttyname_0)));
}


/* -------- isatty -------- */
static s7_pointer s7__isatty(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__isatty_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__isatty_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)isatty(s7__isatty_0)));
}
static s7_int isatty_i_i(s7_int i1) {return(isatty(i1));}


/* -------- link -------- */
static s7_pointer s7__link(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__link_0;
  char* s7__link_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__link_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__link_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_integer(sc, (s7_int)link(s7__link_0, s7__link_1)));
}


/* -------- unlink -------- */
static s7_pointer s7__unlink(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__unlink_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__unlink_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)unlink(s7__unlink_0)));
}


/* -------- rmdir -------- */
static s7_pointer s7__rmdir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__rmdir_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__rmdir_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_integer(sc, (s7_int)rmdir(s7__rmdir_0)));
}


/* -------- tcgetpgrp -------- */
static s7_pointer s7__tcgetpgrp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__tcgetpgrp_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcgetpgrp_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)tcgetpgrp(s7__tcgetpgrp_0)));
}
static s7_int tcgetpgrp_i_i(s7_int i1) {return(tcgetpgrp(i1));}


/* -------- tcsetpgrp -------- */
static s7_pointer s7__tcsetpgrp(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__tcsetpgrp_0;
  int s7__tcsetpgrp_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcsetpgrp_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcsetpgrp_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)tcsetpgrp(s7__tcsetpgrp_0, s7__tcsetpgrp_1)));
}
static s7_int tcsetpgrp_i_ii(s7_int i1, s7_int i2) {return(tcsetpgrp(i1, i2));}


/* -------- getlogin -------- */
static s7_pointer s7__getlogin(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, (char*)getlogin()));
}


/* -------- truncate -------- */
static s7_pointer s7__truncate(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__truncate_0;
  int s7__truncate_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__truncate_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__truncate_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)truncate(s7__truncate_0, s7__truncate_1)));
}


/* -------- ftruncate -------- */
static s7_pointer s7__ftruncate(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__ftruncate_0;
  int s7__ftruncate_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__ftruncate_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__ftruncate_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)ftruncate(s7__ftruncate_0, s7__ftruncate_1)));
}
static s7_int ftruncate_i_ii(s7_int i1, s7_int i2) {return(ftruncate(i1, i2));}

extern char **environ; 
                  static s7_pointer getenvs(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    int i;
                    p = s7_nil(sc);
                    for (i = 0; environ[i]; i++)
                      {
                       const char *eq;
                       s7_pointer name, value;
                       eq = strchr((const char *)environ[i], (int)'=');
                       name = s7_make_string_with_length(sc, environ[i], eq - environ[i]);
                       value = s7_make_string(sc, (char *)(eq + 1));
                       p = s7_cons(sc, s7_cons(sc, name, value), p);
                      }
                    return(p);
           }
                  static s7_pointer g_getgroups(s7_scheme *sc, s7_pointer args)
                  {
                    gid_t *gds;
                    int i, size, res;
                    s7_pointer lst;
                    size = s7_integer(s7_car(args));
                    if (size == 0)
                      return(s7_make_integer(sc, getgroups(0, NULL)));
                    gds = (gid_t *)calloc(size, sizeof(gid_t));
                    res = getgroups(size, gds);
                    if (res != -1)
                      {
                        lst = s7_nil(sc);
                        for (i = 0; i < size; i++)
                          lst = s7_cons(sc, s7_make_integer(sc, gds[i]), lst);
                      }
                    else lst = s7_make_integer(sc, -1);
                    free(gds);
                    return(lst);
                  }
                  

/* -------- opendir -------- */
static s7_pointer s7__opendir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__opendir_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__opendir_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)opendir(s7__opendir_0), DIR__symbol, s7_f(sc)));
}


/* -------- closedir -------- */
static s7_pointer s7__closedir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  DIR* s7__closedir_0;
  p = args;
  arg = s7_car(p);
  s7__closedir_0 = (DIR*)s7_c_pointer_with_type(sc, arg, DIR__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)closedir(s7__closedir_0)));
}


/* -------- rewinddir -------- */
static s7_pointer s7__rewinddir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  DIR* s7__rewinddir_0;
  p = args;
  arg = s7_car(p);
  s7__rewinddir_0 = (DIR*)s7_c_pointer_with_type(sc, arg, DIR__symbol, __func__, 0);
  rewinddir(s7__rewinddir_0);
  return(s7_unspecified(sc));
}

static char *read_dir(DIR *p)
                           {                            
                             struct dirent *dirp;        
                             dirp = readdir(p);          
                             if (!dirp) return(NULL);    
                             else return(dirp->d_name);  
                           }

/* -------- read_dir -------- */
static s7_pointer s7__read_dir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  DIR* s7__read_dir_0;
  p = args;
  arg = s7_car(p);
  s7__read_dir_0 = (DIR*)s7_c_pointer_with_type(sc, arg, DIR__symbol, __func__, 0);
  return(s7_make_string(sc, (char*)read_dir(s7__read_dir_0)));
}

static s7_scheme *internal_ftw_sc = NULL;
                  static s7_pointer internal_ftw_closure = NULL, internal_ftw_arglist = NULL;
                           
                  static int internal_ftw_function(const char *fpath, const struct stat *sb, int typeflag)
                  {
                    s7_list_set(internal_ftw_sc, internal_ftw_arglist, 0, s7_make_string(internal_ftw_sc, fpath));
                    s7_list_set(internal_ftw_sc, internal_ftw_arglist, 1, 
                                s7_make_c_pointer_with_type(internal_ftw_sc, (void *)sb,  /* need cast due to const */
                                                            s7_make_symbol(internal_ftw_sc, "stat*"), s7_f(internal_ftw_sc)));
                    s7_list_set(internal_ftw_sc, internal_ftw_arglist, 2, s7_make_integer(internal_ftw_sc, typeflag));
                    return((int)s7_integer(s7_call(internal_ftw_sc, internal_ftw_closure, internal_ftw_arglist)));
                  }
                    
                  static s7_pointer g_ftw(s7_scheme *sc, s7_pointer args)
                  {
                    if (!internal_ftw_sc)
                      {
                        internal_ftw_sc = sc;
                        internal_ftw_arglist = s7_list(sc, 3, s7_nil(sc), s7_nil(sc), s7_nil(sc));
                        s7_gc_protect(sc, internal_ftw_arglist);
                      }
                    internal_ftw_closure = s7_cadr(args);
                    return(s7_make_integer(sc, ftw(s7_string(s7_car(args)), internal_ftw_function, s7_integer(s7_caddr(args)))));
                  }
static s7_pointer g_stat(s7_scheme *sc, s7_pointer args)
                    {return(s7_make_integer(sc, stat(s7_string(s7_car(args)), 
                                                     (struct stat *)s7_c_pointer_with_type(sc, s7_cadr(args), s7_make_symbol(sc, "stat*"), __func__, 1))));}
                  static s7_pointer g_fstat(s7_scheme *sc, s7_pointer args)
                    {return(s7_make_integer(sc, fstat(s7_integer(s7_car(args)), 
                                                      (struct stat *)s7_c_pointer_with_type(sc, s7_cadr(args), s7_make_symbol(sc, "stat*"), __func__, 1))));}
                  static s7_pointer g_lstat(s7_scheme *sc, s7_pointer args)
                    {return(s7_make_integer(sc, lstat(s7_string(s7_car(args)), 
                                                (struct stat *)s7_c_pointer_with_type(sc, s7_cadr(args), s7_make_symbol(sc, "stat*"), __func__, 1))));}
                  

/* -------- chmod -------- */
static s7_pointer s7__chmod(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__chmod_0;
  int s7__chmod_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__chmod_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__chmod_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)chmod(s7__chmod_0, s7__chmod_1)));
}


/* -------- mkdir -------- */
static s7_pointer s7__mkdir(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__mkdir_0;
  int s7__mkdir_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__mkdir_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__mkdir_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)mkdir(s7__mkdir_0, s7__mkdir_1)));
}


/* -------- mknod -------- */
static s7_pointer s7__mknod(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__mknod_0;
  int s7__mknod_1;
  int s7__mknod_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__mknod_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__mknod_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__mknod_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)mknod(s7__mknod_0, s7__mknod_1, s7__mknod_2)));
}


/* -------- mkfifo -------- */
static s7_pointer s7__mkfifo(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__mkfifo_0;
  int s7__mkfifo_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__mkfifo_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__mkfifo_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)mkfifo(s7__mkfifo_0, s7__mkfifo_1)));
}

static s7_pointer g_isdir(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISDIR(s7_integer(s7_car(args)))));}
                  static s7_pointer g_ischr(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISCHR(s7_integer(s7_car(args)))));}
                  static s7_pointer g_isblk(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISBLK(s7_integer(s7_car(args)))));}
                  static s7_pointer g_isreg(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISREG(s7_integer(s7_car(args)))));}
                  static s7_pointer g_isfifo(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISFIFO(s7_integer(s7_car(args)))));}
                  static s7_pointer g_islnk(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISLNK(s7_integer(s7_car(args)))));}
                  static s7_pointer g_issock(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_boolean(sc, S_ISSOCK(s7_integer(s7_car(args)))));}
                  static s7_pointer g_st_dev(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_dev));}
                  static s7_pointer g_st_ino(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_ino));}
                  static s7_pointer g_st_mode(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_mode));}
                  static s7_pointer g_st_nlink(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_nlink));}
                  static s7_pointer g_st_uid(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_uid));}
                  static s7_pointer g_st_gid(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_gid));}
                  static s7_pointer g_st_rdev(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_rdev));}
                  static s7_pointer g_st_size(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_size));}
                  static s7_pointer g_st_blksize(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_blksize));}
                  static s7_pointer g_st_blocks(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_blocks));}
                  static s7_pointer g_st_atime(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_atime));}
                  static s7_pointer g_st_mtime(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_mtime));}
                  static s7_pointer g_st_ctime(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct stat *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "stat*"), __func__, 1))->st_ctime));}
                  static s7_pointer g_stat_make(s7_scheme *sc, s7_pointer args)
                    {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct stat)), s7_make_symbol(sc, "stat*"), s7_f(sc)));}
                  

/* -------- clock -------- */
static s7_pointer s7__clock(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_integer(sc, (s7_int)clock()));
}


/* -------- time -------- */
static s7_pointer s7__time(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  time_t* s7__time_0;
  p = args;
  arg = s7_car(p);
  s7__time_0 = (time_t*)s7_c_pointer_with_type(sc, arg, time_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)time(s7__time_0)));
}


/* -------- difftime -------- */
static s7_pointer s7__difftime(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  time_t s7__difftime_0;
  time_t s7__difftime_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__difftime_0 = (time_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__difftime_1 = (time_t)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_real(sc, (s7_double)difftime(s7__difftime_0, s7__difftime_1)));
}


/* -------- gmtime -------- */
static s7_pointer s7__gmtime(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  time_t* s7__gmtime_0;
  p = args;
  arg = s7_car(p);
  s7__gmtime_0 = (time_t*)s7_c_pointer_with_type(sc, arg, time_t__symbol, __func__, 0);
  return(s7_make_c_pointer_with_type(sc, (void*)gmtime(s7__gmtime_0), tm__symbol, s7_f(sc)));
}


/* -------- ctime -------- */
static s7_pointer s7__ctime(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  time_t* s7__ctime_0;
  p = args;
  arg = s7_car(p);
  s7__ctime_0 = (time_t*)s7_c_pointer_with_type(sc, arg, time_t__symbol, __func__, 0);
  return(s7_make_string(sc, (char*)ctime(s7__ctime_0)));
}


/* -------- localtime -------- */
static s7_pointer s7__localtime(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  time_t* s7__localtime_0;
  p = args;
  arg = s7_car(p);
  s7__localtime_0 = (time_t*)s7_c_pointer_with_type(sc, arg, time_t__symbol, __func__, 0);
  return(s7_make_c_pointer_with_type(sc, (void*)localtime(s7__localtime_0), tm__symbol, s7_f(sc)));
}

static s7_pointer g_mktime(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, (s7_int)mktime((struct tm *)s7_c_pointer(s7_car(args)))));
                  }
                  static s7_pointer g_time_make(s7_scheme *sc, s7_pointer args) 
                  {
                    time_t *tm;
                    tm = (time_t *)calloc(1, sizeof(time_t));
                    (*tm) = (time_t)s7_integer(s7_car(args));
                    return(s7_make_c_pointer_with_type(sc, (void *)tm, s7_make_symbol(sc, "time_t*"), s7_f(sc)));
                  }
                  static s7_pointer g_strftime(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, (s7_int)strftime((char *)s7_string(s7_car(args)), 
                  				             (size_t)s7_integer(s7_cadr(args)), 
                  					     s7_string(s7_caddr(args)), 
                  					     (const struct tm *)s7_c_pointer(s7_cadddr(args)))));
                  }
                  static s7_pointer g_asctime(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_string(sc, asctime((const struct tm *)s7_c_pointer(s7_car(args)))));
                  }
                  static s7_pointer g_gettimeofday(s7_scheme *sc, s7_pointer args)
                  {
                    struct timeval t0;
                    gettimeofday(&t0, NULL);
                    return(s7_list(sc, 2, s7_make_integer(sc, t0.tv_sec), s7_make_integer(sc, t0.tv_usec)));
                  }
                  static s7_pointer g_nanosleep(s7_scheme *sc, s7_pointer args)
                  {
                    struct timespec t0;
                    t0.tv_sec = (time_t)s7_integer(s7_car(args));
                    t0.tv_nsec = (long)s7_integer(s7_cadr(args));
                    return(s7_make_integer(sc, nanosleep(&t0, NULL)));
                  }
                  static s7_pointer g_clock_getres(s7_scheme *sc, s7_pointer args)
                  {
                    #if (!__APPLE__)
                    struct timespec t0;
                    int res;
                    res = clock_getres(s7_integer(s7_car(args)), &t0);
                    return(s7_list(sc, 3, s7_make_integer(sc, res), s7_make_integer(sc, t0.tv_sec), s7_make_integer(sc, t0.tv_nsec)));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  static s7_pointer g_clock_gettime(s7_scheme *sc, s7_pointer args)
                  {
                    #if (!__APPLE__)
                    struct timespec t0;
                    int res;
                    res = clock_gettime(s7_integer(s7_car(args)), &t0);
                    return(s7_list(sc, 3, s7_make_integer(sc, res), s7_make_integer(sc, t0.tv_sec), s7_make_integer(sc, t0.tv_nsec)));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  static s7_pointer g_clock_settime(s7_scheme *sc, s7_pointer args)
                  {
                    #if (!__APPLE__)
                    struct timespec t0;
                    t0.tv_sec = (time_t)s7_integer(s7_cadr(args));
                    t0.tv_nsec = (long)s7_integer(s7_caddr(args));
                    return(s7_make_integer(sc, clock_settime(s7_integer(s7_car(args)), &t0)));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  static s7_pointer g_clock_getcpuclockid(s7_scheme *sc, s7_pointer args)
                  {
                    #if __linux__
                    clockid_t c = 0;
                    clock_getcpuclockid((pid_t)s7_integer(s7_car(args)), &c);
                    return(s7_make_integer(sc, (s7_int)c));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  static s7_pointer g_clock_nanosleep(s7_scheme *sc, s7_pointer args)
                  {
                    #if __linux__
                    struct timespec t0;
                    t0.tv_sec = (time_t)s7_integer(s7_caddr(args));
                    t0.tv_nsec = (long)s7_integer(s7_cadddr(args));
                    return(s7_make_integer(sc, clock_nanosleep((clockid_t)s7_integer(s7_car(args)), (int)s7_integer(s7_cadr(args)), &t0, NULL)));
                    #else
                    return(s7_make_integer(sc, -1));
                    #endif
                  }
                  
static s7_pointer g_utime(s7_scheme *sc, s7_pointer args)
                  {
                    struct utimbuf tb;
                    tb.actime = (time_t)s7_integer(s7_cadr(args));
                    tb.modtime = (time_t)s7_integer(s7_caddr(args));
                    return(s7_make_integer(sc, utime(s7_string(s7_car(args)), &tb)));
                  }

/* -------- tcsendbreak -------- */
static s7_pointer s7__tcsendbreak(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__tcsendbreak_0;
  int s7__tcsendbreak_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcsendbreak_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcsendbreak_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)tcsendbreak(s7__tcsendbreak_0, s7__tcsendbreak_1)));
}
static s7_int tcsendbreak_i_ii(s7_int i1, s7_int i2) {return(tcsendbreak(i1, i2));}


/* -------- tcdrain -------- */
static s7_pointer s7__tcdrain(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__tcdrain_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcdrain_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)tcdrain(s7__tcdrain_0)));
}
static s7_int tcdrain_i_i(s7_int i1) {return(tcdrain(i1));}


/* -------- tcflush -------- */
static s7_pointer s7__tcflush(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__tcflush_0;
  int s7__tcflush_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcflush_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcflush_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)tcflush(s7__tcflush_0, s7__tcflush_1)));
}
static s7_int tcflush_i_ii(s7_int i1, s7_int i2) {return(tcflush(i1, i2));}


/* -------- tcflow -------- */
static s7_pointer s7__tcflow(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__tcflow_0;
  int s7__tcflow_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcflow_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__tcflow_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)tcflow(s7__tcflow_0, s7__tcflow_1)));
}
static s7_int tcflow_i_ii(s7_int i1, s7_int i2) {return(tcflow(i1, i2));}

static s7_pointer g_cfgetospeed(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "termios*"), __func__, 1);
                    return(s7_make_integer(sc, (s7_int)cfgetospeed(p)));
                  }
                  static s7_pointer g_cfgetispeed(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "termios*"), __func__, 1);
                    return(s7_make_integer(sc, (s7_int)cfgetispeed(p)));
                  }
                  static s7_pointer g_cfsetospeed(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "termios*"), __func__, 1);
                    return(s7_make_integer(sc, cfsetospeed(p, (speed_t)s7_integer(s7_cadr(args)))));
                  }
                  static s7_pointer g_cfsetispeed(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "termios*"), __func__, 1);
                    return(s7_make_integer(sc, cfsetispeed(p, (speed_t)s7_integer(s7_cadr(args)))));
                  }
                  static s7_pointer g_tcgetattr(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer_with_type(sc, s7_cadr(args), s7_make_symbol(sc, "termios*"), __func__, 1);
                    return(s7_make_integer(sc, tcgetattr(s7_integer(s7_car(args)), p)));
                  }
                  static s7_pointer g_tcsetattr(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer_with_type(sc, s7_caddr(args), s7_make_symbol(sc, "termios*"), __func__, 1);
                    return(s7_make_integer(sc, tcsetattr(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), p)));
                   }
                  static s7_pointer g_termios_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct termios)), s7_make_symbol(sc, "termios*"), s7_f(sc)));}

                  static s7_pointer g_termios_c_lflag(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "termios*"), __func__, 1);
                    return(s7_make_integer(sc, (s7_int)(p->c_lflag)));
                  }
                  static s7_pointer g_termios_set_c_lflag(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "termios*"), __func__, 1);
                    p->c_lflag = (tcflag_t)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }
                  static s7_pointer g_termios_set_c_cc(s7_scheme *sc, s7_pointer args)
                  {
                    struct termios *p;
                    p = (struct termios *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "termios*"), __func__, 1);
                    p->c_cc[(int)s7_integer(s7_cadr(args))] = (cc_t)s7_integer(s7_caddr(args));
                    return(s7_caddr(args));
                  }
                  
static s7_pointer g_getgrgid(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, getgrgid(s7_integer(s7_car(args))), s7_make_symbol(sc, "group*"), s7_f(sc)));}
                  static s7_pointer g_getgrnam(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, getgrnam(s7_string(s7_car(args))), s7_make_symbol(sc, "group*"), s7_f(sc)));}
                  static s7_pointer g_group_gr_name(s7_scheme *sc, s7_pointer args) 
                    {
                      struct group *g; 
                      g = (struct group *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "group*"), __func__, 1);
                      if (!g) return(s7_make_string(sc, ""));
                      return(s7_make_string(sc, ((struct group *)g)->gr_name));
                    }
                  static s7_pointer g_group_gr_passwd(s7_scheme *sc, s7_pointer args) 
                    {
                      struct group *g; 
                      g = (struct group *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "group*"), __func__, 1);
                      if (!g) return(s7_make_string(sc, ""));
                      return(s7_make_string(sc, ((struct group *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "group*"), __func__, 1))->gr_passwd));
                    }
                  static s7_pointer g_group_gr_gid(s7_scheme *sc, s7_pointer args) 
                    {
                      struct group *g; 
                      g = (struct group *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "group*"), __func__, 1);
                      if (!g) return(s7_make_integer(sc, -1));
                      return(s7_make_integer(sc, (s7_int)(((struct group *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "group*"), __func__, 1))->gr_gid)));
                    }
                  static s7_pointer g_group_gr_mem(s7_scheme *sc, s7_pointer args)
                    {
                      s7_pointer p;
                      int i;
                      struct group *g;
                      g = (struct group *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "group*"), __func__, 1);
                      p = s7_nil(sc);
                      for (i = 0; g->gr_mem[i]; i++)
                        p = s7_cons(sc, s7_make_string(sc, g->gr_mem[i]), p);
                      return(p);
                      }
                      

/* -------- setpwent -------- */
static s7_pointer s7__setpwent(s7_scheme *sc, s7_pointer args)
{
  setpwent();
  return(s7_unspecified(sc));
}


/* -------- endpwent -------- */
static s7_pointer s7__endpwent(s7_scheme *sc, s7_pointer args)
{
  endpwent();
  return(s7_unspecified(sc));
}


/* -------- getpwent -------- */
static s7_pointer s7__getpwent(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void*)getpwent(), passwd__symbol, s7_f(sc)));
}


/* -------- getpwuid -------- */
static s7_pointer s7__getpwuid(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__getpwuid_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getpwuid_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)getpwuid(s7__getpwuid_0), passwd__symbol, s7_f(sc)));
}


/* -------- getpwnam -------- */
static s7_pointer s7__getpwnam(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__getpwnam_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__getpwnam_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)getpwnam(s7__getpwnam_0), passwd__symbol, s7_f(sc)));
}

static s7_pointer g_passwd_pw_name(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "passwd*"), __func__, 1))->pw_name));}
                  static s7_pointer g_passwd_pw_passwd(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "passwd*"), __func__, 1))->pw_passwd));}
                  static s7_pointer g_passwd_pw_uid(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct passwd *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "passwd*"), __func__, 1))->pw_uid));}
                  static s7_pointer g_passwd_pw_gid(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_integer(sc, ((struct passwd *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "passwd*"), __func__, 1))->pw_gid));}
                  static s7_pointer g_passwd_pw_gecos(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "passwd*"), __func__, 1))->pw_gecos));}
                  static s7_pointer g_passwd_pw_dir(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "passwd*"), __func__, 1))->pw_dir));}
                  static s7_pointer g_passwd_pw_shell(s7_scheme *sc, s7_pointer args) 
                    {return(s7_make_string(sc, ((struct passwd *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "passwd*"), __func__, 1))->pw_shell));}
                  

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

/* -------- globfree -------- */
static s7_pointer s7__globfree(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  glob_t* s7__globfree_0;
  p = args;
  arg = s7_car(p);
  s7__globfree_0 = (glob_t*)s7_c_pointer_with_type(sc, arg, glob_t__symbol, __func__, 0);
  globfree(s7__globfree_0);
  return(s7_unspecified(sc));
}

static s7_pointer g_glob_make(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(glob_t)), s7_make_symbol(sc, "glob_t*"), s7_f(sc)));}
                           static s7_pointer g_glob_gl_pathc(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_integer(sc, ((glob_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "glob_t*"), __func__, 1))->gl_pathc));}
                           static s7_pointer g_glob(s7_scheme *sc, s7_pointer args)
                           {return(s7_make_integer(sc, glob(s7_string(s7_car(args)), s7_integer(s7_cadr(args)), NULL, 
                                                            (glob_t *)s7_c_pointer_with_type(sc, s7_caddr(args), s7_make_symbol(sc, "glob_t*"), __func__, 1))));}
                           static s7_pointer g_glob_gl_pathv(s7_scheme *sc, s7_pointer args)
                           {
                             s7_pointer p;
                             size_t i;
                             glob_t *g;
                             g = (glob_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "glob_t*"), __func__, 1);
                             p = s7_nil(sc);
                             for (i = 0; i < g->gl_pathc; i++)
                               p = s7_cons(sc, s7_make_string(sc, g->gl_pathv[i]), p);
                             return(p);
                           }

/* -------- kill -------- */
static s7_pointer s7__kill(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__kill_0;
  int s7__kill_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__kill_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__kill_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)kill(s7__kill_0, s7__kill_1)));
}
static s7_int kill_i_ii(s7_int i1, s7_int i2) {return(kill(i1, i2));}


/* -------- raise -------- */
static s7_pointer s7__raise(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__raise_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__raise_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)raise(s7__raise_0)));
}
static s7_int raise_i_i(s7_int i1) {return(raise(i1));}


/* -------- sigemptyset -------- */
static s7_pointer s7__sigemptyset(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  sigset_t* s7__sigemptyset_0;
  p = args;
  arg = s7_car(p);
  s7__sigemptyset_0 = (sigset_t*)s7_c_pointer_with_type(sc, arg, sigset_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)sigemptyset(s7__sigemptyset_0)));
}


/* -------- sigfillset -------- */
static s7_pointer s7__sigfillset(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  sigset_t* s7__sigfillset_0;
  p = args;
  arg = s7_car(p);
  s7__sigfillset_0 = (sigset_t*)s7_c_pointer_with_type(sc, arg, sigset_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)sigfillset(s7__sigfillset_0)));
}


/* -------- sigaddset -------- */
static s7_pointer s7__sigaddset(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  sigset_t* s7__sigaddset_0;
  int s7__sigaddset_1;
  p = args;
  arg = s7_car(p);
  s7__sigaddset_0 = (sigset_t*)s7_c_pointer_with_type(sc, arg, sigset_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sigaddset_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)sigaddset(s7__sigaddset_0, s7__sigaddset_1)));
}


/* -------- sigdelset -------- */
static s7_pointer s7__sigdelset(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  sigset_t* s7__sigdelset_0;
  int s7__sigdelset_1;
  p = args;
  arg = s7_car(p);
  s7__sigdelset_0 = (sigset_t*)s7_c_pointer_with_type(sc, arg, sigset_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sigdelset_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)sigdelset(s7__sigdelset_0, s7__sigdelset_1)));
}


/* -------- sigismember -------- */
static s7_pointer s7__sigismember(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  sigset_t* s7__sigismember_0;
  int s7__sigismember_1;
  p = args;
  arg = s7_car(p);
  s7__sigismember_0 = (sigset_t*)s7_c_pointer_with_type(sc, arg, sigset_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sigismember_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)sigismember(s7__sigismember_0, s7__sigismember_1)));
}


/* -------- sigprocmask -------- */
static s7_pointer s7__sigprocmask(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__sigprocmask_0;
  sigset_t* s7__sigprocmask_1;
  sigset_t* s7__sigprocmask_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sigprocmask_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__sigprocmask_1 = (sigset_t*)s7_c_pointer_with_type(sc, arg, sigset_t__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__sigprocmask_2 = (sigset_t*)s7_c_pointer_with_type(sc, arg, sigset_t__symbol, __func__, 3);
  return(s7_make_integer(sc, (s7_int)sigprocmask(s7__sigprocmask_0, s7__sigprocmask_1, s7__sigprocmask_2)));
}


/* -------- sigsuspend -------- */
static s7_pointer s7__sigsuspend(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  sigset_t* s7__sigsuspend_0;
  p = args;
  arg = s7_car(p);
  s7__sigsuspend_0 = (sigset_t*)s7_c_pointer_with_type(sc, arg, sigset_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)sigsuspend(s7__sigsuspend_0)));
}


/* -------- sigpending -------- */
static s7_pointer s7__sigpending(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  sigset_t* s7__sigpending_0;
  p = args;
  arg = s7_car(p);
  s7__sigpending_0 = (sigset_t*)s7_c_pointer_with_type(sc, arg, sigset_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)sigpending(s7__sigpending_0)));
}


/* -------- getpriority -------- */
static s7_pointer s7__getpriority(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__getpriority_0;
  int s7__getpriority_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getpriority_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getpriority_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)getpriority(s7__getpriority_0, s7__getpriority_1)));
}
static s7_int getpriority_i_ii(s7_int i1, s7_int i2) {return(getpriority(i1, i2));}


/* -------- setpriority -------- */
static s7_pointer s7__setpriority(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__setpriority_0;
  int s7__setpriority_1;
  int s7__setpriority_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setpriority_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setpriority_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setpriority_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)setpriority(s7__setpriority_0, s7__setpriority_1, s7__setpriority_2)));
}

static s7_pointer g_rlimit_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct rlimit)), s7_make_symbol(sc, "rlimit*"), s7_f(sc)));}
                  static s7_pointer g_rlimit_rlim_cur(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rlimit *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rlimit*"), __func__, 1))->rlim_cur));}
                  static s7_pointer g_rlimit_rlim_max(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rlimit *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rlimit*"), __func__, 1))->rlim_max));}

                  static s7_pointer g_rusage_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct rusage)), s7_make_symbol(sc, "rusage*"), s7_f(sc)));}
                  static s7_pointer g_rusage_ru_maxrss(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rusage*"), __func__, 1))->ru_maxrss));}
                  static s7_pointer g_rusage_ru_minflt(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rusage*"), __func__, 1))->ru_minflt));}
                  static s7_pointer g_rusage_ru_majflt(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rusage*"), __func__, 1))->ru_majflt));}
                  static s7_pointer g_rusage_ru_inblock(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rusage*"), __func__, 1))->ru_inblock));}
                  static s7_pointer g_rusage_ru_oublock(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rusage*"), __func__, 1))->ru_oublock));}
                  static s7_pointer g_rusage_ru_nvcsw(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rusage*"), __func__, 1))->ru_nvcsw));}
                  static s7_pointer g_rusage_ru_nivcsw(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct rusage *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rusage*"), __func__, 1))->ru_nivcsw));}
                  static s7_pointer g_rusage_ru_utime(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, &(((struct rusage *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rusage*"), __func__, 1))->ru_utime)));}
                  static s7_pointer g_rusage_ru_stime(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, &(((struct rusage *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "rusage*"), __func__, 1))->ru_stime)));}
                  static s7_pointer g_getrusage(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, getrusage(s7_integer(s7_car(args)), 
                                                (struct rusage *)s7_c_pointer_with_type(sc, s7_cadr(args), s7_make_symbol(sc, "rusage*"), __func__, 1))));}

                  static s7_pointer g_sigset_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(sigset_t)), s7_make_symbol(sc, "sigset_t*"), s7_f(sc)));}

                  #if __linux__
                  static s7_pointer g_WEXITSTATUS(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WEXITSTATUS(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WTERMSIG(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WTERMSIG(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WSTOPSIG(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WSTOPSIG(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WIFEXITED(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WIFEXITED(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WIFSIGNALED(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WIFSIGNALED(s7_integer(s7_car(args)))));}
                  static s7_pointer g_WIFSTOPPED(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, WIFSTOPPED(s7_integer(s7_car(args)))));}
                  #endif

                  static s7_pointer g_wait(s7_scheme *sc, s7_pointer args)
                  {
                    int status, result;
                    result = wait(&status);
                    return(s7_list(sc, 2, s7_make_integer(sc, result), s7_make_integer(sc, status)));
                  }
                  static s7_pointer g_waitpid(s7_scheme *sc, s7_pointer args)
                  {
                    int status, result;
                    result = waitpid((pid_t)s7_integer(s7_car(args)), &status, s7_integer(s7_cadr(args)));
                    return(s7_list(sc, 2, s7_make_integer(sc, result), s7_make_integer(sc, status)));
                  }
                  static s7_pointer g_sigqueue(s7_scheme *sc, s7_pointer args)
                  {
                    #if (__linux__)
                      union sigval val;
                      if (s7_is_integer(s7_caddr(args)))
                        val.sival_int = (int)s7_integer(s7_caddr(args));
                      else val.sival_ptr = (void *)s7_c_pointer(s7_caddr(args));
                      return(s7_make_integer(sc, sigqueue((pid_t)s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), val)));
                    #else
                      return(s7_f(sc));
                    #endif
                  }
                  static s7_pointer g_sigwait(s7_scheme *sc, s7_pointer args)
                  {
                    #if (!__sun)
                    int status, result;
                    result = sigwait((const sigset_t *)s7_c_pointer(s7_car(args)), &status);
                    return(s7_list(sc, 2, s7_make_integer(sc, result), s7_make_integer(sc, status)));
                    #else
                    return(s7_f(sc));
                    #endif
                  }
                  static s7_pointer g_sigtimedwait(s7_scheme *sc, s7_pointer args)
                  {
                    #if (__linux__)
                     return(s7_make_integer(sc, sigtimedwait((const sigset_t *)s7_c_pointer(s7_car(args)), 
                  					   (siginfo_t *)s7_c_pointer(s7_cadr(args)),
                                                             (const struct timespec *)s7_c_pointer(s7_caddr(args)))));
                    #else
                      return(s7_f(sc));
                    #endif
                  }
                  #if __linux__
                  static s7_pointer g_siginfo_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(siginfo_t)), s7_make_symbol(sc, "siginfo_t*"), s7_f(sc)));}
                  static s7_pointer g_siginfo_si_signo(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_signo));}
                  static s7_pointer g_siginfo_si_errno(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_errno));}
                  static s7_pointer g_siginfo_si_code(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_code));}
                  static s7_pointer g_siginfo_si_pid(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_pid));}
                  static s7_pointer g_siginfo_si_uid(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_uid));}
                  static s7_pointer g_siginfo_si_status(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_status));}
                  static s7_pointer g_siginfo_si_utime(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_utime));}
                  static s7_pointer g_siginfo_si_stime(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_stime));}
                  static s7_pointer g_siginfo_si_value(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, &(((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_value)));}
                  static s7_pointer g_siginfo_si_int(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_int));}
                  static s7_pointer g_siginfo_si_overrun(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_overrun));}
                  static s7_pointer g_siginfo_si_timerid(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_timerid));}
                  static s7_pointer g_siginfo_si_band(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_band));}
                  static s7_pointer g_siginfo_si_fd(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_fd));}
                  static s7_pointer g_siginfo_si_ptr(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, 
                            ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_ptr, 
                            s7_make_symbol(sc, "siginfo_t*"), s7_f(sc)));}
                  static s7_pointer g_siginfo_si_addr(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, 
                            ((siginfo_t *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "siginfo_t*"), __func__, 1))->si_addr, 
                            s7_make_symbol(sc, "siginfo_t*"), s7_f(sc)));}
                  #endif

                  static s7_pointer g_timespec_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct timespec)), s7_make_symbol(sc, "timespec*"), s7_f(sc)));}
                  static s7_pointer g_timespec_tv_sec(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct timespec *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "timespec*"), __func__, 1))->tv_sec));}
                  static s7_pointer g_timespec_tv_nsec(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct timespec *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "timespec*"), __func__, 1))->tv_nsec));}

                  static s7_pointer g_sigaction_make(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct sigaction)), s7_make_symbol(sc, "sigaction*"), s7_f(sc)));}
                  static s7_pointer g_sigaction_sa_flags(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct sigaction *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "sigaction*"), __func__, 1))->sa_flags));}
                  static s7_pointer g_sigaction_set_sa_flags(s7_scheme *sc, s7_pointer args)
                  {((struct sigaction *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "sigaction*"), __func__, 1))->sa_flags = s7_integer(s7_cadr(args)); return(s7_cadr(args));}
                  static s7_pointer g_sigaction_sa_mask(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer_with_type(sc, (void *)(&(((struct sigaction *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "sigaction*"), __func__, 1))->sa_mask)), s7_make_symbol(sc, "sigset_t*"), s7_f(sc)));}
                  static s7_pointer g_sigaction_sa_handler(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_c_pointer(sc, (void *)(((struct sigaction *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "sigaction*"), __func__, 1))->sa_handler)));}

                  static s7_pointer sighandlers = NULL;
                  static s7_scheme *sighandlers_s7 = NULL;
                  static void s7_signal_handler(int sig)
                  {
                    if (sighandlers)
                      {
                        s7_pointer handler;
                        handler = s7_vector_ref(sighandlers_s7, sighandlers, sig);
                        if (handler != s7_nil(sighandlers_s7))
                           s7_call(sighandlers_s7, handler, s7_cons(sighandlers_s7, s7_make_integer(sighandlers_s7, sig), s7_nil(sighandlers_s7)));
                       }
                  }
                  #ifndef SIGUNUSED
                    #define SIGUNUSED 65
                  #endif
                  static s7_pointer g_sigaction_set_sa_handler(s7_scheme *sc, s7_pointer args)
                  {
                    /* (sigaction.set_sa_handler ptr handler) */
                    if (!sighandlers)
                      {
                        sighandlers_s7 = sc;
                        sighandlers = s7_make_and_fill_vector(sc, SIGUNUSED + 1, s7_nil(sc));
                        s7_gc_protect(sc, sighandlers);
                      }
                    if (s7_is_c_pointer(s7_cadr(args)))
                      {
                        if (s7_c_pointer(s7_cadr(args)) == (void *)SIG_DFL)
                           ((struct sigaction *)s7_c_pointer(s7_car(args)))->sa_handler = SIG_DFL;
                        else
                          {
                            if (s7_c_pointer(s7_cadr(args)) == (void *)SIG_IGN)
                               ((struct sigaction *)s7_c_pointer(s7_car(args)))->sa_handler = SIG_IGN;
                          }}
                    else 
                      {
                        ((struct sigaction *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "sigaction*"), __func__, 1))->sa_handler = s7_signal_handler;
                        s7_vector_set(sighandlers_s7, sighandlers, SIGUNUSED, 
                          s7_cons(sc, s7_cons(sc, s7_car(args), s7_cadr(args)), s7_vector_ref(sighandlers_s7, sighandlers, SIGUNUSED)));
                      }
                    return(s7_cadr(args));
                  }
                static s7_pointer g_sigaction(s7_scheme *sc, s7_pointer args)
                {
                  int sig;
                  const struct sigaction *new_act;
                  struct sigaction *old_act;
                  s7_pointer handler;
                  sig = (int)s7_integer(s7_car(args));
                  new_act = (const struct sigaction *)s7_c_pointer_with_type(sc, s7_cadr(args), s7_make_symbol(sc, "sigaction*"), __func__, 1);
                  old_act = (struct sigaction *)s7_c_pointer_with_type(sc, s7_caddr(args), s7_make_symbol(sc, "sigaction*"), __func__, 1);
                  if (s7_is_pair(s7_vector_ref(sighandlers_s7, sighandlers, SIGUNUSED)))
                    {
                      handler = s7_assq(sc, s7_cadr(args), s7_vector_ref(sighandlers_s7, sighandlers, SIGUNUSED));
                      if (s7_is_pair(handler))
                        s7_vector_set(sighandlers_s7, sighandlers, sig, s7_cdr(handler));
                    }
                  return(s7_make_integer(sc, sigaction(sig, new_act, old_act)));
                }
                static s7_pointer g_signal(s7_scheme *sc, s7_pointer args)
                {
                  int sig;
                  if (!sighandlers)
                    {
                      sighandlers_s7 = sc;
                      sighandlers = s7_make_and_fill_vector(sc, SIGUNUSED + 1, s7_nil(sc));
                      s7_gc_protect(sc, sighandlers);
                    }
                  sig = s7_integer(s7_car(args));
                  if (s7_is_c_pointer(s7_cadr(args)))
                    {
                      if (s7_c_pointer(s7_cadr(args)) == (void *)SIG_DFL)
                         return(s7_make_c_pointer(sc, (void *)signal(sig, SIG_DFL)));
                      if (s7_c_pointer(s7_cadr(args)) == (void *)SIG_IGN)
                         return(s7_make_c_pointer(sc, (void *)signal(sig, SIG_IGN)));
                     }
                  s7_vector_set(sc, sighandlers, sig, s7_cadr(args));
                  return(s7_make_c_pointer(sc, (void *)signal(sig, s7_signal_handler)));
                }
                  

/* -------- getrlimit -------- */
static s7_pointer s7__getrlimit(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__getrlimit_0;
  struct rlimit* s7__getrlimit_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getrlimit_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__getrlimit_1 = (struct rlimit*)s7_c_pointer_with_type(sc, arg, struct_rlimit__symbol, __func__, 2);
  return(s7_make_integer(sc, (s7_int)getrlimit(s7__getrlimit_0, s7__getrlimit_1)));
}


/* -------- setrlimit -------- */
static s7_pointer s7__setrlimit(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__setrlimit_0;
  struct rlimit* s7__setrlimit_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setrlimit_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__setrlimit_1 = (struct rlimit*)s7_c_pointer_with_type(sc, arg, struct_rlimit__symbol, __func__, 2);
  return(s7_make_integer(sc, (s7_int)setrlimit(s7__setrlimit_0, s7__setrlimit_1)));
}


/* -------- sethostent -------- */
static s7_pointer s7__sethostent(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__sethostent_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sethostent_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  sethostent(s7__sethostent_0);
  return(s7_unspecified(sc));
}


/* -------- endhostent -------- */
static s7_pointer s7__endhostent(s7_scheme *sc, s7_pointer args)
{
  endhostent();
  return(s7_unspecified(sc));
}


/* -------- gethostent -------- */
static s7_pointer s7__gethostent(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void*)gethostent(), hostent__symbol, s7_f(sc)));
}


/* -------- setservent -------- */
static s7_pointer s7__setservent(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__setservent_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setservent_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  setservent(s7__setservent_0);
  return(s7_unspecified(sc));
}


/* -------- endservent -------- */
static s7_pointer s7__endservent(s7_scheme *sc, s7_pointer args)
{
  endservent();
  return(s7_unspecified(sc));
}


/* -------- getservent -------- */
static s7_pointer s7__getservent(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void*)getservent(), servent__symbol, s7_f(sc)));
}


/* -------- setprotoent -------- */
static s7_pointer s7__setprotoent(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__setprotoent_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setprotoent_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  setprotoent(s7__setprotoent_0);
  return(s7_unspecified(sc));
}


/* -------- endprotoent -------- */
static s7_pointer s7__endprotoent(s7_scheme *sc, s7_pointer args)
{
  endprotoent();
  return(s7_unspecified(sc));
}


/* -------- getprotoent -------- */
static s7_pointer s7__getprotoent(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void*)getprotoent(), protoent__symbol, s7_f(sc)));
}


/* -------- setnetent -------- */
static s7_pointer s7__setnetent(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__setnetent_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__setnetent_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  setnetent(s7__setnetent_0);
  return(s7_unspecified(sc));
}


/* -------- endnetent -------- */
static s7_pointer s7__endnetent(s7_scheme *sc, s7_pointer args)
{
  endnetent();
  return(s7_unspecified(sc));
}


/* -------- getnetent -------- */
static s7_pointer s7__getnetent(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_c_pointer_with_type(sc, (void*)getnetent(), netent__symbol, s7_f(sc)));
}


/* -------- socket -------- */
static s7_pointer s7__socket(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__socket_0;
  int s7__socket_1;
  int s7__socket_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__socket_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__socket_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__socket_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)socket(s7__socket_0, s7__socket_1, s7__socket_2)));
}


/* -------- listen -------- */
static s7_pointer s7__listen(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__listen_0;
  int s7__listen_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__listen_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__listen_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)listen(s7__listen_0, s7__listen_1)));
}
static s7_int listen_i_ii(s7_int i1, s7_int i2) {return(listen(i1, i2));}


/* -------- shutdown -------- */
static s7_pointer s7__shutdown(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__shutdown_0;
  int s7__shutdown_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__shutdown_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__shutdown_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)shutdown(s7__shutdown_0, s7__shutdown_1)));
}
static s7_int shutdown_i_ii(s7_int i1, s7_int i2) {return(shutdown(i1, i2));}


/* -------- gethostbyname -------- */
static s7_pointer s7__gethostbyname(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__gethostbyname_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__gethostbyname_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)gethostbyname(s7__gethostbyname_0), hostent__symbol, s7_f(sc)));
}


/* -------- gethostbyaddr -------- */
static s7_pointer s7__gethostbyaddr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  void* s7__gethostbyaddr_0;
  int s7__gethostbyaddr_1;
  int s7__gethostbyaddr_2;
  p = args;
  arg = s7_car(p);
  s7__gethostbyaddr_0 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__gethostbyaddr_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__gethostbyaddr_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)gethostbyaddr(s7__gethostbyaddr_0, s7__gethostbyaddr_1, s7__gethostbyaddr_2), hostent__symbol, s7_f(sc)));
}


/* -------- getnetbyname -------- */
static s7_pointer s7__getnetbyname(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__getnetbyname_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__getnetbyname_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)getnetbyname(s7__getnetbyname_0), netent__symbol, s7_f(sc)));
}


/* -------- getnetbyaddr -------- */
static s7_pointer s7__getnetbyaddr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__getnetbyaddr_0;
  int s7__getnetbyaddr_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getnetbyaddr_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getnetbyaddr_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)getnetbyaddr(s7__getnetbyaddr_0, s7__getnetbyaddr_1), netent__symbol, s7_f(sc)));
}


/* -------- getservbyname -------- */
static s7_pointer s7__getservbyname(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__getservbyname_0;
  char* s7__getservbyname_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__getservbyname_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "string"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__getservbyname_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)getservbyname(s7__getservbyname_0, s7__getservbyname_1), servent__symbol, s7_f(sc)));
}


/* -------- getservbyport -------- */
static s7_pointer s7__getservbyport(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__getservbyport_0;
  char* s7__getservbyport_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getservbyport_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__getservbyport_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 2, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)getservbyport(s7__getservbyport_0, s7__getservbyport_1), servent__symbol, s7_f(sc)));
}


/* -------- getprotobyname -------- */
static s7_pointer s7__getprotobyname(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* s7__getprotobyname_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    s7__getprotobyname_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "string"));
  return(s7_make_c_pointer_with_type(sc, (void*)getprotobyname(s7__getprotobyname_0), protoent__symbol, s7_f(sc)));
}


/* -------- getprotobynumber -------- */
static s7_pointer s7__getprotobynumber(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__getprotobynumber_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__getprotobynumber_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_c_pointer_with_type(sc, (void*)getprotobynumber(s7__getprotobynumber_0), protoent__symbol, s7_f(sc)));
}


/* -------- freeaddrinfo -------- */
static s7_pointer s7__freeaddrinfo(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  struct addrinfo* s7__freeaddrinfo_0;
  p = args;
  arg = s7_car(p);
  s7__freeaddrinfo_0 = (struct addrinfo*)s7_c_pointer_with_type(sc, arg, struct_addrinfo__symbol, __func__, 0);
  freeaddrinfo(s7__freeaddrinfo_0);
  return(s7_unspecified(sc));
}


/* -------- gai_strerror -------- */
static s7_pointer s7__gai_strerror(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__gai_strerror_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__gai_strerror_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 0, arg, "integer"));
  return(s7_make_string(sc, (char*)gai_strerror(s7__gai_strerror_0)));
}


/* -------- bind -------- */
static s7_pointer s7__bind(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__bind_0;
  const struct sockaddr* s7__bind_1;
  int s7__bind_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__bind_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__bind_1 = (const struct sockaddr*)s7_c_pointer_with_type(sc, arg, const_struct_sockaddr__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__bind_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)bind(s7__bind_0, s7__bind_1, s7__bind_2)));
}


/* -------- connect -------- */
static s7_pointer s7__connect(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__connect_0;
  const struct sockaddr* s7__connect_1;
  int s7__connect_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__connect_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__connect_1 = (const struct sockaddr*)s7_c_pointer_with_type(sc, arg, const_struct_sockaddr__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__connect_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)connect(s7__connect_0, s7__connect_1, s7__connect_2)));
}


/* -------- send -------- */
static s7_pointer s7__send(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__send_0;
  void* s7__send_1;
  int s7__send_2;
  int s7__send_3;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__send_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__send_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__send_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__send_3 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 4, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)send(s7__send_0, s7__send_1, s7__send_2, s7__send_3)));
}


/* -------- recv -------- */
static s7_pointer s7__recv(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__recv_0;
  void* s7__recv_1;
  int s7__recv_2;
  int s7__recv_3;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__recv_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__recv_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__recv_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__recv_3 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 4, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)recv(s7__recv_0, s7__recv_1, s7__recv_2, s7__recv_3)));
}


/* -------- sendto -------- */
static s7_pointer s7__sendto(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__sendto_0;
  void* s7__sendto_1;
  int s7__sendto_2;
  int s7__sendto_3;
  const struct sockaddr* s7__sendto_4;
  int s7__sendto_5;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sendto_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__sendto_1 = (void*)s7_c_pointer_with_type(sc, arg, void__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sendto_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sendto_3 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 4, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__sendto_4 = (const struct sockaddr*)s7_c_pointer_with_type(sc, arg, const_struct_sockaddr__symbol, __func__, 5);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sendto_5 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 6, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)sendto(s7__sendto_0, s7__sendto_1, s7__sendto_2, s7__sendto_3, s7__sendto_4, s7__sendto_5)));
}


/* -------- sendmsg -------- */
static s7_pointer s7__sendmsg(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__sendmsg_0;
  const struct msghdr* s7__sendmsg_1;
  int s7__sendmsg_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sendmsg_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__sendmsg_1 = (const struct msghdr*)s7_c_pointer_with_type(sc, arg, const_struct_msghdr__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__sendmsg_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)sendmsg(s7__sendmsg_0, s7__sendmsg_1, s7__sendmsg_2)));
}


/* -------- recvmsg -------- */
static s7_pointer s7__recvmsg(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__recvmsg_0;
  struct msghdr* s7__recvmsg_1;
  int s7__recvmsg_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__recvmsg_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 1, arg, "integer"));
  p = s7_cdr(p);
  arg = s7_car(p);
  s7__recvmsg_1 = (struct msghdr*)s7_c_pointer_with_type(sc, arg, struct_msghdr__symbol, __func__, 2);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__recvmsg_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_arg_error(sc, __func__, 3, arg, "integer"));
  return(s7_make_integer(sc, (s7_int)recvmsg(s7__recvmsg_0, s7__recvmsg_1, s7__recvmsg_2)));
}

static s7_pointer g_ntohl(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ntohl(s7_integer(s7_car(args)))));}
                  static s7_pointer g_ntohs(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, ntohs(s7_integer(s7_car(args)))));}
                  static s7_pointer g_htonl(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, htonl(s7_integer(s7_car(args)))));}
                  static s7_pointer g_htons(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, htons(s7_integer(s7_car(args)))));}

                  static s7_pointer g_addrinfo_make(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_c_pointer_with_type(sc, (void *)calloc(1, sizeof(struct addrinfo)), s7_make_symbol(sc, "addrinfo*"), s7_f(sc)));
                  }

                  static s7_pointer g_addrinfo_ai_flags(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, ((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_flags));
                  }
                  static s7_pointer g_addrinfo_set_ai_flags(s7_scheme *sc, s7_pointer args) 
                  {
                    ((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_flags = (int)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }

                  static s7_pointer g_addrinfo_ai_family(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, ((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_family));
                  }
                  static s7_pointer g_addrinfo_set_ai_family(s7_scheme *sc, s7_pointer args) 
                  {
                    ((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_family = (int)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }

                  static s7_pointer g_addrinfo_ai_socktype(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, ((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_socktype));
                  }
                  static s7_pointer g_addrinfo_set_ai_socktype(s7_scheme *sc, s7_pointer args) 
                  {
                    ((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_socktype = (int)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }

                  static s7_pointer g_addrinfo_ai_protocol(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_integer(sc, ((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_protocol));
                  }
                  static s7_pointer g_addrinfo_set_ai_protocol(s7_scheme *sc, s7_pointer args) 
                  {
                    ((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_protocol = (int)s7_integer(s7_cadr(args));
                    return(s7_cadr(args));
                  }
                  static s7_pointer g_addrinfo_ai_canonname(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_string(sc, ((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_canonname));
                  }
                  static s7_pointer g_addrinfo_ai_next(s7_scheme *sc, s7_pointer args) 
                  {
                    return(s7_make_c_pointer_with_type(sc, (void *)(((struct addrinfo *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1))->ai_next), s7_make_symbol(sc, "addrinfo*"), s7_f(sc)));
                  }

                  static s7_pointer g_getaddrinfo(s7_scheme *sc, s7_pointer args) 
                  {
                     struct addrinfo *result;
                     int err;
                     err = getaddrinfo(s7_string(s7_car(args)), 
                  		     s7_string(s7_cadr(args)),
                  		     (const struct addrinfo *)s7_c_pointer_with_type(sc, s7_caddr(args), s7_make_symbol(sc, "addrinfo*"), __func__, 1),
                                     &result);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), 
                                          s7_make_c_pointer_with_type(sc, (void *)result, s7_make_symbol(sc, "addrinfo*"), s7_f(sc))));
                  }

                  static s7_pointer g_getnameinfo(s7_scheme *sc, s7_pointer args) 
                  {
                    #ifndef NI_MAXHOST
                      #define NI_MAXHOST 1025
                    #endif
                    #ifndef NI_MAXSERV
                      #define NI_MAXSERV 32
                    #endif
                    char *host, *service;
                    int err;
                    host = (char *)calloc(NI_MAXHOST, sizeof(char));
                    service = (char *)calloc(NI_MAXSERV, sizeof(char));
                    err = getnameinfo((const struct sockaddr *)s7_c_pointer(s7_car(args)), s7_integer(s7_cadr(args)),
                  		    host, NI_MAXHOST,
                  		    service, NI_MAXSERV,
                  		    s7_integer(s7_caddr(args)));
                    return(s7_list(sc, 3, s7_make_integer(sc, err), s7_make_string(sc, host), s7_make_string(sc, service)));
                  }
                           
                  static s7_pointer g_socketpair(s7_scheme *sc, s7_pointer args) 
                  {
                    int fds[2];
                    int err;
                    err = socketpair(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)), fds);
                    return(s7_list(sc, 3, s7_make_integer(sc, err), s7_make_integer(sc, fds[0]), s7_make_integer(sc, fds[1])));
                  }
                           
                  static s7_pointer g_getsockname(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = s7_integer(s7_caddr(args));
                    err = getsockname(s7_integer(s7_car(args)), (struct sockaddr *)s7_c_pointer(s7_cadr(args)), &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, res)));
                  }
                  static s7_pointer g_getpeername(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = s7_integer(s7_caddr(args));
                    err = getpeername(s7_integer(s7_car(args)), (struct sockaddr *)s7_c_pointer(s7_cadr(args)), &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, res)));
                  }
                  static s7_pointer g_accept(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = s7_integer(s7_caddr(args));
                    err = accept(s7_integer(s7_car(args)), (struct sockaddr *)s7_c_pointer(s7_cadr(args)), &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, res)));
                  }
                  static s7_pointer g_getsockopt(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = (socklen_t)s7_integer(s7_list_ref(sc, args, 4));
                    err = getsockopt(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), s7_integer(s7_caddr(args)), s7_c_pointer(s7_cadddr(args)), &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, (s7_int)res)));
                  }
                  static s7_pointer g_setsockopt(s7_scheme *sc, s7_pointer args) 
                  {
                    socklen_t res;
                    res = (socklen_t)s7_integer(s7_list_ref(sc, args, 4));
                    return(s7_make_integer(sc, setsockopt(s7_integer(s7_car(args)), s7_integer(s7_cadr(args)), 
                           s7_integer(s7_caddr(args)), s7_c_pointer(s7_cadddr(args)), res)));
                  }
                  static s7_pointer g_recvfrom(s7_scheme *sc, s7_pointer args) 
                  {
                    int err;
                    socklen_t res;
                    res = (socklen_t)s7_integer(s7_list_ref(sc, args, 5));
                    err = recvfrom(s7_integer(s7_car(args)), 
                  		 s7_c_pointer(s7_cadr(args)),  /* void* buf */
                  		 s7_integer(s7_caddr(args)), 
                  		 s7_integer(s7_cadddr(args)), 
                  		 (struct sockaddr *)s7_c_pointer(s7_list_ref(sc, args, 4)),
                  		 &res);
                    return(s7_list(sc, 2, s7_make_integer(sc, err), s7_make_integer(sc, (s7_int)res)));
                  }

                  static s7_pointer g_hostent_h_name(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct hostent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "hostent*"), __func__, 1))->h_name));}
                  static s7_pointer g_netent_n_name(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct netent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "netent*"), __func__, 1))->n_name));}
                  static s7_pointer g_servent_s_name(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct servent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "servent*"), __func__, 1))->s_name));}
                  static s7_pointer g_servent_s_proto(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct servent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "servent*"), __func__, 1))->s_proto));}
                  static s7_pointer g_protoent_p_name(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_string(sc, ((struct protoent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "protoent*"), __func__, 1))->p_name));}

                  static s7_pointer g_hostent_h_addrtype(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct hostent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "hostent*"), __func__, 1))->h_addrtype));}
                  static s7_pointer g_hostent_h_length(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct hostent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "hostent*"), __func__, 1))->h_length));}
                  static s7_pointer g_netent_n_addrtype(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct netent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "netent*"), __func__, 1))->n_addrtype));}
                  static s7_pointer g_netent_n_net(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct netent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "netent*"), __func__, 1))->n_net));}
                  static s7_pointer g_servent_s_port(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct servent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "servent*"), __func__, 1))->s_port));}
                  static s7_pointer g_protoent_p_proto(s7_scheme *sc, s7_pointer args)
                  {return(s7_make_integer(sc, ((struct protoent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "protoent*"), __func__, 1))->p_proto));}

                  static s7_pointer g_hostent_h_aliases(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    char **str;	     
                    struct hostent *h;
                    p = s7_nil(sc);
                    h = (struct hostent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "hostent*"), __func__, 1);
                    for (str = h->h_aliases; (str) && (*str); str++)
                      p = s7_cons(sc, s7_make_string(sc, *str), p);
                    return(p);
                  }
                  static s7_pointer g_servent_s_aliases(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    char **str;	     
                    struct servent *h;
                    p = s7_nil(sc);
                    h = (struct servent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "servent*"), __func__, 1);
                    for (str = h->s_aliases; (str) && (*str); str++)
                      p = s7_cons(sc, s7_make_string(sc, *str), p);
                    return(p);
                  }
                  static s7_pointer g_netent_n_aliases(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    char **str;	     
                    struct netent *h;
                    p = s7_nil(sc);
                    h = (struct netent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "netvent*"), __func__, 1);
                    for (str = h->n_aliases; (str) && (*str); str++)
                      p = s7_cons(sc, s7_make_string(sc, *str), p);
                    return(p);
                  }
                  static s7_pointer g_protoent_p_aliases(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer p;
                    char **str;	     
                    struct protoent *h;
                    p = s7_nil(sc);
                    h = (struct protoent *)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "protoent*"), __func__, 1);
                    for (str = h->p_aliases; (str) && (*str); str++)
                      p = s7_cons(sc, s7_make_string(sc, *str), p);
                    return(p);
                  }

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

void libc_s7_init(s7_scheme *sc);
void libc_s7_init(s7_scheme *sc)
{
    /* obazl printf("%s%d:%s entry\n", __FILE__, __LINE__, __func__); */
  s7_pointer cur_env;
  s7_pointer pcl_t, pl_tx, pl_ts, pl_ti, pl_txs, pcl_x, pl_xs, pcl_xi, pl_xi, pl_xt, pcl_xs, pl_xxi, pcl_xxi, pl_xis, pl_xxxi, pl_xssx, pcl_i, pl_ix, pl_it, pcl_is, pl_is, pcl_ix, pcl_iix, pcl_isi, pl_ixi, pl_isi, pl_iix, pl_isx, pcl_ixi, pl_isxi, pl_iisi, pcl_iixi, pl_iixi, pl_issi, pl_ixxi, pcl_ixsi, pl_ixiix, pl_iixiixi, pcl_di, pl_ds, pcl_s, pl_sx, pl_st, pl_si, pl_sis, pl_ssi, pl_sssi, pl_ssix, pl_sisi;
  {
    s7_pointer t, x, s, d, i;
    t = s7_t(sc);
    x = s7_make_symbol(sc, "c-pointer?");
    s = s7_make_symbol(sc, "string?");
    d = s7_make_symbol(sc, "float?");
    i = s7_make_symbol(sc, "integer?");

    pcl_t = s7_make_circular_signature(sc, 0, 1, t);
    pl_tx = s7_make_signature(sc, 2, t, x);
    pl_ts = s7_make_signature(sc, 2, t, s);
    pl_ti = s7_make_signature(sc, 2, t, i);
    pl_txs = s7_make_signature(sc, 3, t, x, s);
    pcl_x = s7_make_circular_signature(sc, 0, 1, x);
    pl_xs = s7_make_signature(sc, 2, x, s);
    pcl_xi = s7_make_circular_signature(sc, 1, 2, x, i);
    pl_xi = s7_make_signature(sc, 2, x, i);
    pl_xt = s7_make_signature(sc, 2, x, t);
    pcl_xs = s7_make_circular_signature(sc, 1, 2, x, s);
    pl_xxi = s7_make_signature(sc, 3, x, x, i);
    pcl_xxi = s7_make_circular_signature(sc, 2, 3, x, x, i);
    pl_xis = s7_make_signature(sc, 3, x, i, s);
    pl_xxxi = s7_make_signature(sc, 4, x, x, x, i);
    pl_xssx = s7_make_signature(sc, 4, x, s, s, x);
    pcl_i = s7_make_circular_signature(sc, 0, 1, i);
    pl_ix = s7_make_signature(sc, 2, i, x);
    pl_it = s7_make_signature(sc, 2, i, t);
    pcl_is = s7_make_circular_signature(sc, 1, 2, i, s);
    pl_is = s7_make_signature(sc, 2, i, s);
    pcl_ix = s7_make_circular_signature(sc, 1, 2, i, x);
    pcl_iix = s7_make_circular_signature(sc, 2, 3, i, i, x);
    pcl_isi = s7_make_circular_signature(sc, 2, 3, i, s, i);
    pl_ixi = s7_make_signature(sc, 3, i, x, i);
    pl_isi = s7_make_signature(sc, 3, i, s, i);
    pl_iix = s7_make_signature(sc, 3, i, i, x);
    pl_isx = s7_make_signature(sc, 3, i, s, x);
    pcl_ixi = s7_make_circular_signature(sc, 2, 3, i, x, i);
    pl_isxi = s7_make_signature(sc, 4, i, s, x, i);
    pl_iisi = s7_make_signature(sc, 4, i, i, s, i);
    pcl_iixi = s7_make_circular_signature(sc, 3, 4, i, i, x, i);
    pl_iixi = s7_make_signature(sc, 4, i, i, x, i);
    pl_issi = s7_make_signature(sc, 4, i, s, s, i);
    pl_ixxi = s7_make_signature(sc, 4, i, x, x, i);
    pcl_ixsi = s7_make_circular_signature(sc, 3, 4, i, x, s, i);
    pl_ixiix = s7_make_signature(sc, 5, i, x, i, i, x);
    pl_iixiixi = s7_make_signature(sc, 7, i, i, x, i, i, x, i);
    pcl_di = s7_make_circular_signature(sc, 1, 2, d, i);
    pl_ds = s7_make_signature(sc, 2, d, s);
    pcl_s = s7_make_circular_signature(sc, 0, 1, s);
    pl_sx = s7_make_signature(sc, 2, s, x);
    pl_st = s7_make_signature(sc, 2, s, t);
    pl_si = s7_make_signature(sc, 2, s, i);
    pl_sis = s7_make_signature(sc, 3, s, i, s);
    pl_ssi = s7_make_signature(sc, 3, s, s, i);
    pl_sssi = s7_make_signature(sc, 4, s, s, s, i);
    pl_ssix = s7_make_signature(sc, 4, s, s, i, x);
    pl_sisi = s7_make_signature(sc, 4, s, i, s, i);
  }

  cur_env = s7_curlet(sc);

  struct_msghdr__symbol = s7_make_symbol(sc, "struct-msghdr*");
  const_struct_msghdr__symbol = s7_make_symbol(sc, "const-struct-msghdr*");
  const_struct_sockaddr__symbol = s7_make_symbol(sc, "const-struct-sockaddr*");
  struct_addrinfo__symbol = s7_make_symbol(sc, "struct-addrinfo*");
  netent__symbol = s7_make_symbol(sc, "netent*");
  protoent__symbol = s7_make_symbol(sc, "protoent*");
  servent__symbol = s7_make_symbol(sc, "servent*");
  hostent__symbol = s7_make_symbol(sc, "hostent*");
  struct_rlimit__symbol = s7_make_symbol(sc, "struct-rlimit*");
  sigset_t__symbol = s7_make_symbol(sc, "sigset_t*");
  glob_t__symbol = s7_make_symbol(sc, "glob_t*");
  wordexp_t__symbol = s7_make_symbol(sc, "wordexp_t*");
  passwd__symbol = s7_make_symbol(sc, "passwd*");
  tm__symbol = s7_make_symbol(sc, "tm*");
  time_t__symbol = s7_make_symbol(sc, "time_t*");
  DIR__symbol = s7_make_symbol(sc, "DIR*");
  int__symbol = s7_make_symbol(sc, "int*");
  fpos_t__symbol = s7_make_symbol(sc, "fpos_t*");
  FILE__symbol = s7_make_symbol(sc, "FILE*");
  void__symbol = s7_make_symbol(sc, "void*");
  fenv_t__symbol = s7_make_symbol(sc, "fenv_t*");
  fexcept_t__symbol = s7_make_symbol(sc, "fexcept_t*");

  s7_define(sc, cur_env, s7_make_symbol(sc, "SIG_IGN"), s7_make_c_pointer_with_type(sc, (void*)SIG_IGN, s7_make_symbol(sc, "void*"), s7_f(sc)));
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIG_DFL"), s7_make_c_pointer_with_type(sc, (void*)SIG_DFL, s7_make_symbol(sc, "void*"), s7_f(sc)));
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIG_ERR"), s7_make_c_pointer_with_type(sc, (void*)SIG_ERR, s7_make_symbol(sc, "void*"), s7_f(sc)));
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
  s7_define(sc, cur_env, s7_make_symbol(sc, "stderr"), s7_make_c_pointer_with_type(sc, (void*)stderr, s7_make_symbol(sc, "FILE*"), s7_f(sc)));
  s7_define(sc, cur_env, s7_make_symbol(sc, "stdout"), s7_make_c_pointer_with_type(sc, (void*)stdout, s7_make_symbol(sc, "FILE*"), s7_f(sc)));
  s7_define(sc, cur_env, s7_make_symbol(sc, "stdin"), s7_make_c_pointer_with_type(sc, (void*)stdin, s7_make_symbol(sc, "FILE*"), s7_f(sc)));

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
#ifdef _PATH_SERVICES
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PATH_SERVICES"), s7_make_string(sc, (char*)_PATH_SERVICES));
#endif
#ifdef _PATH_PROTOCOLS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PATH_PROTOCOLS"), s7_make_string(sc, (char*)_PATH_PROTOCOLS));
#endif
#ifdef _PATH_NSSWITCH_CONF
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PATH_NSSWITCH_CONF"), s7_make_string(sc, (char*)_PATH_NSSWITCH_CONF));
#endif
#ifdef _PATH_NETWORKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PATH_NETWORKS"), s7_make_string(sc, (char*)_PATH_NETWORKS));
#endif
#ifdef _PATH_HOSTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PATH_HOSTS"), s7_make_string(sc, (char*)_PATH_HOSTS));
#endif
#ifdef _PATH_HEQUIV
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PATH_HEQUIV"), s7_make_string(sc, (char*)_PATH_HEQUIV));
#endif
#ifdef SHUT_RDWR
  s7_define(sc, cur_env, s7_make_symbol(sc, "SHUT_RDWR"), s7_make_integer(sc, (s7_int)SHUT_RDWR));
#endif
#ifdef SHUT_WR
  s7_define(sc, cur_env, s7_make_symbol(sc, "SHUT_WR"), s7_make_integer(sc, (s7_int)SHUT_WR));
#endif
#ifdef SHUT_RD
  s7_define(sc, cur_env, s7_make_symbol(sc, "SHUT_RD"), s7_make_integer(sc, (s7_int)SHUT_RD));
#endif
#ifdef SOL_IRDA
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOL_IRDA"), s7_make_integer(sc, (s7_int)SOL_IRDA));
#endif
#ifdef SOL_AAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOL_AAL"), s7_make_integer(sc, (s7_int)SOL_AAL));
#endif
#ifdef SOL_ATM
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOL_ATM"), s7_make_integer(sc, (s7_int)SOL_ATM));
#endif
#ifdef SOL_PACKET
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOL_PACKET"), s7_make_integer(sc, (s7_int)SOL_PACKET));
#endif
#ifdef SOL_X25
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOL_X25"), s7_make_integer(sc, (s7_int)SOL_X25));
#endif
#ifdef SOL_DECNET
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOL_DECNET"), s7_make_integer(sc, (s7_int)SOL_DECNET));
#endif
#ifdef SOL_RAW
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOL_RAW"), s7_make_integer(sc, (s7_int)SOL_RAW));
#endif
#ifdef IPPROTO_RAW
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_RAW"), s7_make_integer(sc, (s7_int)IPPROTO_RAW));
#endif
#ifdef IPPROTO_UDPLITE
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_UDPLITE"), s7_make_integer(sc, (s7_int)IPPROTO_UDPLITE));
#endif
#ifdef IPPROTO_SCTP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_SCTP"), s7_make_integer(sc, (s7_int)IPPROTO_SCTP));
#endif
#ifdef IPPROTO_COMP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_COMP"), s7_make_integer(sc, (s7_int)IPPROTO_COMP));
#endif
#ifdef IPPROTO_PIM
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_PIM"), s7_make_integer(sc, (s7_int)IPPROTO_PIM));
#endif
#ifdef IPPROTO_ENCAP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_ENCAP"), s7_make_integer(sc, (s7_int)IPPROTO_ENCAP));
#endif
#ifdef IPPROTO_MTP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_MTP"), s7_make_integer(sc, (s7_int)IPPROTO_MTP));
#endif
#ifdef IPPROTO_DSTOPTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_DSTOPTS"), s7_make_integer(sc, (s7_int)IPPROTO_DSTOPTS));
#endif
#ifdef IPPROTO_NONE
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_NONE"), s7_make_integer(sc, (s7_int)IPPROTO_NONE));
#endif
#ifdef IPPROTO_ICMPV6
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_ICMPV6"), s7_make_integer(sc, (s7_int)IPPROTO_ICMPV6));
#endif
#ifdef IPPROTO_AH
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_AH"), s7_make_integer(sc, (s7_int)IPPROTO_AH));
#endif
#ifdef IPPROTO_ESP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_ESP"), s7_make_integer(sc, (s7_int)IPPROTO_ESP));
#endif
#ifdef IPPROTO_GRE
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_GRE"), s7_make_integer(sc, (s7_int)IPPROTO_GRE));
#endif
#ifdef IPPROTO_RSVP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_RSVP"), s7_make_integer(sc, (s7_int)IPPROTO_RSVP));
#endif
#ifdef IPPROTO_FRAGMENT
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_FRAGMENT"), s7_make_integer(sc, (s7_int)IPPROTO_FRAGMENT));
#endif
#ifdef IPPROTO_ROUTING
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_ROUTING"), s7_make_integer(sc, (s7_int)IPPROTO_ROUTING));
#endif
#ifdef IPPROTO_IPV6
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_IPV6"), s7_make_integer(sc, (s7_int)IPPROTO_IPV6));
#endif
#ifdef IPPROTO_DCCP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_DCCP"), s7_make_integer(sc, (s7_int)IPPROTO_DCCP));
#endif
#ifdef IPPROTO_TP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_TP"), s7_make_integer(sc, (s7_int)IPPROTO_TP));
#endif
#ifdef IPPROTO_IDP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_IDP"), s7_make_integer(sc, (s7_int)IPPROTO_IDP));
#endif
#ifdef IPPROTO_UDP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_UDP"), s7_make_integer(sc, (s7_int)IPPROTO_UDP));
#endif
#ifdef IPPROTO_PUP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_PUP"), s7_make_integer(sc, (s7_int)IPPROTO_PUP));
#endif
#ifdef IPPROTO_EGP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_EGP"), s7_make_integer(sc, (s7_int)IPPROTO_EGP));
#endif
#ifdef IPPROTO_TCP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_TCP"), s7_make_integer(sc, (s7_int)IPPROTO_TCP));
#endif
#ifdef IPPROTO_IPIP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_IPIP"), s7_make_integer(sc, (s7_int)IPPROTO_IPIP));
#endif
#ifdef IPPROTO_IGMP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_IGMP"), s7_make_integer(sc, (s7_int)IPPROTO_IGMP));
#endif
#ifdef IPPROTO_ICMP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_ICMP"), s7_make_integer(sc, (s7_int)IPPROTO_ICMP));
#endif
#ifdef IPPROTO_HOPOPTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_HOPOPTS"), s7_make_integer(sc, (s7_int)IPPROTO_HOPOPTS));
#endif
#ifdef IPPROTO_IP
  s7_define(sc, cur_env, s7_make_symbol(sc, "IPPROTO_IP"), s7_make_integer(sc, (s7_int)IPPROTO_IP));
#endif
#ifdef MSG_CMSG_CLOEXEC
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_CMSG_CLOEXEC"), s7_make_integer(sc, (s7_int)MSG_CMSG_CLOEXEC));
#endif
#ifdef MSG_MORE
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_MORE"), s7_make_integer(sc, (s7_int)MSG_MORE));
#endif
#ifdef MSG_NOSIGNAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_NOSIGNAL"), s7_make_integer(sc, (s7_int)MSG_NOSIGNAL));
#endif
#ifdef MSG_ERRQUEUE
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_ERRQUEUE"), s7_make_integer(sc, (s7_int)MSG_ERRQUEUE));
#endif
#ifdef MSG_RST
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_RST"), s7_make_integer(sc, (s7_int)MSG_RST));
#endif
#ifdef MSG_CONFIRM
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_CONFIRM"), s7_make_integer(sc, (s7_int)MSG_CONFIRM));
#endif
#ifdef MSG_SYN
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_SYN"), s7_make_integer(sc, (s7_int)MSG_SYN));
#endif
#ifdef MSG_FIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_FIN"), s7_make_integer(sc, (s7_int)MSG_FIN));
#endif
#ifdef MSG_WAITALL
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_WAITALL"), s7_make_integer(sc, (s7_int)MSG_WAITALL));
#endif
#ifdef MSG_WAITFORONE
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_WAITFORONE"), s7_make_integer(sc, (s7_int)MSG_WAITFORONE));
#endif
#ifdef MSG_EOR
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_EOR"), s7_make_integer(sc, (s7_int)MSG_EOR));
#endif
#ifdef MSG_DONTWAIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_DONTWAIT"), s7_make_integer(sc, (s7_int)MSG_DONTWAIT));
#endif
#ifdef MSG_TRUNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_TRUNC"), s7_make_integer(sc, (s7_int)MSG_TRUNC));
#endif
#ifdef MSG_PROXY
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_PROXY"), s7_make_integer(sc, (s7_int)MSG_PROXY));
#endif
#ifdef MSG_CTRUNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_CTRUNC"), s7_make_integer(sc, (s7_int)MSG_CTRUNC));
#endif
#ifdef MSG_DONTROUTE
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_DONTROUTE"), s7_make_integer(sc, (s7_int)MSG_DONTROUTE));
#endif
#ifdef MSG_PEEK
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_PEEK"), s7_make_integer(sc, (s7_int)MSG_PEEK));
#endif
#ifdef MSG_OOB
  s7_define(sc, cur_env, s7_make_symbol(sc, "MSG_OOB"), s7_make_integer(sc, (s7_int)MSG_OOB));
#endif
#ifdef AF_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_MAX"), s7_make_integer(sc, (s7_int)AF_MAX));
#endif
#ifdef AF_IEEE802154
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_IEEE802154"), s7_make_integer(sc, (s7_int)AF_IEEE802154));
#endif
#ifdef AF_PHONET
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_PHONET"), s7_make_integer(sc, (s7_int)AF_PHONET));
#endif
#ifdef AF_ISDN
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_ISDN"), s7_make_integer(sc, (s7_int)AF_ISDN));
#endif
#ifdef AF_RXRPC
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_RXRPC"), s7_make_integer(sc, (s7_int)AF_RXRPC));
#endif
#ifdef AF_IUCV
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_IUCV"), s7_make_integer(sc, (s7_int)AF_IUCV));
#endif
#ifdef AF_BLUETOOTH
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_BLUETOOTH"), s7_make_integer(sc, (s7_int)AF_BLUETOOTH));
#endif
#ifdef AF_TIPC
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_TIPC"), s7_make_integer(sc, (s7_int)AF_TIPC));
#endif
#ifdef AF_CAN
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_CAN"), s7_make_integer(sc, (s7_int)AF_CAN));
#endif
#ifdef AF_LLC
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_LLC"), s7_make_integer(sc, (s7_int)AF_LLC));
#endif
#ifdef AF_WANPIPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_WANPIPE"), s7_make_integer(sc, (s7_int)AF_WANPIPE));
#endif
#ifdef AF_PPPOX
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_PPPOX"), s7_make_integer(sc, (s7_int)AF_PPPOX));
#endif
#ifdef AF_IRDA
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_IRDA"), s7_make_integer(sc, (s7_int)AF_IRDA));
#endif
#ifdef AF_SNA
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_SNA"), s7_make_integer(sc, (s7_int)AF_SNA));
#endif
#ifdef AF_RDS
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_RDS"), s7_make_integer(sc, (s7_int)AF_RDS));
#endif
#ifdef AF_ATMSVC
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_ATMSVC"), s7_make_integer(sc, (s7_int)AF_ATMSVC));
#endif
#ifdef AF_ECONET
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_ECONET"), s7_make_integer(sc, (s7_int)AF_ECONET));
#endif
#ifdef AF_ASH
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_ASH"), s7_make_integer(sc, (s7_int)AF_ASH));
#endif
#ifdef AF_PACKET
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_PACKET"), s7_make_integer(sc, (s7_int)AF_PACKET));
#endif
#ifdef AF_ROUTE
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_ROUTE"), s7_make_integer(sc, (s7_int)AF_ROUTE));
#endif
#ifdef AF_NETLINK
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_NETLINK"), s7_make_integer(sc, (s7_int)AF_NETLINK));
#endif
#ifdef AF_KEY
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_KEY"), s7_make_integer(sc, (s7_int)AF_KEY));
#endif
#ifdef AF_SECURITY
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_SECURITY"), s7_make_integer(sc, (s7_int)AF_SECURITY));
#endif
#ifdef AF_NETBEUI
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_NETBEUI"), s7_make_integer(sc, (s7_int)AF_NETBEUI));
#endif
#ifdef AF_DECnet
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_DECnet"), s7_make_integer(sc, (s7_int)AF_DECnet));
#endif
#ifdef AF_ROSE
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_ROSE"), s7_make_integer(sc, (s7_int)AF_ROSE));
#endif
#ifdef AF_INET6
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_INET6"), s7_make_integer(sc, (s7_int)AF_INET6));
#endif
#ifdef AF_X25
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_X25"), s7_make_integer(sc, (s7_int)AF_X25));
#endif
#ifdef AF_ATMPVC
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_ATMPVC"), s7_make_integer(sc, (s7_int)AF_ATMPVC));
#endif
#ifdef AF_BRIDGE
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_BRIDGE"), s7_make_integer(sc, (s7_int)AF_BRIDGE));
#endif
#ifdef AF_NETROM
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_NETROM"), s7_make_integer(sc, (s7_int)AF_NETROM));
#endif
#ifdef AF_APPLETALK
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_APPLETALK"), s7_make_integer(sc, (s7_int)AF_APPLETALK));
#endif
#ifdef AF_IPX
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_IPX"), s7_make_integer(sc, (s7_int)AF_IPX));
#endif
#ifdef AF_AX25
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_AX25"), s7_make_integer(sc, (s7_int)AF_AX25));
#endif
#ifdef AF_INET
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_INET"), s7_make_integer(sc, (s7_int)AF_INET));
#endif
#ifdef AF_FILE
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_FILE"), s7_make_integer(sc, (s7_int)AF_FILE));
#endif
#ifdef AF_UNIX
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_UNIX"), s7_make_integer(sc, (s7_int)AF_UNIX));
#endif
#ifdef AF_LOCAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_LOCAL"), s7_make_integer(sc, (s7_int)AF_LOCAL));
#endif
#ifdef AF_UNSPEC
  s7_define(sc, cur_env, s7_make_symbol(sc, "AF_UNSPEC"), s7_make_integer(sc, (s7_int)AF_UNSPEC));
#endif
#ifdef PF_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_MAX"), s7_make_integer(sc, (s7_int)PF_MAX));
#endif
#ifdef PF_IEEE802154
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_IEEE802154"), s7_make_integer(sc, (s7_int)PF_IEEE802154));
#endif
#ifdef PF_PHONET
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_PHONET"), s7_make_integer(sc, (s7_int)PF_PHONET));
#endif
#ifdef PF_ISDN
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_ISDN"), s7_make_integer(sc, (s7_int)PF_ISDN));
#endif
#ifdef PF_RXRPC
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_RXRPC"), s7_make_integer(sc, (s7_int)PF_RXRPC));
#endif
#ifdef PF_IUCV
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_IUCV"), s7_make_integer(sc, (s7_int)PF_IUCV));
#endif
#ifdef PF_BLUETOOTH
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_BLUETOOTH"), s7_make_integer(sc, (s7_int)PF_BLUETOOTH));
#endif
#ifdef PF_TIPC
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_TIPC"), s7_make_integer(sc, (s7_int)PF_TIPC));
#endif
#ifdef PF_CAN
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_CAN"), s7_make_integer(sc, (s7_int)PF_CAN));
#endif
#ifdef PF_LLC
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_LLC"), s7_make_integer(sc, (s7_int)PF_LLC));
#endif
#ifdef PF_WANPIPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_WANPIPE"), s7_make_integer(sc, (s7_int)PF_WANPIPE));
#endif
#ifdef PF_PPPOX
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_PPPOX"), s7_make_integer(sc, (s7_int)PF_PPPOX));
#endif
#ifdef PF_IRDA
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_IRDA"), s7_make_integer(sc, (s7_int)PF_IRDA));
#endif
#ifdef PF_SNA
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_SNA"), s7_make_integer(sc, (s7_int)PF_SNA));
#endif
#ifdef PF_RDS
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_RDS"), s7_make_integer(sc, (s7_int)PF_RDS));
#endif
#ifdef PF_ATMSVC
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_ATMSVC"), s7_make_integer(sc, (s7_int)PF_ATMSVC));
#endif
#ifdef PF_ECONET
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_ECONET"), s7_make_integer(sc, (s7_int)PF_ECONET));
#endif
#ifdef PF_ASH
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_ASH"), s7_make_integer(sc, (s7_int)PF_ASH));
#endif
#ifdef PF_PACKET
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_PACKET"), s7_make_integer(sc, (s7_int)PF_PACKET));
#endif
#ifdef PF_ROUTE
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_ROUTE"), s7_make_integer(sc, (s7_int)PF_ROUTE));
#endif
#ifdef PF_NETLINK
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_NETLINK"), s7_make_integer(sc, (s7_int)PF_NETLINK));
#endif
#ifdef PF_KEY
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_KEY"), s7_make_integer(sc, (s7_int)PF_KEY));
#endif
#ifdef PF_SECURITY
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_SECURITY"), s7_make_integer(sc, (s7_int)PF_SECURITY));
#endif
#ifdef PF_NETBEUI
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_NETBEUI"), s7_make_integer(sc, (s7_int)PF_NETBEUI));
#endif
#ifdef PF_DECnet
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_DECnet"), s7_make_integer(sc, (s7_int)PF_DECnet));
#endif
#ifdef PF_ROSE
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_ROSE"), s7_make_integer(sc, (s7_int)PF_ROSE));
#endif
#ifdef PF_INET6
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_INET6"), s7_make_integer(sc, (s7_int)PF_INET6));
#endif
#ifdef PF_X25
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_X25"), s7_make_integer(sc, (s7_int)PF_X25));
#endif
#ifdef PF_ATMPVC
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_ATMPVC"), s7_make_integer(sc, (s7_int)PF_ATMPVC));
#endif
#ifdef PF_BRIDGE
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_BRIDGE"), s7_make_integer(sc, (s7_int)PF_BRIDGE));
#endif
#ifdef PF_NETROM
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_NETROM"), s7_make_integer(sc, (s7_int)PF_NETROM));
#endif
#ifdef PF_APPLETALK
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_APPLETALK"), s7_make_integer(sc, (s7_int)PF_APPLETALK));
#endif
#ifdef PF_IPX
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_IPX"), s7_make_integer(sc, (s7_int)PF_IPX));
#endif
#ifdef PF_AX25
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_AX25"), s7_make_integer(sc, (s7_int)PF_AX25));
#endif
#ifdef PF_INET
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_INET"), s7_make_integer(sc, (s7_int)PF_INET));
#endif
#ifdef PF_FILE
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_FILE"), s7_make_integer(sc, (s7_int)PF_FILE));
#endif
#ifdef PF_UNIX
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_UNIX"), s7_make_integer(sc, (s7_int)PF_UNIX));
#endif
#ifdef PF_LOCAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_LOCAL"), s7_make_integer(sc, (s7_int)PF_LOCAL));
#endif
#ifdef PF_UNSPEC
  s7_define(sc, cur_env, s7_make_symbol(sc, "PF_UNSPEC"), s7_make_integer(sc, (s7_int)PF_UNSPEC));
#endif
#ifdef SOCK_NONBLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOCK_NONBLOCK"), s7_make_integer(sc, (s7_int)SOCK_NONBLOCK));
#endif
#ifdef SOCK_CLOEXEC
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOCK_CLOEXEC"), s7_make_integer(sc, (s7_int)SOCK_CLOEXEC));
#endif
#ifdef SOCK_PACKET
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOCK_PACKET"), s7_make_integer(sc, (s7_int)SOCK_PACKET));
#endif
#ifdef SOCK_DCCP
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOCK_DCCP"), s7_make_integer(sc, (s7_int)SOCK_DCCP));
#endif
#ifdef SOCK_SEQPACKET
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOCK_SEQPACKET"), s7_make_integer(sc, (s7_int)SOCK_SEQPACKET));
#endif
#ifdef SOCK_RDM
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOCK_RDM"), s7_make_integer(sc, (s7_int)SOCK_RDM));
#endif
#ifdef SOCK_RAW
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOCK_RAW"), s7_make_integer(sc, (s7_int)SOCK_RAW));
#endif
#ifdef SOCK_DGRAM
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOCK_DGRAM"), s7_make_integer(sc, (s7_int)SOCK_DGRAM));
#endif
#ifdef SOCK_STREAM
  s7_define(sc, cur_env, s7_make_symbol(sc, "SOCK_STREAM"), s7_make_integer(sc, (s7_int)SOCK_STREAM));
#endif
#ifdef NI_DGRAM
  s7_define(sc, cur_env, s7_make_symbol(sc, "NI_DGRAM"), s7_make_integer(sc, (s7_int)NI_DGRAM));
#endif
#ifdef NI_NAMEREQD
  s7_define(sc, cur_env, s7_make_symbol(sc, "NI_NAMEREQD"), s7_make_integer(sc, (s7_int)NI_NAMEREQD));
#endif
#ifdef NI_NOFQDN
  s7_define(sc, cur_env, s7_make_symbol(sc, "NI_NOFQDN"), s7_make_integer(sc, (s7_int)NI_NOFQDN));
#endif
#ifdef NI_NUMERICSERV
  s7_define(sc, cur_env, s7_make_symbol(sc, "NI_NUMERICSERV"), s7_make_integer(sc, (s7_int)NI_NUMERICSERV));
#endif
#ifdef NI_NUMERICHOST
  s7_define(sc, cur_env, s7_make_symbol(sc, "NI_NUMERICHOST"), s7_make_integer(sc, (s7_int)NI_NUMERICHOST));
#endif
#ifdef EAI_OVERFLOW
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_OVERFLOW"), s7_make_integer(sc, (s7_int)EAI_OVERFLOW));
#endif
#ifdef EAI_SYSTEM
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_SYSTEM"), s7_make_integer(sc, (s7_int)EAI_SYSTEM));
#endif
#ifdef EAI_MEMORY
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_MEMORY"), s7_make_integer(sc, (s7_int)EAI_MEMORY));
#endif
#ifdef EAI_SERVICE
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_SERVICE"), s7_make_integer(sc, (s7_int)EAI_SERVICE));
#endif
#ifdef EAI_SOCKTYPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_SOCKTYPE"), s7_make_integer(sc, (s7_int)EAI_SOCKTYPE));
#endif
#ifdef EAI_FAMILY
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_FAMILY"), s7_make_integer(sc, (s7_int)EAI_FAMILY));
#endif
#ifdef EAI_FAIL
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_FAIL"), s7_make_integer(sc, (s7_int)EAI_FAIL));
#endif
#ifdef EAI_AGAIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_AGAIN"), s7_make_integer(sc, (s7_int)EAI_AGAIN));
#endif
#ifdef EAI_NONAME
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_NONAME"), s7_make_integer(sc, (s7_int)EAI_NONAME));
#endif
#ifdef EAI_BADFLAGS
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAI_BADFLAGS"), s7_make_integer(sc, (s7_int)EAI_BADFLAGS));
#endif
#ifdef AI_NUMERICSERV
  s7_define(sc, cur_env, s7_make_symbol(sc, "AI_NUMERICSERV"), s7_make_integer(sc, (s7_int)AI_NUMERICSERV));
#endif
#ifdef AI_ADDRCONFIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "AI_ADDRCONFIG"), s7_make_integer(sc, (s7_int)AI_ADDRCONFIG));
#endif
#ifdef AI_ALL
  s7_define(sc, cur_env, s7_make_symbol(sc, "AI_ALL"), s7_make_integer(sc, (s7_int)AI_ALL));
#endif
#ifdef AI_V4MAPPED
  s7_define(sc, cur_env, s7_make_symbol(sc, "AI_V4MAPPED"), s7_make_integer(sc, (s7_int)AI_V4MAPPED));
#endif
#ifdef AI_NUMERICHOST
  s7_define(sc, cur_env, s7_make_symbol(sc, "AI_NUMERICHOST"), s7_make_integer(sc, (s7_int)AI_NUMERICHOST));
#endif
#ifdef AI_CANONNAME
  s7_define(sc, cur_env, s7_make_symbol(sc, "AI_CANONNAME"), s7_make_integer(sc, (s7_int)AI_CANONNAME));
#endif
#ifdef AI_PASSIVE
  s7_define(sc, cur_env, s7_make_symbol(sc, "AI_PASSIVE"), s7_make_integer(sc, (s7_int)AI_PASSIVE));
#endif
#ifdef SIG_SETMASK
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIG_SETMASK"), s7_make_integer(sc, (s7_int)SIG_SETMASK));
#endif
#ifdef SIG_UNBLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIG_UNBLOCK"), s7_make_integer(sc, (s7_int)SIG_UNBLOCK));
#endif
#ifdef SIG_BLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIG_BLOCK"), s7_make_integer(sc, (s7_int)SIG_BLOCK));
#endif
#ifdef SA_STACK
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_STACK"), s7_make_integer(sc, (s7_int)SA_STACK));
#endif
#ifdef SA_ONESHOT
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_ONESHOT"), s7_make_integer(sc, (s7_int)SA_ONESHOT));
#endif
#ifdef SA_NOMASK
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_NOMASK"), s7_make_integer(sc, (s7_int)SA_NOMASK));
#endif
#ifdef SA_RESETHAND
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_RESETHAND"), s7_make_integer(sc, (s7_int)SA_RESETHAND));
#endif
#ifdef SA_NODEFER
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_NODEFER"), s7_make_integer(sc, (s7_int)SA_NODEFER));
#endif
#ifdef SA_RESTART
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_RESTART"), s7_make_integer(sc, (s7_int)SA_RESTART));
#endif
#ifdef SA_ONSTACK
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_ONSTACK"), s7_make_integer(sc, (s7_int)SA_ONSTACK));
#endif
#ifdef SA_SIGINFO
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_SIGINFO"), s7_make_integer(sc, (s7_int)SA_SIGINFO));
#endif
#ifdef SA_NOCLDWAIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_NOCLDWAIT"), s7_make_integer(sc, (s7_int)SA_NOCLDWAIT));
#endif
#ifdef SA_NOCLDSTOP
  s7_define(sc, cur_env, s7_make_symbol(sc, "SA_NOCLDSTOP"), s7_make_integer(sc, (s7_int)SA_NOCLDSTOP));
#endif
#ifdef PRIO_USER
  s7_define(sc, cur_env, s7_make_symbol(sc, "PRIO_USER"), s7_make_integer(sc, (s7_int)PRIO_USER));
#endif
#ifdef PRIO_PGRP
  s7_define(sc, cur_env, s7_make_symbol(sc, "PRIO_PGRP"), s7_make_integer(sc, (s7_int)PRIO_PGRP));
#endif
#ifdef PRIO_PROCESS
  s7_define(sc, cur_env, s7_make_symbol(sc, "PRIO_PROCESS"), s7_make_integer(sc, (s7_int)PRIO_PROCESS));
#endif
#ifdef PRIO_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "PRIO_MAX"), s7_make_integer(sc, (s7_int)PRIO_MAX));
#endif
#ifdef PRIO_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "PRIO_MIN"), s7_make_integer(sc, (s7_int)PRIO_MIN));
#endif
#ifdef RUSAGE_LWP
  s7_define(sc, cur_env, s7_make_symbol(sc, "RUSAGE_LWP"), s7_make_integer(sc, (s7_int)RUSAGE_LWP));
#endif
#ifdef RUSAGE_THREAD
  s7_define(sc, cur_env, s7_make_symbol(sc, "RUSAGE_THREAD"), s7_make_integer(sc, (s7_int)RUSAGE_THREAD));
#endif
#ifdef RUSAGE_CHILDREN
  s7_define(sc, cur_env, s7_make_symbol(sc, "RUSAGE_CHILDREN"), s7_make_integer(sc, (s7_int)RUSAGE_CHILDREN));
#endif
#ifdef RUSAGE_SELF
  s7_define(sc, cur_env, s7_make_symbol(sc, "RUSAGE_SELF"), s7_make_integer(sc, (s7_int)RUSAGE_SELF));
#endif
#ifdef RLIM_SAVED_CUR
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIM_SAVED_CUR"), s7_make_integer(sc, (s7_int)RLIM_SAVED_CUR));
#endif
#ifdef RLIM_SAVED_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIM_SAVED_MAX"), s7_make_integer(sc, (s7_int)RLIM_SAVED_MAX));
#endif
#ifdef RLIM_INFINITY
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIM_INFINITY"), s7_make_integer(sc, (s7_int)RLIM_INFINITY));
#endif
#ifdef RLIM_NLIMITS
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIM_NLIMITS"), s7_make_integer(sc, (s7_int)RLIM_NLIMITS));
#endif
#ifdef RLIMIT_NLIMITS
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_NLIMITS"), s7_make_integer(sc, (s7_int)RLIMIT_NLIMITS));
#endif
#ifdef RLIMIT_RTPRIO
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_RTPRIO"), s7_make_integer(sc, (s7_int)RLIMIT_RTPRIO));
#endif
#ifdef RLIMIT_NICE
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_NICE"), s7_make_integer(sc, (s7_int)RLIMIT_NICE));
#endif
#ifdef RLIMIT_MSGQUEUE
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_MSGQUEUE"), s7_make_integer(sc, (s7_int)RLIMIT_MSGQUEUE));
#endif
#ifdef RLIMIT_SIGPENDING
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_SIGPENDING"), s7_make_integer(sc, (s7_int)RLIMIT_SIGPENDING));
#endif
#ifdef RLIMIT_LOCKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_LOCKS"), s7_make_integer(sc, (s7_int)RLIMIT_LOCKS));
#endif
#ifdef RLIMIT_MEMLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_MEMLOCK"), s7_make_integer(sc, (s7_int)RLIMIT_MEMLOCK));
#endif
#ifdef RLIMIT_NPROC
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_NPROC"), s7_make_integer(sc, (s7_int)RLIMIT_NPROC));
#endif
#ifdef RLIMIT_AS
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_AS"), s7_make_integer(sc, (s7_int)RLIMIT_AS));
#endif
#ifdef RLIMIT_OFILE
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_OFILE"), s7_make_integer(sc, (s7_int)RLIMIT_OFILE));
#endif
#ifdef RLIMIT_NOFILE
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_NOFILE"), s7_make_integer(sc, (s7_int)RLIMIT_NOFILE));
#endif
#ifdef RLIMIT_RSS
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_RSS"), s7_make_integer(sc, (s7_int)RLIMIT_RSS));
#endif
#ifdef RLIMIT_CORE
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_CORE"), s7_make_integer(sc, (s7_int)RLIMIT_CORE));
#endif
#ifdef RLIMIT_STACK
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_STACK"), s7_make_integer(sc, (s7_int)RLIMIT_STACK));
#endif
#ifdef RLIMIT_DATA
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_DATA"), s7_make_integer(sc, (s7_int)RLIMIT_DATA));
#endif
#ifdef RLIMIT_FSIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_FSIZE"), s7_make_integer(sc, (s7_int)RLIMIT_FSIZE));
#endif
#ifdef RLIMIT_CPU
  s7_define(sc, cur_env, s7_make_symbol(sc, "RLIMIT_CPU"), s7_make_integer(sc, (s7_int)RLIMIT_CPU));
#endif
#ifdef WNOWAIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "WNOWAIT"), s7_make_integer(sc, (s7_int)WNOWAIT));
#endif
#ifdef WCONTINUED
  s7_define(sc, cur_env, s7_make_symbol(sc, "WCONTINUED"), s7_make_integer(sc, (s7_int)WCONTINUED));
#endif
#ifdef WEXITED
  s7_define(sc, cur_env, s7_make_symbol(sc, "WEXITED"), s7_make_integer(sc, (s7_int)WEXITED));
#endif
#ifdef WSTOPPED
  s7_define(sc, cur_env, s7_make_symbol(sc, "WSTOPPED"), s7_make_integer(sc, (s7_int)WSTOPPED));
#endif
#ifdef WUNTRACED
  s7_define(sc, cur_env, s7_make_symbol(sc, "WUNTRACED"), s7_make_integer(sc, (s7_int)WUNTRACED));
#endif
#ifdef WNOHANG
  s7_define(sc, cur_env, s7_make_symbol(sc, "WNOHANG"), s7_make_integer(sc, (s7_int)WNOHANG));
#endif
#ifdef SIGSYS
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGSYS"), s7_make_integer(sc, (s7_int)SIGSYS));
#endif
#ifdef SIGPWR
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGPWR"), s7_make_integer(sc, (s7_int)SIGPWR));
#endif
#ifdef SIGIO
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGIO"), s7_make_integer(sc, (s7_int)SIGIO));
#endif
#ifdef SIGPOLL
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGPOLL"), s7_make_integer(sc, (s7_int)SIGPOLL));
#endif
#ifdef SIGWINCH
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGWINCH"), s7_make_integer(sc, (s7_int)SIGWINCH));
#endif
#ifdef SIGPROF
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGPROF"), s7_make_integer(sc, (s7_int)SIGPROF));
#endif
#ifdef SIGVTALRM
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGVTALRM"), s7_make_integer(sc, (s7_int)SIGVTALRM));
#endif
#ifdef SIGXFSZ
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGXFSZ"), s7_make_integer(sc, (s7_int)SIGXFSZ));
#endif
#ifdef SIGXCPU
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGXCPU"), s7_make_integer(sc, (s7_int)SIGXCPU));
#endif
#ifdef SIGURG
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGURG"), s7_make_integer(sc, (s7_int)SIGURG));
#endif
#ifdef SIGTTOU
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGTTOU"), s7_make_integer(sc, (s7_int)SIGTTOU));
#endif
#ifdef SIGTTIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGTTIN"), s7_make_integer(sc, (s7_int)SIGTTIN));
#endif
#ifdef SIGTSTP
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGTSTP"), s7_make_integer(sc, (s7_int)SIGTSTP));
#endif
#ifdef SIGSTOP
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGSTOP"), s7_make_integer(sc, (s7_int)SIGSTOP));
#endif
#ifdef SIGCONT
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGCONT"), s7_make_integer(sc, (s7_int)SIGCONT));
#endif
#ifdef SIGCHLD
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGCHLD"), s7_make_integer(sc, (s7_int)SIGCHLD));
#endif
#ifdef SIGCLD
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGCLD"), s7_make_integer(sc, (s7_int)SIGCLD));
#endif
#ifdef SIGSTKFLT
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGSTKFLT"), s7_make_integer(sc, (s7_int)SIGSTKFLT));
#endif
#ifdef SIGTERM
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGTERM"), s7_make_integer(sc, (s7_int)SIGTERM));
#endif
#ifdef SIGALRM
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGALRM"), s7_make_integer(sc, (s7_int)SIGALRM));
#endif
#ifdef SIGPIPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGPIPE"), s7_make_integer(sc, (s7_int)SIGPIPE));
#endif
#ifdef SIGUSR2
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGUSR2"), s7_make_integer(sc, (s7_int)SIGUSR2));
#endif
#ifdef SIGSEGV
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGSEGV"), s7_make_integer(sc, (s7_int)SIGSEGV));
#endif
#ifdef SIGUSR1
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGUSR1"), s7_make_integer(sc, (s7_int)SIGUSR1));
#endif
#ifdef SIGKILL
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGKILL"), s7_make_integer(sc, (s7_int)SIGKILL));
#endif
#ifdef SIGFPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGFPE"), s7_make_integer(sc, (s7_int)SIGFPE));
#endif
#ifdef SIGBUS
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGBUS"), s7_make_integer(sc, (s7_int)SIGBUS));
#endif
#ifdef SIGIOT
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGIOT"), s7_make_integer(sc, (s7_int)SIGIOT));
#endif
#ifdef SIGABRT
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGABRT"), s7_make_integer(sc, (s7_int)SIGABRT));
#endif
#ifdef SIGTRAP
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGTRAP"), s7_make_integer(sc, (s7_int)SIGTRAP));
#endif
#ifdef SIGILL
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGILL"), s7_make_integer(sc, (s7_int)SIGILL));
#endif
#ifdef SIGQUIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGQUIT"), s7_make_integer(sc, (s7_int)SIGQUIT));
#endif
#ifdef SIGINT
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGINT"), s7_make_integer(sc, (s7_int)SIGINT));
#endif
#ifdef SIGHUP
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIGHUP"), s7_make_integer(sc, (s7_int)SIGHUP));
#endif
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
#ifdef NSS_BUFLEN_PASSWD
  s7_define(sc, cur_env, s7_make_symbol(sc, "NSS_BUFLEN_PASSWD"), s7_make_integer(sc, (s7_int)NSS_BUFLEN_PASSWD));
#endif
#ifdef TCSAFLUSH
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCSAFLUSH"), s7_make_integer(sc, (s7_int)TCSAFLUSH));
#endif
#ifdef TCSADRAIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCSADRAIN"), s7_make_integer(sc, (s7_int)TCSADRAIN));
#endif
#ifdef TCSANOW
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCSANOW"), s7_make_integer(sc, (s7_int)TCSANOW));
#endif
#ifdef TCIOFLUSH
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCIOFLUSH"), s7_make_integer(sc, (s7_int)TCIOFLUSH));
#endif
#ifdef TCOFLUSH
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCOFLUSH"), s7_make_integer(sc, (s7_int)TCOFLUSH));
#endif
#ifdef TCIFLUSH
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCIFLUSH"), s7_make_integer(sc, (s7_int)TCIFLUSH));
#endif
#ifdef TCION
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCION"), s7_make_integer(sc, (s7_int)TCION));
#endif
#ifdef TCIOFF
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCIOFF"), s7_make_integer(sc, (s7_int)TCIOFF));
#endif
#ifdef TCOON
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCOON"), s7_make_integer(sc, (s7_int)TCOON));
#endif
#ifdef TCOOFF
  s7_define(sc, cur_env, s7_make_symbol(sc, "TCOOFF"), s7_make_integer(sc, (s7_int)TCOOFF));
#endif
#ifdef IEXTEN
  s7_define(sc, cur_env, s7_make_symbol(sc, "IEXTEN"), s7_make_integer(sc, (s7_int)IEXTEN));
#endif
#ifdef TOSTOP
  s7_define(sc, cur_env, s7_make_symbol(sc, "TOSTOP"), s7_make_integer(sc, (s7_int)TOSTOP));
#endif
#ifdef NOFLSH
  s7_define(sc, cur_env, s7_make_symbol(sc, "NOFLSH"), s7_make_integer(sc, (s7_int)NOFLSH));
#endif
#ifdef ECHONL
  s7_define(sc, cur_env, s7_make_symbol(sc, "ECHONL"), s7_make_integer(sc, (s7_int)ECHONL));
#endif
#ifdef ECHOK
  s7_define(sc, cur_env, s7_make_symbol(sc, "ECHOK"), s7_make_integer(sc, (s7_int)ECHOK));
#endif
#ifdef ECHOE
  s7_define(sc, cur_env, s7_make_symbol(sc, "ECHOE"), s7_make_integer(sc, (s7_int)ECHOE));
#endif
#ifdef ECHO
  s7_define(sc, cur_env, s7_make_symbol(sc, "ECHO"), s7_make_integer(sc, (s7_int)ECHO));
#endif
#ifdef ICANON
  s7_define(sc, cur_env, s7_make_symbol(sc, "ICANON"), s7_make_integer(sc, (s7_int)ICANON));
#endif
#ifdef ISIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "ISIG"), s7_make_integer(sc, (s7_int)ISIG));
#endif
#ifdef OFDEL
  s7_define(sc, cur_env, s7_make_symbol(sc, "OFDEL"), s7_make_integer(sc, (s7_int)OFDEL));
#endif
#ifdef OFILL
  s7_define(sc, cur_env, s7_make_symbol(sc, "OFILL"), s7_make_integer(sc, (s7_int)OFILL));
#endif
#ifdef ONLRET
  s7_define(sc, cur_env, s7_make_symbol(sc, "ONLRET"), s7_make_integer(sc, (s7_int)ONLRET));
#endif
#ifdef ONOCR
  s7_define(sc, cur_env, s7_make_symbol(sc, "ONOCR"), s7_make_integer(sc, (s7_int)ONOCR));
#endif
#ifdef OCRNL
  s7_define(sc, cur_env, s7_make_symbol(sc, "OCRNL"), s7_make_integer(sc, (s7_int)OCRNL));
#endif
#ifdef ONLCR
  s7_define(sc, cur_env, s7_make_symbol(sc, "ONLCR"), s7_make_integer(sc, (s7_int)ONLCR));
#endif
#ifdef OLCUC
  s7_define(sc, cur_env, s7_make_symbol(sc, "OLCUC"), s7_make_integer(sc, (s7_int)OLCUC));
#endif
#ifdef OPOST
  s7_define(sc, cur_env, s7_make_symbol(sc, "OPOST"), s7_make_integer(sc, (s7_int)OPOST));
#endif
#ifdef IUTF8
  s7_define(sc, cur_env, s7_make_symbol(sc, "IUTF8"), s7_make_integer(sc, (s7_int)IUTF8));
#endif
#ifdef IMAXBEL
  s7_define(sc, cur_env, s7_make_symbol(sc, "IMAXBEL"), s7_make_integer(sc, (s7_int)IMAXBEL));
#endif
#ifdef IXOFF
  s7_define(sc, cur_env, s7_make_symbol(sc, "IXOFF"), s7_make_integer(sc, (s7_int)IXOFF));
#endif
#ifdef IXANY
  s7_define(sc, cur_env, s7_make_symbol(sc, "IXANY"), s7_make_integer(sc, (s7_int)IXANY));
#endif
#ifdef IXON
  s7_define(sc, cur_env, s7_make_symbol(sc, "IXON"), s7_make_integer(sc, (s7_int)IXON));
#endif
#ifdef IUCLC
  s7_define(sc, cur_env, s7_make_symbol(sc, "IUCLC"), s7_make_integer(sc, (s7_int)IUCLC));
#endif
#ifdef ICRNL
  s7_define(sc, cur_env, s7_make_symbol(sc, "ICRNL"), s7_make_integer(sc, (s7_int)ICRNL));
#endif
#ifdef IGNCR
  s7_define(sc, cur_env, s7_make_symbol(sc, "IGNCR"), s7_make_integer(sc, (s7_int)IGNCR));
#endif
#ifdef INLCR
  s7_define(sc, cur_env, s7_make_symbol(sc, "INLCR"), s7_make_integer(sc, (s7_int)INLCR));
#endif
#ifdef ISTRIP
  s7_define(sc, cur_env, s7_make_symbol(sc, "ISTRIP"), s7_make_integer(sc, (s7_int)ISTRIP));
#endif
#ifdef INPCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "INPCK"), s7_make_integer(sc, (s7_int)INPCK));
#endif
#ifdef PARMRK
  s7_define(sc, cur_env, s7_make_symbol(sc, "PARMRK"), s7_make_integer(sc, (s7_int)PARMRK));
#endif
#ifdef IGNPAR
  s7_define(sc, cur_env, s7_make_symbol(sc, "IGNPAR"), s7_make_integer(sc, (s7_int)IGNPAR));
#endif
#ifdef BRKINT
  s7_define(sc, cur_env, s7_make_symbol(sc, "BRKINT"), s7_make_integer(sc, (s7_int)BRKINT));
#endif
#ifdef IGNBRK
  s7_define(sc, cur_env, s7_make_symbol(sc, "IGNBRK"), s7_make_integer(sc, (s7_int)IGNBRK));
#endif
#ifdef VEOL2
  s7_define(sc, cur_env, s7_make_symbol(sc, "VEOL2"), s7_make_integer(sc, (s7_int)VEOL2));
#endif
#ifdef VLNEXT
  s7_define(sc, cur_env, s7_make_symbol(sc, "VLNEXT"), s7_make_integer(sc, (s7_int)VLNEXT));
#endif
#ifdef VWERASE
  s7_define(sc, cur_env, s7_make_symbol(sc, "VWERASE"), s7_make_integer(sc, (s7_int)VWERASE));
#endif
#ifdef VDISCARD
  s7_define(sc, cur_env, s7_make_symbol(sc, "VDISCARD"), s7_make_integer(sc, (s7_int)VDISCARD));
#endif
#ifdef VREPRINT
  s7_define(sc, cur_env, s7_make_symbol(sc, "VREPRINT"), s7_make_integer(sc, (s7_int)VREPRINT));
#endif
#ifdef VEOL
  s7_define(sc, cur_env, s7_make_symbol(sc, "VEOL"), s7_make_integer(sc, (s7_int)VEOL));
#endif
#ifdef VSUSP
  s7_define(sc, cur_env, s7_make_symbol(sc, "VSUSP"), s7_make_integer(sc, (s7_int)VSUSP));
#endif
#ifdef VSTOP
  s7_define(sc, cur_env, s7_make_symbol(sc, "VSTOP"), s7_make_integer(sc, (s7_int)VSTOP));
#endif
#ifdef VSTART
  s7_define(sc, cur_env, s7_make_symbol(sc, "VSTART"), s7_make_integer(sc, (s7_int)VSTART));
#endif
#ifdef VSWTC
  s7_define(sc, cur_env, s7_make_symbol(sc, "VSWTC"), s7_make_integer(sc, (s7_int)VSWTC));
#endif
#ifdef VMIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "VMIN"), s7_make_integer(sc, (s7_int)VMIN));
#endif
#ifdef VTIME
  s7_define(sc, cur_env, s7_make_symbol(sc, "VTIME"), s7_make_integer(sc, (s7_int)VTIME));
#endif
#ifdef VEOF
  s7_define(sc, cur_env, s7_make_symbol(sc, "VEOF"), s7_make_integer(sc, (s7_int)VEOF));
#endif
#ifdef VKILL
  s7_define(sc, cur_env, s7_make_symbol(sc, "VKILL"), s7_make_integer(sc, (s7_int)VKILL));
#endif
#ifdef VERASE
  s7_define(sc, cur_env, s7_make_symbol(sc, "VERASE"), s7_make_integer(sc, (s7_int)VERASE));
#endif
#ifdef VQUIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "VQUIT"), s7_make_integer(sc, (s7_int)VQUIT));
#endif
#ifdef VINTR
  s7_define(sc, cur_env, s7_make_symbol(sc, "VINTR"), s7_make_integer(sc, (s7_int)VINTR));
#endif
#ifdef CLOCK_MONOTONIC_COARSE
  s7_define(sc, cur_env, s7_make_symbol(sc, "CLOCK_MONOTONIC_COARSE"), s7_make_integer(sc, (s7_int)CLOCK_MONOTONIC_COARSE));
#endif
#ifdef CLOCK_REALTIME_COARSE
  s7_define(sc, cur_env, s7_make_symbol(sc, "CLOCK_REALTIME_COARSE"), s7_make_integer(sc, (s7_int)CLOCK_REALTIME_COARSE));
#endif
#ifdef CLOCK_MONOTONIC_RAW
  s7_define(sc, cur_env, s7_make_symbol(sc, "CLOCK_MONOTONIC_RAW"), s7_make_integer(sc, (s7_int)CLOCK_MONOTONIC_RAW));
#endif
#ifdef CLOCK_THREAD_CPUTIME_ID
  s7_define(sc, cur_env, s7_make_symbol(sc, "CLOCK_THREAD_CPUTIME_ID"), s7_make_integer(sc, (s7_int)CLOCK_THREAD_CPUTIME_ID));
#endif
#ifdef CLOCK_PROCESS_CPUTIME_ID
  s7_define(sc, cur_env, s7_make_symbol(sc, "CLOCK_PROCESS_CPUTIME_ID"), s7_make_integer(sc, (s7_int)CLOCK_PROCESS_CPUTIME_ID));
#endif
#ifdef CLOCK_MONOTONIC
  s7_define(sc, cur_env, s7_make_symbol(sc, "CLOCK_MONOTONIC"), s7_make_integer(sc, (s7_int)CLOCK_MONOTONIC));
#endif
#ifdef CLOCK_REALTIME
  s7_define(sc, cur_env, s7_make_symbol(sc, "CLOCK_REALTIME"), s7_make_integer(sc, (s7_int)CLOCK_REALTIME));
#endif
#ifdef CLOCKS_PER_SEC
  s7_define(sc, cur_env, s7_make_symbol(sc, "CLOCKS_PER_SEC"), s7_make_integer(sc, (s7_int)CLOCKS_PER_SEC));
#endif
#ifdef S_IFLNK
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IFLNK"), s7_make_integer(sc, (s7_int)S_IFLNK));
#endif
#ifdef FTW_NS
  s7_define(sc, cur_env, s7_make_symbol(sc, "FTW_NS"), s7_make_integer(sc, (s7_int)FTW_NS));
#endif
#ifdef FTW_DNR
  s7_define(sc, cur_env, s7_make_symbol(sc, "FTW_DNR"), s7_make_integer(sc, (s7_int)FTW_DNR));
#endif
#ifdef FTW_D
  s7_define(sc, cur_env, s7_make_symbol(sc, "FTW_D"), s7_make_integer(sc, (s7_int)FTW_D));
#endif
#ifdef FTW_F
  s7_define(sc, cur_env, s7_make_symbol(sc, "FTW_F"), s7_make_integer(sc, (s7_int)FTW_F));
#endif
#ifdef _CS_GNU_LIBPTHREAD_VERSION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_CS_GNU_LIBPTHREAD_VERSION"), s7_make_integer(sc, (s7_int)_CS_GNU_LIBPTHREAD_VERSION));
#endif
#ifdef _SC_LEVEL4_CACHE_ASSOC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL4_CACHE_ASSOC"), s7_make_integer(sc, (s7_int)_SC_LEVEL4_CACHE_ASSOC));
#endif
#ifdef _SC_LEVEL2_CACHE_ASSOC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL2_CACHE_ASSOC"), s7_make_integer(sc, (s7_int)_SC_LEVEL2_CACHE_ASSOC));
#endif
#ifdef _SC_THREAD_PRIORITY_SCHEDULING
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_PRIORITY_SCHEDULING"), s7_make_integer(sc, (s7_int)_SC_THREAD_PRIORITY_SCHEDULING));
#endif
#ifdef _CS_GNU_LIBC_VERSION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_CS_GNU_LIBC_VERSION"), s7_make_integer(sc, (s7_int)_CS_GNU_LIBC_VERSION));
#endif
#ifdef _CS_PATH
  s7_define(sc, cur_env, s7_make_symbol(sc, "_CS_PATH"), s7_make_integer(sc, (s7_int)_CS_PATH));
#endif
#ifdef _SC_THREAD_ROBUST_PRIO_PROTECT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_ROBUST_PRIO_PROTECT"), s7_make_integer(sc, (s7_int)_SC_THREAD_ROBUST_PRIO_PROTECT));
#endif
#ifdef _SC_THREAD_ROBUST_PRIO_INHERIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_ROBUST_PRIO_INHERIT"), s7_make_integer(sc, (s7_int)_SC_THREAD_ROBUST_PRIO_INHERIT));
#endif
#ifdef _SC_TRACE_USER_EVENT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TRACE_USER_EVENT_MAX"), s7_make_integer(sc, (s7_int)_SC_TRACE_USER_EVENT_MAX));
#endif
#ifdef _SC_TRACE_SYS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TRACE_SYS_MAX"), s7_make_integer(sc, (s7_int)_SC_TRACE_SYS_MAX));
#endif
#ifdef _SC_TRACE_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TRACE_NAME_MAX"), s7_make_integer(sc, (s7_int)_SC_TRACE_NAME_MAX));
#endif
#ifdef _SC_TRACE_EVENT_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TRACE_EVENT_NAME_MAX"), s7_make_integer(sc, (s7_int)_SC_TRACE_EVENT_NAME_MAX));
#endif
#ifdef _SC_SS_REPL_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SS_REPL_MAX"), s7_make_integer(sc, (s7_int)_SC_SS_REPL_MAX));
#endif
#ifdef _SC_RAW_SOCKETS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_RAW_SOCKETS"), s7_make_integer(sc, (s7_int)_SC_RAW_SOCKETS));
#endif
#ifdef _SC_IPV6
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_IPV6"), s7_make_integer(sc, (s7_int)_SC_IPV6));
#endif
#ifdef _SC_LEVEL4_CACHE_LINESIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL4_CACHE_LINESIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL4_CACHE_LINESIZE));
#endif
#ifdef _SC_LEVEL4_CACHE_SIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL4_CACHE_SIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL4_CACHE_SIZE));
#endif
#ifdef _SC_LEVEL3_CACHE_LINESIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL3_CACHE_LINESIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL3_CACHE_LINESIZE));
#endif
#ifdef _SC_LEVEL3_CACHE_ASSOC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL3_CACHE_ASSOC"), s7_make_integer(sc, (s7_int)_SC_LEVEL3_CACHE_ASSOC));
#endif
#ifdef _SC_LEVEL3_CACHE_SIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL3_CACHE_SIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL3_CACHE_SIZE));
#endif
#ifdef _SC_LEVEL2_CACHE_LINESIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL2_CACHE_LINESIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL2_CACHE_LINESIZE));
#endif
#ifdef _SC_LEVEL2_CACHE_SIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL2_CACHE_SIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL2_CACHE_SIZE));
#endif
#ifdef _SC_LEVEL1_DCACHE_LINESIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL1_DCACHE_LINESIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL1_DCACHE_LINESIZE));
#endif
#ifdef _SC_LEVEL1_DCACHE_ASSOC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL1_DCACHE_ASSOC"), s7_make_integer(sc, (s7_int)_SC_LEVEL1_DCACHE_ASSOC));
#endif
#ifdef _SC_LEVEL1_DCACHE_SIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL1_DCACHE_SIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL1_DCACHE_SIZE));
#endif
#ifdef _SC_LEVEL1_ICACHE_LINESIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL1_ICACHE_LINESIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL1_ICACHE_LINESIZE));
#endif
#ifdef _SC_LEVEL1_ICACHE_ASSOC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL1_ICACHE_ASSOC"), s7_make_integer(sc, (s7_int)_SC_LEVEL1_ICACHE_ASSOC));
#endif
#ifdef _SC_LEVEL1_ICACHE_SIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LEVEL1_ICACHE_SIZE"), s7_make_integer(sc, (s7_int)_SC_LEVEL1_ICACHE_SIZE));
#endif
#ifdef _SC_TRACE_LOG
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TRACE_LOG"), s7_make_integer(sc, (s7_int)_SC_TRACE_LOG));
#endif
#ifdef _SC_TRACE_INHERIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TRACE_INHERIT"), s7_make_integer(sc, (s7_int)_SC_TRACE_INHERIT));
#endif
#ifdef _SC_TRACE_EVENT_FILTER
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TRACE_EVENT_FILTER"), s7_make_integer(sc, (s7_int)_SC_TRACE_EVENT_FILTER));
#endif
#ifdef _SC_TRACE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TRACE"), s7_make_integer(sc, (s7_int)_SC_TRACE));
#endif
#ifdef _SC_HOST_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_HOST_NAME_MAX"), s7_make_integer(sc, (s7_int)_SC_HOST_NAME_MAX));
#endif
#ifdef _SC_2_PBS_CHECKPOINT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_PBS_CHECKPOINT"), s7_make_integer(sc, (s7_int)_SC_2_PBS_CHECKPOINT));
#endif
#ifdef _SC_STREAMS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_STREAMS"), s7_make_integer(sc, (s7_int)_SC_STREAMS));
#endif
#ifdef _SC_SYMLOOP_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SYMLOOP_MAX"), s7_make_integer(sc, (s7_int)_SC_SYMLOOP_MAX));
#endif
#ifdef _SC_2_PBS_TRACK
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_PBS_TRACK"), s7_make_integer(sc, (s7_int)_SC_2_PBS_TRACK));
#endif
#ifdef _SC_2_PBS_MESSAGE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_PBS_MESSAGE"), s7_make_integer(sc, (s7_int)_SC_2_PBS_MESSAGE));
#endif
#ifdef _SC_2_PBS_LOCATE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_PBS_LOCATE"), s7_make_integer(sc, (s7_int)_SC_2_PBS_LOCATE));
#endif
#ifdef _SC_2_PBS_ACCOUNTING
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_PBS_ACCOUNTING"), s7_make_integer(sc, (s7_int)_SC_2_PBS_ACCOUNTING));
#endif
#ifdef _SC_2_PBS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_PBS"), s7_make_integer(sc, (s7_int)_SC_2_PBS));
#endif
#ifdef _SC_USER_GROUPS_R
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_USER_GROUPS_R"), s7_make_integer(sc, (s7_int)_SC_USER_GROUPS_R));
#endif
#ifdef _SC_USER_GROUPS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_USER_GROUPS"), s7_make_integer(sc, (s7_int)_SC_USER_GROUPS));
#endif
#ifdef _SC_TYPED_MEMORY_OBJECTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TYPED_MEMORY_OBJECTS"), s7_make_integer(sc, (s7_int)_SC_TYPED_MEMORY_OBJECTS));
#endif
#ifdef _SC_TIMEOUTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TIMEOUTS"), s7_make_integer(sc, (s7_int)_SC_TIMEOUTS));
#endif
#ifdef _SC_SYSTEM_DATABASE_R
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SYSTEM_DATABASE_R"), s7_make_integer(sc, (s7_int)_SC_SYSTEM_DATABASE_R));
#endif
#ifdef _SC_SYSTEM_DATABASE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SYSTEM_DATABASE"), s7_make_integer(sc, (s7_int)_SC_SYSTEM_DATABASE));
#endif
#ifdef _SC_THREAD_SPORADIC_SERVER
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_SPORADIC_SERVER"), s7_make_integer(sc, (s7_int)_SC_THREAD_SPORADIC_SERVER));
#endif
#ifdef _SC_SPORADIC_SERVER
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SPORADIC_SERVER"), s7_make_integer(sc, (s7_int)_SC_SPORADIC_SERVER));
#endif
#ifdef _SC_SPAWN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SPAWN"), s7_make_integer(sc, (s7_int)_SC_SPAWN));
#endif
#ifdef _SC_SIGNALS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SIGNALS"), s7_make_integer(sc, (s7_int)_SC_SIGNALS));
#endif
#ifdef _SC_SHELL
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SHELL"), s7_make_integer(sc, (s7_int)_SC_SHELL));
#endif
#ifdef _SC_REGEX_VERSION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_REGEX_VERSION"), s7_make_integer(sc, (s7_int)_SC_REGEX_VERSION));
#endif
#ifdef _SC_REGEXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_REGEXP"), s7_make_integer(sc, (s7_int)_SC_REGEXP));
#endif
#ifdef _SC_SPIN_LOCKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SPIN_LOCKS"), s7_make_integer(sc, (s7_int)_SC_SPIN_LOCKS));
#endif
#ifdef _SC_READER_WRITER_LOCKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_READER_WRITER_LOCKS"), s7_make_integer(sc, (s7_int)_SC_READER_WRITER_LOCKS));
#endif
#ifdef _SC_NETWORKING
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NETWORKING"), s7_make_integer(sc, (s7_int)_SC_NETWORKING));
#endif
#ifdef _SC_SINGLE_PROCESS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SINGLE_PROCESS"), s7_make_integer(sc, (s7_int)_SC_SINGLE_PROCESS));
#endif
#ifdef _SC_MULTI_PROCESS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MULTI_PROCESS"), s7_make_integer(sc, (s7_int)_SC_MULTI_PROCESS));
#endif
#ifdef _SC_MONOTONIC_CLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MONOTONIC_CLOCK"), s7_make_integer(sc, (s7_int)_SC_MONOTONIC_CLOCK));
#endif
#ifdef _SC_FILE_SYSTEM
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_FILE_SYSTEM"), s7_make_integer(sc, (s7_int)_SC_FILE_SYSTEM));
#endif
#ifdef _SC_FILE_LOCKING
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_FILE_LOCKING"), s7_make_integer(sc, (s7_int)_SC_FILE_LOCKING));
#endif
#ifdef _SC_FILE_ATTRIBUTES
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_FILE_ATTRIBUTES"), s7_make_integer(sc, (s7_int)_SC_FILE_ATTRIBUTES));
#endif
#ifdef _SC_PIPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PIPE"), s7_make_integer(sc, (s7_int)_SC_PIPE));
#endif
#ifdef _SC_FIFO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_FIFO"), s7_make_integer(sc, (s7_int)_SC_FIFO));
#endif
#ifdef _SC_FD_MGMT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_FD_MGMT"), s7_make_integer(sc, (s7_int)_SC_FD_MGMT));
#endif
#ifdef _SC_DEVICE_SPECIFIC_R
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_DEVICE_SPECIFIC_R"), s7_make_integer(sc, (s7_int)_SC_DEVICE_SPECIFIC_R));
#endif
#ifdef _SC_DEVICE_SPECIFIC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_DEVICE_SPECIFIC"), s7_make_integer(sc, (s7_int)_SC_DEVICE_SPECIFIC));
#endif
#ifdef _SC_DEVICE_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_DEVICE_IO"), s7_make_integer(sc, (s7_int)_SC_DEVICE_IO));
#endif
#ifdef _SC_THREAD_CPUTIME
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_CPUTIME"), s7_make_integer(sc, (s7_int)_SC_THREAD_CPUTIME));
#endif
#ifdef _SC_CPUTIME
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_CPUTIME"), s7_make_integer(sc, (s7_int)_SC_CPUTIME));
#endif
#ifdef _SC_CLOCK_SELECTION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_CLOCK_SELECTION"), s7_make_integer(sc, (s7_int)_SC_CLOCK_SELECTION));
#endif
#ifdef _SC_C_LANG_SUPPORT_R
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_C_LANG_SUPPORT_R"), s7_make_integer(sc, (s7_int)_SC_C_LANG_SUPPORT_R));
#endif
#ifdef _SC_C_LANG_SUPPORT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_C_LANG_SUPPORT"), s7_make_integer(sc, (s7_int)_SC_C_LANG_SUPPORT));
#endif
#ifdef _SC_BASE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_BASE"), s7_make_integer(sc, (s7_int)_SC_BASE));
#endif
#ifdef _SC_BARRIERS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_BARRIERS"), s7_make_integer(sc, (s7_int)_SC_BARRIERS));
#endif
#ifdef _SC_ADVISORY_INFO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_ADVISORY_INFO"), s7_make_integer(sc, (s7_int)_SC_ADVISORY_INFO));
#endif
#ifdef _SC_NL_TEXTMAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NL_TEXTMAX"), s7_make_integer(sc, (s7_int)_SC_NL_TEXTMAX));
#endif
#ifdef _SC_NL_SETMAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NL_SETMAX"), s7_make_integer(sc, (s7_int)_SC_NL_SETMAX));
#endif
#ifdef _SC_NL_NMAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NL_NMAX"), s7_make_integer(sc, (s7_int)_SC_NL_NMAX));
#endif
#ifdef _SC_NL_MSGMAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NL_MSGMAX"), s7_make_integer(sc, (s7_int)_SC_NL_MSGMAX));
#endif
#ifdef _SC_NL_LANGMAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NL_LANGMAX"), s7_make_integer(sc, (s7_int)_SC_NL_LANGMAX));
#endif
#ifdef _SC_NL_ARGMAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NL_ARGMAX"), s7_make_integer(sc, (s7_int)_SC_NL_ARGMAX));
#endif
#ifdef _SC_USHRT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_USHRT_MAX"), s7_make_integer(sc, (s7_int)_SC_USHRT_MAX));
#endif
#ifdef _SC_ULONG_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_ULONG_MAX"), s7_make_integer(sc, (s7_int)_SC_ULONG_MAX));
#endif
#ifdef _SC_UINT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_UINT_MAX"), s7_make_integer(sc, (s7_int)_SC_UINT_MAX));
#endif
#ifdef _SC_UCHAR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_UCHAR_MAX"), s7_make_integer(sc, (s7_int)_SC_UCHAR_MAX));
#endif
#ifdef _SC_SHRT_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SHRT_MIN"), s7_make_integer(sc, (s7_int)_SC_SHRT_MIN));
#endif
#ifdef _SC_SHRT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SHRT_MAX"), s7_make_integer(sc, (s7_int)_SC_SHRT_MAX));
#endif
#ifdef _SC_SCHAR_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SCHAR_MIN"), s7_make_integer(sc, (s7_int)_SC_SCHAR_MIN));
#endif
#ifdef _SC_SCHAR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SCHAR_MAX"), s7_make_integer(sc, (s7_int)_SC_SCHAR_MAX));
#endif
#ifdef _SC_SSIZE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SSIZE_MAX"), s7_make_integer(sc, (s7_int)_SC_SSIZE_MAX));
#endif
#ifdef _SC_NZERO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NZERO"), s7_make_integer(sc, (s7_int)_SC_NZERO));
#endif
#ifdef _SC_MB_LEN_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MB_LEN_MAX"), s7_make_integer(sc, (s7_int)_SC_MB_LEN_MAX));
#endif
#ifdef _SC_WORD_BIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_WORD_BIT"), s7_make_integer(sc, (s7_int)_SC_WORD_BIT));
#endif
#ifdef _SC_LONG_BIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LONG_BIT"), s7_make_integer(sc, (s7_int)_SC_LONG_BIT));
#endif
#ifdef _SC_INT_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_INT_MIN"), s7_make_integer(sc, (s7_int)_SC_INT_MIN));
#endif
#ifdef _SC_INT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_INT_MAX"), s7_make_integer(sc, (s7_int)_SC_INT_MAX));
#endif
#ifdef _SC_CHAR_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_CHAR_MIN"), s7_make_integer(sc, (s7_int)_SC_CHAR_MIN));
#endif
#ifdef _SC_CHAR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_CHAR_MAX"), s7_make_integer(sc, (s7_int)_SC_CHAR_MAX));
#endif
#ifdef _SC_CHAR_BIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_CHAR_BIT"), s7_make_integer(sc, (s7_int)_SC_CHAR_BIT));
#endif
#ifdef _SC_2_UPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_UPE"), s7_make_integer(sc, (s7_int)_SC_2_UPE));
#endif
#ifdef _SC_2_C_VERSION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_C_VERSION"), s7_make_integer(sc, (s7_int)_SC_2_C_VERSION));
#endif
#ifdef _SC_2_CHAR_TERM
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_CHAR_TERM"), s7_make_integer(sc, (s7_int)_SC_2_CHAR_TERM));
#endif
#ifdef _SC_PASS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PASS_MAX"), s7_make_integer(sc, (s7_int)_SC_PASS_MAX));
#endif
#ifdef _SC_ATEXIT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_ATEXIT_MAX"), s7_make_integer(sc, (s7_int)_SC_ATEXIT_MAX));
#endif
#ifdef _SC_AVPHYS_PAGES
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_AVPHYS_PAGES"), s7_make_integer(sc, (s7_int)_SC_AVPHYS_PAGES));
#endif
#ifdef _SC_PHYS_PAGES
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PHYS_PAGES"), s7_make_integer(sc, (s7_int)_SC_PHYS_PAGES));
#endif
#ifdef _SC_NPROCESSORS_ONLN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NPROCESSORS_ONLN"), s7_make_integer(sc, (s7_int)_SC_NPROCESSORS_ONLN));
#endif
#ifdef _SC_NPROCESSORS_CONF
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NPROCESSORS_CONF"), s7_make_integer(sc, (s7_int)_SC_NPROCESSORS_CONF));
#endif
#ifdef _SC_THREAD_PROCESS_SHARED
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_PROCESS_SHARED"), s7_make_integer(sc, (s7_int)_SC_THREAD_PROCESS_SHARED));
#endif
#ifdef _SC_THREAD_PRIO_PROTECT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_PRIO_PROTECT"), s7_make_integer(sc, (s7_int)_SC_THREAD_PRIO_PROTECT));
#endif
#ifdef _SC_THREAD_PRIO_INHERIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_PRIO_INHERIT"), s7_make_integer(sc, (s7_int)_SC_THREAD_PRIO_INHERIT));
#endif
#ifdef _SC_THREAD_ATTR_STACKSIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_ATTR_STACKSIZE"), s7_make_integer(sc, (s7_int)_SC_THREAD_ATTR_STACKSIZE));
#endif
#ifdef _SC_THREAD_ATTR_STACKADDR
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_ATTR_STACKADDR"), s7_make_integer(sc, (s7_int)_SC_THREAD_ATTR_STACKADDR));
#endif
#ifdef _SC_THREAD_THREADS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_THREADS_MAX"), s7_make_integer(sc, (s7_int)_SC_THREAD_THREADS_MAX));
#endif
#ifdef _SC_THREAD_STACK_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_STACK_MIN"), s7_make_integer(sc, (s7_int)_SC_THREAD_STACK_MIN));
#endif
#ifdef _SC_THREAD_KEYS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_KEYS_MAX"), s7_make_integer(sc, (s7_int)_SC_THREAD_KEYS_MAX));
#endif
#ifdef _SC_THREAD_DESTRUCTOR_ITERATIONS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_DESTRUCTOR_ITERATIONS"), s7_make_integer(sc, (s7_int)_SC_THREAD_DESTRUCTOR_ITERATIONS));
#endif
#ifdef _SC_TTY_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TTY_NAME_MAX"), s7_make_integer(sc, (s7_int)_SC_TTY_NAME_MAX));
#endif
#ifdef _SC_LOGIN_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LOGIN_NAME_MAX"), s7_make_integer(sc, (s7_int)_SC_LOGIN_NAME_MAX));
#endif
#ifdef _SC_GETPW_R_SIZE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_GETPW_R_SIZE_MAX"), s7_make_integer(sc, (s7_int)_SC_GETPW_R_SIZE_MAX));
#endif
#ifdef _SC_GETGR_R_SIZE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_GETGR_R_SIZE_MAX"), s7_make_integer(sc, (s7_int)_SC_GETGR_R_SIZE_MAX));
#endif
#ifdef _SC_THREAD_SAFE_FUNCTIONS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREAD_SAFE_FUNCTIONS"), s7_make_integer(sc, (s7_int)_SC_THREAD_SAFE_FUNCTIONS));
#endif
#ifdef _SC_THREADS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_THREADS"), s7_make_integer(sc, (s7_int)_SC_THREADS));
#endif
#ifdef _SC_T_IOV_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_T_IOV_MAX"), s7_make_integer(sc, (s7_int)_SC_T_IOV_MAX));
#endif
#ifdef _SC_PII_OSI_M
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII_OSI_M"), s7_make_integer(sc, (s7_int)_SC_PII_OSI_M));
#endif
#ifdef _SC_PII_OSI_CLTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII_OSI_CLTS"), s7_make_integer(sc, (s7_int)_SC_PII_OSI_CLTS));
#endif
#ifdef _SC_PII_OSI_COTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII_OSI_COTS"), s7_make_integer(sc, (s7_int)_SC_PII_OSI_COTS));
#endif
#ifdef _SC_PII_INTERNET_DGRAM
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII_INTERNET_DGRAM"), s7_make_integer(sc, (s7_int)_SC_PII_INTERNET_DGRAM));
#endif
#ifdef _SC_PII_INTERNET_STREAM
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII_INTERNET_STREAM"), s7_make_integer(sc, (s7_int)_SC_PII_INTERNET_STREAM));
#endif
#ifdef _SC_IOV_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_IOV_MAX"), s7_make_integer(sc, (s7_int)_SC_IOV_MAX));
#endif
#ifdef _SC_UIO_MAXIOV
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_UIO_MAXIOV"), s7_make_integer(sc, (s7_int)_SC_UIO_MAXIOV));
#endif
#ifdef _SC_SELECT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SELECT"), s7_make_integer(sc, (s7_int)_SC_SELECT));
#endif
#ifdef _SC_POLL
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_POLL"), s7_make_integer(sc, (s7_int)_SC_POLL));
#endif
#ifdef _SC_PII_OSI
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII_OSI"), s7_make_integer(sc, (s7_int)_SC_PII_OSI));
#endif
#ifdef _SC_PII_INTERNET
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII_INTERNET"), s7_make_integer(sc, (s7_int)_SC_PII_INTERNET));
#endif
#ifdef _SC_PII_SOCKET
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII_SOCKET"), s7_make_integer(sc, (s7_int)_SC_PII_SOCKET));
#endif
#ifdef _SC_PII_XTI
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII_XTI"), s7_make_integer(sc, (s7_int)_SC_PII_XTI));
#endif
#ifdef _SC_PII
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PII"), s7_make_integer(sc, (s7_int)_SC_PII));
#endif
#ifdef _SC_2_LOCALEDEF
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_LOCALEDEF"), s7_make_integer(sc, (s7_int)_SC_2_LOCALEDEF));
#endif
#ifdef _SC_2_SW_DEV
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_SW_DEV"), s7_make_integer(sc, (s7_int)_SC_2_SW_DEV));
#endif
#ifdef _SC_2_FORT_RUN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_FORT_RUN"), s7_make_integer(sc, (s7_int)_SC_2_FORT_RUN));
#endif
#ifdef _SC_2_FORT_DEV
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_FORT_DEV"), s7_make_integer(sc, (s7_int)_SC_2_FORT_DEV));
#endif
#ifdef _SC_2_C_DEV
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_C_DEV"), s7_make_integer(sc, (s7_int)_SC_2_C_DEV));
#endif
#ifdef _SC_2_C_BIND
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_C_BIND"), s7_make_integer(sc, (s7_int)_SC_2_C_BIND));
#endif
#ifdef _SC_2_VERSION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_2_VERSION"), s7_make_integer(sc, (s7_int)_SC_2_VERSION));
#endif
#ifdef _SC_CHARCLASS_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_CHARCLASS_NAME_MAX"), s7_make_integer(sc, (s7_int)_SC_CHARCLASS_NAME_MAX));
#endif
#ifdef _SC_RE_DUP_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_RE_DUP_MAX"), s7_make_integer(sc, (s7_int)_SC_RE_DUP_MAX));
#endif
#ifdef _SC_LINE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_LINE_MAX"), s7_make_integer(sc, (s7_int)_SC_LINE_MAX));
#endif
#ifdef _SC_EXPR_NEST_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_EXPR_NEST_MAX"), s7_make_integer(sc, (s7_int)_SC_EXPR_NEST_MAX));
#endif
#ifdef _SC_EQUIV_CLASS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_EQUIV_CLASS_MAX"), s7_make_integer(sc, (s7_int)_SC_EQUIV_CLASS_MAX));
#endif
#ifdef _SC_COLL_WEIGHTS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_COLL_WEIGHTS_MAX"), s7_make_integer(sc, (s7_int)_SC_COLL_WEIGHTS_MAX));
#endif
#ifdef _SC_BC_STRING_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_BC_STRING_MAX"), s7_make_integer(sc, (s7_int)_SC_BC_STRING_MAX));
#endif
#ifdef _SC_BC_SCALE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_BC_SCALE_MAX"), s7_make_integer(sc, (s7_int)_SC_BC_SCALE_MAX));
#endif
#ifdef _SC_BC_DIM_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_BC_DIM_MAX"), s7_make_integer(sc, (s7_int)_SC_BC_DIM_MAX));
#endif
#ifdef _SC_BC_BASE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_BC_BASE_MAX"), s7_make_integer(sc, (s7_int)_SC_BC_BASE_MAX));
#endif
#ifdef _SC_TIMER_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TIMER_MAX"), s7_make_integer(sc, (s7_int)_SC_TIMER_MAX));
#endif
#ifdef _SC_SIGQUEUE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SIGQUEUE_MAX"), s7_make_integer(sc, (s7_int)_SC_SIGQUEUE_MAX));
#endif
#ifdef _SC_SEM_VALUE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SEM_VALUE_MAX"), s7_make_integer(sc, (s7_int)_SC_SEM_VALUE_MAX));
#endif
#ifdef _SC_SEM_NSEMS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SEM_NSEMS_MAX"), s7_make_integer(sc, (s7_int)_SC_SEM_NSEMS_MAX));
#endif
#ifdef _SC_RTSIG_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_RTSIG_MAX"), s7_make_integer(sc, (s7_int)_SC_RTSIG_MAX));
#endif
#ifdef _SC_PAGE_SIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PAGE_SIZE"), s7_make_integer(sc, (s7_int)_SC_PAGE_SIZE));
#endif
#ifdef _SC_PAGESIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PAGESIZE"), s7_make_integer(sc, (s7_int)_SC_PAGESIZE));
#endif
#ifdef _SC_VERSION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_VERSION"), s7_make_integer(sc, (s7_int)_SC_VERSION));
#endif
#ifdef _SC_MQ_PRIO_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MQ_PRIO_MAX"), s7_make_integer(sc, (s7_int)_SC_MQ_PRIO_MAX));
#endif
#ifdef _SC_MQ_OPEN_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MQ_OPEN_MAX"), s7_make_integer(sc, (s7_int)_SC_MQ_OPEN_MAX));
#endif
#ifdef _SC_DELAYTIMER_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_DELAYTIMER_MAX"), s7_make_integer(sc, (s7_int)_SC_DELAYTIMER_MAX));
#endif
#ifdef _SC_AIO_PRIO_DELTA_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_AIO_PRIO_DELTA_MAX"), s7_make_integer(sc, (s7_int)_SC_AIO_PRIO_DELTA_MAX));
#endif
#ifdef _SC_AIO_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_AIO_MAX"), s7_make_integer(sc, (s7_int)_SC_AIO_MAX));
#endif
#ifdef _SC_AIO_LISTIO_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_AIO_LISTIO_MAX"), s7_make_integer(sc, (s7_int)_SC_AIO_LISTIO_MAX));
#endif
#ifdef _SC_SHARED_MEMORY_OBJECTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SHARED_MEMORY_OBJECTS"), s7_make_integer(sc, (s7_int)_SC_SHARED_MEMORY_OBJECTS));
#endif
#ifdef _SC_SEMAPHORES
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SEMAPHORES"), s7_make_integer(sc, (s7_int)_SC_SEMAPHORES));
#endif
#ifdef _SC_MESSAGE_PASSING
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MESSAGE_PASSING"), s7_make_integer(sc, (s7_int)_SC_MESSAGE_PASSING));
#endif
#ifdef _SC_MEMORY_PROTECTION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MEMORY_PROTECTION"), s7_make_integer(sc, (s7_int)_SC_MEMORY_PROTECTION));
#endif
#ifdef _SC_MEMLOCK_RANGE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MEMLOCK_RANGE"), s7_make_integer(sc, (s7_int)_SC_MEMLOCK_RANGE));
#endif
#ifdef _SC_MEMLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MEMLOCK"), s7_make_integer(sc, (s7_int)_SC_MEMLOCK));
#endif
#ifdef _SC_MAPPED_FILES
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_MAPPED_FILES"), s7_make_integer(sc, (s7_int)_SC_MAPPED_FILES));
#endif
#ifdef _SC_FSYNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_FSYNC"), s7_make_integer(sc, (s7_int)_SC_FSYNC));
#endif
#ifdef _SC_SYNCHRONIZED_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SYNCHRONIZED_IO"), s7_make_integer(sc, (s7_int)_SC_SYNCHRONIZED_IO));
#endif
#ifdef _SC_PRIORITIZED_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PRIORITIZED_IO"), s7_make_integer(sc, (s7_int)_SC_PRIORITIZED_IO));
#endif
#ifdef _SC_ASYNCHRONOUS_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_ASYNCHRONOUS_IO"), s7_make_integer(sc, (s7_int)_SC_ASYNCHRONOUS_IO));
#endif
#ifdef _SC_TIMERS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TIMERS"), s7_make_integer(sc, (s7_int)_SC_TIMERS));
#endif
#ifdef _SC_PRIORITY_SCHEDULING
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_PRIORITY_SCHEDULING"), s7_make_integer(sc, (s7_int)_SC_PRIORITY_SCHEDULING));
#endif
#ifdef _SC_REALTIME_SIGNALS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_REALTIME_SIGNALS"), s7_make_integer(sc, (s7_int)_SC_REALTIME_SIGNALS));
#endif
#ifdef _SC_SAVED_IDS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_SAVED_IDS"), s7_make_integer(sc, (s7_int)_SC_SAVED_IDS));
#endif
#ifdef _SC_JOB_CONTROL
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_JOB_CONTROL"), s7_make_integer(sc, (s7_int)_SC_JOB_CONTROL));
#endif
#ifdef _SC_TZNAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_TZNAME_MAX"), s7_make_integer(sc, (s7_int)_SC_TZNAME_MAX));
#endif
#ifdef _SC_STREAM_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_STREAM_MAX"), s7_make_integer(sc, (s7_int)_SC_STREAM_MAX));
#endif
#ifdef _SC_OPEN_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_OPEN_MAX"), s7_make_integer(sc, (s7_int)_SC_OPEN_MAX));
#endif
#ifdef _SC_NGROUPS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_NGROUPS_MAX"), s7_make_integer(sc, (s7_int)_SC_NGROUPS_MAX));
#endif
#ifdef _SC_CLK_TCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_CLK_TCK"), s7_make_integer(sc, (s7_int)_SC_CLK_TCK));
#endif
#ifdef _SC_CHILD_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_CHILD_MAX"), s7_make_integer(sc, (s7_int)_SC_CHILD_MAX));
#endif
#ifdef _SC_ARG_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_SC_ARG_MAX"), s7_make_integer(sc, (s7_int)_SC_ARG_MAX));
#endif
#ifdef _PC_2_SYMLINKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_2_SYMLINKS"), s7_make_integer(sc, (s7_int)_PC_2_SYMLINKS));
#endif
#ifdef _PC_SYMLINK_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_SYMLINK_MAX"), s7_make_integer(sc, (s7_int)_PC_SYMLINK_MAX));
#endif
#ifdef _PC_ALLOC_SIZE_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_ALLOC_SIZE_MIN"), s7_make_integer(sc, (s7_int)_PC_ALLOC_SIZE_MIN));
#endif
#ifdef _PC_REC_XFER_ALIGN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_REC_XFER_ALIGN"), s7_make_integer(sc, (s7_int)_PC_REC_XFER_ALIGN));
#endif
#ifdef _PC_REC_MIN_XFER_SIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_REC_MIN_XFER_SIZE"), s7_make_integer(sc, (s7_int)_PC_REC_MIN_XFER_SIZE));
#endif
#ifdef _PC_REC_MAX_XFER_SIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_REC_MAX_XFER_SIZE"), s7_make_integer(sc, (s7_int)_PC_REC_MAX_XFER_SIZE));
#endif
#ifdef _PC_REC_INCR_XFER_SIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_REC_INCR_XFER_SIZE"), s7_make_integer(sc, (s7_int)_PC_REC_INCR_XFER_SIZE));
#endif
#ifdef _PC_FILESIZEBITS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_FILESIZEBITS"), s7_make_integer(sc, (s7_int)_PC_FILESIZEBITS));
#endif
#ifdef _PC_SOCK_MAXBUF
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_SOCK_MAXBUF"), s7_make_integer(sc, (s7_int)_PC_SOCK_MAXBUF));
#endif
#ifdef _PC_PRIO_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_PRIO_IO"), s7_make_integer(sc, (s7_int)_PC_PRIO_IO));
#endif
#ifdef _PC_ASYNC_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_ASYNC_IO"), s7_make_integer(sc, (s7_int)_PC_ASYNC_IO));
#endif
#ifdef _PC_SYNC_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_SYNC_IO"), s7_make_integer(sc, (s7_int)_PC_SYNC_IO));
#endif
#ifdef _PC_VDISABLE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_VDISABLE"), s7_make_integer(sc, (s7_int)_PC_VDISABLE));
#endif
#ifdef _PC_NO_TRUNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_NO_TRUNC"), s7_make_integer(sc, (s7_int)_PC_NO_TRUNC));
#endif
#ifdef _PC_CHOWN_RESTRICTED
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_CHOWN_RESTRICTED"), s7_make_integer(sc, (s7_int)_PC_CHOWN_RESTRICTED));
#endif
#ifdef _PC_PIPE_BUF
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_PIPE_BUF"), s7_make_integer(sc, (s7_int)_PC_PIPE_BUF));
#endif
#ifdef _PC_PATH_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_PATH_MAX"), s7_make_integer(sc, (s7_int)_PC_PATH_MAX));
#endif
#ifdef _PC_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_NAME_MAX"), s7_make_integer(sc, (s7_int)_PC_NAME_MAX));
#endif
#ifdef _PC_MAX_INPUT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_MAX_INPUT"), s7_make_integer(sc, (s7_int)_PC_MAX_INPUT));
#endif
#ifdef _PC_MAX_CANON
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_MAX_CANON"), s7_make_integer(sc, (s7_int)_PC_MAX_CANON));
#endif
#ifdef _PC_LINK_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_PC_LINK_MAX"), s7_make_integer(sc, (s7_int)_PC_LINK_MAX));
#endif
#ifdef STDERR_FILENO
  s7_define(sc, cur_env, s7_make_symbol(sc, "STDERR_FILENO"), s7_make_integer(sc, (s7_int)STDERR_FILENO));
#endif
#ifdef STDOUT_FILENO
  s7_define(sc, cur_env, s7_make_symbol(sc, "STDOUT_FILENO"), s7_make_integer(sc, (s7_int)STDOUT_FILENO));
#endif
#ifdef STDIN_FILENO
  s7_define(sc, cur_env, s7_make_symbol(sc, "STDIN_FILENO"), s7_make_integer(sc, (s7_int)STDIN_FILENO));
#endif
#ifdef _POSIX_TYPED_MEMORY_OBJECTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TYPED_MEMORY_OBJECTS"), s7_make_integer(sc, (s7_int)_POSIX_TYPED_MEMORY_OBJECTS));
#endif
#ifdef _POSIX_TRACE_LOG
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TRACE_LOG"), s7_make_integer(sc, (s7_int)_POSIX_TRACE_LOG));
#endif
#ifdef _POSIX_TRACE_INHERIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TRACE_INHERIT"), s7_make_integer(sc, (s7_int)_POSIX_TRACE_INHERIT));
#endif
#ifdef _POSIX_TRACE_EVENT_FILTER
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TRACE_EVENT_FILTER"), s7_make_integer(sc, (s7_int)_POSIX_TRACE_EVENT_FILTER));
#endif
#ifdef _POSIX_TRACE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TRACE"), s7_make_integer(sc, (s7_int)_POSIX_TRACE));
#endif
#ifdef _POSIX_THREAD_SPORADIC_SERVER
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREAD_SPORADIC_SERVER"), s7_make_integer(sc, (s7_int)_POSIX_THREAD_SPORADIC_SERVER));
#endif
#ifdef _POSIX_SPORADIC_SERVER
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SPORADIC_SERVER"), s7_make_integer(sc, (s7_int)_POSIX_SPORADIC_SERVER));
#endif
#ifdef _POSIX2_CHAR_TERM
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_CHAR_TERM"), s7_make_integer(sc, (s7_int)_POSIX2_CHAR_TERM));
#endif
#ifdef _POSIX_RAW_SOCKETS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_RAW_SOCKETS"), s7_make_integer(sc, (s7_int)_POSIX_RAW_SOCKETS));
#endif
#ifdef _POSIX_IPV6
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_IPV6"), s7_make_integer(sc, (s7_int)_POSIX_IPV6));
#endif
#ifdef _POSIX_ADVISORY_INFO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_ADVISORY_INFO"), s7_make_integer(sc, (s7_int)_POSIX_ADVISORY_INFO));
#endif
#ifdef _POSIX_CLOCK_SELECTION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_CLOCK_SELECTION"), s7_make_integer(sc, (s7_int)_POSIX_CLOCK_SELECTION));
#endif
#ifdef _POSIX_MONOTONIC_CLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MONOTONIC_CLOCK"), s7_make_integer(sc, (s7_int)_POSIX_MONOTONIC_CLOCK));
#endif
#ifdef _POSIX_THREAD_PROCESS_SHARED
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREAD_PROCESS_SHARED"), s7_make_integer(sc, (s7_int)_POSIX_THREAD_PROCESS_SHARED));
#endif
#ifdef _POSIX_MESSAGE_PASSING
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MESSAGE_PASSING"), s7_make_integer(sc, (s7_int)_POSIX_MESSAGE_PASSING));
#endif
#ifdef _POSIX_BARRIERS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_BARRIERS"), s7_make_integer(sc, (s7_int)_POSIX_BARRIERS));
#endif
#ifdef _POSIX_TIMERS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TIMERS"), s7_make_integer(sc, (s7_int)_POSIX_TIMERS));
#endif
#ifdef _POSIX_SPAWN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SPAWN"), s7_make_integer(sc, (s7_int)_POSIX_SPAWN));
#endif
#ifdef _POSIX_SPIN_LOCKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SPIN_LOCKS"), s7_make_integer(sc, (s7_int)_POSIX_SPIN_LOCKS));
#endif
#ifdef _POSIX_TIMEOUTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TIMEOUTS"), s7_make_integer(sc, (s7_int)_POSIX_TIMEOUTS));
#endif
#ifdef _POSIX_SHELL
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SHELL"), s7_make_integer(sc, (s7_int)_POSIX_SHELL));
#endif
#ifdef _POSIX_READER_WRITER_LOCKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_READER_WRITER_LOCKS"), s7_make_integer(sc, (s7_int)_POSIX_READER_WRITER_LOCKS));
#endif
#ifdef _POSIX_REGEXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_REGEXP"), s7_make_integer(sc, (s7_int)_POSIX_REGEXP));
#endif
#ifdef _POSIX_THREAD_CPUTIME
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREAD_CPUTIME"), s7_make_integer(sc, (s7_int)_POSIX_THREAD_CPUTIME));
#endif
#ifdef _POSIX_CPUTIME
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_CPUTIME"), s7_make_integer(sc, (s7_int)_POSIX_CPUTIME));
#endif
#ifdef _POSIX_SHARED_MEMORY_OBJECTS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SHARED_MEMORY_OBJECTS"), s7_make_integer(sc, (s7_int)_POSIX_SHARED_MEMORY_OBJECTS));
#endif
#ifdef _POSIX_PRIORITIZED_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_PRIORITIZED_IO"), s7_make_integer(sc, (s7_int)_POSIX_PRIORITIZED_IO));
#endif
#ifdef _POSIX_ASYNC_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_ASYNC_IO"), s7_make_integer(sc, (s7_int)_POSIX_ASYNC_IO));
#endif
#ifdef _POSIX_ASYNCHRONOUS_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_ASYNCHRONOUS_IO"), s7_make_integer(sc, (s7_int)_POSIX_ASYNCHRONOUS_IO));
#endif
#ifdef _POSIX_REALTIME_SIGNALS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_REALTIME_SIGNALS"), s7_make_integer(sc, (s7_int)_POSIX_REALTIME_SIGNALS));
#endif
#ifdef _POSIX_SEMAPHORES
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SEMAPHORES"), s7_make_integer(sc, (s7_int)_POSIX_SEMAPHORES));
#endif
#ifdef _POSIX_THREAD_PRIO_PROTECT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREAD_PRIO_PROTECT"), s7_make_integer(sc, (s7_int)_POSIX_THREAD_PRIO_PROTECT));
#endif
#ifdef _POSIX_THREAD_PRIO_INHERIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREAD_PRIO_INHERIT"), s7_make_integer(sc, (s7_int)_POSIX_THREAD_PRIO_INHERIT));
#endif
#ifdef _POSIX_THREAD_ATTR_STACKADDR
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREAD_ATTR_STACKADDR"), s7_make_integer(sc, (s7_int)_POSIX_THREAD_ATTR_STACKADDR));
#endif
#ifdef _POSIX_THREAD_ATTR_STACKSIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREAD_ATTR_STACKSIZE"), s7_make_integer(sc, (s7_int)_POSIX_THREAD_ATTR_STACKSIZE));
#endif
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREAD_PRIORITY_SCHEDULING"), s7_make_integer(sc, (s7_int)_POSIX_THREAD_PRIORITY_SCHEDULING));
#endif
#ifdef _POSIX_THREAD_SAFE_FUNCTIONS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREAD_SAFE_FUNCTIONS"), s7_make_integer(sc, (s7_int)_POSIX_THREAD_SAFE_FUNCTIONS));
#endif
#ifdef _POSIX_REENTRANT_FUNCTIONS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_REENTRANT_FUNCTIONS"), s7_make_integer(sc, (s7_int)_POSIX_REENTRANT_FUNCTIONS));
#endif
#ifdef _POSIX_THREADS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_THREADS"), s7_make_integer(sc, (s7_int)_POSIX_THREADS));
#endif
#ifdef _POSIX_NO_TRUNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_NO_TRUNC"), s7_make_integer(sc, (s7_int)_POSIX_NO_TRUNC));
#endif
#ifdef _POSIX_VDISABLE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_VDISABLE"), s7_make_integer(sc, (s7_int)_POSIX_VDISABLE));
#endif
#ifdef _POSIX_CHOWN_RESTRICTED
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_CHOWN_RESTRICTED"), s7_make_integer(sc, (s7_int)_POSIX_CHOWN_RESTRICTED));
#endif
#ifdef _POSIX_MEMORY_PROTECTION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MEMORY_PROTECTION"), s7_make_integer(sc, (s7_int)_POSIX_MEMORY_PROTECTION));
#endif
#ifdef _POSIX_MEMLOCK_RANGE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MEMLOCK_RANGE"), s7_make_integer(sc, (s7_int)_POSIX_MEMLOCK_RANGE));
#endif
#ifdef _POSIX_MEMLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MEMLOCK"), s7_make_integer(sc, (s7_int)_POSIX_MEMLOCK));
#endif
#ifdef _POSIX_MAPPED_FILES
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MAPPED_FILES"), s7_make_integer(sc, (s7_int)_POSIX_MAPPED_FILES));
#endif
#ifdef _POSIX_FSYNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_FSYNC"), s7_make_integer(sc, (s7_int)_POSIX_FSYNC));
#endif
#ifdef _POSIX_SYNCHRONIZED_IO
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SYNCHRONIZED_IO"), s7_make_integer(sc, (s7_int)_POSIX_SYNCHRONIZED_IO));
#endif
#ifdef _POSIX_PRIORITY_SCHEDULING
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_PRIORITY_SCHEDULING"), s7_make_integer(sc, (s7_int)_POSIX_PRIORITY_SCHEDULING));
#endif
#ifdef _POSIX_SAVED_IDS
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SAVED_IDS"), s7_make_integer(sc, (s7_int)_POSIX_SAVED_IDS));
#endif
#ifdef _POSIX_JOB_CONTROL
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_JOB_CONTROL"), s7_make_integer(sc, (s7_int)_POSIX_JOB_CONTROL));
#endif
#ifdef _POSIX2_VERSION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_VERSION"), s7_make_integer(sc, (s7_int)_POSIX2_VERSION));
#endif
#ifdef _POSIX_VERSION
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_VERSION"), s7_make_integer(sc, (s7_int)_POSIX_VERSION));
#endif
#ifdef LC_IDENTIFICATION
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_IDENTIFICATION"), s7_make_integer(sc, (s7_int)LC_IDENTIFICATION));
#endif
#ifdef LC_MEASUREMENT
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_MEASUREMENT"), s7_make_integer(sc, (s7_int)LC_MEASUREMENT));
#endif
#ifdef LC_TELEPHONE
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_TELEPHONE"), s7_make_integer(sc, (s7_int)LC_TELEPHONE));
#endif
#ifdef LC_ADDRESS
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_ADDRESS"), s7_make_integer(sc, (s7_int)LC_ADDRESS));
#endif
#ifdef LC_NAME
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_NAME"), s7_make_integer(sc, (s7_int)LC_NAME));
#endif
#ifdef LC_PAPER
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_PAPER"), s7_make_integer(sc, (s7_int)LC_PAPER));
#endif
#ifdef LC_ALL
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_ALL"), s7_make_integer(sc, (s7_int)LC_ALL));
#endif
#ifdef LC_MESSAGES
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_MESSAGES"), s7_make_integer(sc, (s7_int)LC_MESSAGES));
#endif
#ifdef LC_MONETARY
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_MONETARY"), s7_make_integer(sc, (s7_int)LC_MONETARY));
#endif
#ifdef LC_COLLATE
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_COLLATE"), s7_make_integer(sc, (s7_int)LC_COLLATE));
#endif
#ifdef LC_TIME
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_TIME"), s7_make_integer(sc, (s7_int)LC_TIME));
#endif
#ifdef LC_NUMERIC
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_NUMERIC"), s7_make_integer(sc, (s7_int)LC_NUMERIC));
#endif
#ifdef LC_CTYPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "LC_CTYPE"), s7_make_integer(sc, (s7_int)LC_CTYPE));
#endif
#ifdef ERANGE
  s7_define(sc, cur_env, s7_make_symbol(sc, "ERANGE"), s7_make_integer(sc, (s7_int)ERANGE));
#endif
#ifdef EDOM
  s7_define(sc, cur_env, s7_make_symbol(sc, "EDOM"), s7_make_integer(sc, (s7_int)EDOM));
#endif
#ifdef EPIPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "EPIPE"), s7_make_integer(sc, (s7_int)EPIPE));
#endif
#ifdef EMLINK
  s7_define(sc, cur_env, s7_make_symbol(sc, "EMLINK"), s7_make_integer(sc, (s7_int)EMLINK));
#endif
#ifdef EROFS
  s7_define(sc, cur_env, s7_make_symbol(sc, "EROFS"), s7_make_integer(sc, (s7_int)EROFS));
#endif
#ifdef ESPIPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "ESPIPE"), s7_make_integer(sc, (s7_int)ESPIPE));
#endif
#ifdef ENOSPC
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENOSPC"), s7_make_integer(sc, (s7_int)ENOSPC));
#endif
#ifdef EFBIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "EFBIG"), s7_make_integer(sc, (s7_int)EFBIG));
#endif
#ifdef ETXTBSY
  s7_define(sc, cur_env, s7_make_symbol(sc, "ETXTBSY"), s7_make_integer(sc, (s7_int)ETXTBSY));
#endif
#ifdef ENOTTY
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENOTTY"), s7_make_integer(sc, (s7_int)ENOTTY));
#endif
#ifdef EMFILE
  s7_define(sc, cur_env, s7_make_symbol(sc, "EMFILE"), s7_make_integer(sc, (s7_int)EMFILE));
#endif
#ifdef ENFILE
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENFILE"), s7_make_integer(sc, (s7_int)ENFILE));
#endif
#ifdef EINVAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "EINVAL"), s7_make_integer(sc, (s7_int)EINVAL));
#endif
#ifdef EISDIR
  s7_define(sc, cur_env, s7_make_symbol(sc, "EISDIR"), s7_make_integer(sc, (s7_int)EISDIR));
#endif
#ifdef ENOTDIR
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENOTDIR"), s7_make_integer(sc, (s7_int)ENOTDIR));
#endif
#ifdef ENODEV
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENODEV"), s7_make_integer(sc, (s7_int)ENODEV));
#endif
#ifdef EXDEV
  s7_define(sc, cur_env, s7_make_symbol(sc, "EXDEV"), s7_make_integer(sc, (s7_int)EXDEV));
#endif
#ifdef EEXIST
  s7_define(sc, cur_env, s7_make_symbol(sc, "EEXIST"), s7_make_integer(sc, (s7_int)EEXIST));
#endif
#ifdef EBUSY
  s7_define(sc, cur_env, s7_make_symbol(sc, "EBUSY"), s7_make_integer(sc, (s7_int)EBUSY));
#endif
#ifdef ENOTBLK
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENOTBLK"), s7_make_integer(sc, (s7_int)ENOTBLK));
#endif
#ifdef EFAULT
  s7_define(sc, cur_env, s7_make_symbol(sc, "EFAULT"), s7_make_integer(sc, (s7_int)EFAULT));
#endif
#ifdef EACCES
  s7_define(sc, cur_env, s7_make_symbol(sc, "EACCES"), s7_make_integer(sc, (s7_int)EACCES));
#endif
#ifdef ENOMEM
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENOMEM"), s7_make_integer(sc, (s7_int)ENOMEM));
#endif
#ifdef EAGAIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "EAGAIN"), s7_make_integer(sc, (s7_int)EAGAIN));
#endif
#ifdef ECHILD
  s7_define(sc, cur_env, s7_make_symbol(sc, "ECHILD"), s7_make_integer(sc, (s7_int)ECHILD));
#endif
#ifdef EBADF
  s7_define(sc, cur_env, s7_make_symbol(sc, "EBADF"), s7_make_integer(sc, (s7_int)EBADF));
#endif
#ifdef ENOEXEC
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENOEXEC"), s7_make_integer(sc, (s7_int)ENOEXEC));
#endif
#ifdef E2BIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "E2BIG"), s7_make_integer(sc, (s7_int)E2BIG));
#endif
#ifdef ENXIO
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENXIO"), s7_make_integer(sc, (s7_int)ENXIO));
#endif
#ifdef EIO
  s7_define(sc, cur_env, s7_make_symbol(sc, "EIO"), s7_make_integer(sc, (s7_int)EIO));
#endif
#ifdef EINTR
  s7_define(sc, cur_env, s7_make_symbol(sc, "EINTR"), s7_make_integer(sc, (s7_int)EINTR));
#endif
#ifdef ESRCH
  s7_define(sc, cur_env, s7_make_symbol(sc, "ESRCH"), s7_make_integer(sc, (s7_int)ESRCH));
#endif
#ifdef ENOENT
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENOENT"), s7_make_integer(sc, (s7_int)ENOENT));
#endif
#ifdef EPERM
  s7_define(sc, cur_env, s7_make_symbol(sc, "EPERM"), s7_make_integer(sc, (s7_int)EPERM));
#endif
#ifdef EILSEQ
  s7_define(sc, cur_env, s7_make_symbol(sc, "EILSEQ"), s7_make_integer(sc, (s7_int)EILSEQ));
#endif
#ifdef ERFKILL
  s7_define(sc, cur_env, s7_make_symbol(sc, "ERFKILL"), s7_make_integer(sc, (s7_int)ERFKILL));
#endif
#ifdef ENOTRECOVERABLE
  s7_define(sc, cur_env, s7_make_symbol(sc, "ENOTRECOVERABLE"), s7_make_integer(sc, (s7_int)ENOTRECOVERABLE));
#endif
#ifdef EOWNERDEAD
  s7_define(sc, cur_env, s7_make_symbol(sc, "EOWNERDEAD"), s7_make_integer(sc, (s7_int)EOWNERDEAD));
#endif
#ifdef ECANCELED
  s7_define(sc, cur_env, s7_make_symbol(sc, "ECANCELED"), s7_make_integer(sc, (s7_int)ECANCELED));
#endif
#ifdef __GLIBC_MINOR__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__GLIBC_MINOR__"), s7_make_integer(sc, (s7_int)__GLIBC_MINOR__));
#endif
#ifdef __GLIBC__
  s7_define(sc, cur_env, s7_make_symbol(sc, "__GLIBC__"), s7_make_integer(sc, (s7_int)__GLIBC__));
#endif
#ifdef MB_CUR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "MB_CUR_MAX"), s7_make_integer(sc, (s7_int)MB_CUR_MAX));
#endif
#ifdef EXIT_SUCCESS
  s7_define(sc, cur_env, s7_make_symbol(sc, "EXIT_SUCCESS"), s7_make_integer(sc, (s7_int)EXIT_SUCCESS));
#endif
#ifdef EXIT_FAILURE
  s7_define(sc, cur_env, s7_make_symbol(sc, "EXIT_FAILURE"), s7_make_integer(sc, (s7_int)EXIT_FAILURE));
#endif
#ifdef RAND_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "RAND_MAX"), s7_make_integer(sc, (s7_int)RAND_MAX));
#endif
#ifdef P_tmpdir
  s7_define(sc, cur_env, s7_make_symbol(sc, "P_tmpdir"), s7_make_string(sc, (char*)P_tmpdir));
#endif
#ifdef IOV_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "IOV_MAX"), s7_make_integer(sc, (s7_int)IOV_MAX));
#endif
#ifdef FOPEN_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "FOPEN_MAX"), s7_make_integer(sc, (s7_int)FOPEN_MAX));
#endif
#ifdef L_cuserid
  s7_define(sc, cur_env, s7_make_symbol(sc, "L_cuserid"), s7_make_integer(sc, (s7_int)L_cuserid));
#endif
#ifdef L_ctermid
  s7_define(sc, cur_env, s7_make_symbol(sc, "L_ctermid"), s7_make_integer(sc, (s7_int)L_ctermid));
#endif
#ifdef FILENAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "FILENAME_MAX"), s7_make_integer(sc, (s7_int)FILENAME_MAX));
#endif
#ifdef TMP_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "TMP_MAX"), s7_make_integer(sc, (s7_int)TMP_MAX));
#endif
#ifdef L_tmpnam
  s7_define(sc, cur_env, s7_make_symbol(sc, "L_tmpnam"), s7_make_integer(sc, (s7_int)L_tmpnam));
#endif
#ifdef EOF
  s7_define(sc, cur_env, s7_make_symbol(sc, "EOF"), s7_make_integer(sc, (s7_int)EOF));
#endif
#ifdef BUFSIZ
  s7_define(sc, cur_env, s7_make_symbol(sc, "BUFSIZ"), s7_make_integer(sc, (s7_int)BUFSIZ));
#endif
#ifdef _IONBF
  s7_define(sc, cur_env, s7_make_symbol(sc, "_IONBF"), s7_make_integer(sc, (s7_int)_IONBF));
#endif
#ifdef _IOLBF
  s7_define(sc, cur_env, s7_make_symbol(sc, "_IOLBF"), s7_make_integer(sc, (s7_int)_IOLBF));
#endif
#ifdef _IOFBF
  s7_define(sc, cur_env, s7_make_symbol(sc, "_IOFBF"), s7_make_integer(sc, (s7_int)_IOFBF));
#endif
#ifdef FNM_NOMATCH
  s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_NOMATCH"), s7_make_integer(sc, (s7_int)FNM_NOMATCH));
#endif
#ifdef FNM_EXTMATCH
  s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_EXTMATCH"), s7_make_integer(sc, (s7_int)FNM_EXTMATCH));
#endif
#ifdef FNM_CASEFOLD
  s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_CASEFOLD"), s7_make_integer(sc, (s7_int)FNM_CASEFOLD));
#endif
#ifdef FNM_LEADING_DIR
  s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_LEADING_DIR"), s7_make_integer(sc, (s7_int)FNM_LEADING_DIR));
#endif
#ifdef FNM_FILE_NAME
  s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_FILE_NAME"), s7_make_integer(sc, (s7_int)FNM_FILE_NAME));
#endif
#ifdef FNM_PERIOD
  s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_PERIOD"), s7_make_integer(sc, (s7_int)FNM_PERIOD));
#endif
#ifdef FNM_NOESCAPE
  s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_NOESCAPE"), s7_make_integer(sc, (s7_int)FNM_NOESCAPE));
#endif
#ifdef FNM_PATHNAME
  s7_define(sc, cur_env, s7_make_symbol(sc, "FNM_PATHNAME"), s7_make_integer(sc, (s7_int)FNM_PATHNAME));
#endif
#ifdef FE_TOWARDZERO
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_TOWARDZERO"), s7_make_integer(sc, (s7_int)FE_TOWARDZERO));
#endif
#ifdef FE_DOWNWARD
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_DOWNWARD"), s7_make_integer(sc, (s7_int)FE_DOWNWARD));
#endif
#ifdef FE_UPWARD
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_UPWARD"), s7_make_integer(sc, (s7_int)FE_UPWARD));
#endif
#ifdef FE_TONEAREST
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_TONEAREST"), s7_make_integer(sc, (s7_int)FE_TONEAREST));
#endif
#ifdef FE_ALL_EXCEPT
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_ALL_EXCEPT"), s7_make_integer(sc, (s7_int)FE_ALL_EXCEPT));
#endif
#ifdef FE_INVALID
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_INVALID"), s7_make_integer(sc, (s7_int)FE_INVALID));
#endif
#ifdef FE_OVERFLOW
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_OVERFLOW"), s7_make_integer(sc, (s7_int)FE_OVERFLOW));
#endif
#ifdef FE_UNDERFLOW
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_UNDERFLOW"), s7_make_integer(sc, (s7_int)FE_UNDERFLOW));
#endif
#ifdef FE_DIVBYZERO
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_DIVBYZERO"), s7_make_integer(sc, (s7_int)FE_DIVBYZERO));
#endif
#ifdef FE_INEXACT
  s7_define(sc, cur_env, s7_make_symbol(sc, "FE_INEXACT"), s7_make_integer(sc, (s7_int)FE_INEXACT));
#endif
#ifdef POSIX_FADV_NOREUSE
  s7_define(sc, cur_env, s7_make_symbol(sc, "POSIX_FADV_NOREUSE"), s7_make_integer(sc, (s7_int)POSIX_FADV_NOREUSE));
#endif
#ifdef POSIX_FADV_DONTNEED
  s7_define(sc, cur_env, s7_make_symbol(sc, "POSIX_FADV_DONTNEED"), s7_make_integer(sc, (s7_int)POSIX_FADV_DONTNEED));
#endif
#ifdef POSIX_FADV_WILLNEED
  s7_define(sc, cur_env, s7_make_symbol(sc, "POSIX_FADV_WILLNEED"), s7_make_integer(sc, (s7_int)POSIX_FADV_WILLNEED));
#endif
#ifdef POSIX_FADV_SEQUENTIAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "POSIX_FADV_SEQUENTIAL"), s7_make_integer(sc, (s7_int)POSIX_FADV_SEQUENTIAL));
#endif
#ifdef POSIX_FADV_RANDOM
  s7_define(sc, cur_env, s7_make_symbol(sc, "POSIX_FADV_RANDOM"), s7_make_integer(sc, (s7_int)POSIX_FADV_RANDOM));
#endif
#ifdef POSIX_FADV_NORMAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "POSIX_FADV_NORMAL"), s7_make_integer(sc, (s7_int)POSIX_FADV_NORMAL));
#endif
#ifdef F_UNLCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_UNLCK"), s7_make_integer(sc, (s7_int)F_UNLCK));
#endif
#ifdef F_WRLCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_WRLCK"), s7_make_integer(sc, (s7_int)F_WRLCK));
#endif
#ifdef F_RDLCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_RDLCK"), s7_make_integer(sc, (s7_int)F_RDLCK));
#endif
#ifdef FD_CLOEXEC
  s7_define(sc, cur_env, s7_make_symbol(sc, "FD_CLOEXEC"), s7_make_integer(sc, (s7_int)FD_CLOEXEC));
#endif
#ifdef F_SETLKW64
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_SETLKW64"), s7_make_integer(sc, (s7_int)F_SETLKW64));
#endif
#ifdef F_SETLK64
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_SETLK64"), s7_make_integer(sc, (s7_int)F_SETLK64));
#endif
#ifdef F_GETLK64
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_GETLK64"), s7_make_integer(sc, (s7_int)F_GETLK64));
#endif
#ifdef F_SETLKW
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_SETLKW"), s7_make_integer(sc, (s7_int)F_SETLKW));
#endif
#ifdef F_SETLK
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_SETLK"), s7_make_integer(sc, (s7_int)F_SETLK));
#endif
#ifdef F_GETLK
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_GETLK"), s7_make_integer(sc, (s7_int)F_GETLK));
#endif
#ifdef F_SETFL
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_SETFL"), s7_make_integer(sc, (s7_int)F_SETFL));
#endif
#ifdef F_GETFL
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_GETFL"), s7_make_integer(sc, (s7_int)F_GETFL));
#endif
#ifdef F_SETFD
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_SETFD"), s7_make_integer(sc, (s7_int)F_SETFD));
#endif
#ifdef F_GETFD
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_GETFD"), s7_make_integer(sc, (s7_int)F_GETFD));
#endif
#ifdef F_DUPFD
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_DUPFD"), s7_make_integer(sc, (s7_int)F_DUPFD));
#endif
#ifdef O_LARGEFILE
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_LARGEFILE"), s7_make_integer(sc, (s7_int)O_LARGEFILE));
#endif
#ifdef O_RSYNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_RSYNC"), s7_make_integer(sc, (s7_int)O_RSYNC));
#endif
#ifdef O_DSYNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_DSYNC"), s7_make_integer(sc, (s7_int)O_DSYNC));
#endif
#ifdef O_ASYNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_ASYNC"), s7_make_integer(sc, (s7_int)O_ASYNC));
#endif
#ifdef O_FSYNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_FSYNC"), s7_make_integer(sc, (s7_int)O_FSYNC));
#endif
#ifdef O_SYNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_SYNC"), s7_make_integer(sc, (s7_int)O_SYNC));
#endif
#ifdef O_NDELAY
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_NDELAY"), s7_make_integer(sc, (s7_int)O_NDELAY));
#endif
#ifdef O_NONBLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_NONBLOCK"), s7_make_integer(sc, (s7_int)O_NONBLOCK));
#endif
#ifdef O_APPEND
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_APPEND"), s7_make_integer(sc, (s7_int)O_APPEND));
#endif
#ifdef O_TRUNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_TRUNC"), s7_make_integer(sc, (s7_int)O_TRUNC));
#endif
#ifdef O_NOCTTY
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_NOCTTY"), s7_make_integer(sc, (s7_int)O_NOCTTY));
#endif
#ifdef O_EXCL
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_EXCL"), s7_make_integer(sc, (s7_int)O_EXCL));
#endif
#ifdef O_CREAT
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_CREAT"), s7_make_integer(sc, (s7_int)O_CREAT));
#endif
#ifdef O_RDWR
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_RDWR"), s7_make_integer(sc, (s7_int)O_RDWR));
#endif
#ifdef O_WRONLY
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_WRONLY"), s7_make_integer(sc, (s7_int)O_WRONLY));
#endif
#ifdef O_RDONLY
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_RDONLY"), s7_make_integer(sc, (s7_int)O_RDONLY));
#endif
#ifdef O_ACCMODE
  s7_define(sc, cur_env, s7_make_symbol(sc, "O_ACCMODE"), s7_make_integer(sc, (s7_int)O_ACCMODE));
#endif
#ifdef F_TEST
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_TEST"), s7_make_integer(sc, (s7_int)F_TEST));
#endif
#ifdef F_TLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_TLOCK"), s7_make_integer(sc, (s7_int)F_TLOCK));
#endif
#ifdef F_LOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_LOCK"), s7_make_integer(sc, (s7_int)F_LOCK));
#endif
#ifdef F_ULOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_ULOCK"), s7_make_integer(sc, (s7_int)F_ULOCK));
#endif
#ifdef SEEK_END
  s7_define(sc, cur_env, s7_make_symbol(sc, "SEEK_END"), s7_make_integer(sc, (s7_int)SEEK_END));
#endif
#ifdef SEEK_CUR
  s7_define(sc, cur_env, s7_make_symbol(sc, "SEEK_CUR"), s7_make_integer(sc, (s7_int)SEEK_CUR));
#endif
#ifdef SEEK_SET
  s7_define(sc, cur_env, s7_make_symbol(sc, "SEEK_SET"), s7_make_integer(sc, (s7_int)SEEK_SET));
#endif
#ifdef F_OK
  s7_define(sc, cur_env, s7_make_symbol(sc, "F_OK"), s7_make_integer(sc, (s7_int)F_OK));
#endif
#ifdef X_OK
  s7_define(sc, cur_env, s7_make_symbol(sc, "X_OK"), s7_make_integer(sc, (s7_int)X_OK));
#endif
#ifdef W_OK
  s7_define(sc, cur_env, s7_make_symbol(sc, "W_OK"), s7_make_integer(sc, (s7_int)W_OK));
#endif
#ifdef R_OK
  s7_define(sc, cur_env, s7_make_symbol(sc, "R_OK"), s7_make_integer(sc, (s7_int)R_OK));
#endif
#ifdef S_IRWXO
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRWXO"), s7_make_integer(sc, (s7_int)S_IRWXO));
#endif
#ifdef S_IXOTH
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IXOTH"), s7_make_integer(sc, (s7_int)S_IXOTH));
#endif
#ifdef S_IWOTH
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IWOTH"), s7_make_integer(sc, (s7_int)S_IWOTH));
#endif
#ifdef S_IROTH
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IROTH"), s7_make_integer(sc, (s7_int)S_IROTH));
#endif
#ifdef S_IRWXG
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRWXG"), s7_make_integer(sc, (s7_int)S_IRWXG));
#endif
#ifdef S_IXGRP
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IXGRP"), s7_make_integer(sc, (s7_int)S_IXGRP));
#endif
#ifdef S_IWGRP
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IWGRP"), s7_make_integer(sc, (s7_int)S_IWGRP));
#endif
#ifdef S_IRGRP
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRGRP"), s7_make_integer(sc, (s7_int)S_IRGRP));
#endif
#ifdef S_IRWXU
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRWXU"), s7_make_integer(sc, (s7_int)S_IRWXU));
#endif
#ifdef S_IXUSR
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IXUSR"), s7_make_integer(sc, (s7_int)S_IXUSR));
#endif
#ifdef S_IWUSR
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IWUSR"), s7_make_integer(sc, (s7_int)S_IWUSR));
#endif
#ifdef S_IRUSR
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IRUSR"), s7_make_integer(sc, (s7_int)S_IRUSR));
#endif
#ifdef S_ISGID
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_ISGID"), s7_make_integer(sc, (s7_int)S_ISGID));
#endif
#ifdef S_ISUID
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_ISUID"), s7_make_integer(sc, (s7_int)S_ISUID));
#endif
#ifdef S_IFSOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IFSOCK"), s7_make_integer(sc, (s7_int)S_IFSOCK));
#endif
#ifdef __S_IFLNK
  s7_define(sc, cur_env, s7_make_symbol(sc, "__S_IFLNK"), s7_make_integer(sc, (s7_int)__S_IFLNK));
#endif
#ifdef S_IFIFO
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IFIFO"), s7_make_integer(sc, (s7_int)S_IFIFO));
#endif
#ifdef S_IFREG
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IFREG"), s7_make_integer(sc, (s7_int)S_IFREG));
#endif
#ifdef S_IFBLK
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IFBLK"), s7_make_integer(sc, (s7_int)S_IFBLK));
#endif
#ifdef S_IFCHR
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IFCHR"), s7_make_integer(sc, (s7_int)S_IFCHR));
#endif
#ifdef S_IFDIR
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IFDIR"), s7_make_integer(sc, (s7_int)S_IFDIR));
#endif
#ifdef S_IFMT
  s7_define(sc, cur_env, s7_make_symbol(sc, "S_IFMT"), s7_make_integer(sc, (s7_int)S_IFMT));
#endif
#ifdef __LITTLE_ENDIAN
  s7_define(sc, cur_env, s7_make_symbol(sc, "__LITTLE_ENDIAN"), s7_make_integer(sc, (s7_int)__LITTLE_ENDIAN));
#endif
#ifdef __BIG_ENDIAN
  s7_define(sc, cur_env, s7_make_symbol(sc, "__BIG_ENDIAN"), s7_make_integer(sc, (s7_int)__BIG_ENDIAN));
#endif
#ifdef __BYTE_ORDER
  s7_define(sc, cur_env, s7_make_symbol(sc, "__BYTE_ORDER"), s7_make_integer(sc, (s7_int)__BYTE_ORDER));
#endif
#ifdef WINT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "WINT_MAX"), s7_make_integer(sc, (s7_int)WINT_MAX));
#endif
#ifdef WINT_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "WINT_MIN"), s7_make_integer(sc, (s7_int)WINT_MIN));
#endif
#ifdef WCHAR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "WCHAR_MAX"), s7_make_integer(sc, (s7_int)WCHAR_MAX));
#endif
#ifdef WCHAR_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "WCHAR_MIN"), s7_make_integer(sc, (s7_int)WCHAR_MIN));
#endif
#ifdef SIZE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIZE_MAX"), s7_make_integer(sc, (s7_int)SIZE_MAX));
#endif
#ifdef SIG_ATOMIC_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIG_ATOMIC_MAX"), s7_make_integer(sc, (s7_int)SIG_ATOMIC_MAX));
#endif
#ifdef SIG_ATOMIC_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "SIG_ATOMIC_MIN"), s7_make_integer(sc, (s7_int)SIG_ATOMIC_MIN));
#endif
#ifdef PTRDIFF_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "PTRDIFF_MAX"), s7_make_integer(sc, (s7_int)PTRDIFF_MAX));
#endif
#ifdef PTRDIFF_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "PTRDIFF_MIN"), s7_make_integer(sc, (s7_int)PTRDIFF_MIN));
#endif
#ifdef UINTMAX_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINTMAX_MAX"), s7_make_integer(sc, (s7_int)UINTMAX_MAX));
#endif
#ifdef INTMAX_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INTMAX_MAX"), s7_make_integer(sc, (s7_int)INTMAX_MAX));
#endif
#ifdef INTMAX_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INTMAX_MIN"), s7_make_integer(sc, (s7_int)INTMAX_MIN));
#endif
#ifdef UINTPTR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINTPTR_MAX"), s7_make_integer(sc, (s7_int)UINTPTR_MAX));
#endif
#ifdef INTPTR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INTPTR_MAX"), s7_make_integer(sc, (s7_int)INTPTR_MAX));
#endif
#ifdef INTPTR_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INTPTR_MIN"), s7_make_integer(sc, (s7_int)INTPTR_MIN));
#endif
#ifdef UINT_FAST64_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT_FAST64_MAX"), s7_make_integer(sc, (s7_int)UINT_FAST64_MAX));
#endif
#ifdef UINT_FAST32_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT_FAST32_MAX"), s7_make_integer(sc, (s7_int)UINT_FAST32_MAX));
#endif
#ifdef UINT_FAST16_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT_FAST16_MAX"), s7_make_integer(sc, (s7_int)UINT_FAST16_MAX));
#endif
#ifdef UINT_FAST8_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT_FAST8_MAX"), s7_make_integer(sc, (s7_int)UINT_FAST8_MAX));
#endif
#ifdef INT_FAST64_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_FAST64_MAX"), s7_make_integer(sc, (s7_int)INT_FAST64_MAX));
#endif
#ifdef INT_FAST32_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_FAST32_MAX"), s7_make_integer(sc, (s7_int)INT_FAST32_MAX));
#endif
#ifdef INT_FAST16_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_FAST16_MAX"), s7_make_integer(sc, (s7_int)INT_FAST16_MAX));
#endif
#ifdef INT_FAST8_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_FAST8_MAX"), s7_make_integer(sc, (s7_int)INT_FAST8_MAX));
#endif
#ifdef INT_FAST64_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_FAST64_MIN"), s7_make_integer(sc, (s7_int)INT_FAST64_MIN));
#endif
#ifdef INT_FAST32_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_FAST32_MIN"), s7_make_integer(sc, (s7_int)INT_FAST32_MIN));
#endif
#ifdef INT_FAST16_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_FAST16_MIN"), s7_make_integer(sc, (s7_int)INT_FAST16_MIN));
#endif
#ifdef INT_FAST8_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_FAST8_MIN"), s7_make_integer(sc, (s7_int)INT_FAST8_MIN));
#endif
#ifdef UINT_LEAST64_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT_LEAST64_MAX"), s7_make_integer(sc, (s7_int)UINT_LEAST64_MAX));
#endif
#ifdef UINT_LEAST32_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT_LEAST32_MAX"), s7_make_integer(sc, (s7_int)UINT_LEAST32_MAX));
#endif
#ifdef UINT_LEAST16_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT_LEAST16_MAX"), s7_make_integer(sc, (s7_int)UINT_LEAST16_MAX));
#endif
#ifdef UINT_LEAST8_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT_LEAST8_MAX"), s7_make_integer(sc, (s7_int)UINT_LEAST8_MAX));
#endif
#ifdef INT_LEAST64_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_LEAST64_MAX"), s7_make_integer(sc, (s7_int)INT_LEAST64_MAX));
#endif
#ifdef INT_LEAST32_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_LEAST32_MAX"), s7_make_integer(sc, (s7_int)INT_LEAST32_MAX));
#endif
#ifdef INT_LEAST16_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_LEAST16_MAX"), s7_make_integer(sc, (s7_int)INT_LEAST16_MAX));
#endif
#ifdef INT_LEAST8_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_LEAST8_MAX"), s7_make_integer(sc, (s7_int)INT_LEAST8_MAX));
#endif
#ifdef INT_LEAST64_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_LEAST64_MIN"), s7_make_integer(sc, (s7_int)INT_LEAST64_MIN));
#endif
#ifdef INT_LEAST32_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_LEAST32_MIN"), s7_make_integer(sc, (s7_int)INT_LEAST32_MIN));
#endif
#ifdef INT_LEAST16_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_LEAST16_MIN"), s7_make_integer(sc, (s7_int)INT_LEAST16_MIN));
#endif
#ifdef INT_LEAST8_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_LEAST8_MIN"), s7_make_integer(sc, (s7_int)INT_LEAST8_MIN));
#endif
#ifdef UINT64_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT64_MAX"), s7_make_integer(sc, (s7_int)UINT64_MAX));
#endif
#ifdef UINT32_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT32_MAX"), s7_make_integer(sc, (s7_int)UINT32_MAX));
#endif
#ifdef UINT16_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT16_MAX"), s7_make_integer(sc, (s7_int)UINT16_MAX));
#endif
#ifdef UINT8_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT8_MAX"), s7_make_integer(sc, (s7_int)UINT8_MAX));
#endif
#ifdef INT64_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT64_MAX"), s7_make_integer(sc, (s7_int)INT64_MAX));
#endif
#ifdef INT32_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT32_MAX"), s7_make_integer(sc, (s7_int)INT32_MAX));
#endif
#ifdef INT16_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT16_MAX"), s7_make_integer(sc, (s7_int)INT16_MAX));
#endif
#ifdef INT8_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT8_MAX"), s7_make_integer(sc, (s7_int)INT8_MAX));
#endif
#ifdef INT64_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT64_MIN"), s7_make_integer(sc, (s7_int)INT64_MIN));
#endif
#ifdef INT32_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT32_MIN"), s7_make_integer(sc, (s7_int)INT32_MIN));
#endif
#ifdef INT16_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT16_MIN"), s7_make_integer(sc, (s7_int)INT16_MIN));
#endif
#ifdef INT8_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT8_MIN"), s7_make_integer(sc, (s7_int)INT8_MIN));
#endif
#ifdef LDBL_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "LDBL_MIN"), s7_make_real(sc, (s7_double)LDBL_MIN));
#endif
#ifdef DBL_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "DBL_MIN"), s7_make_real(sc, (s7_double)DBL_MIN));
#endif
#ifdef FLT_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_MIN"), s7_make_real(sc, (s7_double)FLT_MIN));
#endif
#ifdef LDBL_EPSILON
  s7_define(sc, cur_env, s7_make_symbol(sc, "LDBL_EPSILON"), s7_make_real(sc, (s7_double)LDBL_EPSILON));
#endif
#ifdef DBL_EPSILON
  s7_define(sc, cur_env, s7_make_symbol(sc, "DBL_EPSILON"), s7_make_real(sc, (s7_double)DBL_EPSILON));
#endif
#ifdef FLT_EPSILON
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_EPSILON"), s7_make_real(sc, (s7_double)FLT_EPSILON));
#endif
#ifdef LDBL_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "LDBL_MAX"), s7_make_real(sc, (s7_double)LDBL_MAX));
#endif
#ifdef DBL_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "DBL_MAX"), s7_make_real(sc, (s7_double)DBL_MAX));
#endif
#ifdef FLT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_MAX"), s7_make_real(sc, (s7_double)FLT_MAX));
#endif
#ifdef FLT_EVAL_METHOD
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_EVAL_METHOD"), s7_make_integer(sc, (s7_int)FLT_EVAL_METHOD));
#endif
#ifdef FLT_ROUNDS
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_ROUNDS"), s7_make_integer(sc, (s7_int)FLT_ROUNDS));
#endif
#ifdef LDBL_MAX_10_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "LDBL_MAX_10_EXP"), s7_make_integer(sc, (s7_int)LDBL_MAX_10_EXP));
#endif
#ifdef DBL_MAX_10_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "DBL_MAX_10_EXP"), s7_make_integer(sc, (s7_int)DBL_MAX_10_EXP));
#endif
#ifdef FLT_MAX_10_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_MAX_10_EXP"), s7_make_integer(sc, (s7_int)FLT_MAX_10_EXP));
#endif
#ifdef LDBL_MAX_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "LDBL_MAX_EXP"), s7_make_integer(sc, (s7_int)LDBL_MAX_EXP));
#endif
#ifdef DBL_MAX_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "DBL_MAX_EXP"), s7_make_integer(sc, (s7_int)DBL_MAX_EXP));
#endif
#ifdef FLT_MAX_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_MAX_EXP"), s7_make_integer(sc, (s7_int)FLT_MAX_EXP));
#endif
#ifdef LDBL_MIN_10_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "LDBL_MIN_10_EXP"), s7_make_integer(sc, (s7_int)LDBL_MIN_10_EXP));
#endif
#ifdef DBL_MIN_10_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "DBL_MIN_10_EXP"), s7_make_integer(sc, (s7_int)DBL_MIN_10_EXP));
#endif
#ifdef FLT_MIN_10_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_MIN_10_EXP"), s7_make_integer(sc, (s7_int)FLT_MIN_10_EXP));
#endif
#ifdef LDBL_MIN_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "LDBL_MIN_EXP"), s7_make_integer(sc, (s7_int)LDBL_MIN_EXP));
#endif
#ifdef DBL_MIN_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "DBL_MIN_EXP"), s7_make_integer(sc, (s7_int)DBL_MIN_EXP));
#endif
#ifdef FLT_MIN_EXP
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_MIN_EXP"), s7_make_integer(sc, (s7_int)FLT_MIN_EXP));
#endif
#ifdef LDBL_DIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "LDBL_DIG"), s7_make_integer(sc, (s7_int)LDBL_DIG));
#endif
#ifdef DBL_DIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "DBL_DIG"), s7_make_integer(sc, (s7_int)DBL_DIG));
#endif
#ifdef FLT_DIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_DIG"), s7_make_integer(sc, (s7_int)FLT_DIG));
#endif
#ifdef LDBL_MANT_DIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "LDBL_MANT_DIG"), s7_make_integer(sc, (s7_int)LDBL_MANT_DIG));
#endif
#ifdef DBL_MANT_DIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "DBL_MANT_DIG"), s7_make_integer(sc, (s7_int)DBL_MANT_DIG));
#endif
#ifdef FLT_MANT_DIG
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_MANT_DIG"), s7_make_integer(sc, (s7_int)FLT_MANT_DIG));
#endif
#ifdef FLT_RADIX
  s7_define(sc, cur_env, s7_make_symbol(sc, "FLT_RADIX"), s7_make_integer(sc, (s7_int)FLT_RADIX));
#endif
#ifdef RE_DUP_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "RE_DUP_MAX"), s7_make_integer(sc, (s7_int)RE_DUP_MAX));
#endif
#ifdef CHARCLASS_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "CHARCLASS_NAME_MAX"), s7_make_integer(sc, (s7_int)CHARCLASS_NAME_MAX));
#endif
#ifdef LINE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "LINE_MAX"), s7_make_integer(sc, (s7_int)LINE_MAX));
#endif
#ifdef EXPR_NEST_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "EXPR_NEST_MAX"), s7_make_integer(sc, (s7_int)EXPR_NEST_MAX));
#endif
#ifdef COLL_WEIGHTS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "COLL_WEIGHTS_MAX"), s7_make_integer(sc, (s7_int)COLL_WEIGHTS_MAX));
#endif
#ifdef BC_STRING_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "BC_STRING_MAX"), s7_make_integer(sc, (s7_int)BC_STRING_MAX));
#endif
#ifdef BC_SCALE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "BC_SCALE_MAX"), s7_make_integer(sc, (s7_int)BC_SCALE_MAX));
#endif
#ifdef BC_DIM_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "BC_DIM_MAX"), s7_make_integer(sc, (s7_int)BC_DIM_MAX));
#endif
#ifdef BC_BASE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "BC_BASE_MAX"), s7_make_integer(sc, (s7_int)BC_BASE_MAX));
#endif
#ifdef _POSIX2_CHARCLASS_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_CHARCLASS_NAME_MAX"), s7_make_integer(sc, (s7_int)_POSIX2_CHARCLASS_NAME_MAX));
#endif
#ifdef _POSIX2_RE_DUP_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_RE_DUP_MAX"), s7_make_integer(sc, (s7_int)_POSIX2_RE_DUP_MAX));
#endif
#ifdef _POSIX2_LINE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_LINE_MAX"), s7_make_integer(sc, (s7_int)_POSIX2_LINE_MAX));
#endif
#ifdef _POSIX2_EXPR_NEST_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_EXPR_NEST_MAX"), s7_make_integer(sc, (s7_int)_POSIX2_EXPR_NEST_MAX));
#endif
#ifdef _POSIX2_COLL_WEIGHTS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_COLL_WEIGHTS_MAX"), s7_make_integer(sc, (s7_int)_POSIX2_COLL_WEIGHTS_MAX));
#endif
#ifdef _POSIX2_BC_STRING_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_BC_STRING_MAX"), s7_make_integer(sc, (s7_int)_POSIX2_BC_STRING_MAX));
#endif
#ifdef _POSIX2_BC_SCALE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_BC_SCALE_MAX"), s7_make_integer(sc, (s7_int)_POSIX2_BC_SCALE_MAX));
#endif
#ifdef _POSIX2_BC_DIM_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_BC_DIM_MAX"), s7_make_integer(sc, (s7_int)_POSIX2_BC_DIM_MAX));
#endif
#ifdef _POSIX2_BC_BASE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX2_BC_BASE_MAX"), s7_make_integer(sc, (s7_int)_POSIX2_BC_BASE_MAX));
#endif
#ifdef NGROUPS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "NGROUPS_MAX"), s7_make_integer(sc, (s7_int)NGROUPS_MAX));
#endif
#ifdef SSIZE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "SSIZE_MAX"), s7_make_integer(sc, (s7_int)SSIZE_MAX));
#endif
#ifdef _POSIX_CLOCKRES_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_CLOCKRES_MIN"), s7_make_integer(sc, (s7_int)_POSIX_CLOCKRES_MIN));
#endif
#ifdef _POSIX_UIO_MAXIOV
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_UIO_MAXIOV"), s7_make_integer(sc, (s7_int)_POSIX_UIO_MAXIOV));
#endif
#ifdef _POSIX_HIWAT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_HIWAT"), s7_make_integer(sc, (s7_int)_POSIX_HIWAT));
#endif
#ifdef _POSIX_QLIMIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_QLIMIT"), s7_make_integer(sc, (s7_int)_POSIX_QLIMIT));
#endif
#ifdef _POSIX_TZNAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TZNAME_MAX"), s7_make_integer(sc, (s7_int)_POSIX_TZNAME_MAX));
#endif
#ifdef _POSIX_TTY_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TTY_NAME_MAX"), s7_make_integer(sc, (s7_int)_POSIX_TTY_NAME_MAX));
#endif
#ifdef _POSIX_TIMER_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_TIMER_MAX"), s7_make_integer(sc, (s7_int)_POSIX_TIMER_MAX));
#endif
#ifdef _POSIX_SYMLOOP_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SYMLOOP_MAX"), s7_make_integer(sc, (s7_int)_POSIX_SYMLOOP_MAX));
#endif
#ifdef _POSIX_SYMLINK_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SYMLINK_MAX"), s7_make_integer(sc, (s7_int)_POSIX_SYMLINK_MAX));
#endif
#ifdef _POSIX_STREAM_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_STREAM_MAX"), s7_make_integer(sc, (s7_int)_POSIX_STREAM_MAX));
#endif
#ifdef _POSIX_SSIZE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SSIZE_MAX"), s7_make_integer(sc, (s7_int)_POSIX_SSIZE_MAX));
#endif
#ifdef _POSIX_SIGQUEUE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SIGQUEUE_MAX"), s7_make_integer(sc, (s7_int)_POSIX_SIGQUEUE_MAX));
#endif
#ifdef _POSIX_SEM_VALUE_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SEM_VALUE_MAX"), s7_make_integer(sc, (s7_int)_POSIX_SEM_VALUE_MAX));
#endif
#ifdef _POSIX_SEM_NSEMS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_SEM_NSEMS_MAX"), s7_make_integer(sc, (s7_int)_POSIX_SEM_NSEMS_MAX));
#endif
#ifdef _POSIX_RTSIG_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_RTSIG_MAX"), s7_make_integer(sc, (s7_int)_POSIX_RTSIG_MAX));
#endif
#ifdef _POSIX_RE_DUP_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_RE_DUP_MAX"), s7_make_integer(sc, (s7_int)_POSIX_RE_DUP_MAX));
#endif
#ifdef _POSIX_PIPE_BUF
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_PIPE_BUF"), s7_make_integer(sc, (s7_int)_POSIX_PIPE_BUF));
#endif
#ifdef _POSIX_PATH_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_PATH_MAX"), s7_make_integer(sc, (s7_int)_POSIX_PATH_MAX));
#endif
#ifdef _POSIX_FD_SETSIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_FD_SETSIZE"), s7_make_integer(sc, (s7_int)_POSIX_FD_SETSIZE));
#endif
#ifdef _POSIX_OPEN_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_OPEN_MAX"), s7_make_integer(sc, (s7_int)_POSIX_OPEN_MAX));
#endif
#ifdef _POSIX_NGROUPS_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_NGROUPS_MAX"), s7_make_integer(sc, (s7_int)_POSIX_NGROUPS_MAX));
#endif
#ifdef _POSIX_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_NAME_MAX"), s7_make_integer(sc, (s7_int)_POSIX_NAME_MAX));
#endif
#ifdef _POSIX_MQ_PRIO_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MQ_PRIO_MAX"), s7_make_integer(sc, (s7_int)_POSIX_MQ_PRIO_MAX));
#endif
#ifdef _POSIX_MQ_OPEN_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MQ_OPEN_MAX"), s7_make_integer(sc, (s7_int)_POSIX_MQ_OPEN_MAX));
#endif
#ifdef _POSIX_MAX_INPUT
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MAX_INPUT"), s7_make_integer(sc, (s7_int)_POSIX_MAX_INPUT));
#endif
#ifdef _POSIX_MAX_CANON
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_MAX_CANON"), s7_make_integer(sc, (s7_int)_POSIX_MAX_CANON));
#endif
#ifdef _POSIX_LOGIN_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_LOGIN_NAME_MAX"), s7_make_integer(sc, (s7_int)_POSIX_LOGIN_NAME_MAX));
#endif
#ifdef _POSIX_LINK_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_LINK_MAX"), s7_make_integer(sc, (s7_int)_POSIX_LINK_MAX));
#endif
#ifdef _POSIX_HOST_NAME_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_HOST_NAME_MAX"), s7_make_integer(sc, (s7_int)_POSIX_HOST_NAME_MAX));
#endif
#ifdef _POSIX_DELAYTIMER_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_DELAYTIMER_MAX"), s7_make_integer(sc, (s7_int)_POSIX_DELAYTIMER_MAX));
#endif
#ifdef _POSIX_CHILD_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_CHILD_MAX"), s7_make_integer(sc, (s7_int)_POSIX_CHILD_MAX));
#endif
#ifdef _POSIX_ARG_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_ARG_MAX"), s7_make_integer(sc, (s7_int)_POSIX_ARG_MAX));
#endif
#ifdef _POSIX_AIO_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_AIO_MAX"), s7_make_integer(sc, (s7_int)_POSIX_AIO_MAX));
#endif
#ifdef _POSIX_AIO_LISTIO_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "_POSIX_AIO_LISTIO_MAX"), s7_make_integer(sc, (s7_int)_POSIX_AIO_LISTIO_MAX));
#endif
#ifdef ULLONG_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "ULLONG_MAX"), s7_make_integer(sc, (s7_int)ULLONG_MAX));
#endif
#ifdef LLONG_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "LLONG_MAX"), s7_make_integer(sc, (s7_int)LLONG_MAX));
#endif
#ifdef LLONG_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "LLONG_MIN"), s7_make_integer(sc, (s7_int)LLONG_MIN));
#endif
#ifdef ULONG_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "ULONG_MAX"), s7_make_integer(sc, (s7_int)ULONG_MAX));
#endif
#ifdef LONG_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "LONG_MAX"), s7_make_integer(sc, (s7_int)LONG_MAX));
#endif
#ifdef LONG_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "LONG_MIN"), s7_make_integer(sc, (s7_int)LONG_MIN));
#endif
#ifdef UINT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UINT_MAX"), s7_make_integer(sc, (s7_int)UINT_MAX));
#endif
#ifdef INT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_MAX"), s7_make_integer(sc, (s7_int)INT_MAX));
#endif
#ifdef INT_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "INT_MIN"), s7_make_integer(sc, (s7_int)INT_MIN));
#endif
#ifdef USHRT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "USHRT_MAX"), s7_make_integer(sc, (s7_int)USHRT_MAX));
#endif
#ifdef SHRT_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "SHRT_MAX"), s7_make_integer(sc, (s7_int)SHRT_MAX));
#endif
#ifdef SHRT_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "SHRT_MIN"), s7_make_integer(sc, (s7_int)SHRT_MIN));
#endif
#ifdef __WORDSIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "__WORDSIZE"), s7_make_integer(sc, (s7_int)__WORDSIZE));
#endif
#ifdef CHAR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "CHAR_MAX"), s7_make_integer(sc, (s7_int)CHAR_MAX));
#endif
#ifdef CHAR_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "CHAR_MIN"), s7_make_integer(sc, (s7_int)CHAR_MIN));
#endif
#ifdef CHAR_BIT
  s7_define(sc, cur_env, s7_make_symbol(sc, "CHAR_BIT"), s7_make_integer(sc, (s7_int)CHAR_BIT));
#endif
#ifdef UCHAR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "UCHAR_MAX"), s7_make_integer(sc, (s7_int)UCHAR_MAX));
#endif
#ifdef SCHAR_MAX
  s7_define(sc, cur_env, s7_make_symbol(sc, "SCHAR_MAX"), s7_make_integer(sc, (s7_int)SCHAR_MAX));
#endif
#ifdef SCHAR_MIN
  s7_define(sc, cur_env, s7_make_symbol(sc, "SCHAR_MIN"), s7_make_integer(sc, (s7_int)SCHAR_MIN));
#endif

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "regerror"),
            s7_make_typed_function(sc, "regerror", g_regerror, 2, 0, false, "regerror", NULL));

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

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "recvfrom"),
            s7_make_typed_function(sc, "recvfrom", g_recvfrom, 6, 0, false, "recvfrom", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setsockopt"),
            s7_make_typed_function(sc, "setsockopt", g_setsockopt, 5, 0, false, "setsockopt", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getsockopt"),
            s7_make_typed_function(sc, "getsockopt", g_getsockopt, 5, 0, false, "getsockopt", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "accept"),
            s7_make_typed_function(sc, "accept", g_accept, 3, 0, false, "accept", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getpeername"),
            s7_make_typed_function(sc, "getpeername", g_getpeername, 3, 0, false, "getpeername", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getsockname"),
            s7_make_typed_function(sc, "getsockname", g_getsockname, 3, 0, false, "getsockname", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "socketpair"),
            s7_make_typed_function(sc, "socketpair", g_socketpair, 3, 0, false, "socketpair", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "protoent.p_aliases"),
            s7_make_typed_function(sc, "protoent.p_aliases", g_protoent_p_aliases, 1, 0, false, "protoent.p_aliases", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "netent.n_aliases"),
            s7_make_typed_function(sc, "netent.n_aliases", g_netent_n_aliases, 1, 0, false, "netent.n_aliases", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "servent.s_aliases"),
            s7_make_typed_function(sc, "servent.s_aliases", g_servent_s_aliases, 1, 0, false, "servent.s_aliases", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "hostent.h_aliases"),
            s7_make_typed_function(sc, "hostent.h_aliases", g_hostent_h_aliases, 1, 0, false, "hostent.h_aliases", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "protoent.p_proto"),
            s7_make_typed_function(sc, "protoent.p_proto", g_protoent_p_proto, 1, 0, false, "protoent.p_proto", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "servent.s_port"),
            s7_make_typed_function(sc, "servent.s_port", g_servent_s_port, 1, 0, false, "servent.s_port", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "netent.n_net"),
            s7_make_typed_function(sc, "netent.n_net", g_netent_n_net, 1, 0, false, "netent.n_net", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "netent.n_addrtype"),
            s7_make_typed_function(sc, "netent.n_addrtype", g_netent_n_addrtype, 1, 0, false, "netent.n_addrtype", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "hostent.h_length"),
            s7_make_typed_function(sc, "hostent.h_length", g_hostent_h_length, 1, 0, false, "hostent.h_length", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "hostent.h_addrtype"),
            s7_make_typed_function(sc, "hostent.h_addrtype", g_hostent_h_addrtype, 1, 0, false, "hostent.h_addrtype", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "protoent.p_name"),
            s7_make_typed_function(sc, "protoent.p_name", g_protoent_p_name, 1, 0, false, "protoent.p_name", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "servent.s_proto"),
            s7_make_typed_function(sc, "servent.s_proto", g_servent_s_proto, 1, 0, false, "servent.s_proto", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "servent.s_name"),
            s7_make_typed_function(sc, "servent.s_name", g_servent_s_name, 1, 0, false, "servent.s_name", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "netent.n_name"),
            s7_make_typed_function(sc, "netent.n_name", g_netent_n_name, 1, 0, false, "netent.n_name", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "hostent.h_name"),
            s7_make_typed_function(sc, "hostent.h_name", g_hostent_h_name, 1, 0, false, "hostent.h_name", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.ai_next"),
            s7_make_typed_function(sc, "addrinfo.ai_next", g_addrinfo_ai_next, 1, 0, false, "addrinfo.ai_next", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.ai_canonname"),
            s7_make_typed_function(sc, "addrinfo.ai_canonname", g_addrinfo_ai_canonname, 1, 0, false, "addrinfo.ai_canonname", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.set_ai_protocol"),
            s7_make_typed_function(sc, "addrinfo.set_ai_protocol", g_addrinfo_set_ai_protocol, 2, 0, false, "addrinfo.set_ai_protocol", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.ai_protocol"),
            s7_make_typed_function(sc, "addrinfo.ai_protocol", g_addrinfo_ai_protocol, 1, 0, false, "addrinfo.ai_protocol", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.set_ai_socktype"),
            s7_make_typed_function(sc, "addrinfo.set_ai_socktype", g_addrinfo_set_ai_socktype, 2, 0, false, "addrinfo.set_ai_socktype", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.ai_socktype"),
            s7_make_typed_function(sc, "addrinfo.ai_socktype", g_addrinfo_ai_socktype, 1, 0, false, "addrinfo.ai_socktype", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.set_ai_family"),
            s7_make_typed_function(sc, "addrinfo.set_ai_family", g_addrinfo_set_ai_family, 2, 0, false, "addrinfo.set_ai_family", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.ai_family"),
            s7_make_typed_function(sc, "addrinfo.ai_family", g_addrinfo_ai_family, 1, 0, false, "addrinfo.ai_family", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.set_ai_flags"),
            s7_make_typed_function(sc, "addrinfo.set_ai_flags", g_addrinfo_set_ai_flags, 2, 0, false, "addrinfo.set_ai_flags", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.ai_flags"),
            s7_make_typed_function(sc, "addrinfo.ai_flags", g_addrinfo_ai_flags, 1, 0, false, "addrinfo.ai_flags", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "addrinfo.make"),
            s7_make_typed_function(sc, "addrinfo.make", g_addrinfo_make, 0, 0, false, "addrinfo.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getnameinfo"),
            s7_make_typed_function(sc, "getnameinfo", g_getnameinfo, 3, 0, false, "getnameinfo", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getaddrinfo"),
            s7_make_typed_function(sc, "getaddrinfo", g_getaddrinfo, 3, 0, false, "getaddrinfo", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ntohs"),
            s7_make_typed_function(sc, "ntohs", g_ntohs, 1, 0, false, "ntohs", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ntohl"),
            s7_make_typed_function(sc, "ntohl", g_ntohl, 1, 0, false, "ntohl", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "htons"),
            s7_make_typed_function(sc, "htons", g_htons, 1, 0, false, "htons", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "htonl"),
            s7_make_typed_function(sc, "htonl", g_htonl, 1, 0, false, "htonl", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "recvmsg"),
            s7_make_typed_function(sc, "recvmsg", s7__recvmsg, 3, 0, false, "int recvmsg(int struct-msghdr* int)", pl_iixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sendmsg"),
            s7_make_typed_function(sc, "sendmsg", s7__sendmsg, 3, 0, false, "int sendmsg(int const-struct-msghdr* int)", pl_iixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sendto"),
            s7_make_typed_function(sc, "sendto", s7__sendto, 6, 0, false, "int sendto(int void* int int const-struct-sockaddr* int)", pl_iixiixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "recv"),
            s7_make_typed_function(sc, "recv", s7__recv, 4, 0, false, "int recv(int void* int int)", pcl_iixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "send"),
            s7_make_typed_function(sc, "send", s7__send, 4, 0, false, "int send(int void* int int)", pcl_iixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "connect"),
            s7_make_typed_function(sc, "connect", s7__connect, 3, 0, false, "int connect(int const-struct-sockaddr* int)", pl_iixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "bind"),
            s7_make_typed_function(sc, "bind", s7__bind, 3, 0, false, "int bind(int const-struct-sockaddr* int)", pl_iixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gai_strerror"),
            s7_make_typed_function(sc, "gai_strerror", s7__gai_strerror, 1, 0, false, "char* gai_strerror(int)", pl_si));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "freeaddrinfo"),
            s7_make_typed_function(sc, "freeaddrinfo", s7__freeaddrinfo, 1, 0, false, "void freeaddrinfo(struct-addrinfo*)", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getprotobynumber"),
            s7_make_typed_function(sc, "getprotobynumber", s7__getprotobynumber, 1, 0, false, "protoent* getprotobynumber(int)", pl_xi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getprotobyname"),
            s7_make_typed_function(sc, "getprotobyname", s7__getprotobyname, 1, 0, false, "protoent* getprotobyname(char*)", pl_xs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getservbyport"),
            s7_make_typed_function(sc, "getservbyport", s7__getservbyport, 2, 0, false, "servent* getservbyport(int char*)", pl_xis));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getservbyname"),
            s7_make_typed_function(sc, "getservbyname", s7__getservbyname, 2, 0, false, "servent* getservbyname(char* char*)", pcl_xs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getnetbyaddr"),
            s7_make_typed_function(sc, "getnetbyaddr", s7__getnetbyaddr, 2, 0, false, "netent* getnetbyaddr(int int)", pcl_xi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getnetbyname"),
            s7_make_typed_function(sc, "getnetbyname", s7__getnetbyname, 1, 0, false, "netent* getnetbyname(char*)", pl_xs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gethostbyaddr"),
            s7_make_typed_function(sc, "gethostbyaddr", s7__gethostbyaddr, 3, 0, false, "hostent* gethostbyaddr(void* int int)", pcl_xxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gethostbyname"),
            s7_make_typed_function(sc, "gethostbyname", s7__gethostbyname, 1, 0, false, "hostent* gethostbyname(char*)", pl_xs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "shutdown"),
            s7_make_typed_function(sc, "shutdown", s7__shutdown, 2, 0, false, "int shutdown(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "listen"),
            s7_make_typed_function(sc, "listen", s7__listen, 2, 0, false, "int listen(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "socket"),
            s7_make_typed_function(sc, "socket", s7__socket, 3, 0, false, "int socket(int int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getnetent"),
            s7_make_typed_function(sc, "getnetent", s7__getnetent, 0, 0, false, "netent* getnetent(void)", pl_xt));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "endnetent"),
            s7_make_typed_function(sc, "endnetent", s7__endnetent, 0, 0, false, "void endnetent(void)", pcl_t));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setnetent"),
            s7_make_typed_function(sc, "setnetent", s7__setnetent, 1, 0, false, "void setnetent(int)", pl_ti));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getprotoent"),
            s7_make_typed_function(sc, "getprotoent", s7__getprotoent, 0, 0, false, "protoent* getprotoent(void)", pl_xt));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "endprotoent"),
            s7_make_typed_function(sc, "endprotoent", s7__endprotoent, 0, 0, false, "void endprotoent(void)", pcl_t));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setprotoent"),
            s7_make_typed_function(sc, "setprotoent", s7__setprotoent, 1, 0, false, "void setprotoent(int)", pl_ti));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getservent"),
            s7_make_typed_function(sc, "getservent", s7__getservent, 0, 0, false, "servent* getservent(void)", pl_xt));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "endservent"),
            s7_make_typed_function(sc, "endservent", s7__endservent, 0, 0, false, "void endservent(void)", pcl_t));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setservent"),
            s7_make_typed_function(sc, "setservent", s7__setservent, 1, 0, false, "void setservent(int)", pl_ti));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gethostent"),
            s7_make_typed_function(sc, "gethostent", s7__gethostent, 0, 0, false, "hostent* gethostent(void)", pl_xt));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "endhostent"),
            s7_make_typed_function(sc, "endhostent", s7__endhostent, 0, 0, false, "void endhostent(void)", pcl_t));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sethostent"),
            s7_make_typed_function(sc, "sethostent", s7__sethostent, 1, 0, false, "void sethostent(int)", pl_ti));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setrlimit"),
            s7_make_typed_function(sc, "setrlimit", s7__setrlimit, 2, 0, false, "int setrlimit(int struct-rlimit*)", pl_iix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getrlimit"),
            s7_make_typed_function(sc, "getrlimit", s7__getrlimit, 2, 0, false, "int getrlimit(int struct-rlimit*)", pl_iix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "signal"),
            s7_make_typed_function(sc, "signal", g_signal, 2, 0, false, "signal", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigset.make"),
            s7_make_typed_function(sc, "sigset.make", g_sigset_make, 0, 0, false, "sigset.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigtimedwait"),
            s7_make_typed_function(sc, "sigtimedwait", g_sigtimedwait, 3, 0, false, "sigtimedwait", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigaction"),
            s7_make_typed_function(sc, "sigaction", g_sigaction, 3, 0, false, "sigaction", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigwait"),
            s7_make_typed_function(sc, "sigwait", g_sigwait, 1, 0, false, "sigwait", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigqueue"),
            s7_make_typed_function(sc, "sigqueue", g_sigqueue, 3, 0, false, "sigqueue", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "waitpid"),
            s7_make_typed_function(sc, "waitpid", g_waitpid, 2, 0, false, "waitpid", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "wait"),
            s7_make_typed_function(sc, "wait", g_wait, 0, 0, false, "wait", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigaction.set_sa_flags"),
            s7_make_typed_function(sc, "sigaction.set_sa_flags", g_sigaction_set_sa_flags, 2, 0, false, "sigaction.set_sa_flags", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigaction.set_sa_handler"),
            s7_make_typed_function(sc, "sigaction.set_sa_handler", g_sigaction_set_sa_handler, 2, 0, false, "sigaction.set_sa_handler", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigaction.sa_mask"),
            s7_make_typed_function(sc, "sigaction.sa_mask", g_sigaction_sa_mask, 1, 0, false, "sigaction.sa_mask", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigaction.sa_flags"),
            s7_make_typed_function(sc, "sigaction.sa_flags", g_sigaction_sa_flags, 1, 0, false, "sigaction.sa_flags", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigaction.sa_handler"),
            s7_make_typed_function(sc, "sigaction.sa_handler", g_sigaction_sa_handler, 1, 0, false, "sigaction.sa_handler", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigaction.make"),
            s7_make_typed_function(sc, "sigaction.make", g_sigaction_make, 0, 0, false, "sigaction.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "timespec.tv_nsec"),
            s7_make_typed_function(sc, "timespec.tv_nsec", g_timespec_tv_nsec, 1, 0, false, "timespec.tv_nsec", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "timespec.tv_sec"),
            s7_make_typed_function(sc, "timespec.tv_sec", g_timespec_tv_sec, 1, 0, false, "timespec.tv_sec", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "timespec.make"),
            s7_make_typed_function(sc, "timespec.make", g_timespec_make, 0, 0, false, "timespec.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.ru_stime"),
            s7_make_typed_function(sc, "rusage.ru_stime", g_rusage_ru_stime, 1, 0, false, "rusage.ru_stime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.ru_utime"),
            s7_make_typed_function(sc, "rusage.ru_utime", g_rusage_ru_utime, 1, 0, false, "rusage.ru_utime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.ru_nivcsw"),
            s7_make_typed_function(sc, "rusage.ru_nivcsw", g_rusage_ru_nivcsw, 1, 0, false, "rusage.ru_nivcsw", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.ru_nvcsw"),
            s7_make_typed_function(sc, "rusage.ru_nvcsw", g_rusage_ru_nvcsw, 1, 0, false, "rusage.ru_nvcsw", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.ru_oublock"),
            s7_make_typed_function(sc, "rusage.ru_oublock", g_rusage_ru_oublock, 1, 0, false, "rusage.ru_oublock", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.ru_inblock"),
            s7_make_typed_function(sc, "rusage.ru_inblock", g_rusage_ru_inblock, 1, 0, false, "rusage.ru_inblock", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.ru_majflt"),
            s7_make_typed_function(sc, "rusage.ru_majflt", g_rusage_ru_majflt, 1, 0, false, "rusage.ru_majflt", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.ru_minflt"),
            s7_make_typed_function(sc, "rusage.ru_minflt", g_rusage_ru_minflt, 1, 0, false, "rusage.ru_minflt", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.ru_maxrss"),
            s7_make_typed_function(sc, "rusage.ru_maxrss", g_rusage_ru_maxrss, 1, 0, false, "rusage.ru_maxrss", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getrusage"),
            s7_make_typed_function(sc, "getrusage", g_getrusage, 2, 0, false, "getrusage", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rusage.make"),
            s7_make_typed_function(sc, "rusage.make", g_rusage_make, 0, 0, false, "rusage.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rlimit.rlim_max"),
            s7_make_typed_function(sc, "rlimit.rlim_max", g_rlimit_rlim_max, 1, 0, false, "rlimit.rlim_max", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rlimit.rlim_cur"),
            s7_make_typed_function(sc, "rlimit.rlim_cur", g_rlimit_rlim_cur, 1, 0, false, "rlimit.rlim_cur", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rlimit.make"),
            s7_make_typed_function(sc, "rlimit.make", g_rlimit_make, 0, 0, false, "rlimit.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setpriority"),
            s7_make_typed_function(sc, "setpriority", s7__setpriority, 3, 0, false, "int setpriority(int int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getpriority"),
            s7_make_typed_function(sc, "getpriority", s7__getpriority, 2, 0, false, "int getpriority(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigpending"),
            s7_make_typed_function(sc, "sigpending", s7__sigpending, 1, 0, false, "int sigpending(sigset_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigsuspend"),
            s7_make_typed_function(sc, "sigsuspend", s7__sigsuspend, 1, 0, false, "int sigsuspend(sigset_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigprocmask"),
            s7_make_typed_function(sc, "sigprocmask", s7__sigprocmask, 3, 0, false, "int sigprocmask(int sigset_t* sigset_t*)", pcl_iix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigismember"),
            s7_make_typed_function(sc, "sigismember", s7__sigismember, 2, 0, false, "int sigismember(sigset_t* int)", pl_ixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigdelset"),
            s7_make_typed_function(sc, "sigdelset", s7__sigdelset, 2, 0, false, "int sigdelset(sigset_t* int)", pl_ixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigaddset"),
            s7_make_typed_function(sc, "sigaddset", s7__sigaddset, 2, 0, false, "int sigaddset(sigset_t* int)", pl_ixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigfillset"),
            s7_make_typed_function(sc, "sigfillset", s7__sigfillset, 1, 0, false, "int sigfillset(sigset_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sigemptyset"),
            s7_make_typed_function(sc, "sigemptyset", s7__sigemptyset, 1, 0, false, "int sigemptyset(sigset_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "raise"),
            s7_make_typed_function(sc, "raise", s7__raise, 1, 0, false, "int raise(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "kill"),
            s7_make_typed_function(sc, "kill", s7__kill, 2, 0, false, "int kill(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "glob"),
            s7_make_typed_function(sc, "glob", g_glob, 3, 0, false, "glob", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "glob.gl_pathv"),
            s7_make_typed_function(sc, "glob.gl_pathv", g_glob_gl_pathv, 1, 0, false, "glob.gl_pathv", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "glob.gl_pathc"),
            s7_make_typed_function(sc, "glob.gl_pathc", g_glob_gl_pathc, 1, 0, false, "glob.gl_pathc", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "glob.make"),
            s7_make_typed_function(sc, "glob.make", g_glob_make, 0, 0, false, "glob.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "globfree"),
            s7_make_typed_function(sc, "globfree", s7__globfree, 1, 0, false, "void globfree(glob_t*)", pl_tx));

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

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "passwd.pw_shell"),
            s7_make_typed_function(sc, "passwd.pw_shell", g_passwd_pw_shell, 1, 0, false, "passwd.pw_shell", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "passwd.pw_dir"),
            s7_make_typed_function(sc, "passwd.pw_dir", g_passwd_pw_dir, 1, 0, false, "passwd.pw_dir", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "passwd.pw_gecos"),
            s7_make_typed_function(sc, "passwd.pw_gecos", g_passwd_pw_gecos, 1, 0, false, "passwd.pw_gecos", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "passwd.pw_gid"),
            s7_make_typed_function(sc, "passwd.pw_gid", g_passwd_pw_gid, 1, 0, false, "passwd.pw_gid", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "passwd.pw_uid"),
            s7_make_typed_function(sc, "passwd.pw_uid", g_passwd_pw_uid, 1, 0, false, "passwd.pw_uid", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "passwd.pw_passwd"),
            s7_make_typed_function(sc, "passwd.pw_passwd", g_passwd_pw_passwd, 1, 0, false, "passwd.pw_passwd", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "passwd.pw_name"),
            s7_make_typed_function(sc, "passwd.pw_name", g_passwd_pw_name, 1, 0, false, "passwd.pw_name", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getpwnam"),
            s7_make_typed_function(sc, "getpwnam", s7__getpwnam, 1, 0, false, "passwd* getpwnam(char*)", pl_xs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getpwuid"),
            s7_make_typed_function(sc, "getpwuid", s7__getpwuid, 1, 0, false, "passwd* getpwuid(int)", pl_xi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getpwent"),
            s7_make_typed_function(sc, "getpwent", s7__getpwent, 0, 0, false, "passwd* getpwent(void)", pl_xt));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "endpwent"),
            s7_make_typed_function(sc, "endpwent", s7__endpwent, 0, 0, false, "void endpwent(void)", pcl_t));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setpwent"),
            s7_make_typed_function(sc, "setpwent", s7__setpwent, 0, 0, false, "void setpwent(void)", pcl_t));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "group.gr_mem"),
            s7_make_typed_function(sc, "group.gr_mem", g_group_gr_mem, 1, 0, false, "group.gr_mem", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "group.gr_gid"),
            s7_make_typed_function(sc, "group.gr_gid", g_group_gr_gid, 1, 0, false, "group.gr_gid", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "group.gr_passwd"),
            s7_make_typed_function(sc, "group.gr_passwd", g_group_gr_passwd, 1, 0, false, "group.gr_passwd", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "group.gr_name"),
            s7_make_typed_function(sc, "group.gr_name", g_group_gr_name, 1, 0, false, "group.gr_name", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getgrnam"),
            s7_make_typed_function(sc, "getgrnam", g_getgrnam, 1, 0, false, "getgrnam", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getgrgid"),
            s7_make_typed_function(sc, "getgrgid", g_getgrgid, 1, 0, false, "getgrgid", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "termios.set_c_cc"),
            s7_make_typed_function(sc, "termios.set_c_cc", g_termios_set_c_cc, 3, 0, false, "termios.set_c_cc", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "termios.set_c_lflag"),
            s7_make_typed_function(sc, "termios.set_c_lflag", g_termios_set_c_lflag, 2, 0, false, "termios.set_c_lflag", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "termios.c_lflag"),
            s7_make_typed_function(sc, "termios.c_lflag", g_termios_c_lflag, 1, 0, false, "termios.c_lflag", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "termios.make"),
            s7_make_typed_function(sc, "termios.make", g_termios_make, 0, 0, false, "termios.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tcsetattr"),
            s7_make_typed_function(sc, "tcsetattr", g_tcsetattr, 3, 0, false, "tcsetattr", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tcgetattr"),
            s7_make_typed_function(sc, "tcgetattr", g_tcgetattr, 2, 0, false, "tcgetattr", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "cfsetispeed"),
            s7_make_typed_function(sc, "cfsetispeed", g_cfsetispeed, 2, 0, false, "cfsetispeed", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "cfsetospeed"),
            s7_make_typed_function(sc, "cfsetospeed", g_cfsetospeed, 2, 0, false, "cfsetospeed", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "cfgetispeed"),
            s7_make_typed_function(sc, "cfgetispeed", g_cfgetispeed, 1, 0, false, "cfgetispeed", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "cfgetospeed"),
            s7_make_typed_function(sc, "cfgetospeed", g_cfgetospeed, 1, 0, false, "cfgetospeed", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tcflow"),
            s7_make_typed_function(sc, "tcflow", s7__tcflow, 2, 0, false, "int tcflow(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tcflush"),
            s7_make_typed_function(sc, "tcflush", s7__tcflush, 2, 0, false, "int tcflush(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tcdrain"),
            s7_make_typed_function(sc, "tcdrain", s7__tcdrain, 1, 0, false, "int tcdrain(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tcsendbreak"),
            s7_make_typed_function(sc, "tcsendbreak", s7__tcsendbreak, 2, 0, false, "int tcsendbreak(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "utime"),
            s7_make_typed_function(sc, "utime", g_utime, 3, 0, false, "utime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "clock_nanosleep"),
            s7_make_typed_function(sc, "clock_nanosleep", g_clock_nanosleep, 4, 0, false, "clock_nanosleep", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "clock_getcpuclockid"),
            s7_make_typed_function(sc, "clock_getcpuclockid", g_clock_getcpuclockid, 1, 0, false, "clock_getcpuclockid", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "clock_settime"),
            s7_make_typed_function(sc, "clock_settime", g_clock_settime, 3, 0, false, "clock_settime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "clock_gettime"),
            s7_make_typed_function(sc, "clock_gettime", g_clock_gettime, 1, 0, false, "clock_gettime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "clock_getres"),
            s7_make_typed_function(sc, "clock_getres", g_clock_getres, 1, 0, false, "clock_getres", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "nanosleep"),
            s7_make_typed_function(sc, "nanosleep", g_nanosleep, 2, 0, false, "nanosleep", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gettimeofday"),
            s7_make_typed_function(sc, "gettimeofday", g_gettimeofday, 0, 0, false, "gettimeofday", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strftime"),
            s7_make_typed_function(sc, "strftime", g_strftime, 4, 0, false, "strftime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "asctime"),
            s7_make_typed_function(sc, "asctime", g_asctime, 1, 0, false, "asctime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "mktime"),
            s7_make_typed_function(sc, "mktime", g_mktime, 1, 0, false, "mktime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "time.make"),
            s7_make_typed_function(sc, "time.make", g_time_make, 1, 0, false, "time.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "localtime"),
            s7_make_typed_function(sc, "localtime", s7__localtime, 1, 0, false, "tm* localtime(time_t*)", pcl_x));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ctime"),
            s7_make_typed_function(sc, "ctime", s7__ctime, 1, 0, false, "char* ctime(time_t*)", pl_sx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gmtime"),
            s7_make_typed_function(sc, "gmtime", s7__gmtime, 1, 0, false, "tm* gmtime(time_t*)", pcl_x));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "difftime"),
            s7_make_typed_function(sc, "difftime", s7__difftime, 2, 0, false, "double difftime((time_t integer) (time_t integer))", pcl_di));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "time"),
            s7_make_typed_function(sc, "time", s7__time, 1, 0, false, "int time(time_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "clock"),
            s7_make_typed_function(sc, "clock", s7__clock, 0, 0, false, "int clock(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.make"),
            s7_make_typed_function(sc, "stat.make", g_stat_make, 0, 0, false, "stat.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_ctime"),
            s7_make_typed_function(sc, "stat.st_ctime", g_st_ctime, 1, 0, false, "stat.st_ctime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_mtime"),
            s7_make_typed_function(sc, "stat.st_mtime", g_st_mtime, 1, 0, false, "stat.st_mtime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_atime"),
            s7_make_typed_function(sc, "stat.st_atime", g_st_atime, 1, 0, false, "stat.st_atime", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_blocks"),
            s7_make_typed_function(sc, "stat.st_blocks", g_st_blocks, 1, 0, false, "stat.st_blocks", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_blksize"),
            s7_make_typed_function(sc, "stat.st_blksize", g_st_blksize, 1, 0, false, "stat.st_blksize", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_size"),
            s7_make_typed_function(sc, "stat.st_size", g_st_size, 1, 0, false, "stat.st_size", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_rdev"),
            s7_make_typed_function(sc, "stat.st_rdev", g_st_rdev, 1, 0, false, "stat.st_rdev", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_gid"),
            s7_make_typed_function(sc, "stat.st_gid", g_st_gid, 1, 0, false, "stat.st_gid", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_uid"),
            s7_make_typed_function(sc, "stat.st_uid", g_st_uid, 1, 0, false, "stat.st_uid", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_nlink"),
            s7_make_typed_function(sc, "stat.st_nlink", g_st_nlink, 1, 0, false, "stat.st_nlink", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_mode"),
            s7_make_typed_function(sc, "stat.st_mode", g_st_mode, 1, 0, false, "stat.st_mode", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_ino"),
            s7_make_typed_function(sc, "stat.st_ino", g_st_ino, 1, 0, false, "stat.st_ino", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat.st_dev"),
            s7_make_typed_function(sc, "stat.st_dev", g_st_dev, 1, 0, false, "stat.st_dev", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "S_ISSOCK"),
            s7_make_typed_function(sc, "S_ISSOCK", g_issock, 1, 0, false, "S_ISSOCK", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "S_ISLNK"),
            s7_make_typed_function(sc, "S_ISLNK", g_islnk, 1, 0, false, "S_ISLNK", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "S_ISFIFO"),
            s7_make_typed_function(sc, "S_ISFIFO", g_isfifo, 1, 0, false, "S_ISFIFO", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "S_ISREG"),
            s7_make_typed_function(sc, "S_ISREG", g_isreg, 1, 0, false, "S_ISREG", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "S_ISBLK"),
            s7_make_typed_function(sc, "S_ISBLK", g_isblk, 1, 0, false, "S_ISBLK", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "S_ISCHR"),
            s7_make_typed_function(sc, "S_ISCHR", g_ischr, 1, 0, false, "S_ISCHR", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "S_ISDIR"),
            s7_make_typed_function(sc, "S_ISDIR", g_isdir, 1, 0, false, "S_ISDIR", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "mkfifo"),
            s7_make_typed_function(sc, "mkfifo", s7__mkfifo, 2, 0, false, "int mkfifo(char* int)", pl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "mknod"),
            s7_make_typed_function(sc, "mknod", s7__mknod, 3, 0, false, "int mknod(char* int int)", pcl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "mkdir"),
            s7_make_typed_function(sc, "mkdir", s7__mkdir, 2, 0, false, "int mkdir(char* int)", pl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "chmod"),
            s7_make_typed_function(sc, "chmod", s7__chmod, 2, 0, false, "int chmod(char* int)", pl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "lstat"),
            s7_make_typed_function(sc, "lstat", g_lstat, 2, 0, false, "lstat", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fstat"),
            s7_make_typed_function(sc, "fstat", g_fstat, 2, 0, false, "fstat", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "stat"),
            s7_make_typed_function(sc, "stat", g_stat, 2, 0, false, "stat", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ftw"),
            s7_make_typed_function(sc, "ftw", g_ftw, 3, 0, false, "ftw", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "read_dir"),
            s7_make_typed_function(sc, "read_dir", s7__read_dir, 1, 0, false, "char* read_dir(DIR*)", pl_sx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rewinddir"),
            s7_make_typed_function(sc, "rewinddir", s7__rewinddir, 1, 0, false, "void rewinddir(DIR*)", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "closedir"),
            s7_make_typed_function(sc, "closedir", s7__closedir, 1, 0, false, "int closedir(DIR*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "opendir"),
            s7_make_typed_function(sc, "opendir", s7__opendir, 1, 0, false, "DIR* opendir(char*)", pl_xs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getgroups"),
            s7_make_typed_function(sc, "getgroups", g_getgroups, 1, 0, false, "getgroups", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getenvs"),
            s7_make_typed_function(sc, "getenvs", getenvs, 0, 0, false, "(getenvs) returns all the environment variables in an alist", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ftruncate"),
            s7_make_typed_function(sc, "ftruncate", s7__ftruncate, 2, 0, false, "int ftruncate(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "truncate"),
            s7_make_typed_function(sc, "truncate", s7__truncate, 2, 0, false, "int truncate(char* int)", pl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getlogin"),
            s7_make_typed_function(sc, "getlogin", s7__getlogin, 0, 0, false, "char* getlogin(void)", pl_st));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tcsetpgrp"),
            s7_make_typed_function(sc, "tcsetpgrp", s7__tcsetpgrp, 2, 0, false, "int tcsetpgrp(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tcgetpgrp"),
            s7_make_typed_function(sc, "tcgetpgrp", s7__tcgetpgrp, 1, 0, false, "int tcgetpgrp(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rmdir"),
            s7_make_typed_function(sc, "rmdir", s7__rmdir, 1, 0, false, "int rmdir(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "unlink"),
            s7_make_typed_function(sc, "unlink", s7__unlink, 1, 0, false, "int unlink(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "link"),
            s7_make_typed_function(sc, "link", s7__link, 2, 0, false, "int link(char* char*)", pcl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isatty"),
            s7_make_typed_function(sc, "isatty", s7__isatty, 1, 0, false, "int isatty(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ttyname"),
            s7_make_typed_function(sc, "ttyname", s7__ttyname, 1, 0, false, "char* ttyname(int)", pl_si));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fork"),
            s7_make_typed_function(sc, "fork", s7__fork, 0, 0, false, "int fork(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setgid"),
            s7_make_typed_function(sc, "setgid", s7__setgid, 1, 0, false, "int setgid(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setuid"),
            s7_make_typed_function(sc, "setuid", s7__setuid, 1, 0, false, "int setuid(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getegid"),
            s7_make_typed_function(sc, "getegid", s7__getegid, 0, 0, false, "int getegid(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getgid"),
            s7_make_typed_function(sc, "getgid", s7__getgid, 0, 0, false, "int getgid(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "geteuid"),
            s7_make_typed_function(sc, "geteuid", s7__geteuid, 0, 0, false, "int geteuid(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getuid"),
            s7_make_typed_function(sc, "getuid", s7__getuid, 0, 0, false, "int getuid(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getsid"),
            s7_make_typed_function(sc, "getsid", s7__getsid, 1, 0, false, "int getsid(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setsid"),
            s7_make_typed_function(sc, "setsid", s7__setsid, 0, 0, false, "int setsid(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setpgid"),
            s7_make_typed_function(sc, "setpgid", s7__setpgid, 2, 0, false, "int setpgid(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getpgid"),
            s7_make_typed_function(sc, "getpgid", s7__getpgid, 1, 0, false, "int getpgid(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getppid"),
            s7_make_typed_function(sc, "getppid", s7__getppid, 0, 0, false, "int getppid(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getpid"),
            s7_make_typed_function(sc, "getpid", s7__getpid, 0, 0, false, "int getpid(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "confstr"),
            s7_make_typed_function(sc, "confstr", s7__confstr, 3, 0, false, "size_t confstr(int char* size_t)", pl_iisi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sysconf"),
            s7_make_typed_function(sc, "sysconf", s7__sysconf, 1, 0, false, "int sysconf(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fpathconf"),
            s7_make_typed_function(sc, "fpathconf", s7__fpathconf, 2, 0, false, "int fpathconf(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "pathconf"),
            s7_make_typed_function(sc, "pathconf", s7__pathconf, 2, 0, false, "int pathconf(char* int)", pl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "_exit"),
            s7_make_typed_function(sc, "_exit", s7___exit, 1, 0, false, "void _exit(int)", pl_ti));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "dup2"),
            s7_make_typed_function(sc, "dup2", s7__dup2, 2, 0, false, "int dup2(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "dup"),
            s7_make_typed_function(sc, "dup", s7__dup, 1, 0, false, "int dup(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getcwd"),
            s7_make_typed_function(sc, "getcwd", s7__getcwd, 2, 0, false, "char* getcwd(char* size_t)", pl_ssi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "chdir"),
            s7_make_typed_function(sc, "chdir", s7__chdir, 1, 0, false, "int chdir(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "chown"),
            s7_make_typed_function(sc, "chown", s7__chown, 3, 0, false, "int chown(char* int int)", pcl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "pause"),
            s7_make_typed_function(sc, "pause", s7__pause, 0, 0, false, "int pause(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "sleep"),
            s7_make_typed_function(sc, "sleep", s7__sleep, 1, 0, false, "int sleep(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "alarm"),
            s7_make_typed_function(sc, "alarm", s7__alarm, 1, 0, false, "int alarm(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "pipe"),
            s7_make_typed_function(sc, "pipe", s7__pipe, 1, 0, false, "int pipe(int*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "pwrite"),
            s7_make_typed_function(sc, "pwrite", s7__pwrite, 4, 0, false, "ssize_t pwrite(int void* size_t int)", pcl_iixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "pread"),
            s7_make_typed_function(sc, "pread", s7__pread, 4, 0, false, "ssize_t pread(int void* size_t int)", pcl_iixi));

  /* s7_define(sc, cur_env, */
  /*           s7_make_symbol(sc, "write"), */
  /*           s7_make_typed_function(sc, "write", s7__write, 3, 0, false, "ssize_t write(int void* size_t)", pl_iixi)); */

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "read"),
            s7_make_typed_function(sc, "read", s7__read, 3, 0, false, "ssize_t read(int void* size_t)", pl_iixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "close"),
            s7_make_typed_function(sc, "close", s7__close, 1, 0, false, "int close(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "lseek"),
            s7_make_typed_function(sc, "lseek", s7__lseek, 3, 0, false, "int lseek(int int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "access"),
            s7_make_typed_function(sc, "access", s7__access, 2, 0, false, "int access(char* int)", pl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "uname"),
            s7_make_typed_function(sc, "uname", g_uname, 0, 0, false, "uname", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "localeconv"),
            s7_make_typed_function(sc, "localeconv", g_localeconv, 0, 0, false, "localeconv", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setlocale"),
            s7_make_typed_function(sc, "setlocale", s7__setlocale, 2, 0, false, "char* setlocale(int char*)", pl_sis));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "set_errno"),
            s7_make_typed_function(sc, "set_errno", g_set_errno, 1, 0, false, "set_errno", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "errno"),
            s7_make_typed_function(sc, "errno", g_errno, 0, 0, false, "errno", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "realpath"),
            s7_make_typed_function(sc, "realpath", g_realpath, 2, 0, false, "realpath", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ldiv"),
            s7_make_typed_function(sc, "ldiv", g_ldiv, 2, 0, false, "ldiv", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "div"),
            s7_make_typed_function(sc, "div", g_ldiv, 2, 0, false, "div", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strtoll"),
            s7_make_typed_function(sc, "strtoll", g_strtoll, 2, 0, false, "strtoll", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strtol"),
            s7_make_typed_function(sc, "strtol", g_strtol, 2, 0, false, "strtol", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strtof"),
            s7_make_typed_function(sc, "strtof", g_strtof, 1, 0, false, "strtof", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strtod"),
            s7_make_typed_function(sc, "strtod", g_strtod, 1, 0, false, "strtod", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "free"),
            s7_make_typed_function(sc, "free", g_free, 1, 0, false, "free", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "llabs"),
            s7_make_typed_function(sc, "llabs", g_llabs, 1, 0, false, "llabs", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "labs"),
            s7_make_typed_function(sc, "labs", s7__labs, 1, 0, false, "int labs(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "abs"),
            s7_make_typed_function(sc, "abs", s7__abs, 1, 0, false, "int abs(int)", pcl_i));

  /* obazl avoid clash with g_system defined in s7.c (WITH_SYSTEM_EXTRAS) */
  /* s7_define(sc, cur_env, */
  /*           s7_make_symbol(sc, "system"), */
  /*           s7_make_typed_function(sc, "system", s7__system, 1, 0, false, "int system(char*)", pl_is)); */

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "mkstemp"),
            s7_make_typed_function(sc, "mkstemp", s7__mkstemp, 1, 0, false, "int mkstemp(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "unsetenv"),
            s7_make_typed_function(sc, "unsetenv", s7__unsetenv, 1, 0, false, "int unsetenv(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setenv"),
            s7_make_typed_function(sc, "setenv", s7__setenv, 3, 0, false, "int setenv(char* char* int)", pl_issi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "putenv"),
            s7_make_typed_function(sc, "putenv", s7__putenv, 1, 0, false, "int putenv(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getenv"),
            s7_make_typed_function(sc, "getenv", s7__getenv, 1, 0, false, "char* getenv(char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "exit"),
            s7_make_typed_function(sc, "exit", s7__exit, 1, 0, false, "void exit(int)", pl_ti));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "abort"),
            s7_make_typed_function(sc, "abort", s7__abort, 0, 0, false, "void abort(void)", pcl_t));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "realloc"),
            s7_make_typed_function(sc, "realloc", s7__realloc, 2, 0, false, "void* realloc(void* size_t)", pl_xxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "calloc"),
            s7_make_typed_function(sc, "calloc", s7__calloc, 2, 0, false, "void* calloc(size_t size_t)", pcl_xi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "malloc"),
            s7_make_typed_function(sc, "malloc", s7__malloc, 1, 0, false, "void* malloc(size_t)", pl_xi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "srand"),
            s7_make_typed_function(sc, "srand", s7__srand, 1, 0, false, "void srand(int)", pl_ti));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rand"),
            s7_make_typed_function(sc, "rand", s7__rand, 0, 0, false, "int rand(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setstate"),
            s7_make_typed_function(sc, "setstate", s7__setstate, 1, 0, false, "char* setstate(char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "initstate"),
            s7_make_typed_function(sc, "initstate", s7__initstate, 3, 0, false, "char* initstate(int char* size_t)", pl_sisi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "srandom"),
            s7_make_typed_function(sc, "srandom", s7__srandom, 1, 0, false, "void srandom(int)", pl_ti));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "random"),
            s7_make_typed_function(sc, "random", s7__random, 0, 0, false, "int random(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "atoll"),
            s7_make_typed_function(sc, "atoll", s7__atoll, 1, 0, false, "int atoll(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "atol"),
            s7_make_typed_function(sc, "atol", s7__atol, 1, 0, false, "int atol(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "atoi"),
            s7_make_typed_function(sc, "atoi", s7__atoi, 1, 0, false, "int atoi(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "atof"),
            s7_make_typed_function(sc, "atof", s7__atof, 1, 0, false, "double atof(char*)", pl_ds));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "funlockfile"),
            s7_make_typed_function(sc, "funlockfile", s7__funlockfile, 1, 0, false, "void funlockfile(FILE*)", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ftrylockfile"),
            s7_make_typed_function(sc, "ftrylockfile", s7__ftrylockfile, 1, 0, false, "int ftrylockfile(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "flockfile"),
            s7_make_typed_function(sc, "flockfile", s7__flockfile, 1, 0, false, "void flockfile(FILE*)", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ctermid"),
            s7_make_typed_function(sc, "ctermid", s7__ctermid, 1, 0, false, "char* ctermid(char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "pclose"),
            s7_make_typed_function(sc, "pclose", s7__pclose, 1, 0, false, "int pclose(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "popen"),
            s7_make_typed_function(sc, "popen", s7__popen, 2, 0, false, "FILE* popen(char* char*)", pcl_xs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fileno"),
            s7_make_typed_function(sc, "fileno", s7__fileno, 1, 0, false, "int fileno(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "perror"),
            s7_make_typed_function(sc, "perror", s7__perror, 1, 0, false, "void perror(char*)", pl_ts));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ferror"),
            s7_make_typed_function(sc, "ferror", s7__ferror, 1, 0, false, "int ferror(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "feof"),
            s7_make_typed_function(sc, "feof", s7__feof, 1, 0, false, "int feof(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "clearerr"),
            s7_make_typed_function(sc, "clearerr", s7__clearerr, 1, 0, false, "void clearerr(FILE*)", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fsetpos"),
            s7_make_typed_function(sc, "fsetpos", s7__fsetpos, 2, 0, false, "int fsetpos(FILE* fpos_t*)", pcl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fgetpos"),
            s7_make_typed_function(sc, "fgetpos", s7__fgetpos, 2, 0, false, "int fgetpos(FILE* fpos_t*)", pcl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rewind"),
            s7_make_typed_function(sc, "rewind", s7__rewind, 1, 0, false, "void rewind(FILE*)", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ftell"),
            s7_make_typed_function(sc, "ftell", s7__ftell, 1, 0, false, "int ftell(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fseek"),
            s7_make_typed_function(sc, "fseek", s7__fseek, 3, 0, false, "int fseek(FILE* int int)", pcl_ixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fwrite"),
            s7_make_typed_function(sc, "fwrite", s7__fwrite, 4, 0, false, "size_t fwrite(void* size_t size_t FILE*)", pl_ixiix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fread"),
            s7_make_typed_function(sc, "fread", s7__fread, 4, 0, false, "size_t fread(void* size_t size_t FILE*)", pl_ixiix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ungetc"),
            s7_make_typed_function(sc, "ungetc", s7__ungetc, 2, 0, false, "int ungetc(int FILE*)", pl_iix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "puts"),
            s7_make_typed_function(sc, "puts", s7__puts, 1, 0, false, "int puts(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fputs"),
            s7_make_typed_function(sc, "fputs", s7__fputs, 2, 0, false, "int fputs(char* FILE*)", pl_isx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fgets"),
            s7_make_typed_function(sc, "fgets", s7__fgets, 3, 0, false, "char* fgets(char* int FILE*)", pl_ssix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "putchar"),
            s7_make_typed_function(sc, "putchar", s7__putchar, 1, 0, false, "int putchar(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "putc"),
            s7_make_typed_function(sc, "putc", s7__putc, 2, 0, false, "int putc(int FILE*)", pl_iix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fputc"),
            s7_make_typed_function(sc, "fputc", s7__fputc, 2, 0, false, "int fputc(int FILE*)", pl_iix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getchar"),
            s7_make_typed_function(sc, "getchar", s7__getchar, 0, 0, false, "int getchar(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "getc"),
            s7_make_typed_function(sc, "getc", s7__getc, 1, 0, false, "int getc(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fgetc"),
            s7_make_typed_function(sc, "fgetc", s7__fgetc, 1, 0, false, "int fgetc(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setlinebuf"),
            s7_make_typed_function(sc, "setlinebuf", s7__setlinebuf, 1, 0, false, "void setlinebuf(FILE*)", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setvbuf"),
            s7_make_typed_function(sc, "setvbuf", s7__setvbuf, 4, 0, false, "int setvbuf(FILE* char* int size_t)", pcl_ixsi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "setbuf"),
            s7_make_typed_function(sc, "setbuf", s7__setbuf, 2, 0, false, "void setbuf(FILE* char*)", pl_txs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fdopen"),
            s7_make_typed_function(sc, "fdopen", s7__fdopen, 2, 0, false, "FILE* fdopen(int char*)", pl_xis));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "freopen"),
            s7_make_typed_function(sc, "freopen", s7__freopen, 3, 0, false, "FILE* freopen(char* char* FILE*)", pl_xssx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fopen"),
            s7_make_typed_function(sc, "fopen", s7__fopen, 2, 0, false, "FILE* fopen(char* char*)", pcl_xs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fflush"),
            s7_make_typed_function(sc, "fflush", s7__fflush, 1, 0, false, "int fflush(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fclose"),
            s7_make_typed_function(sc, "fclose", s7__fclose, 1, 0, false, "int fclose(FILE*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tmpfile"),
            s7_make_typed_function(sc, "tmpfile", s7__tmpfile, 0, 0, false, "FILE* tmpfile(void)", pl_xt));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "rename"),
            s7_make_typed_function(sc, "rename", s7__rename, 2, 0, false, "int rename(char* char*)", pcl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "remove"),
            s7_make_typed_function(sc, "remove", s7__remove, 1, 0, false, "int remove(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strncasecmp"),
            s7_make_typed_function(sc, "strncasecmp", s7__strncasecmp, 3, 0, false, "int strncasecmp(char* char* size_t)", pl_issi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strcasecmp"),
            s7_make_typed_function(sc, "strcasecmp", s7__strcasecmp, 2, 0, false, "int strcasecmp(char* char*)", pcl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strerror"),
            s7_make_typed_function(sc, "strerror", s7__strerror, 1, 0, false, "char* strerror(int)", pl_si));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strlen"),
            s7_make_typed_function(sc, "strlen", s7__strlen, 1, 0, false, "size_t strlen(char*)", pl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strtok"),
            s7_make_typed_function(sc, "strtok", s7__strtok, 2, 0, false, "char* strtok(char* char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strstr"),
            s7_make_typed_function(sc, "strstr", s7__strstr, 2, 0, false, "char* strstr(char* char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strpbrk"),
            s7_make_typed_function(sc, "strpbrk", s7__strpbrk, 2, 0, false, "char* strpbrk(char* char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strspn"),
            s7_make_typed_function(sc, "strspn", s7__strspn, 2, 0, false, "size_t strspn(char* char*)", pcl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strcspn"),
            s7_make_typed_function(sc, "strcspn", s7__strcspn, 2, 0, false, "size_t strcspn(char* char*)", pcl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strrchr"),
            s7_make_typed_function(sc, "strrchr", s7__strrchr, 2, 0, false, "char* strrchr(char* int)", pl_ssi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strchr"),
            s7_make_typed_function(sc, "strchr", s7__strchr, 2, 0, false, "char* strchr(char* int)", pl_ssi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strxfrm"),
            s7_make_typed_function(sc, "strxfrm", s7__strxfrm, 3, 0, false, "size_t strxfrm(char* char* size_t)", pl_issi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strcoll"),
            s7_make_typed_function(sc, "strcoll", s7__strcoll, 2, 0, false, "int strcoll(char* char*)", pcl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strncmp"),
            s7_make_typed_function(sc, "strncmp", s7__strncmp, 3, 0, false, "int strncmp(char* char* size_t)", pl_issi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strcmp"),
            s7_make_typed_function(sc, "strcmp", s7__strcmp, 2, 0, false, "int strcmp(char* char*)", pcl_is));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strncat"),
            s7_make_typed_function(sc, "strncat", s7__strncat, 3, 0, false, "char* strncat(char* char* size_t)", pl_sssi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strcat"),
            s7_make_typed_function(sc, "strcat", s7__strcat, 2, 0, false, "char* strcat(char* char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strncpy"),
            s7_make_typed_function(sc, "strncpy", s7__strncpy, 3, 0, false, "char* strncpy(char* char* size_t)", pl_sssi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "strcpy"),
            s7_make_typed_function(sc, "strcpy", s7__strcpy, 2, 0, false, "char* strcpy(char* char*)", pcl_s));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "memchr"),
            s7_make_typed_function(sc, "memchr", s7__memchr, 3, 0, false, "void* memchr(void* int size_t)", pcl_xxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "memcmp"),
            s7_make_typed_function(sc, "memcmp", s7__memcmp, 3, 0, false, "int memcmp(void* void* size_t)", pl_ixxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "memset"),
            s7_make_typed_function(sc, "memset", s7__memset, 3, 0, false, "void* memset(void* int size_t)", pcl_xxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "memmove"),
            s7_make_typed_function(sc, "memmove", s7__memmove, 3, 0, false, "void* memmove(void* void* size_t)", pl_xxxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "memcpy"),
            s7_make_typed_function(sc, "memcpy", s7__memcpy, 3, 0, false, "void* memcpy(void* void* size_t)", pl_xxxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fnmatch"),
            s7_make_typed_function(sc, "fnmatch", s7__fnmatch, 3, 0, false, "int fnmatch(char* char* int)", pl_issi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fenv_t.make"),
            s7_make_typed_function(sc, "fenv_t.make", g_fenv_t_make, 0, 0, false, "fenv_t.make", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "feupdateenv"),
            s7_make_typed_function(sc, "feupdateenv", s7__feupdateenv, 1, 0, false, "int feupdateenv(fenv_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fesetenv"),
            s7_make_typed_function(sc, "fesetenv", s7__fesetenv, 1, 0, false, "int fesetenv(fenv_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "feholdexcept"),
            s7_make_typed_function(sc, "feholdexcept", s7__feholdexcept, 1, 0, false, "int feholdexcept(fenv_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fegetenv"),
            s7_make_typed_function(sc, "fegetenv", s7__fegetenv, 1, 0, false, "int fegetenv(fenv_t*)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fesetround"),
            s7_make_typed_function(sc, "fesetround", s7__fesetround, 1, 0, false, "int fesetround(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fegetround"),
            s7_make_typed_function(sc, "fegetround", s7__fegetround, 0, 0, false, "int fegetround(void)", pl_it));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fetestexcept"),
            s7_make_typed_function(sc, "fetestexcept", s7__fetestexcept, 1, 0, false, "int fetestexcept(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fesetexceptflag"),
            s7_make_typed_function(sc, "fesetexceptflag", s7__fesetexceptflag, 2, 0, false, "int fesetexceptflag(fexcept_t* int)", pl_ixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "feraiseexcept"),
            s7_make_typed_function(sc, "feraiseexcept", s7__feraiseexcept, 1, 0, false, "int feraiseexcept(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fegetexceptflag"),
            s7_make_typed_function(sc, "fegetexceptflag", s7__fegetexceptflag, 2, 0, false, "int fegetexceptflag(fexcept_t* int)", pl_ixi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "feclearexcept"),
            s7_make_typed_function(sc, "feclearexcept", s7__feclearexcept, 1, 0, false, "int feclearexcept(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "lockf"),
            s7_make_typed_function(sc, "lockf", s7__lockf, 3, 0, false, "int lockf(int int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "creat"),
            s7_make_typed_function(sc, "creat", s7__creat, 2, 0, false, "int creat(char* (mode_t int))", pl_isi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "open"),
            s7_make_typed_function(sc, "open", g_c_open, 2, 1, false, "open", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "fcntl"),
            s7_make_typed_function(sc, "fcntl", s7__fcntl, 2, 0, false, "int fcntl(int int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toupper"),
            s7_make_typed_function(sc, "toupper", s7__toupper, 1, 0, false, "int toupper(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "tolower"),
            s7_make_typed_function(sc, "tolower", s7__tolower, 1, 0, false, "int tolower(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isxdigit"),
            s7_make_typed_function(sc, "isxdigit", s7__isxdigit, 1, 0, false, "int isxdigit(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isupper"),
            s7_make_typed_function(sc, "isupper", s7__isupper, 1, 0, false, "int isupper(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isspace"),
            s7_make_typed_function(sc, "isspace", s7__isspace, 1, 0, false, "int isspace(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "ispunct"),
            s7_make_typed_function(sc, "ispunct", s7__ispunct, 1, 0, false, "int ispunct(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isprint"),
            s7_make_typed_function(sc, "isprint", s7__isprint, 1, 0, false, "int isprint(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isgraph"),
            s7_make_typed_function(sc, "isgraph", s7__isgraph, 1, 0, false, "int isgraph(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "islower"),
            s7_make_typed_function(sc, "islower", s7__islower, 1, 0, false, "int islower(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isdigit"),
            s7_make_typed_function(sc, "isdigit", s7__isdigit, 1, 0, false, "int isdigit(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "iscntrl"),
            s7_make_typed_function(sc, "iscntrl", s7__iscntrl, 1, 0, false, "int iscntrl(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isalpha"),
            s7_make_typed_function(sc, "isalpha", s7__isalpha, 1, 0, false, "int isalpha(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "isalnum"),
            s7_make_typed_function(sc, "isalnum", s7__isalnum, 1, 0, false, "int isalnum(int)", pcl_i));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "string->c-pointer"),
            s7_make_typed_function(sc, "string->c-pointer", g_string_to_c_pointer, 1, 0, false, "string->c-pointer", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "c-pointer->string"),
            s7_make_typed_function(sc, "c-pointer->string", g_c_pointer_to_string, 2, 0, false, "c-pointer->string", NULL));

  /* int optimizer connections */
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "shutdown"), shutdown_i_ii);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "listen"), listen_i_ii);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "getpriority"), getpriority_i_ii);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "raise"), raise_i_i);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "kill"), kill_i_ii);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "tcflow"), tcflow_i_ii);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "tcflush"), tcflush_i_ii);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "tcdrain"), tcdrain_i_i);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "tcsendbreak"), tcsendbreak_i_ii);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "ftruncate"), ftruncate_i_ii);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "tcsetpgrp"), tcsetpgrp_i_ii);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "tcgetpgrp"), tcgetpgrp_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "isatty"), isatty_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "setgid"), setgid_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "setuid"), setuid_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "getsid"), getsid_i_i);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "setpgid"), setpgid_i_ii);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "getpgid"), getpgid_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "sysconf"), sysconf_i_i);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "fpathconf"), fpathconf_i_ii);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "dup2"), dup2_i_ii);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "dup"), dup_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "sleep"), sleep_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "alarm"), alarm_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "close"), close_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "labs"), labs_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "putchar"), putchar_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "fesetround"), fesetround_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "fetestexcept"), fetestexcept_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "feraiseexcept"), feraiseexcept_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "feclearexcept"), feclearexcept_i_i);
  s7_set_i_ii_function(sc, s7_name_to_value(sc, "fcntl"), fcntl_i_ii);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "toupper"), toupper_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "tolower"), tolower_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "isxdigit"), isxdigit_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "isupper"), isupper_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "isspace"), isspace_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "ispunct"), ispunct_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "isprint"), isprint_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "isgraph"), isgraph_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "islower"), islower_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "isdigit"), isdigit_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "iscntrl"), iscntrl_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "isalpha"), isalpha_i_i);
  s7_set_i_i_function(sc, s7_name_to_value(sc, "isalnum"), isalnum_i_i);
}
