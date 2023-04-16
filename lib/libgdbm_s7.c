#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gdbm.h>
#include "s7.h"

static s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string, integer_string;
static s7_pointer GDBM_FILE_symbol;

static s7_pointer g_gdbm_version(s7_scheme *sc, s7_pointer args) {return(s7_make_string(sc, gdbm_version));}
static s7_pointer g_gdbm_errno(s7_scheme *sc, s7_pointer args) {return(s7_make_integer(sc, gdbm_errno));}

/* -------- gdbm_strerror -------- */
static s7_pointer s7__gdbm_strerror(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int s7__gdbm_strerror_0;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    s7__gdbm_strerror_0 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'gdbm_strerror)", 26), 0, arg, integer_string));
  return(s7_make_string(sc, (char*)gdbm_strerror(s7__gdbm_strerror_0)));
}


/* -------- gdbm_fdesc -------- */
static s7_pointer s7__gdbm_fdesc(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  GDBM_FILE s7__gdbm_fdesc_0;
  p = args;
  arg = s7_car(p);
  s7__gdbm_fdesc_0 = (GDBM_FILE)s7_c_pointer_with_type(sc, arg, GDBM_FILE_symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)gdbm_fdesc(s7__gdbm_fdesc_0)));
}


/* -------- gdbm_reorganize -------- */
static s7_pointer s7__gdbm_reorganize(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  GDBM_FILE s7__gdbm_reorganize_0;
  p = args;
  arg = s7_car(p);
  s7__gdbm_reorganize_0 = (GDBM_FILE)s7_c_pointer_with_type(sc, arg, GDBM_FILE_symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)gdbm_reorganize(s7__gdbm_reorganize_0)));
}


/* -------- gdbm_close -------- */
static s7_pointer s7__gdbm_close(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  GDBM_FILE s7__gdbm_close_0;
  p = args;
  arg = s7_car(p);
  s7__gdbm_close_0 = (GDBM_FILE)s7_c_pointer_with_type(sc, arg, GDBM_FILE_symbol, __func__, 0);
  gdbm_close(s7__gdbm_close_0);
  return(s7_unspecified(sc));
}


/* -------- gdbm_sync -------- */
static s7_pointer s7__gdbm_sync(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  GDBM_FILE s7__gdbm_sync_0;
  p = args;
  arg = s7_car(p);
  s7__gdbm_sync_0 = (GDBM_FILE)s7_c_pointer_with_type(sc, arg, GDBM_FILE_symbol, __func__, 0);
  gdbm_sync(s7__gdbm_sync_0);
  return(s7_unspecified(sc));
}


static void *make_datum(datum key) {datum *p; p = (datum *)malloc(sizeof(datum)); p->dptr = key.dptr; p->dsize = key.dsize; return((void *)p);}
static s7_pointer g_gdbm_firstkey(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      datum key;
      key = gdbm_firstkey((GDBM_FILE)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "GDBM_FILE"), __func__, 0));
      if (key.dptr)
         return(s7_cons(sc, s7_make_string_with_length(sc, key.dptr, key.dsize), 
                            s7_make_c_pointer_with_type(sc, make_datum(key), s7_make_symbol(sc, "datum*"), s7_f(sc))));
      return(s7_eof_object(sc));
    }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'firstkey)", 21), 0, s7_car(args), 
                                 s7_make_string_wrapper_with_length(sc, "a gdbm file", 11)));
}

static s7_pointer g_gdbm_nextkey(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_c_pointer(s7_cadr(args)))
        {
	  datum *p;
          datum key, rtn;
          p = (datum *)s7_c_pointer_with_type(sc, s7_cadr(args), s7_make_symbol(sc, "datum*"), __func__, 2);
	  key.dptr = p->dptr;
	  key.dsize = p->dsize;
          rtn = gdbm_nextkey((GDBM_FILE)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "GDBM_FILE"), __func__, 0), key);
          free(key.dptr);
	  free(p);
          if (rtn.dptr)
	     return(s7_cons(sc, s7_make_string_with_length(sc, rtn.dptr, rtn.dsize), 
                                s7_make_c_pointer_with_type(sc, make_datum(rtn), s7_make_symbol(sc, "datum*"), s7_f(sc))));
          return(s7_eof_object(sc));
	}
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'nextkey)", 20), 2, s7_cadr(args), string_string));
    }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'nextkey)", 20), 1, s7_car(args), 
                                 s7_make_string_wrapper_with_length(sc, "a gdbm file", 11)));
}

static s7_pointer g_gdbm_exists(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_string(s7_cadr(args)))
        {
          datum key;
          key.dptr = (char *)s7_string(s7_cadr(args));
          key.dsize = (int)s7_string_length(s7_cadr(args));
          return(s7_make_integer(sc, gdbm_exists((GDBM_FILE)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "GDBM_FILE"), __func__, 0), key)));
	}
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'exists)", 19), 2, s7_cadr(args), string_string));
    }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'exists)", 19), 1, s7_car(args), 
                                 s7_make_string_wrapper_with_length(sc, "a gdbm file", 11)));
}

static s7_pointer g_gdbm_delete(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_string(s7_cadr(args)))
        {
          datum key;
          key.dptr = (char *)s7_string(s7_cadr(args));
          key.dsize = (int)s7_string_length(s7_cadr(args));
          return(s7_make_integer(sc, gdbm_delete((GDBM_FILE)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "GDBM_FILE"), __func__, 0), key)));
        }  
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'delete)", 19), 2, s7_cadr(args), string_string));
    }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'delete)", 19), 1, s7_car(args), 
                                 s7_make_string_wrapper_with_length(sc, "a gdbm file", 11)));
}

static s7_pointer g_gdbm_fetch(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_string(s7_cadr(args)))
        {
          datum key, rtn;
          key.dptr = (char *)s7_string(s7_cadr(args));
          key.dsize = (int)s7_string_length(s7_cadr(args));
          rtn = gdbm_fetch((GDBM_FILE)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "GDBM_FILE"), __func__, 0), key);
          if (rtn.dptr)
            {
  	      s7_pointer result;
              result = s7_make_string_with_length(sc, rtn.dptr, rtn.dsize - 1);
              free(rtn.dptr);
              return(result);
	    }
          else return(s7_make_string_with_length(sc, "#<undefined>", 12));
	}
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'fetch)", 18), 2, s7_cadr(args), string_string));
    }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'fetch)", 18), 1, s7_car(args), 
                                 s7_make_string_wrapper_with_length(sc, "a gdbm file", 11)));
}

static s7_pointer g_gdbm_store(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_c_pointer(s7_car(args)))
    {
      if (s7_is_string(s7_cadr(args)))
        {
          if (s7_is_string(s7_caddr(args)))
            {
              if (s7_is_integer(s7_cadddr(args)))
                {
                  datum key, val;
                  key.dptr = (char *)s7_string(s7_cadr(args));
                  key.dsize = (int)s7_string_length(s7_cadr(args));
                  val.dptr = (char *)s7_string(s7_caddr(args));
                  val.dsize = (int)s7_string_length(s7_caddr(args)) + 1;
                  return(s7_make_integer(sc, gdbm_store((GDBM_FILE)s7_c_pointer_with_type(sc, s7_car(args), s7_make_symbol(sc, "GDBM_FILE"), __func__, 1),
                                                        key, val, (int)s7_integer(s7_cadddr(args)))));
                }
              return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'store)", 18), 4, s7_cadddr(args), 
                                             s7_make_string_wrapper_with_length(sc, "an integer (flag)", 17)));
	    }
          return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'store)", 18), 3, s7_caddr(args), string_string));
	}
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'store)", 18), 2, s7_cadr(args), string_string));
    }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'store)", 18), 1, s7_car(args), 
                                 s7_make_string_wrapper_with_length(sc, "a gdbm file", 11)));
}

static s7_pointer open_error_func = NULL;
static s7_scheme *open_error_s7 = NULL;
static void gdbm_open_error(const char *name)
{
  if (open_error_func)
    s7_apply_function(open_error_s7, open_error_func, s7_list(open_error_s7, 1, s7_make_string(open_error_s7, name)));
}

static s7_pointer g_gdbm_open(s7_scheme *sc, s7_pointer args)
{
  if (s7_is_string(s7_car(args)))
    {
      char *name;
      name = (char *)s7_string(s7_car(args));
      args = s7_cdr(args);
      if (s7_is_integer(s7_car(args)))
        {
	  int block_size;
          block_size = (int)s7_integer(s7_car(args));
          args = s7_cdr(args);
          if (s7_is_integer(s7_car(args)))
            {
	      int flags;
              flags = (int)s7_integer(s7_car(args));
              args = s7_cdr(args);
              if (s7_is_integer(s7_car(args)))
                {
	          int mode;
                  mode = (int)s7_integer(s7_car(args));
		  if (s7_is_procedure(s7_cadr(args)))
		    {
                      open_error_func = s7_cadr(args);
                      open_error_s7 = sc;
                    }
                  else
		    {
                      open_error_func = NULL;
                      open_error_s7 = NULL;
                    }
                  return(s7_make_c_pointer_with_type(sc, (void *)gdbm_open(name, block_size, flags, mode, gdbm_open_error), s7_make_symbol(sc, "GDBM_FILE"), s7_f(sc)));
                }
              return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'open)", 17), 4, s7_car(args), 
                                             s7_make_string_wrapper_with_length(sc, "an integer (mode)", 17)));
            }
          return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'open)", 17), 3, s7_car(args), 
                                         s7_make_string_wrapper_with_length(sc, "an integer (flags)", 18)));
        }
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'open)", 17), 2, s7_car(args), 
                                     s7_make_string_wrapper_with_length(sc, "an integer (block_size)", 23)));
    }
  return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "(*libgdbm* 'open)", 17), 1, s7_car(args), string_string));
}

void libgdbm_s7_init(s7_scheme *sc);
void libgdbm_s7_init(s7_scheme *sc)
{
  s7_pointer cur_env;
  s7_pointer pl_tx, pl_ix, pl_si;
  {
    s7_pointer t, x, s, i;
    t = s7_t(sc);
    x = s7_make_symbol(sc, "c-pointer?");
    s = s7_make_symbol(sc, "string?");
    i = s7_make_symbol(sc, "integer?");

    pl_tx = s7_make_signature(sc, 2, t, x);
    pl_ix = s7_make_signature(sc, 2, i, x);
    pl_si = s7_make_signature(sc, 2, s, i);
  }

  string_string = s7_make_semipermanent_string(sc, "a string");
  c_pointer_string = s7_make_semipermanent_string(sc, "a c-pointer");
  character_string = s7_make_semipermanent_string(sc, "a character");
  boolean_string = s7_make_semipermanent_string(sc, "a boolean");
  real_string = s7_make_semipermanent_string(sc, "a real");
  complex_string = s7_make_semipermanent_string(sc, "a complex number");
  integer_string = s7_make_semipermanent_string(sc, "an integer");
  cur_env = s7_curlet(sc);

  GDBM_FILE_symbol = s7_make_symbol(sc, "GDBM_FILE");

#ifdef GDBM_FILE_EOF
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_FILE_EOF"), s7_make_integer(sc, (s7_int)GDBM_FILE_EOF));
#endif
#ifdef GDBM_FILE_STAT_ERROR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_FILE_STAT_ERROR"), s7_make_integer(sc, (s7_int)GDBM_FILE_STAT_ERROR));
#endif
#ifdef GDBM_BAD_OPEN_FLAGS
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_BAD_OPEN_FLAGS"), s7_make_integer(sc, (s7_int)GDBM_BAD_OPEN_FLAGS));
#endif
#ifdef GDBM_BAD_FILE_OFFSET
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_BAD_FILE_OFFSET"), s7_make_integer(sc, (s7_int)GDBM_BAD_FILE_OFFSET));
#endif
#ifdef GDBM_BYTE_SWAPPED
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_BYTE_SWAPPED"), s7_make_integer(sc, (s7_int)GDBM_BYTE_SWAPPED));
#endif
#ifdef GDBM_OPT_ILLEGAL
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_OPT_ILLEGAL"), s7_make_integer(sc, (s7_int)GDBM_OPT_ILLEGAL));
#endif
#ifdef GDBM_OPT_ALREADY_SET
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_OPT_ALREADY_SET"), s7_make_integer(sc, (s7_int)GDBM_OPT_ALREADY_SET));
#endif
#ifdef GDBM_ILLEGAL_DATA
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_ILLEGAL_DATA"), s7_make_integer(sc, (s7_int)GDBM_ILLEGAL_DATA));
#endif
#ifdef GDBM_CANNOT_REPLACE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_CANNOT_REPLACE"), s7_make_integer(sc, (s7_int)GDBM_CANNOT_REPLACE));
#endif
#ifdef GDBM_REORGANIZE_FAILED
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_REORGANIZE_FAILED"), s7_make_integer(sc, (s7_int)GDBM_REORGANIZE_FAILED));
#endif
#ifdef GDBM_ITEM_NOT_FOUND
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_ITEM_NOT_FOUND"), s7_make_integer(sc, (s7_int)GDBM_ITEM_NOT_FOUND));
#endif
#ifdef GDBM_READER_CANT_REORGANIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_READER_CANT_REORGANIZE"), s7_make_integer(sc, (s7_int)GDBM_READER_CANT_REORGANIZE));
#endif
#ifdef GDBM_READER_CANT_STORE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_READER_CANT_STORE"), s7_make_integer(sc, (s7_int)GDBM_READER_CANT_STORE));
#endif
#ifdef GDBM_READER_CANT_DELETE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_READER_CANT_DELETE"), s7_make_integer(sc, (s7_int)GDBM_READER_CANT_DELETE));
#endif
#ifdef GDBM_CANT_BE_WRITER
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_CANT_BE_WRITER"), s7_make_integer(sc, (s7_int)GDBM_CANT_BE_WRITER));
#endif
#ifdef GDBM_CANT_BE_READER
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_CANT_BE_READER"), s7_make_integer(sc, (s7_int)GDBM_CANT_BE_READER));
#endif
#ifdef GDBM_EMPTY_DATABASE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_EMPTY_DATABASE"), s7_make_integer(sc, (s7_int)GDBM_EMPTY_DATABASE));
#endif
#ifdef GDBM_BAD_MAGIC_NUMBER
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_BAD_MAGIC_NUMBER"), s7_make_integer(sc, (s7_int)GDBM_BAD_MAGIC_NUMBER));
#endif
#ifdef GDBM_FILE_READ_ERROR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_FILE_READ_ERROR"), s7_make_integer(sc, (s7_int)GDBM_FILE_READ_ERROR));
#endif
#ifdef GDBM_FILE_SEEK_ERROR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_FILE_SEEK_ERROR"), s7_make_integer(sc, (s7_int)GDBM_FILE_SEEK_ERROR));
#endif
#ifdef GDBM_FILE_WRITE_ERROR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_FILE_WRITE_ERROR"), s7_make_integer(sc, (s7_int)GDBM_FILE_WRITE_ERROR));
#endif
#ifdef GDBM_FILE_OPEN_ERROR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_FILE_OPEN_ERROR"), s7_make_integer(sc, (s7_int)GDBM_FILE_OPEN_ERROR));
#endif
#ifdef GDBM_BLOCK_SIZE_ERROR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_BLOCK_SIZE_ERROR"), s7_make_integer(sc, (s7_int)GDBM_BLOCK_SIZE_ERROR));
#endif
#ifdef GDBM_MALLOC_ERROR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_MALLOC_ERROR"), s7_make_integer(sc, (s7_int)GDBM_MALLOC_ERROR));
#endif
#ifdef GDBM_NO_ERROR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_NO_ERROR"), s7_make_integer(sc, (s7_int)GDBM_NO_ERROR));
#endif
#ifdef GDBM_VERSION_PATCH
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_VERSION_PATCH"), s7_make_integer(sc, (s7_int)GDBM_VERSION_PATCH));
#endif
#ifdef GDBM_VERSION_MINOR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_VERSION_MINOR"), s7_make_integer(sc, (s7_int)GDBM_VERSION_MINOR));
#endif
#ifdef GDBM_VERSION_MAJOR
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_VERSION_MAJOR"), s7_make_integer(sc, (s7_int)GDBM_VERSION_MAJOR));
#endif
#ifdef GDBM_GETDBNAME
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_GETDBNAME"), s7_make_integer(sc, (s7_int)GDBM_GETDBNAME));
#endif
#ifdef GDBM_GETMAXMAPSIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_GETMAXMAPSIZE"), s7_make_integer(sc, (s7_int)GDBM_GETMAXMAPSIZE));
#endif
#ifdef GDBM_GETCOALESCEBLKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_GETCOALESCEBLKS"), s7_make_integer(sc, (s7_int)GDBM_GETCOALESCEBLKS));
#endif
#ifdef GDBM_GETCENTFREE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_GETCENTFREE"), s7_make_integer(sc, (s7_int)GDBM_GETCENTFREE));
#endif
#ifdef GDBM_GETSYNCMODE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_GETSYNCMODE"), s7_make_integer(sc, (s7_int)GDBM_GETSYNCMODE));
#endif
#ifdef GDBM_GETCACHESIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_GETCACHESIZE"), s7_make_integer(sc, (s7_int)GDBM_GETCACHESIZE));
#endif
#ifdef GDBM_GETMMAP
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_GETMMAP"), s7_make_integer(sc, (s7_int)GDBM_GETMMAP));
#endif
#ifdef GDBM_GETFLAGS
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_GETFLAGS"), s7_make_integer(sc, (s7_int)GDBM_GETFLAGS));
#endif
#ifdef GDBM_SETMMAP
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_SETMMAP"), s7_make_integer(sc, (s7_int)GDBM_SETMMAP));
#endif
#ifdef GDBM_SETMAXMAPSIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_SETMAXMAPSIZE"), s7_make_integer(sc, (s7_int)GDBM_SETMAXMAPSIZE));
#endif
#ifdef GDBM_SETCOALESCEBLKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_SETCOALESCEBLKS"), s7_make_integer(sc, (s7_int)GDBM_SETCOALESCEBLKS));
#endif
#ifdef GDBM_SETCENTFREE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_SETCENTFREE"), s7_make_integer(sc, (s7_int)GDBM_SETCENTFREE));
#endif
#ifdef GDBM_SETSYNCMODE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_SETSYNCMODE"), s7_make_integer(sc, (s7_int)GDBM_SETSYNCMODE));
#endif
#ifdef GDBM_SETCACHESIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_SETCACHESIZE"), s7_make_integer(sc, (s7_int)GDBM_SETCACHESIZE));
#endif
#ifdef GDBM_CLOEXEC
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_CLOEXEC"), s7_make_integer(sc, (s7_int)GDBM_CLOEXEC));
#endif
#ifdef GDBM_NOMMAP
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_NOMMAP"), s7_make_integer(sc, (s7_int)GDBM_NOMMAP));
#endif
#ifdef GDBM_OPENMASK
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_OPENMASK"), s7_make_integer(sc, (s7_int)GDBM_OPENMASK));
#endif
#ifdef GDBM_COALESCEBLKS
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_COALESCEBLKS"), s7_make_integer(sc, (s7_int)GDBM_COALESCEBLKS));
#endif
#ifdef GDBM_CENTFREE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_CENTFREE"), s7_make_integer(sc, (s7_int)GDBM_CENTFREE));
#endif
#ifdef GDBM_SYNCMODE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_SYNCMODE"), s7_make_integer(sc, (s7_int)GDBM_SYNCMODE));
#endif
#ifdef GDBM_FASTMODE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_FASTMODE"), s7_make_integer(sc, (s7_int)GDBM_FASTMODE));
#endif
#ifdef GDBM_CACHESIZE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_CACHESIZE"), s7_make_integer(sc, (s7_int)GDBM_CACHESIZE));
#endif
#ifdef GDBM_REPLACE
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_REPLACE"), s7_make_integer(sc, (s7_int)GDBM_REPLACE));
#endif
#ifdef GDBM_INSERT
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_INSERT"), s7_make_integer(sc, (s7_int)GDBM_INSERT));
#endif
#ifdef GDBM_NOLOCK
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_NOLOCK"), s7_make_integer(sc, (s7_int)GDBM_NOLOCK));
#endif
#ifdef GDBM_SYNC
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_SYNC"), s7_make_integer(sc, (s7_int)GDBM_SYNC));
#endif
#ifdef GDBM_FAST
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_FAST"), s7_make_integer(sc, (s7_int)GDBM_FAST));
#endif
#ifdef GDBM_NEWDB
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_NEWDB"), s7_make_integer(sc, (s7_int)GDBM_NEWDB));
#endif
#ifdef GDBM_WRCREAT
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_WRCREAT"), s7_make_integer(sc, (s7_int)GDBM_WRCREAT));
#endif
#ifdef GDBM_WRITER
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_WRITER"), s7_make_integer(sc, (s7_int)GDBM_WRITER));
#endif
#ifdef GDBM_READER
  s7_define(sc, cur_env, s7_make_symbol(sc, "GDBM_READER"), s7_make_integer(sc, (s7_int)GDBM_READER));
#endif

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_open"),
            s7_make_typed_function(sc, "gdbm_open", g_gdbm_open, 5, 0, false, "(gdbm_open filename size flags mode func) opens a gdbm data base", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_store"),
            s7_make_typed_function(sc, "gdbm_store", g_gdbm_store, 4, 0, false, "(gdbm_store gdbm key context flag)", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_fetch"),
            s7_make_typed_function(sc, "gdbm_fetch", g_gdbm_fetch, 2, 0, false, "(gdbm_fetch gdbm key)", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_nextkey"),
            s7_make_typed_function(sc, "gdbm_nextkey", g_gdbm_nextkey, 2, 0, false, "(gdbm_nextkey gdbm prev)", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_delete"),
            s7_make_typed_function(sc, "gdbm_delete", g_gdbm_delete, 2, 0, false, "(gdbm_delete gdbm key)", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_exists"),
            s7_make_typed_function(sc, "gdbm_exists", g_gdbm_exists, 2, 0, false, "(gdbm_exists gdbm key)", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_firstkey"),
            s7_make_typed_function(sc, "gdbm_firstkey", g_gdbm_firstkey, 1, 0, false, "(gdbm_firstkey gdbm)", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_sync"),
            s7_make_typed_function(sc, "gdbm_sync", s7__gdbm_sync, 1, 0, false, "void gdbm_sync((GDBM_FILE c_pointer))", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_close"),
            s7_make_typed_function(sc, "gdbm_close", s7__gdbm_close, 1, 0, false, "void gdbm_close((GDBM_FILE c_pointer))", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_reorganize"),
            s7_make_typed_function(sc, "gdbm_reorganize", s7__gdbm_reorganize, 1, 0, false, "int gdbm_reorganize((GDBM_FILE c_pointer))", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_fdesc"),
            s7_make_typed_function(sc, "gdbm_fdesc", s7__gdbm_fdesc, 1, 0, false, "int gdbm_fdesc((GDBM_FILE c_pointer))", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_strerror"),
            s7_make_typed_function(sc, "gdbm_strerror", s7__gdbm_strerror, 1, 0, false, "char* gdbm_strerror(int)", pl_si));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_errno"),
            s7_make_typed_function(sc, "gdbm_errno", g_gdbm_errno, 0, 0, false, "(gdbm_errno) returns the current gdbm error number", NULL));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "gdbm_version"),
            s7_make_typed_function(sc, "gdbm_version", g_gdbm_version, 0, 0, false, "(gdbm_version) returns the current gbdm version", NULL));
}
