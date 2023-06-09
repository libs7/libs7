#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "utils.h"
#include "toml.h"
#include "libtoml_s7.h"
/* #include "toml_table_s7.h" */
/* #include "toml_array_s7.h" */

s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string;
s7_pointer integer_string;
static s7_pointer int64_t__symbol, toml_datum_t__symbol, toml_array_t__symbol, toml_table_t__symbol, FILE__symbol;

/* (toml:read) - read current-input-port
 * (toml:read str) - read string str
 * (toml:read p) - read port p
 */
static s7_pointer g_toml_read(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_read);
    s7_pointer p, arg;
    /* TRACE_S7_DUMP("args", args); */

    /* s7_gc_on(s7, false); */

    const char* toml_str = NULL;
    /* log_debug("toml_str ptr: %p", (void*)toml_str); */

    p = args;
    if (p == s7_nil(s7)) {
        /* log_debug("SOURCE: current-input-port"); */
        toml_str = libs7_input_port_to_c_string(s7, s7_current_input_port(s7));
    } else {
        arg = s7_car(p);
        if (s7_is_input_port(s7, arg)) {
            TRACE_LOG_DEBUG("SOURCE: input port", "");
            toml_str = libs7_input_port_to_c_string(s7, arg);
        }
        else if (s7_is_string(arg)) {
            TRACE_LOG_DEBUG("SOURCE: string", "");
            toml_str = (char*)s7_string(arg);
        }
        else {
            return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "toml:read", 10), 1, arg, string_string));
        }
    }
    char errbuff[200];

    //WARNING: this toml_table_t must be freed by client using toml_free
    toml_table_t *t = toml_parse((char*)toml_str, (char*)errbuff, sizeof(errbuff));

    if (t == NULL) {
        log_error("toml:read failure: %s", errbuff);
        return s7_error(s7,
                        s7_make_symbol(s7, "toml:read"),
                        s7_cons(s7, s7_make_string(s7, (char*)errbuff), s7_nil(s7)));
    } else {
        s7_pointer rval = s7_make_c_object(s7, toml_table_type_tag, (void*)t);
        /* TRACE_S7_DUMP("tt", rval); */
        /* log_debug("returning obj"); */
        /* s7_pointer dt = s7_type_of(s7, rval); */
        /* TRACE_S7_DUMP("typ", dt); */
        /* log_debug("toml-table? %d", */
        /*           s7_c_object_type(rval) == toml_table_type_tag); */
        /* log_debug("tag: %d", toml_table_type_tag); */
        return rval;
    }
}

/* -------- toml_read_file -------- */
/* s7_pointer toml_read_file(s7_scheme *sc, s7_pointer args) */
s7_pointer toml_read_file(s7_scheme *s7, char *fname)
{
    TRACE_ENTRY(toml_read_file);
    log_debug("toml file: %s", fname);
    (void)fname;
    /* toml_parse_file(FILE *fp, char *errbuf, int errbufsz); */

    FILE *fileStream;
    fileStream = fopen(fname, "r");
    if (fileStream == NULL) {
        log_error("FAIL: fopen(%s)", fname);
        //FIXME: cleanup
        exit(EXIT_FAILURE);
    }
    char errbuff[200];
    //WARNING: this toml_table_t must be freed by client
    toml_table_t *tt = toml_parse_file(fileStream, errbuff, sizeof(errbuff));
    fclose(fileStream);
    if (tt == 0) {
        log_error("toml_parse_file failure: %s", errbuff);
        return s7_error(s7,
                        s7_make_symbol(s7, "toml_read_file"),
                        s7_cons(s7, s7_make_string(s7, (char*)errbuff), s7_nil(s7)));
    } else {
        s7_pointer rval = s7_make_c_object(s7,
                                           toml_table_type_tag,
                                           (void*)tt);
        log_debug("returning obj");
        s7_pointer dt = s7_type_of(s7, rval);
        (void)dt;
        TRACE_S7_DUMP("typ", dt);
        log_debug("toml-table? %d",
                  s7_c_object_type(rval) == toml_table_type_tag);
        log_debug("tag: %d", toml_table_type_tag);
       return rval;
    }

  /* s7_pointer p, arg; */
  /* FILE* toml_toml_read_file_0; */
  /* char* toml_toml_read_file_1; */
  /* int toml_toml_read_file_2; */
  /* p = args; */
  /* arg = s7_car(p); */
  /* toml_toml_read_file_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 1); */
  /* p = s7_cdr(p); */
  /* arg = s7_car(p); */
  /* if (s7_is_string(arg)) */
  /*   toml_toml_read_file_1 = (char*)s7_string(arg); */
  /* else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:parse-file", 15), 2, arg, string_string)); */
  /* p = s7_cdr(p); */
  /* arg = s7_car(p); */
  /* if (s7_is_integer(arg)) */
  /*   toml_toml_read_file_2 = (int)s7_integer(arg); */
  /* else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:parse-file", 15), 3, arg, integer_string)); */

  /* toml_table_t *tt = toml_parse_file(toml_toml_read_file_0, toml_toml_read_file_1, toml_toml_read_file_2); */

  /* return(s7_make_c_object(s7, toml_table_type_tag, tt)) */

  /* return(s7_make_c_pointer_with_type(sc, */
  /*                                    //tt, */
  /*                                    toml_table_t__symbol, s7_f(sc))); */

    return s7_nil(s7);
}

/* -------- toml_free -------- */
static s7_pointer toml_toml_free(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_free_0;
  p = args;
  arg = s7_car(p);
  toml_toml_free_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 0);
  toml_free(toml_toml_free_0);
  return(s7_unspecified(sc));
}


/* -------- toml_key_in -------- */
/* ... retrieve the key in table at keyidx. Return 0 if out of range. */
/* scheme: toml:map-key-for-index */
static s7_pointer toml_toml_key_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_key_in_0;
  int toml_toml_key_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_key_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_key_in_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:key-in", 11), 2, arg, integer_string));
  return(s7_make_string(sc, (char*)toml_key_in(toml_toml_key_in_0, toml_toml_key_in_1)));
}


/* -------- toml_key_exists -------- */
static s7_pointer toml_toml_key_exists(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  p = args;
  arg = s7_car(p);
  toml_table_t *t = (toml_table_t*)s7_c_object_value_checked(arg, toml_table_type_tag);
  if (t) {
      p = s7_cdr(p);
      arg = s7_car(p);
      char* k;
      if (s7_is_string(arg))
          k = (char*)s7_string(arg);
      else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:key-exists", 15), 2, arg, string_string));

      bool exists = (bool)toml_key_exists(t, k);
      return(s7_make_boolean(sc, exists));
  } else {
      log_error("Bad arg: expected table, got ???");
      //FIXME: handle error
      return(s7_unspecified(sc));
  }
}


/* -------- toml_toml_table_ref -------- */
// implementation in toml_table_s7.c

/* -------- toml_array_kind -------- */
static s7_pointer toml_toml_array_kind(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  p = args;
  arg = s7_car(p);
  toml_array_t *a = (toml_array_t*)s7_c_object_value_checked(arg, toml_array_type_tag);
  char c = toml_array_kind(a);
  if (c) {
      return(s7_make_character(sc, c));
  } else {
      return(s7_unspecified(sc));
  }
}


/* -------- toml_array_type -------- */
static s7_pointer toml_toml_array_type(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  p = args;
  arg = s7_car(p);
  toml_array_t *a = (toml_array_t*)s7_c_object_value_checked(arg, toml_array_type_tag);
  char c = toml_array_type(a);
  if (c) {
      return(s7_make_character(sc, c));
  } else {
      return(s7_undefined(sc));
  }
}


/* -------- toml_array_key -------- */
static s7_pointer toml_toml_array_key(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  p = args;
  arg = s7_car(p);
  toml_array_t *a = (toml_array_t*)s7_c_object_value_checked(arg, toml_array_type_tag);
  char *k = (char*)toml_array_key(a);
  return(s7_make_string(sc, k));
  /* toml_toml_array_key_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 0); */
  /* return(s7_make_string(sc, (char*)toml_array_key(toml_toml_array_key_0))); */
}


/* -------- toml_table_nkval -------- */
// WARNING: does NOT return length of table!
// only counts items with atomic vals (not arrays or tables)
static s7_pointer toml_toml_table_nkval(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_toml_table_nkval);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_table_t *t = (toml_table_t*)s7_c_object_value_checked(arg, toml_table_type_tag);
    if (t) {
        int ival = toml_table_nkval(t);
        s7_pointer i = s7_make_integer(s7, ival);
        return(i);
    } else {
        log_error("Bad arg, expected table, actual: %d", s7_c_object_type(arg));
        //FIXME: throw error
        return(s7_unspecified(s7));
    }
}

/* -------- toml_table_narr -------- */
static s7_pointer toml_toml_table_narr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  p = args;
  arg = s7_car(p);
  toml_table_t *t = (toml_table_t*)s7_c_object_value_checked(arg, toml_table_type_tag);
  if (t) {
      int narr = toml_table_narr(t);
      s7_pointer i = s7_make_integer(sc, narr);
      return(i);
  } else {
      log_error("Bad arg, expected table, actual: %d", s7_c_object_type(arg));
      //FIXME: throw error
      return(s7_unspecified(sc));
  }
  /* toml_toml_table_narr_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 0); */
  /* return(s7_make_integer(sc, (s7_int)toml_table_narr(toml_toml_table_narr_0))); */
}


/* -------- toml_table_ntab -------- */
static s7_pointer toml_toml_table_ntab(s7_scheme *sc, s7_pointer args)
{
    TRACE_ENTRY(toml_toml_table_ntab);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_table_t *t = (toml_table_t*)s7_c_object_value_checked(arg, toml_table_type_tag);
    if (t) {
        int ntab = toml_table_ntab(t);
        s7_pointer i = s7_make_integer(sc, ntab);
        return(i);
    } else {
        log_error("Bad arg, expected table, actual: %d", s7_c_object_type(arg));
        //FIXME: throw error
        return(s7_unspecified(sc));
    }

    /* toml_toml_table_ntab_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 0); */
    /* return(s7_make_integer(sc, (s7_int)toml_table_ntab(toml_toml_table_ntab_0))); */
}


/* -------- toml_table_key -------- */
/* returns key of table */
static s7_pointer toml_toml_table_key(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_toml_table_key);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_table_t *t = (toml_table_t*)s7_c_object_value_checked(arg, toml_table_type_tag);
    char *k = (char*)toml_table_key(t);
    /* log_debug("k: %s", k); */
    return(s7_make_string(s7, k));
}


/* -------- toml_utf8_to_ucs -------- */
static s7_pointer toml_toml_utf8_to_ucs(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* toml_toml_utf8_to_ucs_0;
  int toml_toml_utf8_to_ucs_1;
  int64_t* toml_toml_utf8_to_ucs_2;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_utf8_to_ucs_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:utf8-to-ucs", 16), 1, arg, string_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_utf8_to_ucs_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:utf8-to-ucs", 16), 2, arg, integer_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  toml_toml_utf8_to_ucs_2 = (int64_t*)s7_c_pointer_with_type(sc, arg, int64_t__symbol, __func__, 3);
  return(s7_make_integer(sc, (s7_int)toml_utf8_to_ucs(toml_toml_utf8_to_ucs_0, toml_toml_utf8_to_ucs_1, toml_toml_utf8_to_ucs_2)));
}


/* -------- toml_ucs_to_utf8 -------- */
static s7_pointer toml_toml_ucs_to_utf8(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  int64_t toml_toml_ucs_to_utf8_0;
  char* toml_toml_ucs_to_utf8_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_ucs_to_utf8_0 = (int64_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:ucs-to-utf8", 16), 1, arg, integer_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_ucs_to_utf8_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:ucs-to-utf8", 16), 2, arg, string_string));
  return(s7_make_integer(sc, (s7_int)toml_ucs_to_utf8(toml_toml_ucs_to_utf8_0, toml_toml_ucs_to_utf8_1)));
}

s7_pointer pl_tx, pl_xx, pl_xxs,pl_sx, pl_sxi, pl_ix, pl_iis, pl_isix, pl_bxs;

s7_pointer libtoml_s7_init(s7_scheme *sc);
s7_pointer libtoml_s7_init(s7_scheme *sc)
{
    TRACE_ENTRY(libtoml_s7_init);
  s7_pointer cur_env;
  /* s7_pointer pl_tx, pl_xxs,pl_sx, pl_sxi, pl_ix, pl_iis, pl_isix, pl_bxs; */
  //  pl_xxsi, pl_ixs
  {
      s7_pointer t, x, b, s, i;

      t = s7_t(sc);
      x = s7_make_symbol(sc, "c-pointer?");
      b = s7_make_symbol(sc, "boolean?");
      s = s7_make_symbol(sc, "string?");
      i = s7_make_symbol(sc, "integer?");

      pl_tx = s7_make_signature(sc, 2, t, x);
      pl_xx = s7_make_signature(sc, 2, x, x);
      pl_xxs = s7_make_signature(sc, 3, x, x, s);
      /* pl_xxsi = s7_make_signature(sc, 4, x, x, s, i); */
      pl_sx = s7_make_signature(sc, 2, s, x);
      pl_sxi = s7_make_signature(sc, 3, s, x, i);
      pl_ix = s7_make_signature(sc, 2, i, x);
      pl_iis = s7_make_signature(sc, 3, i, i, s);
      pl_bxs = s7_make_signature(sc, 3, b, x, s);
      /* pl_ixs = s7_make_signature(sc, 3, i, x, s); */
      pl_isix = s7_make_signature(sc, 4, i, s, i, x);
  }

  string_string = s7_make_semipermanent_string(sc, "a string");
  c_pointer_string = s7_make_semipermanent_string(sc, "a c-pointer");
  character_string = s7_make_semipermanent_string(sc, "a character");
  boolean_string = s7_make_semipermanent_string(sc, "a boolean");
  real_string = s7_make_semipermanent_string(sc, "a real");
  complex_string = s7_make_semipermanent_string(sc, "a complex number");
  integer_string = s7_make_semipermanent_string(sc, "an integer");
  cur_env = s7_inlet(sc, s7_nil(sc));
  s7_pointer old_shadow = s7_set_shadow_rootlet(sc, cur_env);

  toml_table_init(sc, cur_env);
  toml_array_init(sc, cur_env);
  toml_datetime_init(sc, cur_env);

  int64_t__symbol = s7_make_symbol(sc, "int64_t*");
  toml_datum_t__symbol = s7_make_symbol(sc, "toml_datum_t*");
  toml_array_t__symbol = s7_make_symbol(sc, "toml_array_t*");
  toml_table_t__symbol = s7_make_symbol(sc, "toml_table_t*");
  FILE__symbol = s7_make_symbol(sc, "FILE*");

  s7_define_constant(sc, "toml:version", s7_make_string(sc, "1.0-beta"));

  /* -------- table ops -------- */
  // TODO: move to toml_table_s7.c
  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-key"),
            s7_make_typed_function(sc, "toml:map-key",
                                   toml_toml_table_key, 1, 0, false,
                                   "(toml:map-key t) returns the key of table t.", pl_sx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-keys"),
            s7_make_typed_function(sc, "toml:map-keys",
                                   toml_table_keys, 1, 0, false,
                                   "(toml:map-keys t) returns Scheme list of keys of table t.",
                                   pl_sx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-values"),
            s7_make_typed_function(sc, "toml:map-values",
                                   toml_table_values, 1, 0, false,
                                   "(toml:map-keys t) returns Scheme list of values of table t.",
                                   pl_sx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-ntab"),
            s7_make_typed_function(sc, "toml:map-ntab", toml_toml_table_ntab, 1, 0, false, "(toml:map-ntab t) returns number of tables in table t.", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-subtable-count"),
            s7_make_typed_function(sc, "toml:map-subtable-count", toml_toml_table_ntab, 1, 0, false, "(toml:map-subtable-count t) alias of toml:map-ntab.", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-narr"), // nbr of arrays
            s7_make_typed_function(sc, "toml:map-narr", toml_toml_table_narr, 1, 0, false, "(toml:map-narr t)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-array-count"),
            s7_make_typed_function(sc, "toml:map-array-count", toml_toml_table_narr, 1, 0, false, "(toml:map-array-count t) alias of toml:map-narr.", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-nkval"),
            s7_make_typed_function(sc, "toml:map-nkval",
                                   toml_toml_table_nkval,
                                   1, 0, false,
                                   "(toml:map-nkval t) nbr of kv pairs in t.", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-atomic-count"),
            s7_make_typed_function(sc, "toml:map-atomic-count",
                                   toml_toml_table_nkval,
                                   1, 0, false,
                                   "(toml:map-atomic-count t) nbr of kv pairs in t whose values are atomic (bool, int, float, string, timestamp).", pl_ix));

  /* not a tomlc99 api */
  /* srfi-69: hash-table-size */
  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-length"), // R6RS: table-size
            s7_make_typed_function(sc, "toml:map-length",
                                   //toml_toml_table_length,
                                   g_toml_table_length,
                                   1, 0, false,
                                   "(toml:map-length t) total number of entries in table, regardless of value type.", pl_ix));

  /* srfi-69: hash-table-exists? */
  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:key-exists?"),
            s7_make_typed_function(sc, "toml:key-exists?",
                                   toml_toml_key_exists, 2, 0, false,
                                   "(toml:key-exists? t k) is true if key k is in table t",
                                   pl_bxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:map-contains?"),
            s7_make_typed_function(sc, "toml:map-contains?",
                                   toml_toml_key_exists, 2, 0, false,
                                   "(toml:map-contains? t k) alias of toml:key-exists?", pl_bxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:key-in"),
            s7_make_typed_function(sc, "toml:key-in", toml_toml_key_in,
                                   2, 0, false,
                                   "(toml:key-in t idx) returns key at index idx of table t",
                                   pl_sxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:key-for-index"),
            s7_make_typed_function(sc, "toml:key-for-index", toml_toml_key_in, 2, 0, false,
                                   "(toml:key-for-index t idx) alias of toml:key-in", pl_sxi));

  /* -------- array ops -------- */

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-key"),
            s7_make_typed_function(sc, "toml:array-key", toml_toml_array_key, 1, 0, false,
                                   "(toml:array-key a) returns key of array a", pl_sx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-type"),
            s7_make_typed_function(sc, "toml:array-type", toml_toml_array_type, 1, 0, false,
                                   "(toml:array-type a) returns type of vals of array", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-kind"),
            s7_make_typed_function(sc, "toml:array-kind", toml_toml_array_kind, 1, 0, false,
                                   "(toml:array-kind a) returns 'kind' or array (t, a, v or m)", pl_ix));

  /* s7_define(sc, cur_env, */
  /*           s7_make_symbol(sc, "toml:array-ref"), */
  /*           s7_make_typed_function(sc, "toml:array-ref", */
  /*                                  toml_array_ref, 2, 0, false, */
  /*                                  "(toml:array-ref a i) value of array a at index i", pl_xxs)); */

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-nelem"),
            s7_make_typed_function(sc, "toml:array-nelem",
                                   toml_array_length, 1, 0, false,
                                   "(toml:array-nelem a) nbr of elements in array a",
                                   pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-length"),
            s7_make_typed_function(sc, "toml:array-length",
                                   toml_array_length, 1, 0, false,
                                   "(toml:array-length a) alias of toml:array-nelem",
                                   pl_ix));

  /* -------- parsing ops -------- */

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:free"),
            s7_make_typed_function(sc, "toml:free", toml_toml_free, 1, 0, false,
                                   "(toml:free t) free table t", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:read"),
            s7_make_typed_function(sc, "toml:read",
                                   g_toml_read,
                                   // 0 args: thunk reads from curr in port
                                   // (for with-input-from-string )
                                   0,
                                   1, // string or port
                                   false,
                                   "(toml:read x) read toml from string or port", NULL));


  /* s7_define(sc, cur_env, */
  /*           s7_make_symbol(sc, "toml:parse-file"), */
  /*           s7_make_typed_function(sc, "toml:parse-file", toml_toml_read_file, 3, 0, false, */
  /*                                  "(toml:parse-file port) parse string from port", pl_xxsi)); */

  /* -------- misc ops -------- */

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:ucs-to-utf8"),
            s7_make_typed_function(sc, "toml:ucs-to-utf8", toml_toml_ucs_to_utf8, 2, 0, false,
                                   "(toml:ucs-to-utf8 c)", pl_iis));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:utf8-to-ucs"),
            s7_make_typed_function(sc, "toml:utf8-to-ucs", toml_toml_utf8_to_ucs, 3, 0, false,
                                   "(toml:utf8_to_ucs c)", pl_isix));


  s7_set_shadow_rootlet(sc, old_shadow);


  return(cur_env);
}
