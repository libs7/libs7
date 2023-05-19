#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <toml.h>
#include "s7.h"

#include "log.h"

static s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string, integer_string;
static s7_pointer int64_t__symbol, toml_datum_t__symbol, toml_array_t__symbol, toml_table_t__symbol, FILE__symbol;


/* -------- toml_parse_file -------- */
static s7_pointer toml_toml_parse_file(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  FILE* toml_toml_parse_file_0;
  char* toml_toml_parse_file_1;
  int toml_toml_parse_file_2;
  p = args;
  arg = s7_car(p);
  toml_toml_parse_file_0 = (FILE*)s7_c_pointer_with_type(sc, arg, FILE__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_parse_file_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:parse-file", 15), 2, arg, string_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_parse_file_2 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:parse-file", 15), 3, arg, integer_string));
  return(s7_make_c_pointer_with_type(sc, (void*)toml_parse_file(toml_toml_parse_file_0, toml_toml_parse_file_1, toml_toml_parse_file_2), toml_table_t__symbol, s7_f(sc)));
}

static s7_pointer g_toml_parse(s7_scheme *sc, s7_pointer args)
{
    s7_pointer p, arg;
    char* toml_toml_parse_0;
    p = args;
    arg = s7_car(p);
    if (s7_is_string(arg))
       toml_toml_parse_0 = (char*)s7_string(arg);
    else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:parse", 10), 1, arg, string_string));
    char errbuff[200];
    return(s7_make_c_pointer_with_type(sc, (void*)toml_parse(toml_toml_parse_0, errbuff, sizeof(errbuff)), toml_table_t__symbol, s7_f(sc)));
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


/* -------- toml_array_nelem -------- */
static s7_pointer toml_toml_array_nelem(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_array_nelem_0;
  p = args;
  arg = s7_car(p);
  toml_toml_array_nelem_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)toml_array_nelem(toml_toml_array_nelem_0)));
}


/* -------- toml_toml_array_ref -------- */
static s7_pointer toml_toml_array_ref(s7_scheme *sc, s7_pointer args)
{
    log_debug("toml_toml_array_ref");
    s7_pointer p, arg;
    toml_array_t* array;
    int idx;
    p = args;
    arg = s7_car(p);

    // extract the c pointer
    array = (toml_array_t*)s7_c_pointer(arg);

    p = s7_cdr(p);
    arg = s7_car(p);
    if (s7_is_integer(arg))
        idx = (int)s7_integer(arg);
    else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:array-ref", 15), 2, arg, integer_string));

    toml_datum_t datum;

    datum = toml_string_at(array, idx);
    if (datum.ok) { return(s7_make_string(sc, datum.u.s)); }

    datum = toml_bool_at(array, idx);
    if (datum.ok) { return(s7_make_boolean(sc, datum.u.b)); }

    datum = toml_int_at(array, idx);
    if (datum.ok) { return(s7_make_integer(sc, datum.u.i)); }

    datum = toml_double_at(array, idx);
    if (datum.ok) { return(s7_make_real(sc, datum.u.d)); }

    datum = toml_timestamp_at(array, idx);
    if (datum.ok) {
        /* not yet supported */
        return(s7_f(sc));
    }

    toml_array_t *array_ptr = toml_array_at(array, idx);
    if (array_ptr) {
        return(s7_make_c_pointer_with_type(sc, (void*)array_ptr, toml_array_t__symbol, s7_f(sc)));
    }

    toml_table_t *table_val = toml_table_at(array, idx);
    if (table_val) {
        return(s7_make_c_pointer_with_type(sc, (void*)table_val, toml_array_t__symbol, s7_f(sc)));
    }

    return(s7_f(sc));
}

/* -------- toml_string_at -------- */
static s7_pointer toml_toml_string_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_string_at_0;
  int toml_toml_string_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_string_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_string_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:string-at", 14), 2, arg, integer_string));
  toml_string_at(toml_toml_string_at_0, toml_toml_string_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_bool_at -------- */
static s7_pointer toml_toml_bool_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_bool_at_0;
  int toml_toml_bool_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_bool_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_bool_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:bool-at", 12), 2, arg, integer_string));
  toml_bool_at(toml_toml_bool_at_0, toml_toml_bool_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_int_at -------- */
static s7_pointer toml_toml_int_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_int_at_0;
  int toml_toml_int_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_int_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_int_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:int-at", 11), 2, arg, integer_string));
  toml_int_at(toml_toml_int_at_0, toml_toml_int_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_double_at -------- */
static s7_pointer toml_toml_double_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_double_at_0;
  int toml_toml_double_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_double_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_double_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:double-at", 14), 2, arg, integer_string));
  toml_double_at(toml_toml_double_at_0, toml_toml_double_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_timestamp_at -------- */
static s7_pointer toml_toml_timestamp_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_timestamp_at_0;
  int toml_toml_timestamp_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_timestamp_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_timestamp_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:timestamp-at", 17), 2, arg, integer_string));
  toml_timestamp_at(toml_toml_timestamp_at_0, toml_toml_timestamp_at_1);
  return(s7_unspecified(sc));
}


/* -------- toml_array_at -------- */
static s7_pointer toml_toml_array_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_array_at_0;
  int toml_toml_array_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_array_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_array_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:array-at", 13), 2, arg, integer_string));
  return(s7_make_c_pointer_with_type(sc, (void*)toml_array_at(toml_toml_array_at_0, toml_toml_array_at_1), toml_array_t__symbol, s7_f(sc)));
}


/* -------- toml_table_at -------- */
static s7_pointer toml_toml_table_at(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_table_at_0;
  int toml_toml_table_at_1;
  p = args;
  arg = s7_car(p);
  toml_toml_table_at_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    toml_toml_table_at_1 = (int)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:table-at", 13), 2, arg, integer_string));
  return(s7_make_c_pointer_with_type(sc, (void*)toml_table_at(toml_toml_table_at_0, toml_toml_table_at_1), toml_table_t__symbol, s7_f(sc)));
}


/* -------- toml_key_in -------- */
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
  toml_table_t* toml_toml_key_exists_0;
  char* toml_toml_key_exists_1;
  p = args;
  arg = s7_car(p);
  /* toml_toml_key_exists_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1); */
  toml_toml_key_exists_0 = (toml_table_t*)s7_c_pointer(arg);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_key_exists_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:key-exists", 15), 2, arg, string_string));
  return(s7_make_integer(sc, (s7_int)toml_key_exists(toml_toml_key_exists_0, toml_toml_key_exists_1)));
}


/* -------- toml_toml_table_ref -------- */
static s7_pointer toml_toml_table_ref(s7_scheme *sc, s7_pointer args)
{
    log_debug("toml_toml_table_ref");
    s7_pointer p, arg;
    toml_table_t* table;
    char* key;
    p = args;
    arg = s7_car(p);
    /* table = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1); */
    // extract the c pointer
    table = (toml_table_t*)s7_c_pointer(arg);

    p = s7_cdr(p);
    arg = s7_car(p);
    if (s7_is_string(arg))
        key = (char*)s7_string(arg);
    else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:table-ref", 14), 2, arg, string_string));

    toml_datum_t datum;

    datum = toml_string_in(table, key);
    if (datum.ok) {
        // FIXME: free(datum.u.s)???
        return(s7_make_string(sc, datum.u.s));
    }

    datum = toml_bool_in(table, key);
    if (datum.ok) { return(s7_make_boolean(sc, datum.u.b)); }

    datum = toml_int_in(table, key);
    if (datum.ok) { return(s7_make_integer(sc, datum.u.i)); }

    datum = toml_double_in(table, key);
    if (datum.ok) { return(s7_make_real(sc, datum.u.d)); }

    datum = toml_timestamp_in(table, key);
    if (datum.ok) {
        /* not yet supported */
        return(s7_f(sc));
    }

    toml_array_t *array_ptr = toml_array_in(table, key);
    if (array_ptr) {
        return(s7_make_c_pointer_with_type(sc, (void*)array_ptr, toml_array_t__symbol, s7_f(sc)));
    }

    toml_table_t *table_val = toml_table_in(table, key);
    if (table_val) {
        return(s7_make_c_pointer_with_type(sc, (void*)table_val, toml_array_t__symbol, s7_f(sc)));
    }

    return(s7_f(sc));
}

/* -------- toml_string_in -------- */
static s7_pointer toml_toml_string_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_string_in_0;
  char* toml_toml_string_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_string_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_string_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:string-in", 14), 2, arg, string_string));
  return(s7_unspecified(sc));
  /* return(s7_make_c_pointer_with_type(sc, (void*)toml_string_in(toml_toml_string_in_0, toml_toml_string_in_1), toml_datum_t__symbol, s7_f(sc))); */
}


/* -------- toml_bool_in -------- */
static s7_pointer toml_toml_bool_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_bool_in_0;
  char* toml_toml_bool_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_bool_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_bool_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:bool-in", 12), 2, arg, string_string));
  toml_bool_in(toml_toml_bool_in_0, toml_toml_bool_in_1);
  return(s7_unspecified(sc));
}


/* -------- toml_int_in -------- */
static s7_pointer toml_toml_int_in(s7_scheme *sc, s7_pointer args)
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
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:int-in", 11), 2, arg, string_string));
  toml_int_in(toml_toml_int_in_0, toml_toml_int_in_1);
  return(s7_unspecified(sc));
}


/* -------- toml_double_in -------- */
static s7_pointer toml_toml_double_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_double_in_0;
  char* toml_toml_double_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_double_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_double_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:double-in", 14), 2, arg, string_string));
  toml_double_in(toml_toml_double_in_0, toml_toml_double_in_1);
  return(s7_unspecified(sc));
}


/* -------- toml_timestamp_in -------- */
static s7_pointer toml_toml_timestamp_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_timestamp_in_0;
  char* toml_toml_timestamp_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_timestamp_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_timestamp_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:timestamp-in", 17), 2, arg, string_string));
  toml_timestamp_in(toml_toml_timestamp_in_0, toml_toml_timestamp_in_1);
  return(s7_unspecified(sc));
}


/* -------- toml_array_in -------- */
static s7_pointer toml_toml_array_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_array_in_0;
  char* toml_toml_array_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_array_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_array_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:array-in", 13), 2, arg, string_string));
  return(s7_make_c_pointer_with_type(sc, (void*)toml_array_in(toml_toml_array_in_0, toml_toml_array_in_1), toml_array_t__symbol, s7_f(sc)));
}


/* -------- toml_table_in -------- */
static s7_pointer toml_toml_table_in(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_table_in_0;
  char* toml_toml_table_in_1;
  p = args;
  arg = s7_car(p);
  toml_toml_table_in_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 1);
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_string(arg))
    toml_toml_table_in_1 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "toml:table-in", 13), 2, arg, string_string));
  return(s7_make_c_pointer_with_type(sc, (void*)toml_table_in(toml_toml_table_in_0, toml_toml_table_in_1), toml_table_t__symbol, s7_f(sc)));
}


/* -------- toml_array_kind -------- */
static s7_pointer toml_toml_array_kind(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* array;
  p = args;
  arg = s7_car(p);
  // toml_toml_array_kind_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 0);
  array = (toml_array_t*)s7_c_pointer(arg);
  char c = toml_array_kind(array);
  return(s7_make_character(sc, c));
}


/* -------- toml_array_type -------- */
static s7_pointer toml_toml_array_type(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* array;
  p = args;
  arg = s7_car(p);
  /* toml_toml_array_type_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 0); */
  array = (toml_array_t*)s7_c_pointer(arg);
  char c = toml_array_type(array);
  return(s7_make_character(sc, c));
  /* return(s7_make_integer(sc, (s7_int)toml_array_type(toml_toml_array_type_0))); */
}


/* -------- toml_array_key -------- */
static s7_pointer toml_toml_array_key(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_array_t* toml_toml_array_key_0;
  p = args;
  arg = s7_car(p);
  toml_toml_array_key_0 = (toml_array_t*)s7_c_pointer_with_type(sc, arg, toml_array_t__symbol, __func__, 0);
  return(s7_make_string(sc, (char*)toml_array_key(toml_toml_array_key_0)));
}


/* -------- toml_table_nkval -------- */
static s7_pointer toml_toml_table_nkval(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* table;
  p = args;
  arg = s7_car(p);
  /* toml_toml_table_nkval_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 0); */
  table = (toml_table_t*)s7_c_pointer(arg);
  return(s7_make_integer(sc, (s7_int)toml_table_nkval(table)));
}


/* -------- toml_table_narr -------- */
static s7_pointer toml_toml_table_narr(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_table_narr_0;
  p = args;
  arg = s7_car(p);
  toml_toml_table_narr_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)toml_table_narr(toml_toml_table_narr_0)));
}


/* -------- toml_table_ntab -------- */
static s7_pointer toml_toml_table_ntab(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* toml_toml_table_ntab_0;
  p = args;
  arg = s7_car(p);
  toml_toml_table_ntab_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)toml_table_ntab(toml_toml_table_ntab_0)));
}


/* -------- toml_table_key -------- */
static s7_pointer toml_toml_table_key(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  toml_table_t* table;
  p = args;
  arg = s7_car(p);
  /* toml_toml_table_key_0 = (toml_table_t*)s7_c_pointer_with_type(sc, arg, toml_table_t__symbol, __func__, 0); */
  table = (toml_table_t*)s7_c_pointer(arg);
  return(s7_make_string(sc, (char*)toml_table_key(table)));
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

s7_pointer libtoml_s7_init(s7_scheme *sc);
s7_pointer libtoml_s7_init(s7_scheme *sc)
{
  s7_pointer cur_env;
  s7_pointer pl_tx, pl_txs, pl_txi, pl_xxi, pl_xxs, pl_xxsi, pl_sx, pl_sxi, pl_ix, pl_iis, pl_ixs, pl_isix;
  {
    s7_pointer t, x, s, i;
    t = s7_t(sc);
    x = s7_make_symbol(sc, "c-pointer?");
    s = s7_make_symbol(sc, "string?");
    i = s7_make_symbol(sc, "integer?");

    pl_tx = s7_make_signature(sc, 2, t, x);
    pl_txs = s7_make_signature(sc, 3, t, x, s);
    pl_txi = s7_make_signature(sc, 3, t, x, i);
    pl_xxi = s7_make_signature(sc, 3, x, x, i);
    pl_xxs = s7_make_signature(sc, 3, x, x, s);
    pl_xxsi = s7_make_signature(sc, 4, x, x, s, i);
    pl_sx = s7_make_signature(sc, 2, s, x);
    pl_sxi = s7_make_signature(sc, 3, s, x, i);
    pl_ix = s7_make_signature(sc, 2, i, x);
    pl_iis = s7_make_signature(sc, 3, i, i, s);
    pl_ixs = s7_make_signature(sc, 3, i, x, s);
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

  int64_t__symbol = s7_make_symbol(sc, "int64_t*");
  toml_datum_t__symbol = s7_make_symbol(sc, "toml_datum_t*");
  toml_array_t__symbol = s7_make_symbol(sc, "toml_array_t*");
  toml_table_t__symbol = s7_make_symbol(sc, "toml_table_t*");
  FILE__symbol = s7_make_symbol(sc, "FILE*");

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:ucs-to-utf8"),
            s7_make_typed_function(sc, "toml:ucs-to-utf8", toml_toml_ucs_to_utf8, 2, 0, false,
                                   "(toml:ucs-to-utf8 c)", pl_iis));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:utf8-to-ucs"),
            s7_make_typed_function(sc, "toml:utf8-to-ucs", toml_toml_utf8_to_ucs, 3, 0, false,
                                   "(toml:utf8_to_ucs c)", pl_isix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-key"),
            s7_make_typed_function(sc, "toml:table-key", toml_toml_table_key, 1, 0, false, "(toml:table-key table key) returns the value of table at key.", pl_sx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-ntab"),
            s7_make_typed_function(sc, "toml:table-ntab", toml_toml_table_ntab, 1, 0, false, "(toml:table-ntab t) returns number of tables in table t.", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-subtable-count"),
            s7_make_typed_function(sc, "toml:table-subtable-count", toml_toml_table_ntab, 1, 0, false, "(toml:table-subtable-count t) alias of toml:table-ntab.", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-narr"), // nbr of arrays
            s7_make_typed_function(sc, "toml:table-narr", toml_toml_table_narr, 1, 0, false, "(toml:table-narr t)", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-array-count"),
            s7_make_typed_function(sc, "toml:table-array-count", toml_toml_table_narr, 1, 0, false, "(toml:table-array-count t) alias of toml:table-narr.", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-nkval"),
            s7_make_typed_function(sc, "toml:table-nkval", toml_toml_table_nkval, 1, 0, false, "(toml:table-nkval t) nbr of kv pairs in t.", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-entries-count"), // R6RS: table-size
            s7_make_typed_function(sc, "toml:table-entries-count",
                                   toml_toml_table_nkval, 1, 0, false,
                                   "(toml:table-entries-count t) alias of toml_table-nkval", pl_ix));

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

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-ref"),
            s7_make_typed_function(sc, "toml:table-ref",
                                   toml_toml_table_ref, 2, 0, false,
                                   "(toml:table-ref t k) returns value of table t at key k", pl_xxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-in"),
            s7_make_typed_function(sc, "toml:table-in", toml_toml_table_in, 2, 0, false,
                                   "(toml:table-in t k) returns value of t at k, if val is a table", pl_xxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-in"),
            s7_make_typed_function(sc, "toml:array-in", toml_toml_array_in, 2, 0, false,
                                   "(toml:array-in t k) returns value of t at k, if val is an array", pl_xxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:timestamp-in"),
            s7_make_typed_function(sc, "toml:timestamp-in", toml_toml_timestamp_in, 2, 0, false,
                                   "(toml:timestamp-in t k) returns value of t at k, if val is a timestamp", pl_txs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:double-in"),
            s7_make_typed_function(sc, "toml:double-in", toml_toml_double_in, 2, 0, false,
                                   "(toml:double-in t k) returns value of t at k, if val is a double", pl_txs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:int-in"),
            s7_make_typed_function(sc, "toml:int-in", toml_toml_int_in, 2, 0, false,
                                   "(toml:int-in t k) returns value of t at k, if val is an int",
                                   pl_txs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:bool-in"),
            s7_make_typed_function(sc, "toml:bool-in", toml_toml_bool_in, 2, 0, false,
                                   "(toml:bool-in t k) returns value of t at k, if val is a bool",
                                   pl_txs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:string-in"),
            s7_make_typed_function(sc, "toml:string-in", toml_toml_string_in, 2, 0, false,
                                   "(toml:string-in t k) returns value of t at k, if val is a string",
                                   pl_xxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:key-exists?"),
            s7_make_typed_function(sc, "toml:key-exists?", toml_toml_key_exists, 2, 0, false,
                                   "(toml:key-exists? t k) is true if key k is in table t", pl_ixs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-contains?"),
            s7_make_typed_function(sc, "toml:table-contains?", toml_toml_key_exists, 2, 0, false,
                                   "(toml:table-contains? t k) alias of toml:key-exists?", pl_ixs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:key-in"),
            s7_make_typed_function(sc, "toml:key-in", toml_toml_key_in, 2, 0, false,
                                   "(toml:key-in t idx) returns key at index idx of table t", pl_sxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:key-for-index"),
            s7_make_typed_function(sc, "toml:key-for-index", toml_toml_key_in, 2, 0, false,
                                   "(toml:key-for-index t idx) alias of toml:key-in", pl_sxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-ref"),
            s7_make_typed_function(sc, "toml:array-ref",
                                   toml_toml_array_ref, 2, 0, false,
                                   "(toml:array-ref a i) value of array a at index i", pl_xxs));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:table-at"),
            s7_make_typed_function(sc, "toml:table-at", toml_toml_table_at, 2, 0, false,
                                   "(toml:table-at a i) value of array a at index i, if val is a table",
                                   pl_xxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-at"),
            s7_make_typed_function(sc, "toml:array-at", toml_toml_array_at, 2, 0, false, "toml_array_t* toml_array_at(toml_array_t* int)", pl_xxi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:timestamp-at"),
            s7_make_typed_function(sc, "toml:timestamp-at", toml_toml_timestamp_at, 2, 0, false, "toml_datum_t toml_timestamp_at(toml_array_t* int)", pl_txi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:double-at"),
            s7_make_typed_function(sc, "toml:double-at", toml_toml_double_at, 2, 0, false, "toml_datum_t toml_double_at(toml_array_t* int)", pl_txi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:int-at"),
            s7_make_typed_function(sc, "toml:int-at", toml_toml_int_at, 2, 0, false, "toml_datum_t toml_int_at(toml_array_t* int)", pl_txi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:bool-at"),
            s7_make_typed_function(sc, "toml:bool-at", toml_toml_bool_at, 2, 0, false, "toml_datum_t toml_bool_at(toml_array_t* int)", pl_txi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:string-at"),
            s7_make_typed_function(sc, "toml:string-at", toml_toml_string_at, 2, 0, false, "toml_datum_t toml_string_at(toml_array_t* int)", pl_txi));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-nelem"),
            s7_make_typed_function(sc, "toml:array-nelem", toml_toml_array_nelem, 1, 0, false,
                                   "(toml:array-nelem a) nbr of elements in array a", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:array-length"),
            s7_make_typed_function(sc, "toml:array-length", toml_toml_array_nelem, 1, 0, false,
                                   "(toml:array-length a) alias of toml:array-nelem", pl_ix));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:free"),
            s7_make_typed_function(sc, "toml:free", toml_toml_free, 1, 0, false,
                                   "(toml:free t) free table t", pl_tx));

  s7_define(sc, cur_env,
            s7_make_symbol(sc, "toml:read"),
            s7_make_typed_function(sc, "toml:read", g_toml_parse, 1, 0, false,
                                   "(toml:read port) parse toml string from port", NULL));

  /* s7_define(sc, cur_env, */
  /*           s7_make_symbol(sc, "toml:parse-file"), */
  /*           s7_make_typed_function(sc, "toml:parse-file", toml_toml_parse_file, 3, 0, false, */
  /*                                  "(toml:parse-file port) parse string from port", pl_xxsi)); */

  s7_set_shadow_rootlet(sc, old_shadow);


  return(cur_env);

}
