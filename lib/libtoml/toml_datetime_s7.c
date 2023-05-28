#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "libtoml_s7.h"
/* #include "toml_array_s7.h" */

int toml_datetime_type_tag = 0;

/* helper prototypes */
s7_pointer toml_datetime_to_alist(s7_scheme *s7, toml_timestamp_t *ts, bool clone);

/* Timestamp types. The year, month, day, hour, minute, second, z
 * fields may be NULL if they are not relevant. e.g. In a DATE
 * type, the hour, minute, second and z fields will be NULLs.
 */
/* struct toml_timestamp_t { */
/*   struct { /\* internal. do not use. *\/ */
/*     int year, month, day; */
/*     int hour, minute, second, millisec; */
/*     char z[10]; */
/*   } __buffer; */
/*   int *year, *month, *day; */
/*   int *hour, *minute, *second, *millisec; */
/*   char *z; */
/* }; */

/* ****************************************************************
 * API:
 *    (toml:date-year ts) etc.
 *    (toml:time-hour ts) etc.
 *    (toml:datetime-ref "year") ??
 * toml:datetime? equal?, equivalent?,
 * ****************************************************************/
// (toml:date-year ts)
static s7_pointer toml_date_year(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_date_year);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_timestamp_t *ts = (toml_timestamp_t*)s7_c_object_value_checked(arg, toml_datetime_type_tag);
    if (!ts) {
        log_error("Bad arg, toml_date_year");
        log_error("obj typ: %d", s7_c_object_type(arg));
        log_error("expected type: %d", toml_datetime_type_tag);
        //FIXME: throw error
        return s7_unspecified(s7);
    } else {
        return s7_make_integer(s7,  *ts->year);
    }
}

// (toml:date-month ts)
static s7_pointer toml_date_month(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_date_month);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_timestamp_t *ts = (toml_timestamp_t*)s7_c_object_value_checked(arg, toml_datetime_type_tag);
    if (!ts) {
        log_error("Bad arg, toml_date_month");
        //FIXME: throw error
        return s7_unspecified(s7);
    } else {
        return s7_make_integer(s7,  *ts->month);
    }
}
// ... etc. ...

/* **************************************************************** */
static s7_pointer g_free_toml_datetime(s7_scheme *s7, s7_pointer obj)
{
    TRACE_ENTRY(g_free_toml_datetime);
    free(s7_c_object_value(obj));
    return(NULL);
}

static s7_pointer g_mark_toml_datetime(s7_scheme *s7, s7_pointer obj)
{
    TRACE_ENTRY(g_mark_toml_datetime);
  /* toml_datetime_t *t = (toml_datetime_t*)s7_c_object_value(obj); */
  /* s7_mark(o->data); */
  return(NULL);
}

s7_pointer g_is_toml_datetime(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_is_toml_datetime);
    return(s7_make_boolean(s7,
                           s7_is_c_object(s7_car(args)) &&
                           s7_c_object_type(s7_car(args)) == toml_datetime_type_tag));
}

static s7_pointer g_toml_datetime_is_equal(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_is_equal);
    return s7_nil(s7);
}

static s7_pointer g_toml_datetime_is_equivalent(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_is_equivalent);
    return s7_nil(s7);
}

static s7_pointer g_toml_datetime_ref(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_ref);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_timestamp_t *ts = (toml_timestamp_t*)s7_c_object_value_checked(arg, toml_datetime_type_tag);
    if (!ts) {
        return s7_f(s7);
    }
    p = s7_cdr(p);
    arg = s7_car(p);
    char* key;
    if (s7_is_string(arg)) {
        key = (char*)s7_string(arg);
    }
    else if (s7_is_symbol(arg)) {
        key = (char*)s7_symbol_name(arg);
    }
    else if (s7_is_keyword(arg)) {
        key = (char*)s7_keyword_to_symbol(s7, arg);
        /* key = (char*)s7_symbol_name(arg); */
    }
    else return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "toml:map-ref", 14), 2, arg, string_string));

    TRACE_LOG_DEBUG("ref key: %s", key);
    if ((strncmp(key, "year", 4) == 0) && (strlen(key) == 4)) {
        return(s7_make_integer(s7, *ts->year));
    }
    else if ((strncmp(key, "month", 5) == 0) && (strlen(key) == 5)) {
        return(s7_make_integer(s7, *ts->month));
    }
    else if ((strncmp(key, "day", 3) == 0) && (strlen(key) == 3)) {
        return(s7_make_integer(s7, *ts->day));
    }
    else if ((strncmp(key, "hour", 4) == 0) && (strlen(key) == 4)) {
        return(s7_make_integer(s7, *ts->hour));
    }
    else if ((strncmp(key, "minute", 6) == 0) && (strlen(key) == 6)) {
        return(s7_make_integer(s7, *ts->minute));
    }
    else if ((strncmp(key, "second", 6) == 0) && (strlen(key) == 6)) {
        return(s7_make_integer(s7, *ts->second));
    }
    else if ((strncmp(key, "millisecond", 11) == 0) && (strlen(key) == 11)) {
        return(s7_make_integer(s7, *ts->millisec));
    }
    else if ((strncmp(key, "offset", 6) == 0) && (strlen(key) == 6)) {
        /* log_debug("offset len: %d", strlen(ts->z)); */
        return(s7_make_string(s7, ts->z));
    }

    return s7_nil(s7);
}

static s7_pointer g_toml_datetime_set(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_set);
    return s7_nil(s7);
}

static s7_pointer g_toml_datetime_length(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_length);
    return s7_nil(s7);
}

static s7_pointer g_toml_datetime_copy(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_set);
    // UNSUPPORTED
    return s7_nil(s7);
}

static s7_pointer g_toml_datetime_fill(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_fill);
    // UNSUPPORTED
    return s7_nil(s7);
}

static s7_pointer g_toml_datetime_reverse(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_reverse);
    // UNSUPPORTED
    return s7_nil(s7);
}

static s7_pointer g_toml_datetime_to_list(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_to_list);
    /* s7_pointer p, arg; */
    /* p = args; */
    /* arg = s7_car(p); */
    //    toml_timestamp_t *ts = (toml_timestamp_t*)s7_c_object_value_checked(arg, toml_datetime_type_tag);

    /* bool clone = true; //FIXME: get from optional :clone arg */
    return s7_unspecified(s7);
}

// FIXME: conversion to srfi-19 struct?
static s7_pointer g_toml_datetime_to_alist(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_to_list);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_timestamp_t *ts = (toml_timestamp_t*)s7_c_object_value_checked(arg, toml_datetime_type_tag);

    bool clone = true; //FIXME: get optional :clone flag

    s7_pointer lst = toml_datetime_to_alist(s7, ts, clone);
    return lst;
}

static s7_pointer g_toml_datetime_to_hash_table(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_to_vector);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_timestamp_t *ts = (toml_timestamp_t*)s7_c_object_value_checked(arg, toml_datetime_type_tag);
    /* FIXME: handle error */

    return toml_datetime_to_hash_table(s7, ts);
}

static s7_pointer g_toml_datetime_to_string(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_to_string);
    TRACE_LOG_DEBUG("arg ct: %d", s7_list_length(s7, args));
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_timestamp_t *ts = (toml_timestamp_t*)s7_c_object_value_checked(arg, toml_datetime_type_tag);
    if (!ts) {
    } else {

    }

    bool use_write = false;
    p = s7_cdr(p);
    if (p != s7_nil(s7)) {
        arg = s7_car(p);
        TRACE_S7_DUMP("boolarg", arg);
        if (s7_is_boolean(arg)) {
            use_write = s7_boolean(s7, arg);
        } else {
            log_error("Bad use_write arg");
        }
    }

    char *s = tomlx_datetime_to_string(ts, use_write);
    TRACE_LOG_DEBUG("returning: %s", s);
    return s7_make_string(s7, s);
}

static s7_pointer g_toml_datetime_getter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_datetime_getter);
    return s7_nil(s7);
}

/* static s7_pointer toml_datetime_setter(s7_scheme *s7, s7_pointer args) */
/* { */
/*     TRACE_ENTRY(toml_datetime_setter); */
/*     return s7_nil(s7); */
/* } */

void toml_datetime_init(s7_scheme *s7, s7_pointer cur_env)
{
    TRACE_ENTRY(toml_datetime_init);
    toml_datetime_type_tag = s7_make_c_type(s7, "toml_datetime");
    /* TRACE_LOG_DEBUG("toml_datetime_type_tag: %d", toml_datetime_type_tag); */

    s7_c_type_set_gc_free      (s7, toml_datetime_type_tag, g_free_toml_datetime);
    s7_c_type_set_gc_mark      (s7, toml_datetime_type_tag, g_mark_toml_datetime);
    s7_c_type_set_is_equal     (s7, toml_datetime_type_tag, g_toml_datetime_is_equal);
    s7_c_type_set_is_equivalent(s7, toml_datetime_type_tag, g_toml_datetime_is_equivalent);
    s7_c_type_set_ref          (s7, toml_datetime_type_tag, g_toml_datetime_ref);
    s7_c_type_set_set          (s7, toml_datetime_type_tag, g_toml_datetime_set);
    s7_c_type_set_length       (s7, toml_datetime_type_tag, g_toml_datetime_length);
    s7_c_type_set_copy         (s7, toml_datetime_type_tag, g_toml_datetime_copy);
    s7_c_type_set_fill         (s7, toml_datetime_type_tag, g_toml_datetime_fill);
    s7_c_type_set_reverse      (s7, toml_datetime_type_tag, g_toml_datetime_reverse);
    s7_c_type_set_to_list      (s7, toml_datetime_type_tag, g_toml_datetime_to_list);
    s7_c_type_set_to_string    (s7, toml_datetime_type_tag, g_toml_datetime_to_string);

    s7_define_function(s7, "toml:datetime-getter",
                       g_toml_datetime_getter, 2, 0, false,
                       "(toml:datetime-getter t k) gets value for key k from array t");
    s7_c_type_set_getter       (s7, toml_datetime_type_tag, s7_name_to_value(s7, "toml:datetime-getter"));

    /* s7_define_function(s7, "toml:datetime-setter", */
    /*                    toml_datetime_setter, 2, 0, false, */
    /*                    "(toml:datetime-setter t k) sets value for key k from array t"); */
    /* s7_c_type_set_setter       (s7, toml_datetime_type_tag, s7_name_to_value(s7, "toml:datetime-setter")); */

    s7_define_function(s7, "toml:datetime?",
                       g_is_toml_datetime,
                       1, 0, false,
                       "(toml:datetime? obj) returns #t if its argument is a toml_datetime object");

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "toml:datetime-ref"),
              s7_make_typed_function(s7, "toml:datetime-ref",
                                     g_toml_datetime_ref,
                                     2, 0, false,
                                     "(toml:datetime-ref t k) returns value of datetime ts for field k", pl_xxs));

    s7_define_function(s7, "toml:date-year",
                       toml_date_year,
                       1, 0, false,
                       "(toml:date-year obj) returns the year value of a toml datetime.");

    s7_define_function(s7, "toml:date-month", toml_date_month, 1, 0, false,
                       "(toml:date-month obj) returns the month value of a toml datetime.");

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "toml:datetime->alist"),
              s7_make_typed_function(s7, "toml:datetime->alist",
                                     g_toml_datetime_to_alist,
                                     1, 0, false,
              "(toml:datetime->alist ts) converts toml datetime to s7 alist",
                                     pl_xx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "toml:datetime->hash-map"),
              s7_make_typed_function(s7, "toml:datetime->hash-table",
                                     g_toml_datetime_to_hash_table,
                                     1, 0, false,
              "(toml:datetime->hash-map ts) converts toml datetime to s7 hash-map",
                                     pl_xx));
}

/* ****************************************************************
 * Helper functions */

/*
  WARNING: toml: the 'T' in e.g. 1979-05-27T07:32:00Z is optional, a
  space may also be used. tomlc99 does not retain the character used,
  so we normalize to 'T'. That means '1979-05-27 07:32:00Z' (with
  space) will print as '1979-05-27T07:32:00Z' (with T).
 */
/* char *tomlx_datetime_to_string(toml_timestamp_t *ts, bool use_write) */
/* { */
/*     TRACE_ENTRY(tomlx_datetime_to_string); */
/*     TRACE_LOG_DEBUG("use_write: %d", use_write); */
/*     const int BUFSZ = 4096; */
/*     char *buf;          /\* WARNING: malloc *\/ */
/*     buf = calloc(BUFSZ, sizeof(char)); */
/*     if (!buf) { */
/*         log_error("OOM"); */
/*         return NULL; */
/*     } else { */
/*         TRACE_LOG_DEBUG("callocated %d chars for buffer", BUFSZ); */
/*     } */
/*     size_t bufsz = BUFSZ; */
/*     size_t char_ct = 0; */
/*     int ct; */
/*     (void)ct; */

/*     // print leading " */
/*     if (use_write) { */
/*         errno = 0; */
/*         TRACE_LOG_DEBUG("snprintfing header", ""); */
/*         // FIXME: check buf sz */
/*         ct = snprintf(buf, 2, "%s", "\""); */
/*         if (errno) { */
/*             log_error("snprintf: %s", strerror(errno)); */
/*             return NULL; */
/*         } else { */
/*             TRACE_LOG_DEBUG("snprintf hdr ct: %d", ct); */
/*         } */
/*         char_ct += 1; // do not include terminating '\0' */
/*     } */

/*     int zlen; */
/*     if (ts->z == NULL) { */
/*         zlen = 0; */
/*     } else { */
/*         zlen = strlen(ts->z); */
/*         log_debug("z: %s", ts->z); */
/*     } */
/*     log_debug("zlen %d", zlen); */

/*     if (ts->year == NULL) { */
/*         // local time */
/*     } */
/*     else if (ts->hour == NULL) { */
/*         // local date */
/*     } */
/*     else if (ts->millisec == NULL) { */
/*         log_debug("NO MILLIS"); */
/*         // e.g. 1979-05-27T07:32:00 */
/*         snprintf(buf + char_ct, 20 + zlen, "%.4d-%0.2d-%0.2dT%0.2d:%02.d:%0.2d%s", */
/*                  *ts->year, *ts->month, *ts->day, */
/*                  *ts->hour, *ts->minute, *ts->second, */
/*                  (zlen>0)? ts->z : "X"); */
/*         char_ct += 20 +  zlen - 1; */
/*     } */
/*     else { */
/*         log_debug("MILLIS"); */
/*         // tomlc99: only 3 decimal places for millis */
/*         // e.g. 1979-05-27T00:32:00.999999 */
/*         snprintf(buf + char_ct, 24 + zlen, "%.4d-%0.2d-%0.2dT%0.2d:%02.d:%0.2d.%d%s", */
/*                  *ts->year, *ts->month, *ts->day, */
/*                  *ts->hour, *ts->minute, *ts->second, */
/*                  *ts->millisec, */
/*                  (zlen>0)? ts->z : ""); */
/*         char_ct += 24 + zlen - 1; */
/*     } */
/*     log_debug("buf: %s", buf); */

/*     // print footer */
/*     if (use_write) { */
/*         errno = 0; */
/*         TRACE_LOG_DEBUG("snprintfing datetime footer", ""); */
/*         ct = snprintf(buf+char_ct, 2, "%s", "\""); */
/*         if (errno) { */
/*             log_error("snprintf: %s", strerror(errno)); */
/*             return NULL; */
/*         } else { */
/*             TRACE_LOG_DEBUG("snprintf footer ct: %d", ct); */
/*         } */
/*         char_ct += 1; // do not include terminating '\0' */
/*         TRACE_LOG_DEBUG("buf len: %d", strlen(buf)); */
/*         TRACE_LOG_DEBUG("buf: %s", buf); */
/*     } */
/*     TRACE_LOG_DEBUG("tomlx_datetime_to_string returning: %s", buf); */
/*     return buf; */
/* } */

s7_pointer toml_datetime_to_alist(s7_scheme *s7, toml_timestamp_t *ts, bool clone)
{
    TRACE_ENTRY(toml_datetime_to_alist);

    s7_pointer the_alist = s7_make_list(s7, (s7_int)8, s7_nil(s7));
    s7_pointer year = s7_cons(s7,
                              s7_make_keyword(s7, "year"),
                              s7_make_integer(s7, *ts->year));
    s7_list_set(s7, the_alist, (s7_int)0, year);
    TRACE_S7_DUMP("returning alist", the_alist);
    return the_alist;
}

s7_pointer toml_datetime_to_hash_table(s7_scheme *s7, toml_timestamp_t *ts)
{
    TRACE_ENTRY(toml_datetime_to_hash_table);

    s7_pointer ht = s7_make_hash_table(s7, 8);
    s7_hash_table_set(s7, ht, s7_make_keyword(s7, "year"),
                      s7_make_integer(s7, *ts->year));
    TRACE_S7_DUMP("returning hash-table", ht);
    return ht;
}


