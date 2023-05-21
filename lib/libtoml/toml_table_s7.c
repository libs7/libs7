#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "libtoml_s7.h"
/* #include "toml_table_s7.h" */
/* #include "toml_array_s7.h" */

int toml_table_type_tag = 0;

s7_pointer string_string;

/*
 * TODO:
 *  srfi-69: toml:table->alist, toml:table->fold, etc.
 */

/* **** helper function prototypes, impl at bottom of file **** */
static toml_datum_t _toml_table_datum_for_key(toml_table_t *tt,
                                              char *key,
                                              int *typ);
static void *_toml_table_seq_for_key(toml_table_t *tt, char *key, int *typ);

/* **************************************************************** */
/*
  The toml_table_t returned by toml_parse() and toml_parser_file must
  be freed, but the table pointers projected from those tables need
  not be freed.
 */
static s7_pointer free_toml_table(s7_scheme *s7, s7_pointer obj)
{
    TRACE_ENTRY(free_toml_table);
    free(s7_c_object_value(obj));
    return(NULL);
}

static s7_pointer mark_toml_table(s7_scheme *s7, s7_pointer obj)
{
  /* toml_table_t *t = (toml_table_t*)s7_c_object_value(obj); */
  /* s7_mark(o->data); */
  return(NULL);
}

/* static */ s7_pointer is_toml_table(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(is_toml_table);
    return(s7_make_boolean(s7,
              s7_is_c_object(s7_car(args)) &&
              s7_c_object_type(s7_car(args)) == toml_table_type_tag));
}

static s7_pointer toml_table_is_equal(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_is_equal);
    return s7_nil(s7);
}

static s7_pointer toml_table_is_equivalent(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_is_equivalent);
    return s7_nil(s7);
}

/* returns list */
s7_pointer toml_table_keys(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_keys);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_table_t *t = (toml_table_t*)s7_c_object_value_checked(arg, toml_table_type_tag);

    /* tomlc99 has no 'keys' fn, instead we iterate over the entries
       by integer index using toml_key_in
     */
    int ntab = toml_table_ntab(t);
    int narr = toml_table_narr(t);
    int nkv = toml_table_nkval(t);
    int key_ct = ntab + narr + nkv;
    s7_pointer keys = s7_make_list(s7, key_ct, s7_nil(s7));
    const char* k;
    for (int i = 0; i < key_ct; i++) {
        k = toml_key_in(t, i);
        /* log_debug("table key: %s", k); */
        s7_list_set(s7, keys, i, s7_make_string(s7, k));
    }
    /* TRACE_S7_DUMP("keys", keys); */
    return keys;
}

/* returns list */
s7_pointer toml_table_values(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_vals);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_table_t *t = (toml_table_t*)s7_c_object_value_checked(arg, toml_table_type_tag);

    /* tomlc99 has no 'keys' fn, instead we iterate over the entries
       by integer index using toml_key_in
     */
    int ntab = toml_table_ntab(t);
    int narr = toml_table_narr(t);
    int nkv = toml_table_nkval(t);
    int key_ct = ntab + narr + nkv;
    s7_pointer keys = s7_make_list(s7, key_ct, s7_nil(s7));
    const char* k;
    for (int i = 0; i < key_ct; i++) {
        k = toml_key_in(t, i);
        /* log_debug("table key: %s", k); */
        //FIXME: get value for key
        s7_list_set(s7, keys, i, s7_make_string(s7, k));
    }
    /* TRACE_S7_DUMP("keys", keys); */
    return s7_unspecified(s7);
}

s7_pointer toml_table_ref(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_ref);
    s7_pointer p, arg;
    char* key;
    p = args;
    arg = s7_car(p);
    toml_table_t *t = (toml_table_t*)s7_c_object_value_checked(arg, toml_table_type_tag);

    p = s7_cdr(p);
    arg = s7_car(p);
    if (s7_is_string(arg))
        key = (char*)s7_string(arg);
    else return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "toml:table-ref", 14), 2, arg, string_string));

    toml_datum_t datum;

    /* datum = _toml_table_value_for_key(t, key); */

    datum = toml_string_in(t, key);
    if (datum.ok) {
        s7_pointer s = s7_make_string(s7, datum.u.s);
        free(datum.u.s);
        return s;
    }

    datum = toml_bool_in(t, key);
    if (datum.ok) { return(s7_make_boolean(s7, datum.u.b)); }

    datum = toml_int_in(t, key);
    if (datum.ok) { return(s7_make_integer(s7, datum.u.i)); }

    datum = toml_double_in(t, key);
    if (datum.ok) { return(s7_make_real(s7, datum.u.d)); }

    datum = toml_timestamp_in(t, key);
    if (datum.ok) {
        /* not yet supported */
        return(s7_f(s7));
    }

    toml_array_t *a = toml_array_in(t, key);
    if (a) {
        /* log_debug("array"); */
        s7_pointer rval = s7_make_c_object(s7,
                                           toml_array_type_tag,
                                           (void*)a);
        return rval;
    } else {
        /* log_debug("not array"); */
    }

    toml_table_t *subt = toml_table_in(t, key);
    if (t) {
        /* log_debug("table: %p", subt); */
        s7_pointer rval = s7_make_c_object(s7,
                                           toml_table_type_tag,
                                           (void*)subt);
        /* void *optr = (void*)s7_c_object_value(rval); */
        /* void *optr = (void*)s7_c_object_value_checked(rval, toml_table_type_tag); */
        /* log_debug("rval ptr: %p", rval); */
        /* log_debug("rval objptr: %p", optr); */

        return rval;
    } else {
        /* log_debug("not table"); */
    }

    log_debug("returning #f");
    return(s7_f(s7));
}

static s7_pointer toml_table_set(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_set);
    return s7_nil(s7);
}

s7_pointer toml_table_length(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_length);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);

    void *t = (void*)s7_c_object_value_checked(arg, toml_table_type_tag);
    if (t) {
        int ntab = toml_table_ntab(t);
        int narr = toml_table_narr(t);
        int nkv = toml_table_nkval(t);
        s7_pointer i = s7_make_integer(s7, ntab + narr + nkv);
        return(i);
    } else {
        log_error("Bad arg, expected table, actual: %d", s7_c_object_type(arg));
        //FIXME: throw error
        return(s7_unspecified(s7));
    }
}

static s7_pointer toml_table_copy(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_set);
    return s7_nil(s7);
}

static s7_pointer toml_table_fill(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_set);
    return s7_nil(s7);
}

static s7_pointer toml_table_reverse(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_set);
    return s7_nil(s7);
}

static s7_pointer g_toml_table_to_alist(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_table_to_alist);
    return s7_f(s7);
    return s7_list(s7, 3,
                   s7_make_symbol(s7, "toml-table"),
                   s7_make_keyword(s7, "fld1"),
                   s7_make_integer(s7, 99));
}

static s7_pointer toml_table_to_list(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_to_list);
    return s7_f(s7);
    return s7_list(s7, 3,
                   s7_make_symbol(s7, "toml-table"),
                   s7_make_keyword(s7, "fld1"),
                   s7_make_integer(s7, 99));
}

static s7_pointer g_toml_table_to_string(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_table_to_string);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_table_t *tt = (toml_table_t*)s7_c_object_value_checked(arg, toml_table_type_tag);

    char *s = toml_table_to_string(tt);
    log_debug("returning: %s", s);
    return s7_make_string(s7, s);
}

static s7_pointer toml_table_getter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_getter);
    return s7_nil(s7);
}

static s7_pointer toml_table_setter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_table_setter);
    return s7_nil(s7);
}

void toml_table_init(s7_scheme *s7, s7_pointer cur_env)
{
    TRACE_ENTRY(toml_table_init);
    toml_table_type_tag = s7_make_c_type(s7, "toml_table");
    /* log_debug("toml_table_type_tag: %d", toml_table_type_tag); */

    s7_c_type_set_gc_free      (s7, toml_table_type_tag, free_toml_table);
    s7_c_type_set_gc_mark      (s7, toml_table_type_tag, mark_toml_table);
    s7_c_type_set_is_equal     (s7, toml_table_type_tag, toml_table_is_equal);
    s7_c_type_set_is_equivalent(s7, toml_table_type_tag, toml_table_is_equivalent);
    s7_c_type_set_ref          (s7, toml_table_type_tag, toml_table_ref);
    s7_c_type_set_set          (s7, toml_table_type_tag, toml_table_set);
    s7_c_type_set_length       (s7, toml_table_type_tag, toml_table_length);
    s7_c_type_set_copy         (s7, toml_table_type_tag, toml_table_copy);
    s7_c_type_set_fill         (s7, toml_table_type_tag, toml_table_fill);
    s7_c_type_set_reverse      (s7, toml_table_type_tag, toml_table_reverse);
    /* s7_c_type_set_to_list      (s7, toml_table_type_tag, toml_table_to_list); */
    s7_c_type_set_to_list      (s7, toml_table_type_tag, g_toml_table_to_alist);
    s7_c_type_set_to_string    (s7, toml_table_type_tag, g_toml_table_to_string);

    s7_define_function(s7, "toml:table-getter",
                       toml_table_getter, 2, 0, false,
                       "(toml:table-getter t k) gets value for key k from table t");
    s7_c_type_set_getter       (s7, toml_table_type_tag, s7_name_to_value(s7, "toml:table-getter"));

    s7_define_function(s7, "toml:table-setter",
                       toml_table_setter, 2, 0, false,
                       "(toml:table-setter t k) sets value for key k from table t");
    s7_c_type_set_setter       (s7, toml_table_type_tag, s7_name_to_value(s7, "toml:table-setter"));

    s7_define_function(s7, "toml:table?", is_toml_table, 1, 0, false,
                       "(toml:table? t) returns #t if its argument is a toml_table object");

    string_string = s7_make_semipermanent_string(s7, "a string");
}

/* ****************************************************************
 * Helper functions
 */
static toml_datum_t _toml_table_datum_for_key(toml_table_t *tt, char *key, int *typ)
{
    TRACE_ENTRY(_toml_table_datum_for_key);
    toml_datum_t datum;

    datum = toml_string_in(tt, key);
    if (datum.ok) {
        log_debug("datum: s");
        *typ = TOML_STRING;
        return datum;
    }

    datum = toml_bool_in(tt, key);
    if (datum.ok) {
        log_debug("datum: b");
        *typ = TOML_BOOL;
        return datum;
    }

    datum = toml_int_in(tt, key);
    if (datum.ok) {
        log_debug("datum: i");
        *typ = TOML_INT;
        return datum;
    }

    datum = toml_double_in(tt, key);
    if (datum.ok) {
        log_debug("datum: d");
        *typ = TOML_DOUBLE;
        return datum;
    }

    datum = toml_timestamp_in(tt, key);
    if (datum.ok) {
        log_debug("datum: ts");
        *typ = TOML_TIMESTAMP;
        /* not yet supported */
        return datum;
    }
    log_debug("datum: NULL");
    *typ = TOML_NONDATUM;
    return datum;
}

static void *_toml_table_seq_for_key(toml_table_t *tt, char *key, int *typ)
{
    TRACE_ENTRY(_toml_table_seq_for_key);

    toml_array_t *a = toml_array_in(tt, key);
    if (a) {
        log_debug("array");
        *typ = TOML_ARRAY;
        return a;
    } else {
        log_debug("not array");
    }

    toml_table_t *subt = toml_table_in(tt, key);
    if (subt) {
        log_debug("table: %p", subt);
        *typ = TOML_TABLE;
        return subt;
    } else {
        log_debug("not table");
    }
    return NULL;
}

char *toml_table_to_string(toml_table_t *tt)
{
    TRACE_ENTRY(toml_table_to_string);
    toml_datum_t datum;
    int typ;

    const int BUFSZ = 4096;
    char *buf;          /* WARNING: malloc */
    buf = calloc(BUFSZ, sizeof(char));
    if (!buf) {
        log_error("OOM");
        return NULL;
    } else {
        log_debug("callocated %d chars for buffer", BUFSZ);
    }
    size_t bufsz = BUFSZ;
    int char_ct = 0;
    int ct;

    // print header
    {
        errno = 0;
        log_debug("snprintfing header");
        ct = snprintf(buf, 14, "%s", "<#toml-table ");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            log_debug("snprintf hdr ct: %d", ct);
        }
        char_ct += 13; // do not include terminating '\0'
        log_debug("buf len: %d", strlen(buf));
        log_debug("buf: %s", buf);
    }

    // print fields
    int ntab = toml_table_ntab(tt);
    int narr = toml_table_narr(tt);
    int nkv = toml_table_nkval(tt);
    int key_ct = ntab + narr + nkv;
    char *k, *v;
    int len;
    for (int i = 0; i < key_ct; i++) {
        k = toml_key_in(tt, i);
        if (!k) {
            log_error("toml_key_in failure for idx: %d", i);
            return NULL;
        }
        log_debug("table key: %s", k);

        // print comma
        if (i > 0) {
            if ((char_ct + 3) > bufsz) {
                log_error("realloc for comma");
            } else {
                errno = 0;
                log_debug("snprintfing comma");
                ct = snprintf(buf+char_ct, 3, "%s", ", ");
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    log_debug("snprintf comma ct: %d", ct);
                }
                char_ct += 2; // do not include terminating '\0'
                log_debug("buf len: %d", strlen(buf));
                log_debug("buf: %s", buf);
            }
        }

        // print key to buf
        {
            len = strlen(k) + 3; // for " = "
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) { // + 1 for '\0'
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            errno = 0;
            log_debug("snprintfing key len %d", len);
            ct = snprintf(buf+char_ct, len, "%s = ", k);
            if (errno) {
                log_error("snprintf: %s", strerror(errno));
                break;
            } else {
                log_debug("snprintf ct: %d", ct);
            }
            char_ct += len - 1; // do not include terminating '\0'
            log_debug("buf len: %d", strlen(buf));
            log_debug("buf: %s", buf);
        }

        // print value
        datum = _toml_table_datum_for_key(tt, k, &typ);
        char *seq_str;
        log_debug("datum typ: %d", typ);
        if (typ == TOML_NONDATUM) {
            void *seq = _toml_table_seq_for_key(tt, k, &typ);
            switch(typ) {
            case TOML_ARRAY:
                log_debug("array seq: %p", seq);
                seq_str = toml_array_to_string((toml_array_t*)seq);
                log_debug("ARRAY: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                log_debug("snprintfing array len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    log_debug("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                log_debug("buf len: %d", strlen(buf));
                log_debug("buf: %s", buf);
                break;
            case TOML_TABLE:
                log_debug("table seq: %p", seq);
                seq_str = toml_table_to_string((toml_table_t*)seq);
                log_debug("TABLE: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                log_debug("snprintfing array len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    log_debug("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                log_debug("buf len: %d", strlen(buf));
                log_debug("buf: %s", buf);
                break;
            default:
                log_error("Bad toml seq type: %d", typ);
            }
        } else {
            switch(typ) {
            case TOML_INT:
                log_debug("toml datum val: %d", datum.u.i);
                len = snprintf(NULL, 0, "%d", datum.u.i);
                len++; // for terminating '\0';
                log_debug("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                log_debug("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%d", datum.u.i);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    log_debug("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                log_debug("buf len: %d", strlen(buf));
                log_debug("buf: %s", buf);
                break;
            case TOML_STRING:
                log_debug("toml datum val: %s", datum.u.s);
                // add 2 for quotes
                len = snprintf(NULL, 0, "%s", datum.u.s) + 2;
                len++; // for terminating '\0';
                log_debug("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                log_debug("snprintfing string, len %d", len);
                ct = snprintf(buf+char_ct, len, "'%s'", datum.u.s);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    log_debug("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                log_debug("buf len: %d", strlen(buf));
                log_debug("buf: %s", buf);
                free(datum.u.s);
                break;
            case TOML_BOOL:
                // tomlc99 bool val is int
                log_debug("toml datum val: %d", datum.u.b);
                if (datum.u.b) {
                    len = 5; // "true" + \0
                } else {
                    len = 6; // "false" + \0
                }
                log_debug("bool str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                log_debug("snprintfing len %d", len);
                if (datum.u.b) {
                    ct = snprintf(buf+char_ct, len, "%s", "true");
                } else {
                    ct = snprintf(buf+char_ct, len, "%s", "false");
                }
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    log_debug("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                log_debug("buf len: %d", strlen(buf));
                log_debug("buf: %s", buf);
                break;
            case TOML_DOUBLE:
                log_debug("toml datum val: %g", datum.u.d);
                len = snprintf(NULL, 0, "%g", datum.u.d);
                len++; // for terminating '\0';
                log_debug("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                log_debug("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%g", datum.u.d);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    log_debug("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                log_debug("buf len: %d", strlen(buf));
                log_debug("buf: %s", buf);
                break;
            case TOML_TIMESTAMP:
                log_error("toml timestamp (not yet)");
                if ((char_ct + 18) > bufsz) {
                    // realloc
                } else {
                    errno = 0;
                    snprintf(buf, 18, "%s", "<#toml-timestamp>");
                    if (errno) {
                        log_error("snprintf: %s", strerror(errno));
                        break;
                    }
                    char_ct += 18;
                }
                break;
            case TOML_NONDATUM:
                // should not happen
                log_error("Unexpected TOML_NON_DATUM");
                //FIXME: throw error
                return NULL;
                break;
            default:
                log_error("Bad toml_datum constant: %d", typ);
                //FIXME: throw error
                return NULL;
            }
        }
    }

    // print footer
    {
        errno = 0;
        log_debug("snprintfing footer");
        ct = snprintf(buf+char_ct, 2, "%s", ">");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            log_debug("snprintf hdr ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
        log_debug("buf len: %d", strlen(buf));
        log_debug("buf: %s", buf);
    }
    log_debug("toml_table_to_string returning: %s", buf);
    return buf;
}
