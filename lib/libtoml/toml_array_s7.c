#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "libtoml_s7.h"
/* #include "toml_array_s7.h" */

int toml_array_type_tag = 0;

/*
 * OPS: vector?, vector-length, vector-ref
 * TODO:
 *  srfi-43, 133: vector-empty? vector-map, vector-fold,
 *  vector-for-each, etc.
 */

/* **** helper function prototypes, impl at bottom of file **** */
static toml_datum_t _toml_array_datum_for_idx(toml_array_t *ta,
                                              int idx,
                                              int *typ);
static void *_toml_array_seq_for_idx(toml_array_t *ta, int idx, int *typ);
static s7_pointer _toml_array_to_list(s7_scheme *s7, toml_array_t *ta);
static s7_pointer _toml_array_to_vector(s7_scheme *s7, toml_array_t *ta);

/* **************************************************************** */
static s7_pointer free_toml_array(s7_scheme *s7, s7_pointer obj)
{
    TRACE_ENTRY(free_toml_array);
    free(s7_c_object_value(obj));
    return(NULL);
}

static s7_pointer mark_toml_array(s7_scheme *s7, s7_pointer obj)
{
  /* toml_array_t *t = (toml_array_t*)s7_c_object_value(obj); */
  /* s7_mark(o->data); */
  return(NULL);
}

/* static */ s7_pointer is_toml_array(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(is_toml_array);
    /* TRACE_LOG_DEBUG("c obj?: %d", s7_is_c_object(s7_car(args))); */
    /* TRACE_LOG_DEBUG("c obj typ: %d", s7_c_object_type(s7_car(args))); */
    /* TRACE_LOG_DEBUG("toml-array typ: %d", toml_array_type_tag); */
  return(s7_make_boolean(s7,
		 s7_is_c_object(s7_car(args)) &&
                 s7_c_object_type(s7_car(args)) == toml_array_type_tag));
}

static s7_pointer toml_array_is_equal(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_is_equal);
    return s7_nil(s7);
}

static s7_pointer toml_array_is_equivalent(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_is_equivalent);
    return s7_nil(s7);
}

/* -------- toml_array_ref -------- */
s7_pointer toml_array_ref(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_toml_array_ref);
    s7_pointer p, arg;
    int idx;
    p = args;
    arg = s7_car(p);

    // array = (toml_array_t*)s7_c_pointer(arg);
    toml_array_t *a = (toml_array_t*)s7_c_object_value_checked(arg, toml_array_type_tag);
    if (!a) {
        log_error("Bad arg: expected toml array,...");
        return s7_unspecified(s7);
    }

    p = s7_cdr(p);
    arg = s7_car(p);
    TRACE_S7_DUMP("arg", arg);
    if (s7_is_integer(arg))
        idx = (int)s7_integer(arg);
    else return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "toml:array-ref", 15), 2, arg, integer_string));

    int a_sz = toml_array_nelem(a);
    if (idx >= a_sz) {
        log_error("Idx out-of-range: %d (a len: %d)", idx, a_sz);
        //TODO: include a_sz in error msg
        /* char *msg; */
        return s7_out_of_range_error(s7,
                                     "toml:array-ref",
                                     2, // s7_int arg_n,
                                     arg, // s7_pointer arg,
                                     "too large"); // const char *descr);
    }

    toml_datum_t datum;
    datum = toml_string_at(a, idx);
    if (datum.ok) { return(s7_make_string(s7, datum.u.s)); }

    datum = toml_bool_at(a, idx);
    if (datum.ok) { return(s7_make_boolean(s7, datum.u.b)); }

    datum = toml_int_at(a, idx);
    if (datum.ok) { return(s7_make_integer(s7, datum.u.i)); }

    datum = toml_double_at(a, idx);
    if (datum.ok) { return(s7_make_real(s7, datum.u.d)); }

    datum = toml_timestamp_at(a, idx);
    if (datum.ok) {
        /* not yet supported */
        return(s7_f(s7));
    }

    toml_array_t *array_ptr = toml_array_at(a, idx);
    if (array_ptr) {
        s7_pointer rval = s7_make_c_object(s7,
                                           toml_array_type_tag,
                                           (void*)a);
        return rval;
    }


    toml_table_t *table_val = toml_table_at(a, idx);
    if (table_val) {
        s7_pointer rval = s7_make_c_object(s7,
                                           toml_table_type_tag,
                                           (void*)a);
        return rval;
    }
    return(s7_f(s7));
}

/* static s7_pointer toml_array_ref(s7_scheme *s7, s7_pointer args) */
/* { */
/*     TRACE_ENTRY(toml_array_ref); */
/*     return s7_nil(s7); */
/* } */

static s7_pointer toml_array_set(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_set);
    return s7_nil(s7);
}

/* -------- toml_array_length -------- */
/* s7_pointer toml_toml_array_nelem(s7_scheme *sc, s7_pointer args) */
s7_pointer toml_array_length(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_length);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_array_t *a = (toml_array_t*)s7_c_object_value_checked(arg, toml_array_type_tag);
    if (a) {
        int nelem = toml_array_nelem(a);
        return(s7_make_integer(s7, nelem));
    } else {
        log_error("Bad arg, expected array, actual: %d", s7_c_object_type(arg));
        //FIXME: throw error
        return(s7_unspecified(s7));
    }
}

static s7_pointer toml_array_copy(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_set);
    // UNSUPPORTED
    return s7_nil(s7);
}

static s7_pointer toml_array_fill(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_set);
    // UNSUPPORTED
    return s7_nil(s7);
}

static s7_pointer toml_array_reverse(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_set);
    return s7_nil(s7);
}

static s7_pointer g_toml_array_to_list(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_array_to_list);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_array_t *ta = (toml_array_t*)s7_c_object_value_checked(arg, toml_array_type_tag);

    s7_pointer lst = _toml_array_to_list(s7, ta);
    return lst;
}

static s7_pointer g_toml_array_to_vector(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_array_to_vector);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_array_t *ta = (toml_array_t*)s7_c_object_value_checked(arg, toml_array_type_tag);

    s7_pointer vec = _toml_array_to_vector(s7, ta);
    return vec;
}

static s7_pointer g_toml_array_to_string(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_toml_array_to_string);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    toml_array_t *ta = (toml_array_t*)s7_c_object_value_checked(arg, toml_array_type_tag);

    char *s = toml_array_to_string(ta);
    TRACE_LOG_DEBUG("returning: %s", s);
    return s7_make_string(s7, s);
}

static s7_pointer toml_array_getter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_getter);
    return s7_nil(s7);
}

static s7_pointer toml_array_setter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(toml_array_setter);
    return s7_nil(s7);
}

void toml_array_init(s7_scheme *s7, s7_pointer cur_env)
{
    TRACE_ENTRY(toml_array_init);
    toml_array_type_tag = s7_make_c_type(s7, "toml_array");
    /* TRACE_LOG_DEBUG("toml_array_type_tag: %d", toml_array_type_tag); */

    s7_c_type_set_gc_free      (s7, toml_array_type_tag, free_toml_array);
    s7_c_type_set_gc_mark      (s7, toml_array_type_tag, mark_toml_array);
    s7_c_type_set_is_equal     (s7, toml_array_type_tag, toml_array_is_equal);
    s7_c_type_set_is_equivalent(s7, toml_array_type_tag, toml_array_is_equivalent);
    s7_c_type_set_ref          (s7, toml_array_type_tag, toml_array_ref);
    s7_c_type_set_set          (s7, toml_array_type_tag, toml_array_set);
    s7_c_type_set_length       (s7, toml_array_type_tag, toml_array_length);
    s7_c_type_set_copy         (s7, toml_array_type_tag, toml_array_copy);
    s7_c_type_set_fill         (s7, toml_array_type_tag, toml_array_fill);
    s7_c_type_set_reverse      (s7, toml_array_type_tag, toml_array_reverse);
    s7_c_type_set_to_list      (s7, toml_array_type_tag, g_toml_array_to_list);
    s7_c_type_set_to_string    (s7, toml_array_type_tag, g_toml_array_to_string);

    s7_define_function(s7, "toml:array-getter",
                       toml_array_getter, 2, 0, false,
                       "(toml:array-getter t k) gets value for key k from array t");
    s7_c_type_set_getter       (s7, toml_array_type_tag, s7_name_to_value(s7, "toml:array-getter"));

    s7_define_function(s7, "toml:array-setter",
                       toml_array_setter, 2, 0, false,
                       "(toml:array-setter t k) sets value for key k from array t");
    s7_c_type_set_setter       (s7, toml_array_type_tag, s7_name_to_value(s7, "toml:array-setter"));

    s7_define_function(s7, "toml:array?", is_toml_array, 1, 0, false,
                       "(toml:array? t) returns #t if its argument is a toml_array object");

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "toml:array-ref"),
              s7_make_typed_function(s7, "toml:array-ref",
                                     toml_array_ref, 2, 0, false,
              "(toml:array-ref a i) value of array a at index i",
                                     pl_xxs));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "toml:array->list"),
              s7_make_typed_function(s7, "toml:array->list",
                                     g_toml_array_to_list,
                                     1, 0, false,
              "(toml:array->list a) converts toml array to s7 list",
                                     pl_xx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "toml:array->vector"),
              s7_make_typed_function(s7, "toml:array->vector",
                                     g_toml_array_to_vector,
                                     1, 0, false,
              "(toml:array->vector a) converts toml array to s7 vector",
                                     pl_xx));
}

/* ****************************************************************
 * Helper functions
 */
static toml_datum_t _toml_array_datum_for_idx(toml_array_t *ta, int idx, int *typ)
{
    TRACE_ENTRY(_toml_array_datum_for_idx);
    toml_datum_t datum;

    datum = toml_string_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: s", "");
        *typ = TOML_STRING;
        return datum;
    }

    datum = toml_bool_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: b", "");
        *typ = TOML_BOOL;
        return datum;
    }

    datum = toml_int_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: i", "");
        *typ = TOML_INT;
        return datum;
    }

    datum = toml_double_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: d", "");
        *typ = TOML_DOUBLE;
        return datum;
    }

    datum = toml_timestamp_at(ta, idx);
    if (datum.ok) {
        TRACE_LOG_DEBUG("datum: ts", "");
        *typ = TOML_TIMESTAMP;
        /* not yet supported */
        return datum;
    }
    TRACE_LOG_DEBUG("datum: NULL", "");
    *typ = TOML_NONDATUM;
    return datum;
}

static void *_toml_array_seq_for_idx(toml_array_t *ta, int idx, int *typ)
{
    TRACE_ENTRY(_toml_array_seq_for_idx);

    toml_array_t *a = toml_array_at(ta, idx);
    if (a) {
        TRACE_LOG_DEBUG("array", "");
        *typ = TOML_ARRAY;
        return a;
    } else {
        TRACE_LOG_DEBUG("not array", "");
    }

    toml_table_t *subt = toml_table_at(ta, idx);
    if (subt) {
        TRACE_LOG_DEBUG("table: %p", subt);
        *typ = TOML_TABLE;
        return subt;
    } else {
        TRACE_LOG_DEBUG("not table", "");
    }
    return NULL;
}

char *toml_array_to_string(toml_array_t *ta)
{
    TRACE_ENTRY(toml_array_to_string);
    toml_datum_t datum;
    int typ;

    const int BUFSZ = 4096;
    char *buf;          /* WARNING: malloc */
    buf = calloc(BUFSZ, sizeof(char));
    if (!buf) {
        log_error("OOM");
        return NULL;
    } else {
        TRACE_LOG_DEBUG("callocated %d chars for buffer", BUFSZ);
    }
    size_t bufsz = BUFSZ;
    size_t char_ct = 0;
    int ct;

    // print header
    {
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing header", "");
        ct = snprintf(buf, 2, "%s", "[");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TRACE_LOG_DEBUG("snprintf hdr ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
        TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
        TRACE_LOG_DEBUG("buf: %s", buf);
    }

    // print elements
    int idx_ct = toml_array_nelem(ta);
    /* char *k, *v; */
    int len;
    for (int i = 0; i < idx_ct; i++) {
        // print comma
        if (i > 0) {
            if ((char_ct + 3) > bufsz) {
                log_error("realloc for comma");
            } else {
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing comma", "");
                ct = snprintf(buf+char_ct, 3, "%s", ", ");
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf comma ct: %d", ct);
                }
                char_ct += 2; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
            }
        }

        // print value
        datum = _toml_array_datum_for_idx(ta, i, &typ);
        char *seq_str;
        TRACE_LOG_DEBUG("datum typ: %d", typ);
        if (typ == TOML_NONDATUM) {
            void *seq = _toml_array_seq_for_idx(ta, i, &typ);
            switch(typ) {
            case TOML_ARRAY:
                TRACE_LOG_DEBUG("array seq: %p", seq);
                seq_str = toml_array_to_string((toml_array_t*)seq);
                TRACE_LOG_DEBUG("ARRAY str: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing array len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_TABLE:
                TRACE_LOG_DEBUG("table seq: %p", seq);
                seq_str = toml_table_to_string((toml_table_t*)seq);
                TRACE_LOG_DEBUG("TABLE: %s", seq_str);
                len = strlen(seq_str) + 1;  // + 1 for '\0'
                if ((char_ct + len) > bufsz) {
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing table len %d", len);
                ct = snprintf(buf+char_ct, len, "%s = ", seq_str);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1; // do not include terminating '\0'
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            default:
                log_error("Bad toml seq type: %d", typ);
            }
        } else {
            switch(typ) {
            case TOML_INT:
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.i);
                len = snprintf(NULL, 0, "%lld", datum.u.i);
                len++; // for terminating '\0';
                TRACE_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%lld", datum.u.i);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_STRING:
                TRACE_LOG_DEBUG("toml datum val: %s", datum.u.s);
                // add 2 for quotes
                len = snprintf(NULL, 0, "%s", datum.u.s) + 2;
                len++; // for terminating '\0';
                TRACE_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing string, len %d", len);
                ct = snprintf(buf+char_ct, len, "\"%s\"", datum.u.s);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                free(datum.u.s);
                break;
            case TOML_BOOL:
                // tomlc99 bool val is int
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.b);
                if (datum.u.b) {
                    len = 5; // "true" + \0
                } else {
                    len = 6; // "false" + \0
                }
                TRACE_LOG_DEBUG("bool str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing len %d", len);
                if (datum.u.b) {
                    ct = snprintf(buf+char_ct, len, "%s", "true");
                } else {
                    ct = snprintf(buf+char_ct, len, "%s", "false");
                }
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
                break;
            case TOML_DOUBLE:
                TRACE_LOG_DEBUG("toml datum val: %g", datum.u.d);
                len = snprintf(NULL, 0, "%g", datum.u.d);
                len++; // for terminating '\0';
                TRACE_LOG_DEBUG("int str sz: %d", len);
                if ((char_ct + len) > bufsz) { // + 1 for '\0'
                    log_error("exceeded bufsz: %d", char_ct + len);
                    // expand buf
                }
                errno = 0;
                TRACE_LOG_DEBUG("snprintfing len %d", len);
                ct = snprintf(buf+char_ct, len, "%g", datum.u.d);
                if (errno) {
                    log_error("snprintf: %s", strerror(errno));
                    break;
                } else {
                    TRACE_LOG_DEBUG("snprintf ct: %d", ct);
                }
                char_ct += len - 1;
                TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
                TRACE_LOG_DEBUG("buf: %s", buf);
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
        TRACE_LOG_DEBUG("snprintfing footer", "");
        ct = snprintf(buf+char_ct, 2, "%s", "]");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TRACE_LOG_DEBUG("snprintf hdr ct: %d", ct);
        }
        char_ct += 1; // do not include terminating '\0'
        TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
        TRACE_LOG_DEBUG("buf: %s", buf);
    }
    TRACE_LOG_DEBUG("toml_array_to_string returning: %s", buf);
    return buf;
}

static s7_pointer _toml_array_to_list(s7_scheme *s7, toml_array_t *ta)
{
    TRACE_ENTRY(toml_array_to_list);

    /* s7_pointer the_list = s7_nil(s7); */

    toml_datum_t datum;
    int typ;
    int len;
    int idx_ct = toml_array_nelem(ta);
    s7_pointer the_list = s7_make_list(s7, idx_ct, s7_nil(s7));
    s7_pointer subarray, subtable;
    for (int i = 0; i < idx_ct; i++) {

        datum = _toml_array_datum_for_idx(ta, i, &typ);
        char *seq_str;
        TRACE_LOG_DEBUG("datum typ: %d", typ);
        if (typ == TOML_NONDATUM) {
            void *seq = _toml_array_seq_for_idx(ta, i, &typ);
            switch(typ) {
            case TOML_ARRAY:
                TRACE_LOG_DEBUG("recurring subarray to list", "");
                subarray = _toml_array_to_list(s7, seq);
                TRACE_S7_DUMP("recd subarray", subarray);
                s7_list_set(s7, the_list, i, subarray);
                break;
            case TOML_TABLE:
                TRACE_LOG_DEBUG("table seq: %p", seq);
                /* subtable = toml_table_to_hash_table(s7, (toml_table_t*)seq); */
                break;
            default:
                log_error("Bad toml seq type: %d", typ);
            }
        } else {
            switch(typ) {
            case TOML_INT:
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.i);
                /* the_list = s7_cons(s7, the_list, */
                /*                    s7_make_integer(s7, datum.u.i)); */
                s7_list_set(s7, the_list, i, s7_make_integer(s7, datum.u.i));
                break;
            case TOML_STRING:
                TRACE_LOG_DEBUG("toml datum val: %s", datum.u.s);
                break;
            case TOML_BOOL:
                // tomlc99 bool val is int
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.b);
                if (datum.u.b) {
                    len = 5; // "true" + \0
                } else {
                    len = 6; // "false" + \0
                }
                break;
            case TOML_DOUBLE:
                TRACE_LOG_DEBUG("toml datum val: %g", datum.u.d);
                break;
            case TOML_TIMESTAMP:
                log_error("toml timestamp (not yet)");
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
    TRACE_S7_DUMP("returning list", the_list);
    return the_list;
}

static s7_pointer _toml_array_to_vector(s7_scheme *s7, toml_array_t *ta)
{
    TRACE_ENTRY(toml_array_to_vector);

    /* s7_pointer the_list = s7_nil(s7); */

    toml_datum_t datum;
    int typ;
    int len;
    int idx_ct = toml_array_nelem(ta);
    s7_pointer the_vector = s7_make_vector(s7, idx_ct);
    s7_pointer subarray, subtable;
    for (int i = 0; i < idx_ct; i++) {

        datum = _toml_array_datum_for_idx(ta, i, &typ);
        char *seq_str;
        TRACE_LOG_DEBUG("datum typ: %d", typ);
        if (typ == TOML_NONDATUM) {
            void *seq = _toml_array_seq_for_idx(ta, i, &typ);
            switch(typ) {
            case TOML_ARRAY:
                TRACE_LOG_DEBUG("recurring subarray to list", "");
                subarray = _toml_array_to_list(s7, seq);
                TRACE_S7_DUMP("recd subarray", subarray);
                s7_vector_set(s7, the_vector, i, subarray);
                break;
            case TOML_TABLE:
                TRACE_LOG_DEBUG("table seq: %p", seq);
                // if clone flag #t, call table_to_hashtable
                /* seq_str = toml_table_to_string((toml_table_t*)seq); */
                /* TRACE_LOG_DEBUG("TABLE: %s", seq_str); */
                /* len = strlen(seq_str) + 1;  // + 1 for '\0' */
                /* if ((char_ct + len) > bufsz) { */
                /*     log_error("exceeded bufsz: %d", char_ct + len); */
                /*     // expand buf */
                /* } */
                /* errno = 0; */
                /* TRACE_LOG_DEBUG("snprintfing array len %d", len); */
                /* ct = snprintf(buf+char_ct, len, "%s = ", seq_str); */
                /* if (errno) { */
                /*     log_error("snprintf: %s", strerror(errno)); */
                /*     break; */
                /* } else { */
                /*     TRACE_LOG_DEBUG("snprintf ct: %d", ct); */
                /* } */
                /* char_ct += len - 1; // do not include terminating '\0' */
                /* TRACE_LOG_DEBUG("buf len: %d", strlen(buf)); */
                /* TRACE_LOG_DEBUG("buf: %s", buf); */
                break;
            default:
                log_error("Bad toml seq type: %d", typ);
            }
        } else {
            switch(typ) {
            case TOML_INT:
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.i);
                /* the_vector = s7_cons(s7, the_vector, */
                /*                    s7_make_integer(s7, datum.u.i)); */
                s7_vector_set(s7, the_vector, i, s7_make_integer(s7, datum.u.i));
                log_debug("xxxx");
                break;
            case TOML_STRING:
                TRACE_LOG_DEBUG("toml datum val: %s", datum.u.s);
                break;
            case TOML_BOOL:
                // tomlc99 bool val is int
                TRACE_LOG_DEBUG("toml datum val: %d", datum.u.b);
                if (datum.u.b) {
                    len = 5; // "true" + \0
                } else {
                    len = 6; // "false" + \0
                }
                break;
            case TOML_DOUBLE:
                TRACE_LOG_DEBUG("toml datum val: %g", datum.u.d);
                break;
            case TOML_TIMESTAMP:
                log_error("toml timestamp (not yet)");
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
    TRACE_S7_DUMP("returning vector", the_vector);
    return the_vector;
}
