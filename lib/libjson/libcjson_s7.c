#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "trace.h"

#include "cJSON.h"
#include "libjson_s7.h"
#include "libs7.h"

#if defined(DEBUGGING)
#define CJSON_TYPE_NAME(x) (char*)#x
char *cjson_types[256] = {
    [cJSON_Invalid] = CJSON_TYPE_NAME(cJSON_Invalid), // (0)
    [cJSON_False]   = CJSON_TYPE_NAME(cJSON_False),   // (1 << 0)
    [cJSON_True]    = CJSON_TYPE_NAME(cJSON_True),    // (1 << 1)
    [cJSON_NULL]    = CJSON_TYPE_NAME(cJSON_NULL),    // (1 << 2)
    [cJSON_Number]  = CJSON_TYPE_NAME(cJSON_Number),  // (1 << 3)
    [cJSON_String]  = CJSON_TYPE_NAME(cJSON_String),  // (1 << 4)
    [cJSON_Array]   = CJSON_TYPE_NAME(cJSON_Array),   // (1 << 5)
    [cJSON_Object]  = CJSON_TYPE_NAME(cJSON_Object),  // (1 << 6)
    [cJSON_Raw]     = CJSON_TYPE_NAME(cJSON_Raw) // (1 << 7) /* raw json */
};
#endif

int json_object_type_tag = 0;
int json_array_type_tag = 0;

/* signature stuff */
s7_pointer pl_tx, pl_xs, pl_bxs, pl_xi, pl_xx, pl_txs, pl_xxs, pl_xsi, pl_xxi, pl_xsxt, pl_xsixt, pl_dx, pl_st, pl_sx, pl_ix;

// local prototypes
static char *json_array_to_string(s7_scheme *s7, const cJSON *ja);
static char *json_object_to_string(s7_scheme *s7, const cJSON *jo);

static s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string, integer_string;
static s7_pointer cJSON__symbol, char___symbol, cJSON__symbol;

/* **************************************************************** */
/*  s7 integration API */
/* **************************************************************** */
static s7_pointer free_json_object(s7_scheme *s7, s7_pointer obj)
{
    TRACE_ENTRY(free_json_object);
    free(s7_c_object_value(obj));
    return(NULL);
}

static s7_pointer mark_json_object(s7_scheme *s7, s7_pointer obj)
{
  /* json_object_t *t = (json_object_t*)s7_c_object_value(obj); */
  /* s7_mark(o->data); */
  return(NULL);
}

static s7_pointer json_object_is_equal(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_object_is_equal);
    return s7_nil(s7);
}

static s7_pointer json_object_is_equivalent(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_object_is_equivalent);
    return s7_nil(s7);
}

/* returns list */
s7_pointer g_json_object_keys(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_object_keys);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;
    cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_object_type_tag);

    int key_ct = cJSON_GetArraySize(jo);
    TRACE_LOG_DEBUG("JO sz: %d", key_ct);

    s7_pointer key_list = s7_make_list(s7, key_ct, s7_nil(s7));

    cJSON *k;
    for (int i = 0; i < key_ct; i++) {
        k = cJSON_GetArrayItem(jo, i);
        if (!k) {
            log_error("cJSON_GetArrayItem failure for key: %d", i);
            return NULL;
        }
#if defined(DEBUGGING)
        log_debug(GRN "object item" CRESET " %d: k->string: %s, k->type: %s [%d]",
                  i, k->string, cjson_types[k->type], k->type);
#endif
        s7_list_set(s7, key_list, (s7_int)i,
                    s7_make_string(s7, k->string));

        /* switch(item->type) { */
        /* case cJSON_String: */
        /*     break; */
        /* case cJSON_Number: */
        /*     break; */
        /* case cJSON_False: */
        /*     break; */
        /* case cJSON_True: */
        /*     break; */
        /* case cJSON_Array: */
        /*     break; */
        /* case cJSON_Object: */
        /*     break; */
        /* case cJSON_NULL: */
        /*     break; */
        /* case cJSON_Raw: */
        /*     break; */
        /* default: */
        /*     log_error("Bad jo->type"); */
        /* } */
    }
    return(key_list);
}

/* returns list */
s7_pointer json_object_values(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_object_vals);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;
    /* json_object_t *t = (json_object_t*)s7_c_object_value_checked(arg, json_object_type_tag); */

    return s7_unspecified(s7);
}

s7_pointer g_json_object_ref(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_object_ref);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);            /* arg 0: json obj */
    if (arg == NULL) {
        log_error("Bad arg: NULL");
        return s7_error(s7,
                        s7_make_symbol(s7, "map-ref"),
                        s7_cons(s7, s7_make_string(s7, "arg 0: expected json object, got NULL"),
                                s7_nil(s7)));
    }
    cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_object_type_tag);
    if (jo == NULL) {
        log_error("Bad object arg");
        return s7_unspecified(s7);
    }

    p = s7_cdr(p);
    arg = s7_car(p);            /* arg 1: string key */
    char* key;
    if (s7_is_string(arg))
        key = (char*)s7_string(arg);
    else {
        return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "toml:table-ref", 14), 2, arg, string_string));
    }
    TRACE_LOG_DEBUG("arg 1, key: %s", key);

    cJSON *item = cJSON_GetObjectItemCaseSensitive(jo, key);
#ifdef DEBUGGING
    log_debug("item type: [%d]%s", item->type, cjson_types[item->type]);
#endif

    char tmp[64];               /* for expanding doubles */
    char *decimal = NULL;
    s7_pointer result;
    switch(item->type) {
    case cJSON_String:
        result = s7_make_string(s7, item->valuestring);
        return(result);
        break;
    case cJSON_Number:
        // int or double?
        sprintf(tmp, "%15.10g", item->valuedouble);
        log_debug("formatted: %s", (char*)tmp);
        decimal = strchr(tmp, '.');
        if (decimal)
            result = s7_make_real(s7, item->valuedouble);
        else
            result = s7_make_integer(s7, item->valuedouble);
        return(result);
        break;
    case cJSON_False:
        result = s7_f(s7);
        return(result);
        break;
    case cJSON_True:
        result = s7_t(s7);
        return(result);
        break;
    case cJSON_Array:
        result = s7_make_c_object(s7, json_array_type_tag, (void*)item);
        return result;
        break;
    case cJSON_Object:
        result = s7_make_c_object(s7, json_object_type_tag,
                               (void*)item);
        return result;
        break;
    case cJSON_NULL:
        TRACE_LOG_DEBUG("jo->type %s", "NULL");
        break;
    case cJSON_Raw:
        TRACE_LOG_DEBUG("jo->type %s", "Raw");
        break;
    default:
        log_error("Bad jo->type");
    }

    /* datum = toml_string_in(t, key); */
    /* if (datum.ok) { */
    /*     s7_pointer s = s7_make_string(s7, datum.u.s); */
    /*     free(datum.u.s); */
    /*     return s; */
    /* } */

    /* datum = toml_bool_in(t, key); */
    /* if (datum.ok) { return(s7_make_boolean(s7, datum.u.b)); } */

    /* datum = toml_int_in(t, key); */
    /* if (datum.ok) { return(s7_make_integer(s7, datum.u.i)); } */

    /* datum = toml_double_in(t, key); */
    /* if (datum.ok) { return(s7_make_real(s7, datum.u.d)); } */

    /* datum = toml_timestamp_in(t, key); */
    /* if (datum.ok) { */
    /*     /\* not yet supported *\/ */
    /*     return(s7_f(s7)); */
    /* } */

    /* toml_array_t *a = toml_array_in(t, key); */
    /* if (a) { */
    /*     /\* TRACE_LOG_DEBUG("array"); *\/ */
    /*     s7_pointer rval = s7_make_c_object(s7, */
    /*                                        toml_array_type_tag, */
    /*                                        (void*)a); */
    /*     return rval; */
    /* } else { */
    /*     /\* TRACE_LOG_DEBUG("not array"); *\/ */
    /* } */

    /* cJSON *subt = toml_table_in(t, key); */
    /* if (t) { */
    /*     /\* TRACE_LOG_DEBUG("table: %p", subt); *\/ */
    /*     s7_pointer rval = s7_make_c_object(s7, */
    /*                                        json_object_type_tag, */
    /*                                        (void*)subt); */
    /*     /\* void *optr = (void*)s7_c_object_value(rval); *\/ */
    /*     /\* void *optr = (void*)s7_c_object_value_checked(rval, json_object_type_tag); *\/ */
    /*     /\* TRACE_LOG_DEBUG("rval ptr: %p", rval); *\/ */
    /*     /\* TRACE_LOG_DEBUG("rval objptr: %p", optr); *\/ */

    /*     return rval; */
    /* } else { */
    /*     /\* TRACE_LOG_DEBUG("not table"); *\/ */
    /* } */

    /* TRACE_LOG_DEBUG("returning #f", ""); */
    return(s7_f(s7));
}

s7_pointer g_json_object_contains(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_object_contains);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);            /* arg 0: json obj */
    if (arg == NULL) {
        log_error("Bad arg: NULL");
        return s7_error(s7,
                        s7_make_symbol(s7, "map-ref"),
                        s7_cons(s7, s7_make_string(s7, "arg 0: expected json object, got NULL"),
                                s7_nil(s7)));
    }
    cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_object_type_tag);
    if (jo == NULL) {
        log_error("Bad object arg");
        return s7_unspecified(s7);
    }

    p = s7_cdr(p);
    arg = s7_car(p);            /* arg 1: string key */
    char* key;
    if (s7_is_string(arg))
        key = (char*)s7_string(arg);
    else {
        return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "toml:table-ref", 14), 2, arg, string_string));
    }
    TRACE_LOG_DEBUG("arg 1, key: %s", key);

/*     cJSON *item = cJSON_GetObjectItemCaseSensitive(jo, key); */
/* #ifdef DEBUGGING */
/*     log_debug("item type: [%d]%s", item->type, cjson_types[item->type]); */
/* #endif */

    cJSON_bool flag = cJSON_HasObjectItem(jo, key);

    if (flag)
        return(s7_t(s7));
    else
        return(s7_f(s7));
}

static s7_pointer json_object_set(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_object_set);
    return s7_nil(s7);
}

s7_pointer g_json_object_length(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_object_length);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;

    cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_object_type_tag);
    if (jo == NULL) {
        log_error("Bad object arg");
        return s7_unspecified(s7);
    }

    int key_ct = cJSON_GetArraySize(jo);
    return(s7_make_integer(s7, key_ct));
}

static s7_pointer json_object_copy(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_object_set);
    return s7_nil(s7);
}

static s7_pointer json_object_fill(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_object_set);
    return s7_nil(s7);
}

static s7_pointer json_object_reverse(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_object_set);
    return s7_nil(s7);
}

static s7_pointer g_json_object_to_hash_table(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_object_to_hash_table);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;
    /* cJSON *tt = (cJSON*)s7_c_object_value_checked(arg, json_object_type_tag); */

    /* //FIXME: get optional :clone flag */

    /* s7_pointer ht = toml_table_to_hash_table(s7, tt, true); */
    /* return(ht); */
    return s7_nil(s7);
}

static s7_pointer g_json_object_to_alist(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_object_to_alist);
    return s7_f(s7);
}

/* **************************************************************** */
// to_string implementations
static char *json_object_to_string(s7_scheme *s7, const cJSON *jo)
{
    TRACE_ENTRY(json_object_to_string);

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
        ct = snprintf(buf, 15, "%s", "<#json-object ");
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            return NULL;
        } else {
            TRACE_LOG_DEBUG("snprintf hdr ct: %d", ct);
        }
        char_ct += 14; // do not include terminating '\0'
        TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
        TRACE_LOG_DEBUG("buf: %s", buf);
    }

#ifdef DEBUGGING
    // NB: root object has no name, jo->string == NULL
    log_debug("object jo->string: %s, jo->type: %d: %s",
                    jo->string, jo->type, cjson_types[jo->type]);
#endif

    /* switch(jo->type) { */
    /* case cJSON_String: */
    /*     TRACE_LOG_DEBUG("jo->type: String: %s", jo->string); */
    /*     break; */
    /* case cJSON_Array: */
    /*     TRACE_LOG_DEBUG("jo->type: Array: %s", jo->string); */
    /*     break; */
    /* case cJSON_Object: */
    /*     TRACE_LOG_DEBUG("jo->string: %s, jo->type: Object", jo->string); */
    /*     /\* if (jo->string == NULL) { *\/ */
    /*     /\*     // root object has null name, go to content? *\/ */
    /*     /\*     return json_object_to_string(s7, jo->child); *\/ */
    /*     /\* } *\/ */
    /*     break; */
    /* case cJSON_Number: */
    /*     TRACE_LOG_DEBUG("key number value: %g", jo->valuedouble); */
    /*     break; */
    /* case cJSON_False: */
    /*     TRACE_LOG_DEBUG("jo->type: %s", "False"); */
    /*     break; */
    /* case cJSON_True: */
    /*     TRACE_LOG_DEBUG("jo->type: %s", "True"); */
    /*     break; */
    /* case cJSON_NULL: */
    /*     TRACE_LOG_DEBUG("jo->type %s", "NULL"); */
    /*     break; */
    /* case cJSON_Raw: */
    /*     TRACE_LOG_DEBUG("jo->type %s", "Raw"); */
    /*     break; */
    /* default: */
    /*     log_error("Bad jo->type"); */
    /* } */

    int key_ct = cJSON_GetArraySize(jo);
    TRACE_LOG_DEBUG("JOBJ sz: %d", key_ct);

    // print fields
    cJSON *k;
    int len;
    for (int i = 0; i < key_ct; i++) {
        k = cJSON_GetArrayItem(jo, i);
        if (!k) {
            log_error("cJSON_GetArrayItem failure for key: %d", i);
            return NULL;
        }
#if defined(DEBUGGING)
        log_debug(GRN "object item" CRESET " %d: k->name:%s, k->type: [%d]%s",
                  i, k->string, k->type, cjson_types[k->type]);
#endif

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

        // print key to buf
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing key len %d", len);

        TRACE_LOG_DEBUG("k->string: %s", k->string);
        len = strlen(k->string) + 3; // for " = "
        len++; // terminating '\0'
        if ((char_ct + len) > bufsz) { // + 1 for '\0'
            log_error("exceeded bufsz: %d", char_ct + len);
            // expand buf
        }
        ct = snprintf(buf+char_ct, len, "%s = ", k->string);
        if (errno) {
            log_error("snprintf: %s", strerror(errno));
            break;
        } else {
            TRACE_LOG_DEBUG("snprintf ct: %d", ct);
        }
        char_ct += len - 1; // do not include terminating '\0'
        TRACE_LOG_DEBUG("buf len: %d", strlen(buf));
        TRACE_LOG_DEBUG("buf: %s", buf);

        // print value
        char *seq_str;

        switch(k->type) {
        case cJSON_String:
            TRACE_LOG_DEBUG("key type: String: %s", k->string);
            char *value = k->valuestring;
            len = strlen(value) + 2; // for quotes
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) { // + 1 for '\0'
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            ct = snprintf(buf+char_ct, len, "\"%s\"", value);
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
        case cJSON_Array:
            TRACE_LOG_DEBUG("key type: Array: %s", k->string);
            seq_str = json_array_to_string(s7, k);
            TRACE_LOG_DEBUG("array str: %s", seq_str);
            len = strlen(seq_str);
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) {
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            ct = snprintf(buf+char_ct, len, "%s", seq_str);
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
        case cJSON_Object:
            TRACE_LOG_DEBUG("key type: Object: %s", k->string);
            // val of item k is object, serialize it, not its childred:
            /* seq_str = json_object_to_string(s7, k->child); */
            seq_str = json_object_to_string(s7, k);
            TRACE_LOG_DEBUG("obj value: %s", seq_str);
            len = strlen(seq_str);
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) {
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            ct = snprintf(buf+char_ct, len, "%s", seq_str);
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
        case cJSON_Number:
            TRACE_LOG_DEBUG("key number value: %g", k->valuedouble);
            len = snprintf(NULL, 0, "%g", k->valuedouble);
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) { // + 1 for '\0'
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            ct = snprintf(buf+char_ct, len, "%g", k->valuedouble);
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
        case cJSON_False:
            TRACE_LOG_DEBUG("key type: %s", "False");
            break;
        case cJSON_True:
            TRACE_LOG_DEBUG("key type: %s", "True");
            break;
        case cJSON_NULL:
            TRACE_LOG_DEBUG("key type %s", "NULL");
            break;
        case cJSON_Raw:
            TRACE_LOG_DEBUG("key type %s", "Raw");
            break;
        default:
            log_error("Bad key type");
        }

    }

    // print footer
    {
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing footer", "");
        ct = snprintf(buf+char_ct, 2, "%s", ">");
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
    TRACE_LOG_DEBUG("json_object_to_string returning: %s", buf);
    return buf;
}

static char *json_array_to_string(s7_scheme *s7, const cJSON *ja)
{
    TRACE_ENTRY(json_array_to_string);

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

#ifdef DEBUGGING
    // NB: root object has no name, jo->string == NULL
    log_debug("array ja->string: %s, ja->type: %d: %s",
                    ja->string, ja->type, cjson_types[ja->type]);
#endif
    int key_ct = cJSON_GetArraySize(ja);
    TRACE_LOG_DEBUG("array size: %d", key_ct);

    // print fields
    cJSON *k, *v;
    int len;
    for (int i = 0; i < key_ct; i++) {
        k = cJSON_GetArrayItem(ja, i);
        if (!k) {
            log_error("cJSON_GetArrayItem failure for key: %d", i);
            return NULL;
        }
#if defined(DEBUGGING)
        log_debug(GRN "array item" CRESET " %d type: [%d]%s",
                  i, k->type, cjson_types[k->type]);
        if (k->type == cJSON_String)
            log_debug("k->valuestring: %s", k->valuestring);
        else
            log_debug("k->string: %s", k->string);
#endif

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

        /* // print key to buf */
        /* errno = 0; */
        /* TRACE_LOG_DEBUG("snprintfing key len %d", len); */
        /* len = strlen(k->string) + 3; // for " = " */
        /* len++; // terminating '\0' */
        /* if ((char_ct + len) > bufsz) { // + 1 for '\0' */
        /*     log_error("exceeded bufsz: %d", char_ct + len); */
        /*     // expand buf */
        /* } */
        /* ct = snprintf(buf+char_ct, len, "%s = ", k->string); */
        /* if (errno) { */
        /*     log_error("snprintf: %s", strerror(errno)); */
        /*     break; */
        /* } else { */
        /*     TRACE_LOG_DEBUG("snprintf ct: %d", ct); */
        /* } */
        /* char_ct += len - 1; // do not include terminating '\0' */
        /* TRACE_LOG_DEBUG("buf len: %d", strlen(buf)); */
        /* TRACE_LOG_DEBUG("buf: %s", buf); */

        // print value
        char *seq_str;

        switch(k->type) {
        case cJSON_String:
            TRACE_LOG_DEBUG("key type: String: %s", k->valuestring);
            char *value = k->valuestring;
            len = strlen(value) + 2; // for quotes
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) { // + 1 for '\0'
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            ct = snprintf(buf+char_ct, len, "\"%s\"", value);
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
        case cJSON_Array:
            TRACE_LOG_DEBUG("key type: Array: %s", k->string);
            seq_str = json_array_to_string(s7, k);
            len = strlen(seq_str);
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) {
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            ct = snprintf(buf+char_ct, len, "\"%s\"", seq_str);
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
        case cJSON_Object:
            TRACE_LOG_DEBUG("key type: Object: %s", k->string);
            seq_str = json_object_to_string(s7, k);
            len = strlen(seq_str);
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) {
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            ct = snprintf(buf+char_ct, len, "\"%s\"", seq_str);
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
        case cJSON_Number:
            TRACE_LOG_DEBUG("key number value: %g", k->valuedouble);
            len = snprintf(NULL, 0, "%g", k->valuedouble);
            len++; // terminating '\0'
            if ((char_ct + len) > bufsz) { // + 1 for '\0'
                log_error("exceeded bufsz: %d", char_ct + len);
                // expand buf
            }
            ct = snprintf(buf+char_ct, len, "%g", k->valuedouble);
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
        case cJSON_False:
            TRACE_LOG_DEBUG("key type %s", "False");
            break;
        case cJSON_True:
            TRACE_LOG_DEBUG("key type: %s", "True");
            break;
        case cJSON_NULL:
            TRACE_LOG_DEBUG("key type: %s", "NULL");
            break;
        case cJSON_Raw:
            TRACE_LOG_DEBUG("key type: %s", "Raw");
            break;
        default:
            log_error("Bad key type");
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
    TRACE_LOG_DEBUG("json_object_to_string returning: %s", buf);
    return buf;
}


static s7_pointer g_json_object_to_string(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_object_to_string);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;
    const cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_object_type_tag);

    char *s = json_object_to_string(s7, jo);
    /* char *s = cJSON_PrintUnformatted(jo); */
#ifdef DEBUGGING
    log_debug("g_json_object_to_string returning %s", s);
#endif
    return s7_make_string(s7, s);
}

static s7_pointer json_object_getter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_object_getter);
    return s7_nil(s7);
}

static s7_pointer json_object_setter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_object_setter);
    return s7_nil(s7);
}

/* **************************************************************** */
/*   cJSON.h API */
/* **************************************************************** */

/* -------- cJSON_Version -------- */
s7_pointer json_cJSON_Version(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, (char*)cJSON_Version()));
}


/* -------- json_read -------- */
static s7_pointer g_json_read(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_read);
    TRACE_S7_DUMP("args", args);
    s7_pointer p, arg;
    char* json_str;
    p = args;
    arg = s7_car(p);
    if (s7_is_string(arg))
        json_str = (char*)s7_string(arg);
    else {
        return(s7_wrong_type_error(s7,
                                   s7_make_string_wrapper_with_length(s7, "json:Parse", 10),
                                   0, arg, string_string));
    }
    TRACE_LOG_DEBUG("parsing: %s", json_str);
    cJSON *jo = cJSON_Parse(json_str);
    if (jo == NULL) {
        log_error("cJSON_Parse error");
        char *e = cJSON_GetErrorPtr();
        return s7_error(s7,
                        s7_make_symbol(s7, "cJSON_Parse"),
                        s7_cons(s7, s7_make_string(s7, e), s7_nil(s7)));
    }

    s7_pointer res = s7_make_c_object(s7,
                                      json_object_type_tag,
                                      (void*)jo);
                                                 /* s7_f(s7)); */
    /* log_debug("readed obj type: %d", s7_c_object_type(res)); */

    return(res);
}


/* -------- json_read_file -------- */
s7_pointer json_read_file(s7_scheme *s7, char *fname)
{
    TRACE_ENTRY(json_read_file);
    log_debug("json file: %s", fname);
    (void)fname;

    char *json_str = libs7_read_file(fname); //MALLOC: must free

    cJSON *jobj = cJSON_Parse(json_str);

    // can we free(json_str) here?

    if (jobj == 0) {
        log_error("json_read_file failure");
        /* return s7_error(s7, */
        /*                 s7_make_symbol(s7, "json_read_file"), */
        /*                 s7_cons(s7, s7_make_string(s7, (char*)errbuff), s7_nil(s7))); */
    } else {
        s7_pointer rval = s7_make_c_object(s7,
                                           json_object_type_tag,
                                           (void*)jobj);
        log_debug("json returning obj");
        s7_pointer dt = s7_type_of(s7, rval);
        (void)dt;
        TRACE_S7_DUMP("typ", dt);
        log_debug("json-map? %d",
                  s7_c_object_type(rval) == json_object_type_tag);
        log_debug("obj tag: %d", json_object_type_tag);
       return rval;
    }

    return s7_nil(s7);
}

/* -------- cJSON_Parse -------- */
static s7_pointer json_cJSON_Parse(s7_scheme *sc, s7_pointer args)
{
    TRACE_ENTRY(json_cJSON_Parse);
    s7_pointer p, arg;
    char* json_str;
    p = args;
    arg = s7_car(p);
    if (s7_is_string(arg))
        json_str = (char*)s7_string(arg);
    else {
        return(s7_wrong_type_error(sc,
                                   s7_make_string_wrapper_with_length(sc, "json:Parse", 10),
                                   0, arg, string_string));
    }
    log_debug("parsing: %s", json_str);
    cJSON *jo = cJSON_Parse(json_str);

    return(s7_make_c_pointer_with_type(sc, (void*)jo, cJSON__symbol, s7_f(sc)));
}

/* -------- cJSON_ParseWithLength -------- */
static s7_pointer json_cJSON_ParseWithLength(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* json_cJSON_ParseWithLength_0;
  size_t json_cJSON_ParseWithLength_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    json_cJSON_ParseWithLength_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "json:ParseWithLength", 20), 1, arg, string_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    json_cJSON_ParseWithLength_1 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "json:ParseWithLength", 20), 2, arg, integer_string));
  return(s7_make_c_pointer_with_type(sc, (void*)cJSON_ParseWithLength(json_cJSON_ParseWithLength_0, json_cJSON_ParseWithLength_1), cJSON__symbol, s7_f(sc)));
}


/* -------- cJSON_ParseWithOpts -------- */
static s7_pointer json_cJSON_ParseWithOpts(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  p = args;
  arg = s7_car(p);              /* arg 0: char *value */
  char* value;
  if (s7_is_string(arg))
      value = (char*)s7_string(arg);
  else {
      return(s7_wrong_type_error(sc,
                                 s7_make_string_wrapper_with_length(sc, "json:ParseWithOpts", 18), 1, arg, string_string));
  }
  p = s7_cdr(p);                /* arg 1: char **return_parse_end */
  arg = s7_car(p);
  const char** return_parse_end = (const char**)s7_c_pointer_with_type(sc, arg, char___symbol, __func__, 2);
  p = s7_cdr(p);         /* arg 2: cJSON_bool require_null_terminated */
  arg = s7_car(p);
  cJSON_bool require_null_terminated;
  if (s7_is_integer(arg))
    require_null_terminated = (cJSON_bool)s7_integer(arg);
  else {
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "json:ParseWithOpts", 18), 3, arg, boolean_string));
  }

  const cJSON *cjson = cJSON_ParseWithOpts(value, return_parse_end, require_null_terminated);
  return(s7_make_c_pointer_with_type(sc, (void*)cjson, cJSON__symbol, s7_f(sc)));
}


/* -------- cJSON_ParseWithLengthOpts -------- */
static s7_pointer json_cJSON_ParseWithLengthOpts(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  char* value;
  size_t buffer_length;
  cJSON_bool require_null_terminated;

  p = args;
  arg = s7_car(p);              /* arg 0 */
  if (s7_is_string(arg))
      value = (char*)s7_string(arg);
  else {
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "json:ParseWithLengthOpts", 24), 1, arg, string_string));
  }

  p = s7_cdr(p);
  arg = s7_car(p);              /* arg 1 */
  if (s7_is_integer(arg))
    buffer_length = (size_t)s7_integer(arg);
  else {
      return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "json:ParseWithLengthOpts", 24), 2, arg, integer_string));
  }

  p = s7_cdr(p);
  arg = s7_car(p);              /* arg 2 */
  const char **return_parse_end = (const char**)s7_c_pointer_with_type(sc, arg, char___symbol, __func__, 3);

  p = s7_cdr(p);
  arg = s7_car(p);              /* arg 3 */
  if (s7_is_integer(arg))
    require_null_terminated = (cJSON_bool)s7_integer(arg);
  else {
      return(s7_wrong_type_error(sc,
                                 s7_make_string_wrapper_with_length(sc, "json:ParseWithLengthOpts", 24),
                                 4, arg, boolean_string));
  }

  cJSON *jo = (void*)cJSON_ParseWithLengthOpts(value, buffer_length, return_parse_end, require_null_terminated);

  return(s7_make_c_pointer_with_type(sc, jo, cJSON__symbol, s7_f(sc)));
}


/* -------- cJSON_Print -------- */
static s7_pointer json_cJSON_Print(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_Print_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_Print_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  return(s7_make_string(sc, (char*)cJSON_Print(json_cJSON_Print_0)));
}


/* -------- cJSON_GetArraySize -------- */
static s7_pointer json_cJSON_GetArraySize(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_GetArraySize_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_GetArraySize_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  return(s7_make_integer(sc, (s7_int)cJSON_GetArraySize(json_cJSON_GetArraySize_0)));
}


/* -------- cJSON_GetArrayItem -------- */
static s7_pointer json_cJSON_GetArrayItem(s7_scheme *sc, s7_pointer args)
{
    TRACE_ENTRY(json_cJSON_GetArrayItem);
    s7_pointer p, arg;
    cJSON* json_cJSON_GetArrayItem_0;
    int json_cJSON_GetArrayItem_1;
    p = args;
    arg = s7_car(p);
    json_cJSON_GetArrayItem_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 1);
    p = s7_cdr(p);
    arg = s7_car(p);
    if (s7_is_integer(arg))
        json_cJSON_GetArrayItem_1 = (int)s7_integer(arg);
    else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "json:GetArrayItem", 17), 2, arg, integer_string));
    return(s7_make_c_pointer_with_type(sc, (void*)cJSON_GetArrayItem(json_cJSON_GetArrayItem_0, json_cJSON_GetArrayItem_1), cJSON__symbol, s7_f(sc)));
}


/* -------- cJSON_GetObjectItem -------- */
static s7_pointer json_cJSON_GetObjectItem(s7_scheme *sc, s7_pointer args)
{
    TRACE_ENTRY(json_cJSON_GetObjectItem);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);              /* arg 0: cJSON *object */
    const cJSON *object = (const cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 1);

    p = s7_cdr(p);                /* arg 1: char *key */
    arg = s7_car(p);
    const char *key;
    if (s7_is_string(arg))
        key = (char*)s7_string(arg);
    else {
        return(s7_wrong_type_error(sc,
                                   s7_make_string_wrapper_with_length(sc, "json:GetObjectItem", 18),
                                   2, arg, string_string));
    }
    cJSON *item = cJSON_GetObjectItem(object, key);
    return(s7_make_c_pointer_with_type(sc, (void*)item, cJSON__symbol, s7_f(sc)));
}


/* -------- cJSON_GetObjectItemCaseSensitive -------- */
static s7_pointer json_cJSON_GetObjectItemCaseSensitive(s7_scheme *sc, s7_pointer args)
{
    TRACE_ENTRY(json_cJSON_GetObjectItemCaseSensitive);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);              /* arg 0: cJSON *object */
    const cJSON *object = (const cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 1);

    p = s7_cdr(p);                /* arg 1: char *key */
    arg = s7_car(p);
    const char *key;
    if (s7_is_string(arg))
        key = (char*)s7_string(arg);
    else {
        return(s7_wrong_type_error(sc,
                                   s7_make_string_wrapper_with_length(sc, "json:GetObjectItem", 18),
                                   2, arg, string_string));
    }
    cJSON *item = cJSON_GetObjectItemCaseSensitive(object, key);
    return(s7_make_c_pointer_with_type(sc, (void*)item, cJSON__symbol, s7_f(sc)));
}


/* -------- cJSON_HasObjectItem -------- */
static s7_pointer json_cJSON_HasObjectItem(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_cJSON_HasObjectItem);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);              /* arg 0: cJSON *object */
    const cJSON *object = (const cJSON*)s7_c_pointer_with_type(s7, arg, cJSON__symbol, __func__, 1);

    p = s7_cdr(p);                /* arg 1: char *key */
    arg = s7_car(p);
    const char *key;
    if (s7_is_string(arg))
        key = (char*)s7_string(arg);
    else {
        return(s7_wrong_type_error(s7,
                                   s7_make_string_wrapper_with_length(s7, "json:GetObjectItem", 18),
                                   2, arg, string_string));
    }

    cJSON_bool flag = cJSON_HasObjectItem(object, key);

    if (flag == 1) {
        return s7_t(s7);
    } else {
        return s7_f(s7);
    }
}


/* -------- cJSON_GetErrorPtr -------- */
static s7_pointer json_cJSON_GetErrorPtr(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, (char*)cJSON_GetErrorPtr()));
}


/* -------- cJSON_GetStringValue -------- */
static s7_pointer json_cJSON_GetStringValue(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_GetStringValue_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_GetStringValue_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  return(s7_make_string(sc, (char*)cJSON_GetStringValue(json_cJSON_GetStringValue_0)));
}


/* -------- cJSON_GetNumberValue -------- */
static s7_pointer json_cJSON_GetNumberValue(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_GetNumberValue_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_GetNumberValue_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  return(s7_make_real(sc, (s7_double)cJSON_GetNumberValue(json_cJSON_GetNumberValue_0)));
}


/* -------- cJSON_IsInvalid -------- */
static s7_pointer json_cJSON_IsInvalid(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_IsInvalid_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_IsInvalid_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  cJSON_IsInvalid(json_cJSON_IsInvalid_0);
  return(s7_unspecified(sc));
}


/* -------- cJSON_IsFalse -------- */
static s7_pointer json_cJSON_IsFalse(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_IsFalse_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_IsFalse_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  cJSON_IsFalse(json_cJSON_IsFalse_0);
  return(s7_unspecified(sc));
}


/* -------- cJSON_IsTrue -------- */
static s7_pointer json_cJSON_IsTrue(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_IsTrue_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_IsTrue_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  cJSON_IsTrue(json_cJSON_IsTrue_0);
  return(s7_unspecified(sc));
}


/* -------- cJSON_IsBool -------- */
static s7_pointer json_cJSON_IsBool(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_IsBool_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_IsBool_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  cJSON_IsBool(json_cJSON_IsBool_0);
  return(s7_unspecified(sc));
}


/* -------- cJSON_IsNull -------- */
static s7_pointer json_cJSON_IsNull(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_IsNull_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_IsNull_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  cJSON_IsNull(json_cJSON_IsNull_0);
  return(s7_unspecified(sc));
}


/* -------- cJSON_IsNumber -------- */
static s7_pointer json_cJSON_IsNumber(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_IsNumber_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_IsNumber_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  cJSON_IsNumber(json_cJSON_IsNumber_0);
  return(s7_unspecified(sc));
}


/* -------- cJSON_IsString -------- */
static s7_pointer json_cJSON_IsString(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_IsString_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_IsString_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  cJSON_IsString(json_cJSON_IsString_0);
  return(s7_unspecified(sc));
}


/* -------- cJSON_IsArray -------- */
static s7_pointer g_json_cJSON_IsArray(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_cJSON_IsArray);
    s7_pointer p, arg;
    cJSON* json_cJSON_IsArray_0;
    p = args;
    arg = s7_car(p);
    if (s7_c_object_type(arg) != json_array_type_tag) {
        log_error("Bad arg: expected array");
        // FIXME: throw error
        return s7_f(s7);
    }
    cJSON *item = (cJSON*)s7_c_object_value(arg);
    if (cJSON_IsArray(item))
        return(s7_t(s7));
    else
        return(s7_f(s7));
}


/* -------- cJSON_IsObject -------- */
static s7_pointer json_cJSON_IsObject(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_cJSON_IsObject);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    /* log_debug("c obj type: %d", s7_c_object_type(arg)); */
    /* log_debug("json_object_type_tag: %d", json_object_type_tag); */
    if (s7_c_object_type(arg) != json_object_type_tag) {
        log_error("Bad arg");
        // FIXME: throw error
        return s7_f(s7);
    }
    cJSON* item = (cJSON*)s7_c_object_value(arg);
    if (cJSON_IsObject(item))
        return(s7_t(s7));
    else
        return(s7_f(s7));
}


/* -------- cJSON_IsRaw -------- */
static s7_pointer json_cJSON_IsRaw(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p, arg;
  cJSON* json_cJSON_IsRaw_0;
  p = args;
  arg = s7_car(p);
  json_cJSON_IsRaw_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 0);
  cJSON_IsRaw(json_cJSON_IsRaw_0);
  return(s7_unspecified(sc));
}

void json_object_init(s7_scheme *s7, s7_pointer cur_env);
void json_object_init(s7_scheme *s7, s7_pointer cur_env)
{
    TRACE_ENTRY(json_object_init);
    json_object_type_tag = s7_make_c_type(s7, "json_object");
    TRACE_LOG_DEBUG("JSON_OBJECT_TAG: %d", json_object_type_tag);

    json_array_type_tag = s7_make_c_type(s7, "json_array");
    TRACE_LOG_DEBUG("JSON_ARRAY_TAG: %d", json_array_type_tag);

    s7_c_type_set_gc_free      (s7, json_object_type_tag, free_json_object);
    s7_c_type_set_gc_mark      (s7, json_object_type_tag, mark_json_object);
    s7_c_type_set_is_equal     (s7, json_object_type_tag, json_object_is_equal);
    s7_c_type_set_is_equivalent(s7, json_object_type_tag, json_object_is_equivalent);
    s7_c_type_set_ref          (s7, json_object_type_tag, g_json_object_ref);
    s7_c_type_set_set          (s7, json_object_type_tag, json_object_set);
    s7_c_type_set_length       (s7, json_object_type_tag, g_json_object_length);
    s7_c_type_set_copy         (s7, json_object_type_tag, json_object_copy);
    s7_c_type_set_fill         (s7, json_object_type_tag, json_object_fill);
    s7_c_type_set_reverse      (s7, json_object_type_tag, json_object_reverse);
    /* s7_c_type_set_to_list      (s7, json_object_type_tag, json_object_to_list); */
    s7_c_type_set_to_list      (s7, json_object_type_tag, g_json_object_to_alist);
    s7_c_type_set_to_string    (s7, json_object_type_tag, g_json_object_to_string);

    /* **************************************************************** */
    // API

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:map-keys"),
              s7_make_typed_function(s7, "json:map-keys",
                                     g_json_object_keys,
                                     1, 0, false,
                                     "(json:map-keys obj)",
                                     pl_xx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:map-length"),
              s7_make_typed_function(s7, "json:map-length",
                                     g_json_object_length,
                                     1, 0, false,
                                     "(json:map-length obj)",
                                     pl_xi));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:map-ref"),
              s7_make_typed_function(s7, "json:map-ref",
                                     g_json_object_ref,
                                     2, 0, false,
                                     "(json:map-ref obj key)",
                                     pl_xx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:map-contains?"),
              s7_make_typed_function(s7, "json:map-contains",
                                     g_json_object_contains,
                                     2, 0, false,
                                     "(json:map-contains? obj key)",
                                     pl_bxs));

    s7_define_function(s7, "json:table-getter",
                       json_object_getter, 2, 0, false,
                       "(json:table-getter t k) gets value for key k from table t");
    s7_c_type_set_getter       (s7, json_object_type_tag, s7_name_to_value(s7, "json:table-getter"));

    s7_define_function(s7, "json:table-setter",
                       json_object_setter, 2, 0, false,
                       "(json:table-setter t k) sets value for key k from table t");
    s7_c_type_set_setter       (s7, json_object_type_tag, s7_name_to_value(s7, "json:table-setter"));

    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:object->hash-table"), */
    /*           s7_make_typed_function(s7, "json:object->hash-table", */
    /*                                  g_json_object_to_hash_table, */
    /*                                  1, */
    /*                                  1, // optional :clone flag */
    /*                                  false, */
    /*           "(json:object->hash-table t) converts json object to s7 hash-table. Optional :clone #t", */
    /*                                  pl_xx)); */

    string_string = s7_make_semipermanent_string(s7, "a string");
}

s7_pointer libjson_s7_init(s7_scheme *s7);
s7_pointer libjson_s7_init(s7_scheme *s7)
{
    TRACE_ENTRY(libcjson_s7_init);
    s7_pointer cur_env;
    {
        s7_pointer t, b, x, s, d, i;
        t = s7_t(s7);
        b = s7_make_symbol(s7, "boolean?");
        x = s7_make_symbol(s7, "c-pointer?");
        s = s7_make_symbol(s7, "string?");
        d = s7_make_symbol(s7, "float?");
        i = s7_make_symbol(s7, "integer?");

        pl_tx = s7_make_signature(s7, 2, t, x);
        pl_xs = s7_make_signature(s7, 2, x, s);
        pl_bxs = s7_make_signature(s7, 3, b, x, s);
        pl_xi = s7_make_signature(s7, 2, x, i);
        pl_xx = s7_make_signature(s7, 2, x, x);
        pl_txs = s7_make_signature(s7, 3, t, x, s);
        pl_xxs = s7_make_signature(s7, 3, x, x, s);
        pl_xsi = s7_make_signature(s7, 3, x, s, i);
        pl_xxi = s7_make_signature(s7, 3, x, x, i);
        pl_xsxt = s7_make_signature(s7, 4, x, s, x, t);
        pl_xsixt = s7_make_signature(s7, 5, x, s, i, x, t);
        pl_dx = s7_make_signature(s7, 2, d, x);
        pl_st = s7_make_signature(s7, 2, s, t);
        pl_sx = s7_make_signature(s7, 2, s, x);
        pl_ix = s7_make_signature(s7, 2, i, x);
    }

    string_string = s7_make_semipermanent_string(s7, "a string");
    c_pointer_string = s7_make_semipermanent_string(s7, "a c-pointer");
    character_string = s7_make_semipermanent_string(s7, "a character");
    boolean_string = s7_make_semipermanent_string(s7, "a boolean");
    real_string = s7_make_semipermanent_string(s7, "a real");
    complex_string = s7_make_semipermanent_string(s7, "a complex number");
    integer_string = s7_make_semipermanent_string(s7, "an integer");
    cur_env = s7_inlet(s7, s7_nil(s7));
    s7_pointer old_shadow = s7_set_shadow_rootlet(s7, cur_env);

    json_object_init(s7, cur_env);
    /* json_array_init(s7, cur_env); */

    /* cJSON_,_symbol = s7_make_symbol(s7, "cJSON*,"); */
    char___symbol = s7_make_symbol(s7, "char**");
    cJSON__symbol = s7_make_symbol(s7, "cJSON*");

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:raw?"),
              s7_make_typed_function(s7, "json:IsRaw", json_cJSON_IsRaw, 1, 0, false, "cJSON_bool cJSON_IsRaw(cJSON*)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:map?"),
              s7_make_typed_function(s7, "json:map?",
                                     json_cJSON_IsObject,
                                     1, 0, false,
                                     "cJSON_bool cJSON_IsObject(cJSON*)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:array?"),
              s7_make_typed_function(s7, "json:array?",
                                     g_json_cJSON_IsArray, 1, 0, false,
                                     "(json:array? obj)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:vector?"),
              s7_make_typed_function(s7, "json:vector?",
                                     g_json_cJSON_IsArray, 1, 0, false,
                                     "(json:vector? obj)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:IsString"),
              s7_make_typed_function(s7, "json:IsString", json_cJSON_IsString, 1, 0, false, "cJSON_bool cJSON_IsString(cJSON*)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:IsNumber"),
              s7_make_typed_function(s7, "json:IsNumber", json_cJSON_IsNumber, 1, 0, false, "cJSON_bool cJSON_IsNumber(cJSON*)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:IsNull"),
              s7_make_typed_function(s7, "json:IsNull", json_cJSON_IsNull, 1, 0, false, "cJSON_bool cJSON_IsNull(cJSON*)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:IsBool"),
              s7_make_typed_function(s7, "json:IsBool", json_cJSON_IsBool, 1, 0, false, "cJSON_bool cJSON_IsBool(cJSON*)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:IsTrue"),
              s7_make_typed_function(s7, "json:IsTrue", json_cJSON_IsTrue, 1, 0, false, "cJSON_bool cJSON_IsTrue(cJSON*)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:IsFalse"),
              s7_make_typed_function(s7, "json:IsFalse", json_cJSON_IsFalse, 1, 0, false, "cJSON_bool cJSON_IsFalse(cJSON*)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:IsInvalid"),
              s7_make_typed_function(s7, "json:IsInvalid", json_cJSON_IsInvalid, 1, 0, false, "cJSON_bool cJSON_IsInvalid(cJSON*)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:GetNumberValue"),
              s7_make_typed_function(s7, "json:GetNumberValue", json_cJSON_GetNumberValue, 1, 0, false, "double cJSON_GetNumberValue(cJSON*)", pl_dx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:GetStringValue"),
              s7_make_typed_function(s7, "json:GetStringValue", json_cJSON_GetStringValue, 1, 0, false, "char* cJSON_GetStringValue(cJSON*)", pl_sx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:GetErrorPtr"),
              s7_make_typed_function(s7, "json:GetErrorPtr", json_cJSON_GetErrorPtr, 0, 0, false, "char* cJSON_GetErrorPtr(void)", pl_st));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:HasObjectItem"),
              s7_make_typed_function(s7, "json:HasObjectItem", json_cJSON_HasObjectItem, 2, 0, false, "cJSON_bool cJSON_HasObjectItem(cJSON*, char*)", pl_txs));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:GetObjectItemCaseSensitive"),
              s7_make_typed_function(s7, "json:GetObjectItemCaseSensitive", json_cJSON_GetObjectItemCaseSensitive, 2, 0, false, "cJSON* cJSON_GetObjectItemCaseSensitive(cJSON*, char*)", pl_xxs));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:GetObjectItem"),
              s7_make_typed_function(s7, "json:GetObjectItem", json_cJSON_GetObjectItem, 2, 0, false, "cJSON* cJSON_GetObjectItem(cJSON*, char*)", pl_xxs));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:GetArrayItem"),
              s7_make_typed_function(s7, "json:GetArrayItem", json_cJSON_GetArrayItem, 2, 0, false, "cJSON* cJSON_GetArrayItem(cJSON* int)", pl_xxi));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:GetArraySize"),
              s7_make_typed_function(s7, "json:GetArraySize", json_cJSON_GetArraySize, 1, 0, false, "int cJSON_GetArraySize(cJSON*)", pl_ix));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:Print"),
              s7_make_typed_function(s7, "json:Print", json_cJSON_Print, 1, 0, false, "char* cJSON_Print(cJSON*)", pl_sx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:read"),
              s7_make_typed_function(s7, "json:read",
                                     g_json_read,
                                     1, 0, false,
                                     "(json:read s) read JSON string s",
                                     pl_xs));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:Parse"),
              s7_make_typed_function(s7, "json:Parse",
                                     json_cJSON_Parse,
                                     1, 0, false,
                                     "(json:Parse s) parse string s",
                                     pl_xs));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:ParseWithLengthOpts"),
              s7_make_typed_function(s7, "json:ParseWithLengthOpts", json_cJSON_ParseWithLengthOpts, 4, 0, false, "cJSON* cJSON_ParseWithLengthOpts(char* size_t char** cJSON_bool)", pl_xsixt));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:ParseWithOpts"),
              s7_make_typed_function(s7, "json:ParseWithOpts", json_cJSON_ParseWithOpts, 3, 0, false, "cJSON* cJSON_ParseWithOpts(char* char** cJSON_bool)", pl_xsxt));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:ParseWithLength"),
              s7_make_typed_function(s7, "json:ParseWithLength", json_cJSON_ParseWithLength, 2, 0, false, "cJSON* cJSON_ParseWithLength(char* size_t)", pl_xsi));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:Version"),
              s7_make_typed_function(s7, "json:Version", json_cJSON_Version, 0, 0, false, "char* cJSON_Version(void)", pl_st));
    s7_set_shadow_rootlet(s7, old_shadow);

    return(cur_env);
}
