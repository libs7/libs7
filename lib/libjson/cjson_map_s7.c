#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "trace.h"

#include "libcjson_s7.h"

int json_object_type_tag = 0;

/* signature stuff */
s7_pointer pl_tx, pl_xs, pl_bxs, pl_xi, pl_xx, pl_txs, pl_xxs, pl_xsi, pl_xxi, pl_xsxt, pl_xsixt, pl_dx, pl_st, pl_sx, pl_ix;

static s7_pointer string_string;
/* c_pointer_string, character_string, boolean_string, real_string, complex_string, integer_string; */

/* ****************************************************************
 * Public Scheme API for cJSON.h map (object) operations
 *****************************************************************/
static s7_pointer json_is_map(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_is_map);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    /* log_debug("c obj type: %d", s7_c_object_type(arg)); */
    /* log_debug("json_object_type_tag: %d", json_object_type_tag); */
    if (s7_c_object_type(arg) == json_object_type_tag) {
        cJSON* item = (cJSON*)s7_c_object_value(arg);
        if (cJSON_IsObject(item))
            return(s7_t(s7));
        else
            return(s7_f(s7));
    /* } */
    /* else if (s7_c_object_type(arg) == json_vector_type_tag) { */
    /*     return(s7_f(s7)); */
    } else
        return s7_f(s7);
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

/* -------- cJSON_GetObjectItemCaseSensitive -------- */
/* static s7_pointer json_cJSON_GetObjectItemCaseSensitive(s7_scheme *sc, s7_pointer args) */
/* { */
/*     TRACE_ENTRY(json_cJSON_GetObjectItemCaseSensitive); */
/*     s7_pointer p, arg; */
/*     p = args; */
/*     arg = s7_car(p);              /\* arg 0: cJSON *object *\/ */
/*     const cJSON *object = (const cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 1); */

/*     p = s7_cdr(p);                /\* arg 1: char *key *\/ */
/*     arg = s7_car(p); */
/*     const char *key; */
/*     if (s7_is_string(arg)) */
/*         key = (char*)s7_string(arg); */
/*     else { */
/*         return(s7_wrong_type_error(sc, */
/*                                    s7_make_string_wrapper_with_length(sc, "json:GetObjectItem", 18), */
/*                                    2, arg, string_string)); */
/*     } */
/*     cJSON *item = cJSON_GetObjectItemCaseSensitive(object, key); */
/*     return(s7_make_c_pointer_with_type(sc, (void*)item, cJSON__symbol, s7_f(sc))); */
/* } */

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
        return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "json:map-contains", 14), 2, arg, string_string));
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

/* -------- cJSON_HasObjectItem -------- */
/* static s7_pointer json_cJSON_HasObjectItem(s7_scheme *s7, s7_pointer args) */
/* { */
/*     TRACE_ENTRY(json_cJSON_HasObjectItem); */
/*     s7_pointer p, arg; */
/*     p = args; */
/*     arg = s7_car(p);              /\* arg 0: cJSON *object *\/ */
/*     const cJSON *object = (const cJSON*)s7_c_pointer_with_type(s7, arg, cJSON__symbol, __func__, 1); */

/*     p = s7_cdr(p);                /\* arg 1: char *key *\/ */
/*     arg = s7_car(p); */
/*     const char *key; */
/*     if (s7_is_string(arg)) */
/*         key = (char*)s7_string(arg); */
/*     else { */
/*         return(s7_wrong_type_error(s7, */
/*                                    s7_make_string_wrapper_with_length(s7, "json:GetObjectItem", 18), */
/*                                    2, arg, string_string)); */
/*     } */

/*     cJSON_bool flag = cJSON_HasObjectItem(object, key); */

/*     if (flag == 1) { */
/*         return s7_t(s7); */
/*     } else { */
/*         return s7_f(s7); */
/*     } */
/* } */

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

/*
 * Called for json:map-ref
 * Called when a map is applied to a key
 * Called with int args when map or for-each are applied to an object.
 */
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
    char* key = NULL;
    int idx;
    if (s7_is_string(arg)) {
        // for map-ref or map application
        key = (char*)s7_string(arg);
        TRACE_LOG_DEBUG("arg 1, key: %s", key);
    }
    else if (s7_is_keyword(arg)) {
        s7_pointer kwsym = s7_keyword_to_symbol(s7, arg);
        key = (char*)s7_symbol_name(kwsym);
        TRACE_LOG_DEBUG("arg 1, key: %s", key);
    }
    else if (s7_is_symbol(arg)) {
        key = (char*)s7_symbol_name(arg);
        TRACE_LOG_DEBUG("arg 1, key: %s", key);
    }
    else if (s7_is_integer(arg)) {
        // for procedures map, for-each
        idx = s7_integer(arg);
        TRACE_LOG_DEBUG("arg 1, idx: %s", idx);
    }
    else {
        return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "json:map-ref", 14), 2, arg, string_string));
    }

    cJSON *item;
    if (key == NULL) {
        item = cJSON_GetArrayItem(jo, idx);
    } else {
        item = cJSON_GetObjectItemCaseSensitive(jo, key);
    }
#ifdef DEBUGGING
    log_debug("item type: [%d]%s", item->type, cjson_types[item->type]);
#endif

    char buf[64];               /* for expanding doubles */
    char *decimal = NULL;
    s7_pointer tmp, result;
    switch(item->type) {
    case cJSON_String:
        tmp = s7_make_string(s7, item->valuestring);
        if (key) {
            result = tmp;
        } else {
            result = s7_cons(s7, s7_make_string(s7, item->string), tmp);
        }
        return(result);
        break;
    case cJSON_Number:
        // int or double?
        sprintf(buf, "%15.10g", item->valuedouble);
        /* log_debug("formatted: %s", (char*)buf); */
        decimal = strchr(buf, '.');
        if (decimal)
            tmp = s7_make_real(s7, item->valuedouble);
        else
            tmp = s7_make_integer(s7, item->valuedouble);
        if (key) {
            result = tmp;
        } else {
            result = s7_cons(s7, s7_make_string(s7, item->string), tmp);
        }
        return(result);
        break;
    case cJSON_False:
        tmp = s7_f(s7);
        if (key) {
            result = tmp;
        } else {
            result = s7_cons(s7, s7_make_string(s7, item->string), tmp);
        }
        return(result);
        break;
    case cJSON_True:
        tmp = s7_t(s7);
        if (key) {
            result = tmp;
        } else {
            result = s7_cons(s7, s7_make_string(s7, item->string), tmp);
        }
        return(result);
        break;
    case cJSON_Array:
        tmp = s7_make_c_object(s7, json_vector_type_tag, (void*)item);
        if (key) {
            result = tmp;
        } else {
            result = s7_cons(s7, s7_make_string(s7, item->string), tmp);
        }
        return(result);
        break;
    case cJSON_Object:
        tmp = s7_make_c_object(s7, json_object_type_tag,
                               (void*)item);
        if (key) {
            result = tmp;
        } else {
            result = s7_cons(s7, s7_make_string(s7, item->string), tmp);
        }
        return(result);
        break;
    case cJSON_NULL:
        tmp = s7_nil(s7);
        if (key) {
            result = tmp;
        } else {
            result = s7_cons(s7, s7_make_string(s7, item->string), tmp);
        }
        return(result);
        break;
    case cJSON_Raw:
        log_error("Bad arg: raw");
        break;
    default:
        log_error("Bad jo->type");
    }
    return(s7_f(s7));
}
/* -------- cJSON_GetObjectItem -------- */
/* static s7_pointer json_cJSON_GetObjectItem(s7_scheme *sc, s7_pointer args) */
/* { */
/*     TRACE_ENTRY(json_cJSON_GetObjectItem); */
/*     s7_pointer p, arg; */
/*     p = args; */
/*     arg = s7_car(p);              /\* arg 0: cJSON *object *\/ */
/*     const cJSON *object = (const cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 1); */

/*     p = s7_cdr(p);                /\* arg 1: char *key *\/ */
/*     arg = s7_car(p); */
/*     const char *key; */
/*     if (s7_is_string(arg)) */
/*         key = (char*)s7_string(arg); */
/*     else { */
/*         return(s7_wrong_type_error(sc, */
/*                                    s7_make_string_wrapper_with_length(sc, "json:GetObjectItem", 18), */
/*                                    2, arg, string_string)); */
/*     } */
/*     cJSON *item = cJSON_GetObjectItem(object, key); */
/*     return(s7_make_c_pointer_with_type(sc, (void*)item, cJSON__symbol, s7_f(sc))); */
/* } */


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

/* **************************************************************** */
// to_string implementation
char *json_object_to_string(s7_scheme *s7, const cJSON *jo)
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
    (void)ct; // prevent set-but-not-used warning

    // print header
    {
        errno = 0;
        TRACE_LOG_DEBUG("snprintfing header", "");
        ct = snprintf(buf, 15, "%s", "#<json-object ");
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
            seq_str = json_vector_to_string(s7, k);
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
            free(seq_str);
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


/* ****************************************************************
 * conversion functions
 **************************************************************** */
static s7_pointer g_json_object_to_alist(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_object_to_alist);
    return s7_f(s7);
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

void json_object_init(s7_scheme *s7, s7_pointer cur_env)
{
    TRACE_ENTRY(json_object_init);
    json_object_type_tag = s7_make_c_type(s7, "json_object");
    TRACE_LOG_DEBUG("JSON_OBJECT_TAG: %d", json_object_type_tag);

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
              s7_make_symbol(s7, "json:map?"),
              s7_make_typed_function(s7, "json:map?",
                                     json_is_map,
                                     1, 0, false,
                                     "cJSON_bool cJSON_IsObject(cJSON*)", pl_tx));

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

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:map->hash-table"),
              s7_make_typed_function(s7, "json:map->hash-table",
                                     g_json_object_to_hash_table,
                                     1,
                                     1, // optional :clone flag
                                     false,
              "(json:object->hash-table t) converts json object to s7 hash-table. Optional :clone #t",
                                     pl_xx));

    string_string = s7_make_semipermanent_string(s7, "a string");
}
