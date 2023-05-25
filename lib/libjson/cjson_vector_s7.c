#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "trace.h"

#include "libcjson_s7.h"

int json_vector_type_tag = 0;

static s7_pointer string_string;
/* c_pointer_string, character_string, boolean_string, real_string, complex_string, */

/* ****************************************************************
 * Public Scheme API for cJSON.h vector (array) operations
 *****************************************************************/
/* -------- cjson_is_vector -------- */
static s7_pointer g_json_is_vector(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_is_vector);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    if (s7_c_object_type(arg) != json_vector_type_tag) {
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


/* -------- cJSON_GetArrayItem -------- */
/* static s7_pointer json_cJSON_GetArrayItem(s7_scheme *sc, s7_pointer args) */
/* { */
/*     TRACE_ENTRY(json_cJSON_GetArrayItem); */
/*     s7_pointer p, arg; */
/*     cJSON* json_cJSON_GetArrayItem_0; */
/*     int json_cJSON_GetArrayItem_1; */
/*     p = args; */
/*     arg = s7_car(p); */
/*     json_cJSON_GetArrayItem_0 = (cJSON*)s7_c_pointer_with_type(sc, arg, cJSON__symbol, __func__, 1); */
/*     p = s7_cdr(p); */
/*     arg = s7_car(p); */
/*     if (s7_is_integer(arg)) */
/*         json_cJSON_GetArrayItem_1 = (int)s7_integer(arg); */
/*     else return(s7_wrong_type_error(sc, s7_make_string_wrapper_with_length(sc, "json:GetArrayItem", 17), 2, arg, integer_string)); */
/*     return(s7_make_c_pointer_with_type(sc, (void*)cJSON_GetArrayItem(json_cJSON_GetArrayItem_0, json_cJSON_GetArrayItem_1), cJSON__symbol, s7_f(sc))); */
/* } */

/* ****************************************************************
 *  s7 integration API
 * *************************************************************** */
static s7_pointer free_json_vector(s7_scheme *s7, s7_pointer obj)
{
    TRACE_ENTRY(free_json_vector);
    free(s7_c_object_value(obj));
    return(NULL);
}

static s7_pointer mark_json_vector(s7_scheme *s7, s7_pointer obj)
{
  /* json_vector_t *t = (json_vector_t*)s7_c_object_value(obj); */
  /* s7_mark(o->data); */
  return(NULL);
}

static s7_pointer json_vector_is_equal(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_vector_is_equal);
    return s7_nil(s7);
}

static s7_pointer json_vector_is_equivalent(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_vector_is_equivalent);
    return s7_nil(s7);
}

/*
 * Called for json:vector-ref
 * Called when a vector is applied to an index
 * Called with int args when map or for-each are applied to a vector
 */
s7_pointer g_json_vector_ref(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_vector_ref);
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
    cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_vector_type_tag);
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

static s7_pointer json_vector_set(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_vector_set);
    return s7_nil(s7);
}

s7_pointer g_json_vector_length(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_vector_length);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;

    cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_vector_type_tag);
    if (jo == NULL) {
        log_error("Bad object arg");
        return s7_unspecified(s7);
    }

    int key_ct = cJSON_GetArraySize(jo);
    return(s7_make_integer(s7, key_ct));
}

static s7_pointer json_vector_copy(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_vector_set);
    return s7_nil(s7);
}

static s7_pointer json_vector_fill(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_vector_set);
    return s7_nil(s7);
}

static s7_pointer json_vector_reverse(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_vector_set);
    return s7_nil(s7);
}

static s7_pointer g_json_vector_to_vector(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_vector_to_vector);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;
    /* cJSON *tt = (cJSON*)s7_c_object_value_checked(arg, json_vector_type_tag); */

    /* //FIXME: get optional :clone flag */

    /* s7_pointer ht = toml_table_to_hash_table(s7, tt, true); */
    /* return(ht); */
    return s7_nil(s7);
}

static s7_pointer g_json_vector_to_list(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_vector_to_list);
    return s7_f(s7);
}

/* **************************************************************** */
// to_string implementations
char *json_vector_to_string(s7_scheme *s7, const cJSON *ja)
{
    TRACE_ENTRY(json_vector_to_string);

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
    int ct = 0;
    (void)ct; // prevent set-but-not-used warning

    // print header
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

#ifdef DEBUGGING
    // NB: root object has no name, jo->string == NULL
    log_debug("array ja->string: %s, ja->type: %d: %s",
                    ja->string, ja->type, cjson_types[ja->type]);
#endif
    int key_ct = cJSON_GetArraySize(ja);
    TRACE_LOG_DEBUG("array size: %d", key_ct);

    // print fields
    cJSON *k; //, *v;
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
            seq_str = json_vector_to_string(s7, k);
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
    TRACE_LOG_DEBUG("json_vector_to_string returning: %s", buf);
    return buf;
}


static s7_pointer g_json_vector_to_string(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_vector_to_string);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;
    const cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_vector_type_tag);

    char *s = json_vector_to_string(s7, jo);
    /* char *s = cJSON_PrintUnformatted(jo); */
#ifdef DEBUGGING
    log_debug("g_json_vector_to_string returning %s", s);
#endif
    return s7_make_string(s7, s);
}

static s7_pointer json_vector_getter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_vector_getter);
    return s7_nil(s7);
}

static s7_pointer json_vector_setter(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(json_vector_setter);
    return s7_nil(s7);
}

void json_vector_init(s7_scheme *s7, s7_pointer cur_env)
{
    TRACE_ENTRY(json_vector_init);
    json_vector_type_tag = s7_make_c_type(s7, "json_vector");
    TRACE_LOG_DEBUG("JSON_VECTOR_TAG: %d", json_vector_type_tag);

    s7_c_type_set_gc_free      (s7, json_vector_type_tag, free_json_vector);
    s7_c_type_set_gc_mark      (s7, json_vector_type_tag, mark_json_vector);
    s7_c_type_set_is_equal     (s7, json_vector_type_tag, json_vector_is_equal);
    s7_c_type_set_is_equivalent(s7, json_vector_type_tag, json_vector_is_equivalent);
    s7_c_type_set_ref          (s7, json_vector_type_tag, g_json_vector_ref);
    s7_c_type_set_set          (s7, json_vector_type_tag, json_vector_set);
    s7_c_type_set_length       (s7, json_vector_type_tag, g_json_vector_length);
    s7_c_type_set_copy         (s7, json_vector_type_tag, json_vector_copy);
    s7_c_type_set_fill         (s7, json_vector_type_tag, json_vector_fill);
    s7_c_type_set_reverse      (s7, json_vector_type_tag, json_vector_reverse);
    s7_c_type_set_to_list      (s7, json_vector_type_tag, g_json_vector_to_list);
    s7_c_type_set_to_string    (s7, json_vector_type_tag, g_json_vector_to_string);

    /* ***************************************************************
     * Public Scheme API
     * *************************************************************** */
    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:vector?"),
              s7_make_typed_function(s7, "json:vector?",
                                     g_json_is_vector, 1, 0, false,
                                     "(json:vector? obj)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:vector-length"),
              s7_make_typed_function(s7, "json:vector-length",
                                     g_json_vector_length,
                                     1, 0, false,
                                     "(json:vector-length obj)",
                                     pl_xi));
    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:GetArraySize"), */
    /*           s7_make_typed_function(s7, "json:GetArraySize", json_cJSON_GetArraySize, 1, 0, false, "int cJSON_GetArraySize(cJSON*)", pl_ix)); */


    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:vector-ref"),
              s7_make_typed_function(s7, "json:vector-ref",
                                     g_json_vector_ref,
                                     2, 0, false,
                                     "(json:vector-ref obj key)",
                                     pl_xx));
    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:GetArrayItem"), */
    /*           s7_make_typed_function(s7, "json:GetArrayItem", json_cJSON_GetArrayItem, 2, 0, false, "cJSON* cJSON_GetArrayItem(cJSON* int)", pl_xxi)); */

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:vector->list"),
              s7_make_typed_function(s7, "json:vector->list",
                                     g_json_vector_to_list,
                                     1, 0, false,
              "(json:vector->list a) converts json array to s7 list",
                                     pl_xx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:vector->vector"),
              s7_make_typed_function(s7, "json:vector->vector",
                                     g_json_vector_to_vector,
                                     1, 0, false,
              "(json:vector->vector a) converts json array to s7 vector",
                                     pl_xx));

    s7_define_function(s7, "json:vector-getter",
                       json_vector_getter, 2, 0, false,
                       "(json:vector-getter t k) gets value for key k from vector t");
    s7_c_type_set_getter       (s7, json_vector_type_tag, s7_name_to_value(s7, "json:vector-getter"));

    s7_define_function(s7, "json:vector-setter",
                       json_vector_setter, 2, 0, false,
                       "(json:vector-setter t k) sets value for key k from vector t");
    s7_c_type_set_setter       (s7, json_vector_type_tag, s7_name_to_value(s7, "json:vector-setter"));

    //FIXME: why this here?
    string_string = s7_make_semipermanent_string(s7, "a string");
}
