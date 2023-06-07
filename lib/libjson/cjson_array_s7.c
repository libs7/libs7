#include <errno.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "config.h"

#include "libcjson_s7.h"

int json_array_type_tag = 0;

static s7_pointer string_string;
/* c_pointer_string, character_string, boolean_string, real_string, complex_string, */

/* ****************************************************************
 * Public Scheme API for cJSON.h array (array) operations
 *****************************************************************/
/* -------- cjson_is_array -------- */
static s7_pointer g_json_is_array(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_is_array);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    if (s7_c_object_type(arg) == json_array_type_tag) {
        cJSON *item = (cJSON*)s7_c_object_value(arg);
        if (cJSON_IsArray(item))
            return(s7_t(s7));
        else
            return(s7_f(s7));
    } else
        return s7_f(s7);
}

/* ****************************************************************
 *  s7 integration API
 * *************************************************************** */
static s7_pointer free_json_array(s7_scheme *s7, s7_pointer obj)
{
    (void)s7;
    TRACE_ENTRY(free_json_array);
    free(s7_c_object_value(obj));
    return(NULL);
}

static s7_pointer mark_json_array(s7_scheme *s7, s7_pointer obj)
{
    (void)s7;
    (void)obj;
    /* json_array_t *t = (json_array_t*)s7_c_object_value(obj); */
    /* s7_mark(o->data); */
    return(NULL);
}

static s7_pointer json_array_is_equal(s7_scheme *s7, s7_pointer args)
{
    (void)s7;
    (void)args;
    TRACE_ENTRY(json_array_is_equal);
    return s7_nil(s7);
}

static s7_pointer json_array_is_equivalent(s7_scheme *s7, s7_pointer args)
{
    (void)s7;
    (void)args;
    TRACE_ENTRY(json_array_is_equivalent);
    return s7_nil(s7);
}

/*
 * Called for json:array-ref
 * Called when a array is applied to an index
 * Called with int args when map or for-each are applied to a array
 */
s7_pointer g_json_array_ref(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_array_ref);
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
    cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_array_type_tag);
    if (jo == NULL) {
        log_error("Bad object arg");
        return s7_unspecified(s7);
    }

    p = s7_cdr(p);
    arg = s7_car(p);            /* arg 1: string key */
    int idx;
    if (s7_is_integer(arg)) {
        // for procedures map, for-each
        idx = s7_integer(arg);
        TRACE_LOG_DEBUG("arg 1, idx: %d", idx);
    } else {
        return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "json:map-ref", 14), 2, arg, string_string));
    }

    cJSON *item = cJSON_GetArrayItem(jo, idx);
    if (item == NULL) {
        log_error("array-ref: bad index");
        const char *e = cJSON_GetErrorPtr();
        return s7_error(s7,
                        s7_make_symbol(s7, "array-ref index out of range?"),
                        s7_cons(s7, s7_make_string(s7, e), s7_nil(s7)));
    }
#ifdef DEVBUILD
    log_debug("item type: [%d]%s", item->type, cjson_types[item->type]);
#endif

    char buf[64];               /* for expanding doubles */
    char *decimal = NULL;
    s7_pointer result; //, tmp;
    switch(item->type) {
    case cJSON_String:
        result = s7_make_string(s7, item->valuestring);
        return(result);
        break;
    case cJSON_Number:
        // int or double?
        sprintf(buf, "%15.10g", item->valuedouble);
        decimal = strchr(buf, '.');
        if (decimal)
            result = s7_make_real(s7, item->valuedouble);
        else
            result = s7_make_integer(s7, item->valuedouble);
        return(result);
        break;
    case cJSON_False:
        return(s7_f(s7));
        break;
    case cJSON_True:
        return(s7_t(s7));
        break;
    case cJSON_Array:
        result = s7_make_c_object(s7, json_array_type_tag, (void*)item);
        return(result);
        break;
    case cJSON_Object:
        result = s7_make_c_object(s7, json_object_type_tag,
                               (void*)item);
        return(result);
        break;
    case cJSON_NULL:
        return(s7_nil(s7));
        break;
    case cJSON_Raw:
        log_error("Bad arg: raw");
        break;
    default:
        log_error("Bad jo->type");
    }
    return(s7_f(s7));
}

static s7_pointer json_array_set(s7_scheme *s7, s7_pointer args)
{
    (void)s7;
    (void)args;
    TRACE_ENTRY(json_array_set);
    return s7_nil(s7);
}

s7_pointer g_json_array_length(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_array_length);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;

    cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_array_type_tag);
    if (jo == NULL) {
        log_error("Bad object arg");
        return s7_unspecified(s7);
    }

    int key_ct = cJSON_GetArraySize(jo);
    return(s7_make_integer(s7, key_ct));
}

static s7_pointer json_array_copy(s7_scheme *s7, s7_pointer args)
{
    (void)args;
    TRACE_ENTRY(json_array_set);
    return s7_nil(s7);
}

static s7_pointer json_array_fill(s7_scheme *s7, s7_pointer args)
{
    (void)args;
    TRACE_ENTRY(json_array_set);
    return s7_nil(s7);
}

static s7_pointer json_array_reverse(s7_scheme *s7, s7_pointer args)
{
    (void)args;
    TRACE_ENTRY(json_array_set);
    return s7_nil(s7);
}

static s7_pointer g_json_array_to_vector(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_array_to_vector);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    cJSON *ja = (cJSON*)s7_c_object_value_checked(arg, json_array_type_tag);

    /* //FIXME: get optional :clone flag */

    s7_pointer vec = json_array_to_vector(s7, ja, true);
    return vec;
}

static s7_pointer g_json_array_to_list(s7_scheme *s7, s7_pointer args)
{
    (void)args;
    TRACE_ENTRY(g_json_array_to_list);
    return s7_f(s7);
}

/* **************************************************************** */
// to_string implementations
char *json_array_to_string(s7_scheme *s7, const cJSON *ja)
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

#ifdef DEVBUILD
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
#if defined(DEVBUILD)
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
    TRACE_LOG_DEBUG("json_array_to_string returning: %s", buf);
    return buf;
}


static s7_pointer g_json_array_to_string(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_array_to_string);
    s7_pointer p, arg;
    p = args;
    arg = s7_car(p);
    (void)arg;
    const cJSON *jo = (cJSON*)s7_c_object_value_checked(arg, json_array_type_tag);

    char *s = json_array_to_string(s7, jo);
    /* char *s = cJSON_PrintUnformatted(jo); */
#ifdef DEVBUILD
    log_debug("g_json_array_to_string returning %s", s);
#endif
    return s7_make_string(s7, s);
}

static s7_pointer json_array_getter(s7_scheme *s7, s7_pointer args)
{
    (void)args;
    TRACE_ENTRY(json_array_getter);
    return s7_nil(s7);
}

static s7_pointer json_array_setter(s7_scheme *s7, s7_pointer args)
{
    (void)args;
    TRACE_ENTRY(json_array_setter);
    return s7_nil(s7);
}

void json_array_init(s7_scheme *s7, s7_pointer cur_env)
{
    TRACE_ENTRY(json_array_init);
    json_array_type_tag = s7_make_c_type(s7, "json_array");
    /* TRACE_LOG_DEBUG("JSON_ARRAY_TAG: %d", json_array_type_tag); */

    s7_c_type_set_gc_free      (s7, json_array_type_tag, free_json_array);
    s7_c_type_set_gc_mark      (s7, json_array_type_tag, mark_json_array);
    s7_c_type_set_is_equal     (s7, json_array_type_tag, json_array_is_equal);
    s7_c_type_set_is_equivalent(s7, json_array_type_tag, json_array_is_equivalent);
    s7_c_type_set_ref          (s7, json_array_type_tag, g_json_array_ref);
    s7_c_type_set_set          (s7, json_array_type_tag, json_array_set);
    s7_c_type_set_length       (s7, json_array_type_tag, g_json_array_length);
    s7_c_type_set_copy         (s7, json_array_type_tag, json_array_copy);
    s7_c_type_set_fill         (s7, json_array_type_tag, json_array_fill);
    s7_c_type_set_reverse      (s7, json_array_type_tag, json_array_reverse);
    s7_c_type_set_to_list      (s7, json_array_type_tag, g_json_array_to_list);
    s7_c_type_set_to_string    (s7, json_array_type_tag, g_json_array_to_string);

    /* ***************************************************************
     * Public Scheme API
     * *************************************************************** */
    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:array?"),
              s7_make_typed_function(s7, "json:array?",
                                     g_json_is_array, 1, 0, false,
                                     "(json:array? obj)", pl_tx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:array-length"),
              s7_make_typed_function(s7, "json:array-length",
                                     g_json_array_length,
                                     1, 0, false,
                                     "(json:array-length obj)",
                                     pl_xi));
    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:GetArraySize"), */
    /*           s7_make_typed_function(s7, "json:GetArraySize", json_cJSON_GetArraySize, 1, 0, false, "int cJSON_GetArraySize(cJSON*)", pl_ix)); */


    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:array-ref"),
              s7_make_typed_function(s7, "json:array-ref",
                                     g_json_array_ref,
                                     2, 0, false,
                                     "(json:array-ref obj key)",
                                     pl_xx));
    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:GetArrayItem"), */
    /*           s7_make_typed_function(s7, "json:GetArrayItem", json_cJSON_GetArrayItem, 2, 0, false, "cJSON* cJSON_GetArrayItem(cJSON* int)", pl_xxi)); */

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:array->list"),
              s7_make_typed_function(s7, "json:array->list",
                                     g_json_array_to_list,
                                     1, 0, false,
              "(json:array->list a) converts json array to s7 list",
                                     pl_xx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:array->vector"),
              s7_make_typed_function(s7, "json:array->vector",
                                     g_json_array_to_vector,
                                     1, 0, false,
              "(json:array->vector a) converts json array to s7 vector",
                                     pl_xx));

    s7_define_function(s7, "json:array-getter",
                       json_array_getter, 2, 0, false,
                       "(json:array-getter t k) gets value for key k from array t");
    s7_c_type_set_getter       (s7, json_array_type_tag, s7_name_to_value(s7, "json:array-getter"));

    s7_define_function(s7, "json:array-setter",
                       json_array_setter, 2, 0, false,
                       "(json:array-setter t k) sets value for key k from array t");
    s7_c_type_set_setter       (s7, json_array_type_tag, s7_name_to_value(s7, "json:array-setter"));

    //FIXME: why this here?
    string_string = s7_make_semipermanent_string(s7, "a string");
}

/* ****************************************************************
 * Helper functions
 */
s7_pointer json_array_to_vector(s7_scheme *s7, cJSON *ja, bool clone)
{
    TRACE_ENTRY(json_array_to_vector);
    size_t idx_ct = cJSON_GetArraySize(ja);
    s7_pointer the_vector = s7_make_vector(s7, idx_ct);
    cJSON *item;
    char  *str;
    int    n;
    double nbr;

    /* log_debug("array sz: %d", idx_ct); */
    for (size_t i = 0; i < idx_ct; i++) {
        item = cJSON_GetArrayItem(ja, i);
        if (cJSON_IsNumber(item)) {
            nbr = cJSON_GetNumberValue(item);
            if (nbr == trunc(nbr)) {
                n = (int)nbr;
                /* log_debug("\titem[%d] int: %d", i, n); */
                s7_vector_set(s7, the_vector, i, s7_make_integer(s7, n));
            } else {
                /* log_debug("\titem[%d] real: %f", i, nbr); */
                s7_vector_set(s7, the_vector, i, s7_make_real(s7, nbr));
            }
        }
        else if (cJSON_IsString(item)) {
            str = cJSON_GetStringValue(item);
            s7_vector_set(s7, the_vector, i, s7_make_string(s7, str));
        }
        else if (cJSON_IsBool(item)) {
            if (cJSON_IsTrue(item)) {
                s7_vector_set(s7, the_vector, i, s7_t(s7));
            }
            else if (cJSON_IsFalse(item)) {
                s7_vector_set(s7, the_vector, i, s7_f(s7));
            } else {
                log_error("Bad boolean");
            }
        }
        else if (cJSON_IsNull(item)) {
            // js null means "absence of any object value"
            // but '() means empty, not absence
            // that will have to do, since Scheme has no
            // way to express "absence of value" (i.e. null value)
            // (as opposed to null? which is a predicate)
            s7_vector_set(s7, the_vector, i, s7_nil(s7));
        }
        else if (cJSON_IsArray(item)) {
            /* log_debug("NESTED ARRAY"); */
            s7_pointer subarray =  json_array_to_vector(s7, item, clone);
            s7_vector_set(s7, the_vector, i, subarray);
        }
        else if (cJSON_IsObject(item)) {
            /* log_debug("NESTED OBJ"); */
            s7_pointer submap =  json_map_to_hash_table(s7, item, clone);
            s7_vector_set(s7, the_vector, i, submap);
        }
        else if (cJSON_IsRaw(item)) {
            log_error("Raw item");
            s7_vector_set(s7, the_vector, i, s7_undefined(s7));
        }
        else if (cJSON_IsInvalid(item)) {
            log_error("Invalid item");
            s7_vector_set(s7, the_vector, i, s7_unspecified(s7));
        }
        else {
            log_error("Bad cJSON item type");
        }
    }
    return the_vector;
}
