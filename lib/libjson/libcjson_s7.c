#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "config.h"
#include "utils.h"
#include "libcjson_s7.h"
/* #include "cJSON.h" */
/* #include "libjson_s7.h" */
/* #include "libs7.h" */

#if defined(DEBUGGING)
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

/* signature stuff */
s7_pointer pl_tx, pl_xs, pl_bxs, pl_xi, pl_xx, pl_txs, pl_xxs, pl_xsi, pl_xxi, pl_xsxt, pl_xsixt, pl_dx, pl_st, pl_sx, pl_ix;

static s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string, integer_string;

static s7_pointer char___symbol, cJSON__symbol;

/* **************************************************************** */
/*   cJSON.h API */
/* **************************************************************** */

/* -------- cJSON_Version -------- */
s7_pointer json_cjson_version(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_string(sc, (char*)cJSON_Version()));
}


/* -------- json_read -------- */
static s7_pointer g_json_read(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_json_read);
    /* TRACE_S7_DUMP("args", args); */
    s7_pointer p, arg;
    char* json_str;

    if (args == s7_nil(s7)) {
        /* log_debug("null args"); */
        char buf[2 * 4096]; //FIXME: support arbitrary s len
        int i = 0;
        /* read from current-input-port, one char at a time */
        s7_pointer cip = s7_current_input_port(s7);

        //FIXME: use same code to read cip, file, and string ports

        s7_pointer c;
        while (true) {
            c = s7_read_char(s7, cip);
            if (c == s7_eof_object(s7)) {
                buf[i] = '\0';
                break;
            }
            buf[i++] = s7_character(c);
        }
        /* log_debug("readed string: %s", buf); */
        json_str = buf;
    } else {
        p = args;
        arg = s7_car(p);
        /* log_debug("is_input_port? %d", s7_is_input_port(s7, arg)); */
        /* log_debug("is_output_port? %d", s7_is_output_port(s7, arg)); */
        /* s7_pointer dt = s7_type_of(s7, arg); */
        /* trace_S7_DUMP("argtyp", dt); */
        if (s7_is_input_port(s7, arg)) {
            TRACE_LOG_DEBUG("read arg is input port", "");

            json_str = libs7_input_port_to_c_string(s7, arg);

        }
        else if (s7_is_string(arg)) {
            json_str = (char*)s7_string(arg);
        }
        else {
            return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "toml:read", 10), 1, arg, string_string));
        }
    }

    TRACE_LOG_DEBUG("parsing: %s", json_str);
    cJSON *jo = cJSON_Parse(json_str);
    if (jo == NULL) {
        log_error("cJSON_Parse error");
        const char *e = cJSON_GetErrorPtr();
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
// We do not use this, use json_read instead
/* static s7_pointer json_cJSON_Parse(s7_scheme *s7, s7_pointer args) */
/* { */
/*     TRACE_ENTRY(json_cJSON_Parse); */
/*     s7_pointer p, arg; */
/*     char* json_str; */
/*     p = args; */
/*     arg = s7_car(p); */
/*     if (s7_is_string(arg)) */
/*         json_str = (char*)s7_string(arg); */
/*     else { */
/*         return(s7_wrong_type_error(s7, */
/*                                    s7_make_string_wrapper_with_length(s7, "json:Parse", 10), */
/*                                    0, arg, string_string)); */
/*     } */
/*     log_debug("parsing: %s", json_str); */
/*     cJSON *jo = cJSON_Parse(json_str); */

/*     return(s7_make_c_object(s7, json_object_type_tag, (void*)jo)); */

/*     /\* return(s7_make_c_pointer_with_type(s7, (void*)jo, cJSON__symbol, s7_f(s7))); *\/ */
/* } */

/* -------- cJSON_ParseWithLength -------- */
static s7_pointer json_cJSON_ParseWithLength(s7_scheme *s7, s7_pointer args)
{
  s7_pointer p, arg;
  char* json_cJSON_ParseWithLength_0;
  size_t json_cJSON_ParseWithLength_1;
  p = args;
  arg = s7_car(p);
  if (s7_is_string(arg))
    json_cJSON_ParseWithLength_0 = (char*)s7_string(arg);
  else return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "json:ParseWithLength", 20), 1, arg, string_string));
  p = s7_cdr(p);
  arg = s7_car(p);
  if (s7_is_integer(arg))
    json_cJSON_ParseWithLength_1 = (size_t)s7_integer(arg);
  else return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "json:ParseWithLength", 20), 2, arg, integer_string));
  return(s7_make_c_pointer_with_type(s7, (void*)cJSON_ParseWithLength(json_cJSON_ParseWithLength_0, json_cJSON_ParseWithLength_1), cJSON__symbol, s7_f(s7)));
}


/* -------- cJSON_ParseWithOpts -------- */
static s7_pointer json_cJSON_ParseWithOpts(s7_scheme *s7, s7_pointer args)
{
  s7_pointer p, arg;
  p = args;
  arg = s7_car(p);              /* arg 0: char *value */
  char* value;
  if (s7_is_string(arg))
      value = (char*)s7_string(arg);
  else {
      return(s7_wrong_type_error(s7,
                                 s7_make_string_wrapper_with_length(s7, "json:ParseWithOpts", 18), 1, arg, string_string));
  }
  p = s7_cdr(p);                /* arg 1: char **return_parse_end */
  arg = s7_car(p);
  const char** return_parse_end = (const char**)s7_c_pointer_with_type(s7, arg, char___symbol, __func__, 2);
  p = s7_cdr(p);         /* arg 2: cJSON_bool require_null_terminated */
  arg = s7_car(p);
  cJSON_bool require_null_terminated;
  if (s7_is_integer(arg))
    require_null_terminated = (cJSON_bool)s7_integer(arg);
  else {
      return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "json:ParseWithOpts", 18), 3, arg, boolean_string));
  }

  const cJSON *cjson = cJSON_ParseWithOpts(value, return_parse_end, require_null_terminated);
  return(s7_make_c_pointer_with_type(s7, (void*)cjson, cJSON__symbol, s7_f(s7)));
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

/* /\* -------- cJSON_GetObjectItem -------- *\/ */
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


/* /\* -------- cJSON_GetObjectItemCaseSensitive -------- *\/ */
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


/* /\* -------- cJSON_HasObjectItem -------- *\/ */
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
    json_vector_init(s7, cur_env);

    /* cJSON_,_symbol = s7_make_symbol(s7, "cJSON*,"); */
    char___symbol = s7_make_symbol(s7, "char**");
    cJSON__symbol = s7_make_symbol(s7, "cJSON*");

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:raw?"),
              s7_make_typed_function(s7, "json:IsRaw", json_cJSON_IsRaw, 1, 0, false, "cJSON_bool cJSON_IsRaw(cJSON*)", pl_tx));

    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:array?"), */
    /*           s7_make_typed_function(s7, "json:array?", */
    /*                                  g_json_cJSON_IsArray, 1, 0, false, */
    /*                                  "(json:array? obj)", pl_tx)); */

    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:array?"), */
    /*           s7_make_typed_function(s7, "json:array?", */
    /*                                  g_json_cJSON_IsArray, 1, 0, false, */
    /*                                  "(json:array? obj)", pl_tx)); */

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

    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:HasObjectItem"), */
    /*           s7_make_typed_function(s7, "json:HasObjectItem", json_cJSON_HasObjectItem, 2, 0, false, "cJSON_bool cJSON_HasObjectItem(cJSON*, char*)", pl_txs)); */

    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:GetObjectItemCaseSensitive"), */
    /*           s7_make_typed_function(s7, "json:GetObjectItemCaseSensitive", json_cJSON_GetObjectItemCaseSensitive, 2, 0, false, "cJSON* cJSON_GetObjectItemCaseSensitive(cJSON*, char*)", pl_xxs)); */

    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:GetObjectItem"), */
    /*           s7_make_typed_function(s7, "json:GetObjectItem", json_cJSON_GetObjectItem, 2, 0, false, "cJSON* cJSON_GetObjectItem(cJSON*, char*)", pl_xxs)); */

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:Print"),
              s7_make_typed_function(s7, "json:Print", json_cJSON_Print, 1, 0, false, "char* cJSON_Print(cJSON*)", pl_sx));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:read"),
              s7_make_typed_function(s7, "json:read",
                                     g_json_read,
                                     0, 1, false,
                                     "(json:read x) read JSON from string or port",
                                     NULL));

    /* s7_define(s7, cur_env, */
    /*           s7_make_symbol(s7, "json:Parse"), */
    /*           s7_make_typed_function(s7, "json:Parse", */
    /*                                  json_cJSON_Parse, */
    /*                                  1, 0, false, */
    /*                                  "(json:Parse s) parse string s", */
    /*                                  pl_xs)); */

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:ParseWithLengthOpts"),
              s7_make_typed_function(s7, "json:ParseWithLengthOpts", json_cJSON_ParseWithLengthOpts, 4, 0, false, "cJSON* cJSON_ParseWithLengthOpts(char* size_t char** cJSON_bool)", pl_xsixt));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:ParseWithOpts"),
              s7_make_typed_function(s7, "json:ParseWithOpts", json_cJSON_ParseWithOpts, 3, 0, false, "cJSON* cJSON_ParseWithOpts(char* char** cJSON_bool)", pl_xsxt));

    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:ParseWithLength"),
              s7_make_typed_function(s7, "json:ParseWithLength", json_cJSON_ParseWithLength, 2, 0, false, "cJSON* cJSON_ParseWithLength(char* size_t)", pl_xsi));

    s7_define_constant(s7, "*json:version*",
                       s7_make_string(s7, (char*)cJSON_Version()));
    s7_define(s7, cur_env,
              s7_make_symbol(s7, "json:Version"),
              s7_make_typed_function(s7, "json:version",
                                     json_cjson_version, 0, 0, false,
                                     "json:version", pl_st));
    s7_set_shadow_rootlet(s7, old_shadow);

    return(cur_env);
}
