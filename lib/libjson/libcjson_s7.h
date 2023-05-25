#ifndef LIBCJSON_S7_H
#define LIBCJSON_S7_H

#include "cJSON.h"
/* #include "libjson_s7.h" */
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

/* signature stuff used for s7_define* */
extern s7_pointer pl_tx, pl_xs, pl_bxs, pl_xi, pl_xx, pl_txs, pl_xxs, pl_xsi, pl_xxi, pl_xsxt, pl_xsixt, pl_dx, pl_st, pl_sx, pl_ix;

/* json_map */
extern int json_object_type_tag;
char *json_object_to_string(s7_scheme *s7, const cJSON *jo);
void json_object_init(s7_scheme *s7, s7_pointer cur_env);

/* json_vector */
extern int json_vector_type_tag;
char *json_vector_to_string(s7_scheme *s7, const cJSON *jo);
void json_vector_init(s7_scheme *s7, s7_pointer cur_env);

#endif
