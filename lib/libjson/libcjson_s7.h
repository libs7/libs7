#ifndef LIBCJSON_S7_H
#define LIBCJSON_S7_H

#include "cJSON.h"
/* #include "libjson_s7.h" */
#include "libs7.h"

#if defined(DEVBUILD)
#define CJSON_TYPE_NAME(x) (char*)#x
extern char *cjson_types[256];
#endif

/* signature stuff used for s7_define* */
extern s7_pointer pl_tx, pl_xs, pl_bxs, pl_xi, pl_xx, pl_txs, pl_xxs, pl_xsi, pl_xxi, pl_xsxt, pl_xsixt, pl_dx, pl_st, pl_sx, pl_ix;

/* json_map */
extern int json_object_type_tag;
char *json_object_to_string(s7_scheme *s7, const cJSON *jo);
s7_pointer json_map_to_hash_table(s7_scheme *s7, cJSON *jo, bool clone);
void json_object_init(s7_scheme *s7, s7_pointer cur_env);

/* json_array */
extern int json_array_type_tag;
char *json_array_to_string(s7_scheme *s7, const cJSON *jo);
s7_pointer json_array_to_vector(s7_scheme *s7, cJSON *ja, bool clone);
void json_array_init(s7_scheme *s7, s7_pointer cur_env);

#endif
