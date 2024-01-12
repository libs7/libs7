#include <stdio.h>
#include <stdlib.h>
#include "libs7.h"
#include "liblogc.h"

#define TO_STR(x) s7_object_to_c_string(s7, x)
#define NM_TO_STR(x) s7_object_to_c_string(s7, s7_name_to_value(s7, x))
#define TO_BOOL(x) s7_boolean(s7, s7_name_to_value(s7, x))
#define LOG_S7_DEBUG(msg, obj) ((tostr1 = TO_STR(obj)), (fprintf(stderr, GRN " S7: " CRESET "%s:%d " #msg ": %s\n", __FILE__, __LINE__, tostr1)), (free(tostr1)))

#define TRACE_S7_DUMP(lvl, fmt, obj)            \
    if (S7_DEBUG_LEVEL > lvl) { \
            char *s=s7_object_to_c_string(s7, obj); \
            log_debug(fmt, s); \
            fflush(NULL); \
            free(s); \
    }

