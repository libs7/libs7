#include <stdio.h>
#include <stdlib.h>
#include "libs7.h"
#include "liblogc.h"

#define TRACE_S7_DUMP(lvl, fmt, obj)            \
    if (S7_DEBUG_LEVEL > lvl) { \
            char *s=s7_object_to_c_string(s7, obj); \
            log_debug(fmt, s); \
            fflush(NULL); \
            free(s); \
    }

