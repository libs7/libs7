/* WARNING: depends on @liblogc */
/*
  WARNING: client must define local (namespaced) macro
  S7_DEBUG_LEVEL, e.g.
#define S7_DEBUG_LEVEL debug_libs7
int  S7_DEBUG_LEVEL;

This makes fine-grained logging possible; different files can
use different DEBUG_S7_LEVEL flag names in the same build.
 */

#include <stdio.h>
#include <stdlib.h>
#include "libs7.h"
#include "liblogc.h"

#define TO_STR(x) s7_object_to_c_string(s7, x)
#define NM_TO_STR(x) s7_object_to_c_string(s7, s7_name_to_value(s7, x))
#define TO_BOOL(x) s7_boolean(s7, s7_name_to_value(s7, x))

#define LOG_S7_INFO(lvl, msg, obj) \
    if (S7_DEBUG_LEVEL>lvl) { \
        char *tostr1 = TO_STR(obj); \
        fprintf(stderr, GRN "INFO " CRESET "S7: %s:%d " #msg " %s\n", __FILE__, __LINE__, tostr1); \
        free(tostr1);                                                   \
    }

#define LOG_S7_DEBUG(lvl, msg, obj) \
    if (S7_DEBUG_LEVEL>lvl) { \
        char *tostr1 = TO_STR(obj); \
        fprintf(stderr, CYN "DEBUG " CRESET "S7: %s:%d " #msg " %s\n", __FILE__, __LINE__, tostr1); \
        free(tostr1);                                                   \
    }

#define LOG_S7_ERROR(lvl, msg, obj) \
    if (S7_DEBUG_LEVEL>lvl) { \
        char *tostr1 = TO_STR(obj); \
        fprintf(stderr, RED "ERROR " CRESET "S7: %s:%d " #msg " %s\n", __FILE__, __LINE__, tostr1); \
        free(tostr1);                                                   \
    }
