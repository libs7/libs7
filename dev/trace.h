#ifndef TRACE_H
#define TRACE_H
#include "ansi_colors.h"

#ifdef TRACING
#define TRACE_ENTRY(fn) log_trace(RED "entry:" CRESET " " #fn);
#else
#define TRACE_ENTRY(fn)
#endif

#ifdef TRACING
#define TRACE_ENTRY_STR(fn, str) \
    log_trace(RED "entry:" CRESET " " #fn ": %s", str);
#else
#define TRACE_ENTRY_STR(fn, str)
#endif

#ifdef TRACING
#define TRACE_S7_DUMP(msg, x) (({char*s=s7_object_to_c_string(s7, x);log_debug("%s: '%.60s' (first 60 chars)", msg, s);fflush(NULL);free(s);}))
#else
#define TRACE_S7_DUMP(msg, x)
#endif

#ifdef TRACING
#define TRACE_CJSON_DUMP(msg, x)
#else
#define TRACE_CJSON_DUMP(msg, x)
#endif

#ifdef TRACING
#define TRACE_TOMLC99_DUMP(msg, x)
#else
#define TRACE_TOMLC99_DUMP(msg, x)
#endif

#ifdef TRACING
#define DUMP_PREFIX(pfx) \
    if (pfx) {                                          \
        log_debug("\tpfx start: '%.15s'", (pfx)->start);     \
        log_debug("\tpfx len: '%d'", (pfx)->len);     \
    } else {                                            \
        log_debug("prefix: NULL");                      \
    }
#else
#define DUMP_PREFIX(pfx)
#endif

#ifdef TRACING
#define TRACE_LOG_DEBUG(fmt, msg) log_debug(fmt, msg)
#else
#define TRACE_LOG_DEBUG(fmt, msg)
#endif

#endif
