/* config/config.h.  Generated from config.h.in by configure.  */
#ifndef TRACE_H
#define TRACE_H

/* Define as 1 if you have open_memstream.  */
#define HAVE_OPEN_MEMSTREAM 1

#ifndef __STDC_LIB_EXT1__
typedef int errno_t;
#endif

#include "CException.h"
#include "log.h"

#define MALLOC_CHUNK 2048

// #define TOMLX_TRACING
#if defined(TOMLX_TRACING)
#define TOMLX_ENTRY TRACE_ENTRY
#define TOMLX_ENTRY_STR TRACE_ENTRY_STR
#define TOMLX_LOG_DEBUG TRACE_LOG_DEBUG
#else
#define TOMLX_ENTRY(fn)
#define TOMLX_ENTRY_STR(fn,str)
#define TOMLX_LOG_DEBUG(msg, arg)
#endif

#ifdef TRACING
#include "ansi_colors.h"
#endif

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
#define TRACE_TOMLC99_DUMP(msg, x)  \
    do {                            \
    log_debug(msg);                 \
    if (x) {                        \
        switch(x->type) {           \
        case TOML_INT:            \
            log_debug("\t  int: %lld", x->u.i);           \
            break;                              \
        case TOML_ARRAY:            \
            ; \
            s = tomlx_array_to_string(x->u.a, true); \
            log_debug("\t    %s", s);                    \
            free(s);                                  \
            break;                              \
        case TOML_TABLE:            \
            ; \
            s = tomlx_table_to_string(x->u.t, true); \
            log_debug("\t    %s", s);                    \
            free(s);                                  \
            break;                              \
        default:                                \
            log_debug("\t\ttype: %d", x->type);     \
        }                                       \
    } else                                      \
        log_debug("\t  NULL");                  \
    } while(0)
#else
#define TRACE_TOMLC99_DUMP(msg, x)
#endif

#ifdef TRACING
#define DUMP_PREFIX(pfx) \
    if (pfx) {                                          \
        log_debug("\tpfx start: '%.15s'", (pfx)->start);     \
        log_debug("\tpfx len: '%d'", (pfx)->len);     \
        log_debug("\tpfx prefix: '%p'", (pfx)->prefix);     \
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

#ifdef TRACING
#define TRACE_LOG_TIMESTAMP(msg, ts)                    \
    do {                                               \
        char *ts = tomlx_datetime_to_string(ts, true);  \
        log_debug(msg, ts);                             \
     } while(0)
#else
#define TRACE_LOG_TIMESTAMP(msg, ts)
#endif

#endif
