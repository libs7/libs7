// if linux
#define _GNU_SOURCE
#include <dlfcn.h>
#undef _GNU_SOURCE
#include <libgen.h>

#include "s7.h"
#include "libs7.h"

bool libs7_verbose        = false;
bool libs7_debug          = false;
bool libs7_debug_runfiles = false;
bool libs7_trace          = false;

void fs_api_init(s7_scheme *sc);

static char buf[512]; // max len of <libname>_init or lib/shared/<libname><ext>

/* **************************************************************** */
static void _dload_clib(s7_scheme *s7, char *lib)
{
#if defined(DEBUG_TRACE)
    if (libs7_trace)
        log_trace("_dload_clib: %s", lib);
#endif
    /* int len = strlen(libname); */
    sprintf(buf, "lib%s_s7_init", lib);
    s7_pointer init_sym    = s7_make_symbol(s7, "init_func");
    s7_pointer init_fn_sym = s7_make_symbol(s7, buf);
    s7_pointer init_list = s7_list(s7, 2, init_sym, init_fn_sym);

    s7_pointer e = s7_inlet(s7, init_list);
    s7_int gc_loc = s7_gc_protect(s7, e);
    s7_pointer old_e = s7_set_curlet(s7, e);

    char *ws = getenv("TEST_WORKSPACE");
    /* log_debug("dload ns test ws: %s", ws); */
    char *fmt;
    if (ws) {
        if ( (strncmp("libs7", ws, 5) == 0)
             && strlen(ws) == 5 ) {
            fmt = "lib/lib%s/lib%s_s7%s";
        } else {
            fmt = "external/libs7/lib/lib%s/lib%s_s7%s";
        }
    } else {
        ws = getenv("BUILD_WORKSPACE_DIRECTORY");
        /* log_debug("dload ns build ws: %s", ws); */
        if (strncmp(basename(ws), "libs7", 5) == 0)
            fmt = "lib/lib%s/lib%s_s7%s";
        else
            fmt = "external/libs7/lib/lib%s/lib%s_s7%s";
    }
    snprintf(buf,
             512, // 20 + 18 + len + strlen(dso_ext),
             fmt,
             lib,
             lib, DSO_EXT);
#if defined(DEBUG_TRACE)
    log_debug("dso: %s", buf);
#endif

    s7_pointer val = s7_load_with_environment(s7, buf, e);

    if (val) {
        if (libs7_verbose)
            log_debug("loaded %s", buf);
        s7_pointer libs = s7_slot(s7, s7_make_symbol(s7, "*libraries*"));
        snprintf(buf, strlen(lib) + 3, "*%s*", lib);
        s7_define(s7, s7_nil(s7), s7_make_symbol(s7, buf), e);
        snprintf(buf, strlen(lib) + 5, "%s.scm", lib);
        s7_slot_set_value(s7, libs,
              s7_cons(s7,
                s7_cons(s7, s7_make_semipermanent_string(s7, buf), e),
                            s7_slot_value(libs)));
    }
    s7_set_curlet(s7, old_e);       /* restore incoming (curlet) */
    s7_gc_unprotect_at(s7, gc_loc);
    if (!val) {
        log_error("load fail: %s", buf);
        /* exit(EXIT_FAILURE); */
    }
}

void libs7_load_clib(s7_scheme *s7, char *lib)
{
#if defined(DEBUG_TRACE)
    if (libs7_trace)
        log_trace("load_clib: %s", lib);
#endif

    static char init_fn_name[512]; // max len of <libname>_init or lib/shared/<libname><ext>

    init_fn_name[0] = '\0';
    sprintf(init_fn_name, "lib%s_s7_init", lib);
#if defined(DEBUG_TRACE)
    if (libs7_debug)
        log_debug("init_fn_name: %s", init_fn_name);
#endif

    s7_pointer (*init_fn_ptr)(s7_scheme*);
    init_fn_ptr = dlsym(RTLD_DEFAULT, init_fn_name); // mac: RTLD_SELF?
    if (init_fn_ptr == NULL) {
#if defined(DEBUG_TRACE)
        log_debug("%s not statically linked, trying dload", init_fn_name);
#endif
        _dload_clib(s7, lib);
        return;
    }

#if defined(DEBUG_TRACE)
    if (libs7_debug)
        log_debug("dlsym init_fn_ptr: %x", init_fn_ptr);
#endif

    s7_pointer e = s7_inlet(s7, s7_nil(s7)); // empty env
    s7_int gc_loc = s7_gc_protect(s7, e);
    s7_pointer old_e = s7_set_curlet(s7, e);
    s7_pointer old_shadow = s7_set_shadow_rootlet(s7, e);

    s7_pointer clib_let = init_fn_ptr(s7);

    /* s7_varlet(s7, s7_rootlet(s7), clib_let); */

    s7_pointer libs = s7_slot(s7, s7_make_symbol(s7, "*libraries*"));
    snprintf(buf, strlen(lib) + 3, "*%s*", lib);
    s7_define(s7, s7_nil(s7), s7_make_symbol(s7, buf), clib_let); // e);
    snprintf(buf, strlen(lib) + 5, "%s.scm", lib);
    s7_slot_set_value(s7, libs,
                      s7_cons(s7,
                              s7_cons(s7, s7_make_semipermanent_string(s7, buf), clib_let), // e),
                              s7_slot_value(libs)));
    s7_set_curlet(s7, old_e);       /* restore incoming (curlet) */
    s7_gc_unprotect_at(s7, gc_loc);

    s7_set_shadow_rootlet(s7, old_shadow);
}

static s7_pointer g_libs7_load_clib(s7_scheme *s7, s7_pointer args)
{
#if defined(DEBUG_TRACE)
    log_debug("g_libs7_load_clib");
#endif
    s7_pointer p, arg;
    char* lib;
    p = args;
    arg = s7_car(p);
    if (s7_is_string(arg)) {
        lib = (char*)s7_string(arg);
    } else {
        if (s7_is_symbol(arg)) {
            lib = (char*)s7_symbol_name(arg);
        }
        else return(s7_wrong_type_arg_error(s7, __func__, 0, arg, "string"));
    }
    libs7_load_clib(s7, lib);
}

s7_scheme *libs7_init(void)
/* WARNING: dload logic assumes file path <libns>/<libname><dso_ext> */
{
  s7_scheme *s7 = s7_init();
  fs_api_init(s7);

  //FIXME: add runfiles to *load-path*?
  s7_define_function(s7, "load-clib", g_libs7_load_clib,
                     1,         /* required: 1 arg, libname */
                     0,         /* optional: 0 */
                     false,     /* rest args: none */
                     "(load-clib 'libsym) initializes statically linked clib archives and dsos, "
                     "and dloads and initializes dynamically linked dsos. "
                     "libsym may be symbol or string; it should not include 'lib' prefix and '_s7' suffix.");

  return s7;
}
