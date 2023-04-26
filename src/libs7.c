#include "s7.h"
#include "libs7.h"

bool libs7_verbose;

void fs_api_init(s7_scheme *sc);

static char buf[512]; // max len of <libname>_init or lib/shared/<libname><ext>

s7_scheme *libs7_init(void)
{
  s7_scheme *s7 = s7_init();
  fs_api_init(s7);
  return s7;
}

/* import bindings into root env */
void clib_dload_global(s7_scheme *s7,
                       char *libname,   /* e.g. libc_s7 */
                       char *libns,     /* e.g. libc */
                       char *dso_ext)   /* .dylib or .so */
{
    /* log_debug("clib_dload"); */
    /* int len = strlen(libname); */
    sprintf(buf, "%s_init", libname);
    s7_pointer init_sym    = s7_make_symbol(s7, "init_func");
    s7_pointer init_fn_sym = s7_make_symbol(s7, buf);

    // (rootlet)               the top-level (global) environment
    // (curlet)                the current (innermost) environment
    // (varlet env . bindings)  add new bindings directly to env

    s7_pointer e = s7_varlet(s7,
                             s7_rootlet(s7),
                             init_sym, init_fn_sym);
    /* s7_int gc_loc = s7_gc_protect(s7, e); */
    /* s7_pointer old_e = s7_set_curlet(s7, e); */

    char *ws = getenv("TEST_WORKSPACE");
    /* log_debug("ws: %s", ws); */
    char *fmt;
    if (ws) {
        if ( (strncmp("libs7", ws, 5) == 0)
             && strlen(ws) == 5 ) {
            fmt = "lib/shared/%s%s";
        } else {
            fmt = "external/libs7/lib/shared/%s%s";
        }
    } else {
        fmt = "external/libs7/lib/shared/%s%s";
    }
    snprintf(buf,
             512, // 20 + 18 + len + strlen(dso_ext),
             fmt,
             libname, dso_ext);
    /* log_debug("dso: %s", buf); */

    s7_pointer val = s7_load(s7, buf);
    /* s7_pointer val = s7_load_with_environment(s7, buf, e); */

    if (val) {
        if (libs7_verbose)
            log_debug("loaded %s", buf);
        s7_pointer libs = s7_slot(s7, s7_make_symbol(s7, "*libraries*"));
        /* snprintf(buf, strlen(libns) + 3, "*%s*", libns); */
        /* s7_define(s7, s7_nil(s7), s7_make_symbol(s7, buf), e); */
        snprintf(buf, strlen(libns) + 5, "%s.scm", libns);
        s7_slot_set_value(s7, libs,
              s7_cons(s7,
                s7_cons(s7, s7_make_semipermanent_string(s7, buf), e),
                            s7_slot_value(libs)));
    }
    /* s7_set_curlet(s7, old_e);       /\* restore incoming (curlet) *\/ */
    /* s7_gc_unprotect_at(s7, gc_loc); */
    if (!val) {
        log_error("load fail: %s", buf);
        /* exit(EXIT_FAILURE); */
    }
}

/* import bindings into ns sym, e.g. *libc* */
void clib_dload_ns(s7_scheme *s7,
                char *libname,   /* e.g. libc_s7 */
                char *libns,     /* e.g. libc */
                char *dso_ext)   /* .dylib or .so */
{
    /* log_debug("clib_dload"); */
    /* int len = strlen(libname); */
    sprintf(buf, "%s_init", libname);
    s7_pointer init_sym    = s7_make_symbol(s7, "init_func");
    s7_pointer init_fn_sym = s7_make_symbol(s7, buf);
    s7_pointer init_list = s7_list(s7, 2, init_sym, init_fn_sym);

    s7_pointer e = s7_inlet(s7, init_list);
    s7_int gc_loc = s7_gc_protect(s7, e);
    s7_pointer old_e = s7_set_curlet(s7, e);

    char *ws = getenv("TEST_WORKSPACE");
    /* log_debug("ws: %s", ws); */
    char *fmt;
    if (ws) {
        if ( (strncmp("libs7", ws, 5) == 0)
             && strlen(ws) == 5 ) {
            fmt = "lib/shared/%s%s";
        } else {
            fmt = "external/libs7/lib/shared/%s%s";
        }
    } else {
        fmt = "external/libs7/lib/shared/%s%s";
    }
    snprintf(buf,
             512, // 20 + 18 + len + strlen(dso_ext),
             fmt,
             libname, dso_ext);
    /* log_debug("dso: %s", buf); */

    s7_pointer val = s7_load_with_environment(s7, buf, e);

    if (val) {
        if (libs7_verbose)
            log_debug("loaded %s", buf);
        s7_pointer libs = s7_slot(s7, s7_make_symbol(s7, "*libraries*"));
        snprintf(buf, strlen(libns) + 3, "*%s*", libns);
        s7_define(s7, s7_nil(s7), s7_make_symbol(s7, buf), e);
        snprintf(buf, strlen(libns) + 5, "%s.scm", libns);
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

void clib_sinit(s7_scheme *s7,
                s7_pointer (fnptr)(s7_scheme *sc),
                char *libns)     /* e.g. libc */
{

    s7_pointer e = s7_inlet(s7, s7_nil(s7)); // empty env
    s7_int gc_loc = s7_gc_protect(s7, e);
    s7_pointer old_e = s7_set_curlet(s7, e);
    s7_pointer old_shadow = s7_set_shadow_rootlet(s7, e);

    /* libc_s7_init(s7); */
    s7_pointer clib_let = fnptr(s7);

    /* if (libs7_verbose) */
    log_debug("initialized static lib: %s", libns);

    /* s7_varlet(s7, s7_rootlet(s7), clib_let); */

    s7_pointer libs = s7_slot(s7, s7_make_symbol(s7, "*libraries*"));
    snprintf(buf, strlen(libns) + 3, "*%s*", libns);
    s7_define(s7, s7_nil(s7), s7_make_symbol(s7, buf), clib_let); // e);
    snprintf(buf, strlen(libns) + 5, "%s.scm", libns);
    s7_slot_set_value(s7, libs,
                      s7_cons(s7,
                              s7_cons(s7, s7_make_semipermanent_string(s7, buf), clib_let), // e),
                              s7_slot_value(libs)));
    s7_set_curlet(s7, old_e);       /* restore incoming (curlet) */
    s7_gc_unprotect_at(s7, gc_loc);

    s7_set_shadow_rootlet(s7, old_shadow);
}
