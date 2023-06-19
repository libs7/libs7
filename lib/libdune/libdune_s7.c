#if defined(DEVBUILD)
#include <execinfo.h>           /* backtrace */
#include <unistd.h>             /* write */
#endif
#include <libgen.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/errno.h>

#include "config.h"
#include "utstring.h"
#include "libdune_s7.h"
#include "error_handler_dune.h"
#include "s7.h"

extern bool  verbose;

s7_pointer c_pointer_string, string_string, character_string, boolean_string, real_string, complex_string;
s7_pointer integer_string;
static s7_pointer int64_t__symbol, FILE__symbol;

/* needed by read thunk and catcher */
/* const char *g_dunefile; */
s7_pointer g_dune_inport;
s7_pointer g_stanzas;
s7_pointer result;
s7_int gc_dune_inport = -1;
s7_int gc_stanzas = -1;
s7_int gc_dune_read_thunk_catcher_s7 = -1;
s7_pointer e7; // tmp var for error printing
const char *e; // tmp var for error printing

const char *errmsg;

s7_pointer _dune_read_thunk_catcher_s7;

s7_pointer fix_dunefile(s7_scheme *s7, const char *dunefile_name)
{
    TRACE_ENTRY(fix_dunefile);
    //FIXME: this duplicates the code in load_project:_read_dunefile

    const char *dunestring = dunefile_to_string(dunefile_name);

    /* log_debug("readed string: %s", dunestring); */

    /* now s7_read using string port */

    /* first config err handling. clears out prev. error */
    /* close_error_config(); */
    /* error_config(); */
    /* init_error_handling(); */

    /* s7_pointer sport = s7_open_input_string(s7, dunestring); */
    g_dune_inport = s7_open_input_string(s7, dunestring);
    s7_int gc_baddot_loc = s7_gc_protect(s7, g_dune_inport);

    /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */

    if (!s7_is_input_port(s7, g_dune_inport)) {
        log_error("BAD INPUT PORT");
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            log_error("[%s\n]", errmsg);
            /* s7_shutdown(s7); */
            /* exit(EXIT_FAILURE); */
            s7_error(s7, s7_make_symbol(s7, "bad input port"),
                     s7_nil(s7));
        }
    }
#if defined(DEVBUILD)
    log_debug("s7_open_input_string for error correction");
#endif

    /* /\* stanza accumulator *\/ */
    /* s7_pointer stanzas = s7_list(s7, 0); */

    s7_pointer stanzas = s7_call_with_catch(s7,
                                    s7_t(s7),      /* tag */
                                    // _dune_read_thunk_s7
                                    s7_name_to_value(s7, "-dune-read-thunk"),
                                    _dune_read_thunk_catcher_s7
                                    /* s7_name_to_value(s7, "-dune-read-thunk-catcher") */
                                    );
    /* TRACE_S7_DUMP("fixed stanzas", stanzas); */

    /* s7_pointer stanza; */
    /* /\* read all stanzas in dunefile *\/ */
    /* while(true) { */
    /*     log_debug("iter"); */
    /*     /\* stanza = s7_read(s7, g_dune_inport); *\/ */
    /*     /\* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); *\/ */
    /*     /\* if ((errmsg) && (*errmsg)) { *\/ */
    /*     /\*     log_error("[%s\n]", errmsg); *\/ */
    /*     /\*     s7_close_input_port(s7, sport); *\/ */
    /*     /\*     /\\* s7_shutdown(s7); *\\/ *\/ */
    /*     /\*     /\\* exit(EXIT_FAILURE); *\\/ *\/ */
    /*     /\*     s7_error(s7, s7_make_symbol(s7, "bad input port"), *\/ */
    /*     /\*              s7_nil(s7)); *\/ */
    /*     /\*     break; *\/ */
    /*     /\* } *\/ */

    /*     /\* stanza = s7_call(s7, s7_name_to_value(s7, "read"), *\/ */
    /*     /\*                  s7_list(s7, 1, sport)); *\/ */
    /*     stanza = s7_call_with_catch(s7, */
    /*                                 s7_t(s7),      /\* tag *\/ */
    /*                                 // _dune_read_thunk_s7 */
    /*                                 s7_name_to_value(s7, "-dune-read-thunk"), */
    /*                                 _dune_read_thunk_catcher_s7 */
    /*                                 /\* s7_name_to_value(s7, "-dune-read-thunk-catcher") *\/ */
    /*                                 ); */

    /*     TRACE_S7_DUMP("stanza: %s", stanza); */
    /*     if (stanza == s7_eof_object(s7)) break; */
    /*     /\* log_debug("stanza: %s", TO_STR(stanza)); *\/ */
    /*     if (s7_is_null(s7,stanzas)) { */
    /*         stanzas = s7_list(s7, 1, stanza); */
    /*     } else{ */
    /*         stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza)); */
    /*     } */
    /* } */

    /* s7_close_input_port(s7, sport); */
    // g_dune_inport will be closed by caller
    s7_gc_unprotect_at(s7, gc_baddot_loc);
    /* close_error_config(); */

    /* leave error config as-is */
    free((void*)dunestring);
    return s7_reverse(s7, stanzas);
}

#if defined(DEVBUILD)
/* https://stackoverflow.com/questions/6934659/how-to-make-backtrace-backtrace-symbols-print-the-function-names */
static void full_write(int fd, const char *buf, size_t len)
{
        while (len > 0) {
                ssize_t ret = write(fd, buf, len);

                if ((ret == -1) && (errno != EINTR))
                        break;

                buf += (size_t) ret;
                len -= (size_t) ret;
        }
}

void print_c_backtrace(void) // s7_scheme *s7)
{
    static const char s7_start[] = "S7 STACKTRACE ------------\n";
    static const char c_start[] = "C BACKTRACE ------------\n";
    static const char end[] = "----------------------\n";

    full_write(STDERR_FILENO, s7_start, strlen(s7_start));

    /* s7_show_stack(s7); */
    /* s7_stacktrace(s7); */
    /* s7_flush_output_port(s7, s7_current_error_port(s7)); */
    /* fflush(NULL); */

    full_write(STDERR_FILENO, end, strlen(end));

    void *bt[1024];
    int bt_size;
    char **bt_syms;
    int i;

    bt_size = backtrace(bt, 1024);
    bt_syms = backtrace_symbols(bt, bt_size);
    full_write(STDERR_FILENO, c_start, strlen(c_start));
    for (i = 1; i < bt_size; i++) {
        size_t len = strlen(bt_syms[i]);
        full_write(STDERR_FILENO, bt_syms[i], len);
        full_write(STDERR_FILENO, "\n", 1);
    }
    full_write(STDERR_FILENO, end, strlen(end));
    free(bt_syms);
    /* fflush(NULL); */
}
#endif

/*
 * precondition: g_dune_inport is set
 */
s7_pointer _dune_read_thunk(s7_scheme *s7, s7_pointer args)
/* s7_pointer _dune_read_port(s7_scheme *s7, s7_pointer inport) */
{
    TRACE_ENTRY(_dune_read_thunk);
    (void)args;

    if (!s7_is_input_port(s7, g_dune_inport)) {
        errmsg = s7_get_output_string(s7, s7_current_error_port(s7));
        if ((errmsg) && (*errmsg)) {
            log_error("[%s\n]", errmsg);
            //FIXME: throw error
            libs7_shutdown(s7);
            exit(EXIT_FAILURE);
        }
        //FIXME: throw error
    } else {
#if defined(DEVBUILD)
        /* s7_pointer port_filename = s7_call(s7, */
        /*                               s7_name_to_value(s7, "port-filename"), */
        /*                               s7_list(s7, 1, inport)); */
        /*     log_trace("reading dunefile from input port: %s", */
        /*               TO_STR(port_filename)); */
#endif
    }

    g_stanzas = s7_list(s7, 0); // s7_nil(s7));
    gc_stanzas = s7_gc_protect(s7, g_stanzas);

    // so read thunk can access port:
    /* g_dunefile_port = inport; */
    /* gc_dune_inport = s7_gc_protect(s7, g_dunefile_port); */
    gc_dune_inport = s7_gc_protect(s7, g_dune_inport);

#if defined(DEVBUILD)
    const char *dunefile = s7_port_filename(s7, g_dune_inport);
    log_debug("reading dunefile: %s", dunefile);
#endif

    //FIXME: error handling
    /* close_error_config(); */
    /* error_config(); */
    /* init_error_handling(); */

    TRACE_LOG_DEBUG("reading stanzas from dunefile", "");
    /* s7_show_stack(s7); */
/* #if defined(DEVBUILD) */
/*     print_c_backtrace(); */
/* #endif */

    /* repeat until all objects read */
    while(true) {
#if defined(DEVBUILD)
        log_trace("reading stanza");
#endif

        /* s7_show_stack(s7); */
        /* print_c_backtrace(); */
        s7_pointer stanza = s7_read(s7, g_dune_inport);
        if (stanza == s7_eof_object(s7)) {
            /* log_debug("EOF"); */
            break;
        }

/* #if defined(DEVBUILD) */
        TRACE_S7_DUMP("Readed stanza", stanza);
/* #endif */
        /* s7_show_stack(s7); */
        /* print_c_backtrace(); */
        /* errmsg = s7_get_output_string(s7, s7_current_error_port(s7)); */
        /* log_error("errmsg: %s", errmsg); */


        /* close_error_config(); */
        /* init_error_handling(); */
        /* error_config(); */
        /* s7_gc_unprotect_at(s7, gc_dune_inport); */

        if (stanza == s7_eof_object(s7)) {
#if defined(DEVBUILD)
            log_trace("readed eof");
#endif
            break;
        }

        /* LOG_S7_DEBUG("SEXP", stanza); */
        /* if (mibl_debug_traversal) */
        /*     LOG_S7_DEBUG("stanza", stanza); */


        // handle (include ...) stanzas
        if (s7_is_pair(stanza)) {
            if (s7_is_equal(s7, s7_car(stanza),
                            s7_make_symbol(s7, "include"))) {
                /* log_debug("FOUND (include ...)"); */
                /* we can't insert a comment, e.g. ;;(include ...)
                   instead we would have to put the included file in an
                   alist and add a :comment entry. but we needn't bother,
                   we're not going for roundtrippability.
                */

                s7_pointer inc_file = s7_cadr(stanza);
                /* LOG_S7_DEBUG("    including", inc_file); */
                UT_string *dunepath;
                utstring_new(dunepath);
                const char *tostr = s7_string(inc_file);
                utstring_printf(dunepath, "%s/%s",
                                //FIXME: dirname may mutate its arg
                                //dirname(path),
                                "FOOTEST", //FIXME
                                tostr);

                //FIXME: implement dune_read_include(path)
                /* s7_pointer nested = read_dunefile(utstring_body(dunepath)); */
                /* g_dunefile_port = dunefile_port; */
                /* /\* LOG_S7_DEBUG("nested", nested); *\/ */
                /* /\* LOG_S7_DEBUG("stanzas", stanzas); *\/ */
                /* stanzas = s7_append(s7,stanzas, nested); */
                /* alt: (:include "(include dune.inc)" (included ...)) */
            } else {
                g_stanzas = s7_cons(s7, stanza, g_stanzas);
                TRACE_S7_DUMP("g_stanzas", g_stanzas);
                /* if (s7_is_null(s7,stanzas)) { */
                /*     stanzas = s7_cons(s7, stanza, stanzas); */
                /* } else{ */
                /*     stanzas = s7_append(s7,stanzas, s7_list(s7, 1, stanza)); */
                /* } */
            }
        } else {
            /* stanza not a pair - automatically means corrupt dunefile? */
            log_error("corrupt dune file? %s\n",
                      // utstring_body(dunefile_name)
                      "FIXME"
                      );
            /* if exit-on-error */
            exit(EXIT_FAILURE);
        }
    }
    s7_gc_unprotect_at(s7, gc_dune_inport);
    /* fprintf(stderr, "s7_gc_unprotect_at gc_dune_inport: %ld\n", (long)gc_dune_inport); */
    /* s7_close_input_port(s7, inport); */
    // g_dune_inport must be closed by caller (e.g. with-input-from-file)

#if defined(DEVBUILD)
    log_debug("finished reading dunefile");
#endif

    return g_stanzas;
    /* s7_close_input_port(s7, dunefile_port); */
    /* s7_gc_unprotect_at(s7, gc_loc); */
}

s7_pointer _dune_read_thunk_s7; /* initialized by init fn */

/* impl of _dune_read_thunk_s7 */
/* call by s7_call_with_catch as body arg*/
/* s7_pointer x_dune_read_thunk(s7_scheme *s7, s7_pointer args) { */
/*     (void)args; */
/*     TRACE_ENTRY(_dune_read_thunk); */
/* /\* #if defined(DEVBUILD) *\/ */
/* /\*     print_c_backtrace(); *\/ */
/* /\* #endif *\/ */
/*     /\* log_debug("reading dunefile: %s", g_dunefile); *\/ */

/*     return _dune_read_port(s7, g_dune_inport); */
/* } */

void _log_read_error(s7_scheme *s7)
{
    const char *s;
    s7_pointer ow_let = s7_call(s7, s7_name_to_value(s7, "owlet"), s7_nil(s7));
    s7_pointer edatum = s7_call(s7, ow_let,
                               s7_cons(s7,
                                       s7_make_symbol(s7, "error-type"),
                                       s7_nil(s7)));
    s = s7_symbol_name(edatum);
    log_warn("error-type: %s", s);

    edatum = s7_call(s7, ow_let,
                     s7_cons(s7,
                             s7_make_symbol(s7, "error-data"),
                             s7_nil(s7)));
    s = s7_object_to_c_string(s7, edatum);
    log_warn("error-data: %s", s);
    free((void*)s);

    edatum = s7_call(s7, ow_let,
                     s7_cons(s7,
                             s7_make_symbol(s7, "error-code"),
                             s7_nil(s7)));
    s = s7_object_to_c_string(s7, edatum);
    log_warn("error-code: %s", s);
    free((void*)s);

    edatum = s7_call(s7, ow_let,
                     s7_cons(s7,
                             s7_make_symbol(s7, "error-file"),
                             s7_nil(s7)));
    s = s7_object_to_c_string(s7, edatum);
    log_warn("error-file: %s", s);
    free((void*)s);
}

// s7_pointer _dune_read_thunk_catcher_s7; /* initialized by init fn */

/* call by s7_call_with_catch as error_handler arg
   arg0: err symbol, e.g.'read-error
   arg1: msg, e.g. ("unexpected close paren: ...
   WARNING: printing s7_stacktrace clobbers globals, esp. g_dunefile!!!
 */
static s7_pointer _dune_read_thunk_catcher(s7_scheme *s7, s7_pointer args)
{
    (void)s7;
    (void)args;
    TRACE_ENTRY(_dune_read_thunk_catcher);

    TRACE_S7_DUMP("stanzas readed so far", g_stanzas);

/* #if defined(DEVBUILD) */
/*     s7_pointer owlet7 = s7_eval_c_string(s7,  "(owlet)"); */
/*     const char *owlet = s7_object_to_c_string(s7, owlet7); */
/*     log_debug("owlet: %s", owlet); */
/*     free((void*)owlet); */
/* #endif */

    s7_pointer errfile7 = s7_eval_c_string(s7,  "((owlet) 'error-file)");
    const char *errfile = s7_object_to_c_string(s7, errfile7);
    /* log_debug("errfile: %s", errfile); */
    /* free((void*)errfile); */

    if (verbose) {
        log_warn("Error reading dunefile: %s", errfile);
        e7 = s7_eval_c_string(s7,  "((owlet) 'error-data)");
        e = s7_object_to_c_string(s7, e7);
        log_warn("error-data: %s", e);
        free((void*)e);
        e7 = s7_eval_c_string(s7,  "((owlet) 'error-position)");
        e = s7_object_to_c_string(s7, e7);
        log_warn("error-position: %s", e);
        free((void*)e);
        e7 = s7_eval_c_string(s7,  "((owlet) 'error-line)");
        e = s7_object_to_c_string(s7, e7);
        log_warn("error-line: %s", e);
        free((void*)e);
    }

/* #if defined(DEVBUILD) */
/*     print_c_backtrace(); */
/* #endif */

    if (verbose) {
        log_info("fixing dunefile: %s", errfile);
    }

    /* s7_pointer err_sym = s7_car(args); */
    s7_pointer err_msg = s7_cadr(args);

/* #if defined(DEVBUILD) */
/*     TRACE_S7_DUMP("s7_read_thunk_catcher err sym", err_sym); */
/*     TRACE_S7_DUMP("s7_read_thunk_catcher err msg", err_msg); */
/* #endif */

    /* _log_read_error(s7); */

    /* s7_pointer ow_let = s7_call(s7, s7_name_to_value(s7, "owlet"), s7_nil(s7)); */
    /* s7_pointer efile = s7_call(s7, ow_let, */
    /*                      s7_cons(s7, */
    /*                              s7_make_symbol(s7, "error-file"), */
    /*                              s7_nil(s7))); */
    /* const char *s = s7_object_to_c_string(s7, efile); */
    /* log_warn("error-file: %s", s); */
    /* free((void*)s); */

    /* return s7_f(s7); */
    /* s7_show_stack(s7); */

    const char *s = s7_object_to_c_string(s7, err_msg);
    /* log_warn("error-data: %s", s); */
    if (strstr(s, "(\"unexpected close paren:") != NULL) {
        free((void*)s);
        return s7_make_symbol(s7, "dune-baddot-error");
    }
    else if (strstr(s,
                    "(\"end of input encountered while in a string") != NULL) {
        free((void*)s);
        return s7_make_symbol(s7, "dune-eol-string-error");
    }

    return s7_make_symbol(s7, "dune-read-error");
}

/* (dune:read) - read current-input-port
 * (dune:read str) - read string str
 * (dune:read p) - read port p
 */
static s7_pointer _g_dune_read(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(_g_dune_read);
    s7_pointer p, arg;
    TRACE_S7_DUMP("args", args);

#if defined(DEVBUILD)
    print_c_backtrace();
#endif

    /* s7_gc_on(s7, false); */

    /* const char* dune_str = NULL; */
    /* log_debug("dune_str ptr: %p", (void*)dune_str); */

    /* s7_pointer _dune_read_thunk_catcher_s7 = s7_make_function(s7, "-dune-read-thunk-catcher", */
    /*                                              _dune_read_thunk_catcher, */
    /*                                              2, // catcher must take 2 */
    /*                                              0, false, "handle read error"); */
    /* gc_dune_read_thunk_catcher_s7 = s7_gc_protect(s7, _dune_read_thunk_catcher_s7); */

    p = args;
    if (p == s7_nil(s7)) {
        TRACE_LOG_DEBUG("SOURCE: current-input-port", "");
        /* result = _dune_read_port(s7, s7_current_input_port(s7)); */
        g_dune_inport = s7_current_input_port(s7);
        gc_dune_inport = s7_gc_protect(s7, g_dune_inport);
        const char *dunefile = s7_port_filename(s7, g_dune_inport);
#if defined(DEVBUILD)
        log_debug("reading dunefile: %s", dunefile);
#endif
        result = s7_call_with_catch(s7,
                                    s7_t(s7),      /* tag */
                                    // _dune_read_thunk_s7
                                    s7_name_to_value(s7, "-dune-read-thunk"),
                                    _dune_read_thunk_catcher_s7
                                    /* s7_name_to_value(s7, "-dune-read-thunk-catcher") */
                                    );

        if (result == s7_make_symbol(s7, "dune-baddot-error")) {
            /* const char *dunefile = s7_port_filename(s7, g_dune_inport); */
            /* log_debug("fixing baddot error for %s", dunefile); */
            /* TRACE_S7_DUMP("stanzas readed so far", g_stanzas); */
            s7_gc_unprotect_at(s7, gc_stanzas);
            s7_gc_unprotect_at(s7, gc_dune_inport);
            s7_gc_unprotect_at(s7, gc_dune_read_thunk_catcher_s7);

            s7_pointer fixed = fix_dunefile(s7, dunefile);

            return fixed;
        }
        else if (result == s7_make_symbol(s7, "dune-eol-string-error")) {
            /* const char *dunefile = s7_port_filename(s7, g_dune_inport); */
            /* log_debug("fixing eol-string error for %s", dunefile); */
            /* TRACE_S7_DUMP("stanzas readed so far", g_stanzas); */
            s7_gc_unprotect_at(s7, gc_stanzas);
            s7_gc_unprotect_at(s7, gc_dune_read_thunk_catcher_s7);
            s7_gc_unprotect_at(s7, gc_dune_inport);
            s7_close_input_port(s7, g_dune_inport);

            s7_pointer fixed = fix_dunefile(s7, dunefile);
            return fixed;
        }

        s7_gc_unprotect_at(s7, gc_dune_inport);
        s7_gc_unprotect_at(s7, gc_dune_read_thunk_catcher_s7);
        TRACE_S7_DUMP("_g_dune_read call/catch result", result);
        /* s7_flush_output_port(s7, s7_current_output_port(s7)); */
        return s7_reverse(s7, result);
    } else {
        arg = s7_car(p);
        if (s7_is_input_port(s7, arg)) {
            TRACE_LOG_DEBUG("SOURCE: input port", "");
            /* return _dune_read_port(s7, arg); */
            g_dune_inport = arg;
            result = s7_call_with_catch(s7,
                                        s7_t(s7),      /* tag */
                                        _dune_read_thunk_s7,
                                        _dune_read_thunk_catcher_s7);
            TRACE_LOG_DEBUG("read_thunk result", result);
            return result;
        }
        else if (s7_is_string(arg)) {
            TRACE_LOG_DEBUG("SOURCE: string", "");
            /* dune_str = (char*)s7_string(arg); */
        }
        else {
            return(s7_wrong_type_error(s7, s7_make_string_wrapper_with_length(s7, "dune:read", 10), 1, arg, string_string));
        }
    }
    return s7_f(s7);
}

static s7_pointer g_dune_read(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_dune_read);
    /* s7_pointer p, arg; */
    TRACE_S7_DUMP("args", args);

    /* #if defined(DEVBUILD) */
    /*     print_c_backtrace(); */
    /* #endif */

    /* s7_pointer _g_dune_read_s7 = */
    s7_define_function(s7, "-dune-read-s7",
                       _g_dune_read,
                       0, 1, false,
                       "internal dune:read");
    /* s7_int gc_dune_read_s7 = s7_gc_protect(s7, _g_dune_read_s7); */
    /* s7_pointer result = _g_dune_read(s7, args); */
    /* s7_pointer result = s7_call(s7, _g_dune_read_s7, args); */
    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 1,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "xargs"),
                                              args)));
    // WARNING: if the call to -dune-read-s7 raises an error that gets
    // handled by the catcher, then the continuation is whatever
    // called this routine (i.e. the c stack), NOT the assignment to
    // result below.
    s7_pointer result = s7_eval_c_string_with_environment(s7, "(apply -dune-read-s7 xargs)", env);
    /* s7_gc_unprotect_at(s7, gc_dune_read_s7); */
    TRACE_S7_DUMP("read result:", result);
    s7_gc_unprotect_at(s7, gc_stanzas);
    return result;
}

s7_pointer pl_tx, pl_xx, pl_xxs,pl_sx, pl_sxi, pl_ix, pl_iis, pl_isix, pl_bxs;

s7_pointer libdune_s7_init(s7_scheme *s7);
s7_pointer libdune_s7_init(s7_scheme *s7)
{
    TRACE_ENTRY(libdune_s7_init);
  s7_pointer cur_env;
  /* s7_pointer pl_tx, pl_xxs,pl_sx, pl_sxi, pl_ix, pl_iis, pl_isix, pl_bxs; */
  //  pl_xxsi, pl_ixs
  {
      s7_pointer t, x, b, s, i;

      t = s7_t(s7);
      x = s7_make_symbol(s7, "c-pointer?");
      b = s7_make_symbol(s7, "boolean?");
      s = s7_make_symbol(s7, "string?");
      i = s7_make_symbol(s7, "integer?");

      pl_tx = s7_make_signature(s7, 2, t, x);
      pl_xx = s7_make_signature(s7, 2, x, x);
      pl_xxs = s7_make_signature(s7, 3, x, x, s);
      /* pl_xxsi = s7_make_signature(s7, 4, x, x, s, i); */
      pl_sx = s7_make_signature(s7, 2, s, x);
      pl_sxi = s7_make_signature(s7, 3, s, x, i);
      pl_ix = s7_make_signature(s7, 2, i, x);
      pl_iis = s7_make_signature(s7, 3, i, i, s);
      pl_bxs = s7_make_signature(s7, 3, b, x, s);
      /* pl_ixs = s7_make_signature(s7, 3, i, x, s); */
      pl_isix = s7_make_signature(s7, 4, i, s, i, x);
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

  /* dune_table_init(s7, cur_env); */
  /* dune_array_init(s7, cur_env); */
  /* dune_datetime_init(s7, cur_env); */

  int64_t__symbol = s7_make_symbol(s7, "int64_t*");
  /* dune_datum_t__symbol = s7_make_symbol(s7, "dune_datum_t*"); */
  /* dune_array_t__symbol = s7_make_symbol(s7, "dune_array_t*"); */
  /* dune_table_t__symbol = s7_make_symbol(s7, "dune_table_t*"); */
  FILE__symbol = s7_make_symbol(s7, "FILE*");

  /* s7_define_constant(s7, "dune:version", s7_make_string(s7, "1.0-beta")); */

  /* s7_define(s7, cur_env, */
  /*           s7_make_symbol(s7, "dune:free"), */
  /*           s7_make_typed_function(s7, "dune:free", */
  /*                                  g_dune_free, */
  /*                                  1, 0, false, */
  /*                                  "(dune:free t) free table t", pl_tx)); */

  /* public api */
  s7_define(s7, cur_env,
            s7_make_symbol(s7, "dune:read"),
            s7_make_typed_function(s7, "dune:read",
                                   g_dune_read,
                                   0, // 0 args: read from current inport
                                   // (for with-input-from-string or -file)
                                   1, // optional: string or port
                                   false,
                                   "(dune:read) read dunefile from current-input-port; (dune:read src) read dunefile from string or port",
                                   NULL)); //sig


  /* private */
  /* _dune_read_thunk_s7 = s7_make_function(s7, "-dune-read-thunk", */
  /*                                        _dune_read_thunk, */
  /*                                        0, 0, false, ""); */
  /* _dune_read_thunk_s7 = */
  s7_define_function(s7, "-dune-read-thunk",
                     _dune_read_thunk,
                     0, 0, false, "");

  _dune_read_thunk_catcher_s7 =
      s7_make_function(s7, "-dune-read-thunk-catcher",
                         _dune_read_thunk_catcher,
                         2, // catcher must take 2
                         0, false, "handle read error")
;
    /* gc_dune_read_thunk_catcher_s7 = s7_gc_protect(s7, _dune_read_thunk_catcher_s7); */

  /* _dune_read_thunk_catcher_s7 = s7_define_function(s7, "-dune-read-thunk-catcher", */
  /*                                                _dune_read_thunk_catcher, */
  /*                                                2, // catcher must take 2 */
  /*                                                0, false, ""); */

  s7_set_shadow_rootlet(s7, old_shadow);

  return(cur_env);
}
