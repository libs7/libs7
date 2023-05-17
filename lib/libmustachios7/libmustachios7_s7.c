/* see https://github.com/joeltg/json.scm */
/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "log.h"
#include "libmustachios7_s7.h"
#include "trace.h"
/* #include "c_stacktrace.h" */

/* #define CRITICAL(str) \ */
/* 	{print_stacktrace(0); \ */
/* 	fprintf(stderr, "%s (in %s at %s:%i)\n", str, __func__, __FILE__, __LINE__); \ */
/* 	exit(EXIT_FAILURE); \ */
/* 	} */

#include "ansi_colors.h"
#ifdef DEBUG_TRACE
#include "debug.h"
#endif

s7_scheme *s7;

/* ****************************************************************
   API

   (mustach:render template data port)
   (mustach:render-json template data port)
   (mustach:make-template str)
   (mustach:make-data hashtbl)

 */

s7_pointer libs7_mustach_render(s7_scheme *s7, s7_pointer args)
{
#ifdef DEBUG_TRACE
    log_debug("libs7_mustach_render");
#endif
    TRACE_ENTRY(mustach_render)
    TRACE_S7_DUMP("args", args);

    /* args: template, data, port */

    //**** TEMPLATE
    s7_pointer template = s7_car(args);
    TRACE_S7_DUMP("t", template);

    if (!s7_is_string(template)) {
        s7_pointer e = s7_wrong_type_arg_error(s7,
                                               "mustach_render", // caller
                                               1,
                                               template,
                                               "a template string");
        return e;
    }

    //**** DATA
    s7_pointer data     = s7_cadr(args);
    TRACE_S7_DUMP("d", data);
    // data: map, vector, string, number, #t, #f, or '() (ie any JSON value)
    /* if ((!s7_is_hash_table(data)) */
    /*     && (!libs7_is_alist(s7, data))) { */
    /*     s7_pointer e = s7_wrong_type_arg_error(s7, */
    /*                                            "mustach_render", // caller */
    /*                                            2, */
    /*                                            data, */
    /*                                            "a hash-table or alist"); */
    /*     return e; */
    /* } */

    //**** PORT
    /* #f - return string only (default)
       #t - returns string and also sends to current-output-port
       () - send to current-output-port but do not return string
       else must be file or string port
     */
    s7_pointer port     = s7_caddr(args);
    bool return_string = false;
#ifdef DEBUG_TRACE
    DUMP("p", port);
#endif

    if (port == s7_f(s7)) {
        // return string only, no port
        port = s7_unspecified(s7); /* write to buffer */
        /* return_string = true; */
    }
    else if (port == s7_t(s7)) {
#ifdef DEBUG_TRACE
        log_debug("PORT TRUE");
#endif
        // send to current-output-port and return string
        // do not write directly to cop: write to buffer then to port
        port = s7_undefined(s7); // means "to buffer then to current op
        return_string = true;
    }
    else if (port == s7_nil(s7)) {
#ifdef DEBUG_TRACE
        log_debug("PORT NIL");
#endif
        // send to current-output-port and return nothing
        port = s7_undefined(s7); // means "to buffer then to current op
    } else if (s7_is_output_port(s7, port)) {
        // file or string port
    } else {
        s7_pointer e = s7_wrong_type_arg_error(s7,
                                               "mustach_render", // caller
                                               3,
                                               port,
                                               "an output port");
        return e;
    }
#ifdef DEBUG_TRACE
    DUMP("p", port);
#endif

    //**** FLAGS are opt-out
    // we default to all extensions except json ptr enabled
    int flags = Mustach_With_AllExtensions;
    flags &= ~Mustach_With_JsonPointer;
    //FIXME: enable flag optouts
    s7_pointer flags_optout     = s7_cadddr(args);
#ifdef DEBUG_TRACE
    DUMP("f", flags_optout);
#endif
    (void)flags_optout;

    //FIXME: user-passed flags should REPLACE the default


    /* **** render **** */
    size_t size;            /* render outparam */
    char *ret;              /* render outparam */
    struct expl e;
    e.root = (s7_pointer)data;
#ifdef DEBUG_TRACE
    DUMP("root", e.root);
#endif
    if (port == s7_unspecified(s7)) {
        // :port #f - return string only
        // render to buffer, then return str:
        int rc = mustach_wrap_mem(s7_string(template),
                                  0, // tlength,
                                  &mustach_s7_wrap_itf, &e,
                                  flags,
                                  &ret,
                                  &size);
        if (rc < 0) {
            log_error("mustach_wrap_mem rc: %d", rc);
            return s7_make_integer(s7,rc);
        } else {
#ifdef DEBUG_TRACE
            log_debug("mustach:render wrote %s", ret);
#endif
            return s7_make_string(s7, ret);
        }
    }

    if (port == s7_undefined(s7)) {
        // :port #t - send to current-outp, return str
        // :port () - send to current-outp, do NOT return str
        int rc = mustach_wrap_mem(s7_string(template),
                                  0, // tlength,
                                  &mustach_s7_wrap_itf,
                                  &e, /* closure, struct expl* */
                                  flags,
                                  &ret,
                                  &size);
        if (rc < 0) {
            log_error("mustach_wrap_mem failure: %s", strerror(errno));
            return s7_make_integer(s7,rc);
        } else {
#ifdef DEBUG_TRACE
            log_debug("mustach:render wrote %s", ret);
#endif
            s7_pointer s = s7_make_string(s7, ret);
            s7_display(s7, s, s7_current_output_port(s7));
            if (return_string) {
                return s;
            } else {
                return s7_unspecified(s7);
            }
        }
    }

    // (port? port) already verified
    const char *port_filename = s7_port_filename(s7, port);
    (void)port_filename;
#ifdef DEBUG_TRACE
    log_debug("port_filename: %s", port_filename);
#endif
    s7_pointer env = s7_inlet(s7,
                              s7_list(s7, 1,
                                      s7_cons(s7,
                                              s7_make_symbol(s7, "p"),
                                              port)));
    s7_pointer pfile = s7_eval_c_string_with_environment(
                                                         s7, "(port-file p)",
                                                         env);

#ifdef DEBUG_TRACE
    DUMP("port file", pfile);
#endif
    if (s7_c_pointer_type(pfile) == s7_f(s7)) {
#ifdef DEBUG_TRACE
        log_debug("GOT STRING PORT");
#endif

        int rc = mustach_wrap_mem(s7_string(template),
                                  0, // tlength,
                                  &mustach_s7_wrap_itf, &e,
                                  flags,
                                  &ret,
                                  &size);
        if (rc < 0) {
            log_error("mustach_wrap_mem failure: %s", strerror(errno));
            return s7_make_integer(s7,rc);
        } else {
#ifdef DEBUG_TRACE
            log_debug("mustach:render wrote %s", ret);
#endif
            s7_display(s7, s7_make_string(s7, ret), port);
            /* if (return_string) // only true if port = current-output-port */
            /*     return s7_make_string(s7, ret); */
            /* else */
            return s7_make_integer(s7, size);
        }

    } else {
#ifdef DEBUG_TRACE
        log_debug("GOT FILE PORT");
#endif
        /* (void)data_scheme;          /\* currently unused *\/ */
        struct expl e;
        e.root = (s7_pointer)data;
        s7_flush_output_port(s7, s7_current_output_port(s7));
        int rc = mustach_wrap_file(s7_string(template), 0, // tlength,
                                 &mustach_s7_wrap_itf, &e,
                                 flags,
                                 s7_c_pointer(pfile));
        (void)rc;
#ifdef DEBUG_TRACE
        log_debug("FILE port RC: %d", rc);
#endif
        /* cop = s7_current_output_port(s7); */
        /* os = s7_output_string(s7, cop); */
        /* const char *os = s7_get_output_string(s7, cop); */
        /* log_debug("current output str: %s", os); */
        /* log_debug("UUxxxxxxxxxxxxxxxxx"); */
        /* (void) os; */
    }

    return s7_unspecified(s7);

}

/* void libmustachios7_s7_init(s7_scheme *s7) */
s7_pointer libmustachios7_s7_init(s7_scheme *_s7)
{
    TRACE_ENTRY(libmustachios7_s7_init);
    TRACE_LOG_DEBUG("libmustachios7_s7_init");
    s7 = _s7;
    s7_pointer curr_env;
    curr_env = s7_inlet(s7, s7_nil(s7));
    s7_pointer old_shadow = s7_set_shadow_rootlet(s7, curr_env);

    /* s7_define(s7, curr_env, */
    /*           s7_make_symbol(s7, ...), */
    /*           s7_make_function_star(s7, ...)); */

    s7_define_function_star(s7,
                            "mustache:render", libs7_mustach_render,
                            "(template #f) (data #f) (port #f) (flags 0)",
                            "(mustach:render template data (port p)) port defaults to current output port");

    /* s7_define_function_star(s7, */
    /*                         "mustache:render-json", libs7_mustach_render_json, */
    /*                         "(template #f) (data #f) (port #f) (flags 0)", */
    /*                         "(mustach:render template data (port p)) port defaults to current output port"); */

    // a few routines needed by the mustache-s7 implementation
    // TODO: validate args xs is list, k is integer
    char *drop = ""
        "(define (drop xs k) "
        "  (let iter ((xs xs) (k k)) "
        "    (if (zero? k) xs (iter (cdr xs) (- k 1)))))";
    s7_pointer r = s7_eval_c_string(s7, drop);

    char *find_if = ""
        "(define (find-if f sequence) "
        "  (let ((iter (make-iterator sequence))) "
        "    (do ((x (iter) (iter))) "
	"        ((or (eof-object? x) (f x)) "
        "         (and (not (eof-object? x)) x)))))";
    r = s7_eval_c_string(s7, find_if);
    (void)r;

    s7_set_shadow_rootlet(s7, old_shadow);
    return curr_env;
}
