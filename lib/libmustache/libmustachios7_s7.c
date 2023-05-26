/* see https://github.com/joeltg/json.scm */
/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "log.h"
#include "cJSON.h"
#include "s7.h"
#include "libmustachios7_s7.h"
#include "mustachios7_s7.h"
#include "mustachios7_cjson.h"
#include "trace.h"
/* #include "c_stacktrace.h" */

/* #define CRITICAL(str) \ */
/* 	{print_stacktrace(0); \ */
/* 	fprintf(stderr, "%s (in %s at %s:%i)\n", str, __func__, __FILE__, __LINE__); \ */
/* 	exit(EXIT_FAILURE); \ */
/* 	} */

#ifdef DEBUGGING
#include "ansi_colors.h"
#include "debug.h"
#endif

/* s7_scheme *s7; */

/* ****************************************************************
   API

   (mustach:render template data port)
   (mustach:render-json template data port)
   (mustach:make-template str)
   (mustach:make-data hashtbl)

   -- generic json api: string->json, json->string

 */

/* returns a c-pointer object */
s7_pointer mustachios7_read_json(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(mustachios7_read_json);
    s7_pointer json_str = s7_car(args);
    if (!s7_is_string(json_str)) {
        s7_pointer e = s7_wrong_type_arg_error(s7,
                                               "mustachios7_read_json",
                                               1,
                                               json_str,
                                               "a JSON string");
        return e;
    }
#ifdef DEBUGGING
    int len = s7_string_length(json_str);
    log_debug("json (scm) strlen: %d", len);
#endif
    const char *json_c_str = s7_string(json_str);
    int len2 = strlen(json_c_str);
#ifdef DEBUGGING
    log_debug("json (c) strlen: %d", len2);
#endif
    cJSON *jobj = cJSON_ParseWithLength(json_c_str, len2);
    if (jobj == NULL) {
        const char *error_ptr = cJSON_GetErrorPtr();
        log_error("cJSON_ParseWithLength failure at '%s'", error_ptr);
        fflush(NULL);
        return s7_error(s7,
                 s7_make_symbol(s7, "cJSON_ParseWithLength error"),
                 s7_cons(s7,
                         s7_make_string(s7, error_ptr),
                         s7_nil(s7)));
    } else {
        return s7_make_c_pointer_with_type(s7, jobj,
                                           s7_make_symbol(s7, "cJSON*"),
                                           s7_f(s7));
    }
}

s7_pointer g_mustachios7_render(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(mustachios7_render);
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

#ifdef DEBUGGING
    if (s7_is_c_pointer(data)) {
        log_debug("Data is c ptr");
        s7_pointer ptyp = s7_c_pointer_type(data);
        TRACE_S7_DUMP("json ptyp", ptyp);
        // call json processor
    }
#endif
    TRACE_S7_DUMP("d", data);

    //**** PORT
    /* #f - return string only (default)
       #t - returns string and also sends to current-output-port
       () - send to current-output-port but do not return string
       else must be file or string port
     */
    s7_pointer port     = s7_caddr(args);
    bool return_string = false;
#ifdef DEBUGGING
    DUMP("p", port);
#endif

    if (port == s7_f(s7)) {
        // return string only, no port
        port = s7_unspecified(s7); /* write to buffer */
        /* return_string = true; */
    }
    else if (port == s7_t(s7)) {
#ifdef DEBUGGING
        log_debug("PORT TRUE");
#endif
        // send to current-output-port and return string
        // do not write directly to cop: write to buffer then to port
        port = s7_undefined(s7); // means "to buffer then to current op
        return_string = true;
    }
    else if (port == s7_nil(s7)) {
#ifdef DEBUGGING
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
#ifdef DEBUGGING
    DUMP("p", port);
#endif

    //**** FLAGS are opt-out
    // we default to all extensions except json ptr enabled
    int flags = Mustach_With_AllExtensions;
    flags &= ~Mustach_With_JsonPointer;
    //FIXME: enable flag optouts
    s7_pointer flags_optout     = s7_cadddr(args);
#ifdef DEBUGGING
    DUMP("f", flags_optout);
#endif
    (void)flags_optout;

    //FIXME: user-passed flags should REPLACE the default


    /* **** render **** */
    size_t size;            /* render outparam */
    char *ret;              /* render outparam */
    struct tstack_s e;
    e.root = (s7_pointer)data;
#ifdef DEBUGGING
    DUMP("root", e.root);
#endif
    if (port == s7_unspecified(s7)) {
        // :port #f - return string only
        // render to buffer, then return str:

        //FIXME: call mustach_scm_mem, which will supply mustach_wrap_itf_scm
        int rc;
        if (s7_is_c_pointer(data)) {
            rc = mustach_cJSON_mem(s7_string(template),
                                   0, // tlength,
                                   /* &mustach_wrap_itf_json, */
                                   s7_c_pointer(data),
                                   //&e.root,
                                   flags,
                                   &ret,
                                   &size);
        } else {
            rc = mustach_wrap_mem(s7_string(template),
                                      0, // tlength,
                                      &mustach_wrap_itf_scm, &e,
                                      flags,
                                      &ret,
                                      &size);
        }
        if (rc < 0) {
            log_error("mustach_wrap_mem rc: %d", rc);
            return s7_make_integer(s7,rc);
        } else {
#ifdef DEBUGGING
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
                                  &mustach_wrap_itf_scm,
                                  &e, /* closure, struct tstack_s* */
                                  flags,
                                  &ret,
                                  &size);
        if (rc < 0) {
            log_error("mustach_wrap_mem failure: %s", strerror(errno));
            return s7_make_integer(s7,rc);
        } else {
#ifdef DEBUGGING
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
#ifdef DEBUGGING
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

#ifdef DEBUGGING
    DUMP("port file", pfile);
#endif
    if (s7_c_pointer_type(pfile) == s7_f(s7)) {
#ifdef DEBUGGING
        log_debug("GOT STRING PORT");
#endif

        int rc = mustach_wrap_mem(s7_string(template),
                                  0, // tlength,
                                  &mustach_wrap_itf_scm, &e,
                                  flags,
                                  &ret,
                                  &size);
        if (rc < 0) {
            log_error("mustach_wrap_mem failure: %s", strerror(errno));
            return s7_make_integer(s7,rc);
        } else {
#ifdef DEBUGGING
            log_debug("mustach:render wrote %s", ret);
#endif
            s7_display(s7, s7_make_string(s7, ret), port);
            /* if (return_string) // only true if port = current-output-port */
            /*     return s7_make_string(s7, ret); */
            /* else */
            return s7_make_integer(s7, size);
        }

    } else {
#ifdef DEBUGGING
        log_debug("GOT FILE PORT");
#endif
        /* (void)data_scheme;          /\* currently unused *\/ */
        struct tstack_s e;
        e.root = (s7_pointer)data;
        s7_flush_output_port(s7, s7_current_output_port(s7));
        int rc = mustach_wrap_file(s7_string(template), 0, // tlength,
                                 &mustach_wrap_itf_scm, &e,
                                 flags,
                                 s7_c_pointer(pfile));
        (void)rc;
#ifdef DEBUGGING
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
s7_pointer libmustachios7_s7_init(s7_scheme *s7)
{
    TRACE_ENTRY(libmustachios7_s7_init);
    TRACE_LOG_DEBUG("libmustachios7_s7_init", "");

    /* s7 = _s7; */
    s7_pointer curr_env;
    curr_env = s7_inlet(s7, s7_nil(s7));
    s7_pointer old_shadow = s7_set_shadow_rootlet(s7, curr_env);

    /* s7_define(s7, curr_env, */
    /*           s7_make_symbol(s7, ...), */
    /*           s7_make_function_star(s7, ...)); */

    s7_define_function_star(s7,
                            "mustache:render", g_mustachios7_render,
                            "(template #f) (data #f) (port #f) (flags 0)",
                            "(mustach:render template data (port p)) port defaults to current output port");

    s7_define_function_star(s7,
                            "json:read", mustachios7_read_json,
                            "(template #f) (data #f) (port #f) (flags 0)",
                            "(json:read str) encode a string as JSON");

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
