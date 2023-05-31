/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "config.h"
/* #include "cJSON.h" */
#include "s7.h"
#include "libmustache.h"
#include "mustach_s7.h"
#include "mustach_cjson.h"
#include "mustach_tomlc99.h"

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
/* s7_pointer mustachios7_read_json(s7_scheme *s7, s7_pointer args) */
/* { */
/*     TRACE_ENTRY(mustachios7_read_json); */
/*     s7_pointer json_str = s7_car(args); */
/*     if (!s7_is_string(json_str)) { */
/*         s7_pointer e = s7_wrong_type_arg_error(s7, */
/*                                                "mustachios7_read_json", */
/*                                                1, */
/*                                                json_str, */
/*                                                "a JSON string"); */
/*         return e; */
/*     } */
/* #ifdef DEBUGGING */
/*     int len = s7_string_length(json_str); */
/*     log_debug("json (scm) strlen: %d", len); */
/* #endif */
/*     const char *json_c_str = s7_string(json_str); */
/*     int len2 = strlen(json_c_str); */
/* #ifdef DEBUGGING */
/*     log_debug("json (c) strlen: %d", len2); */
/* #endif */
/*     cJSON *jobj = cJSON_ParseWithLength(json_c_str, len2); */
/*     if (jobj == NULL) { */
/*         const char *error_ptr = cJSON_GetErrorPtr(); */
/*         log_error("cJSON_ParseWithLength failure at '%s'", error_ptr); */
/*         fflush(NULL); */
/*         return s7_error(s7, */
/*                  s7_make_symbol(s7, "cJSON_ParseWithLength error"), */
/*                  s7_cons(s7, */
/*                          s7_make_string(s7, error_ptr), */
/*                          s7_nil(s7))); */
/*     } else { */
/*         return s7_make_c_pointer_with_type(s7, jobj, */
/*                                            s7_make_symbol(s7, "cJSON*"), */
/*                                            s7_f(s7)); */
/*     } */
/* } */

/*
 * (mustache:render sink template data flags)
 */
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
    //FIXME: dispatch on data type: scm, toml, or json

    const char *rendered;


    // if scm data
    // scm polymorphic - on fn for all sinks
    rendered = mustach_scm_render(port,
                                  template, 0,
                                  data,
                                  flags);
    // elif toml data
    // if port #f render_to_string, etc.
    /* rendered = mustach_toml_render(port, */
    /*                                template, 0, */
    /*                                data, */
    /*                                flags); */

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
