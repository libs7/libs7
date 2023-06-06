/* #define _GNU_SOURCE */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "config.h"
/* #include "cJSON.h" */
#include "s7.h"
#include "libmustachios_s7.h"
#include "mustache_s7.h"
#include "mustache_cjson.h"
#include "mustache_tomlc99.h"

/* #define CRITICAL(str) \ */
/* 	{print_stacktrace(0); \ */
/* 	fprintf(stderr, "%s (in %s at %s:%i)\n", str, __func__, __FILE__, __LINE__); \ */
/* 	exit(EXIT_FAILURE); \ */
/* 	} */

#ifdef DEVBUILD
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
/* #ifdef DEVBUILD */
/*     int len = s7_string_length(json_str); */
/*     log_debug("json (scm) strlen: %d", len); */
/* #endif */
/*     const char *json_c_str = s7_string(json_str); */
/*     int len2 = strlen(json_c_str); */
/* #ifdef DEVBUILD */
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

/* **************************************************************** */
/*
 *  (mustache:render <sink> template data flags)
 *  sink 7 rendering options:
 *    #f  - return string (default)
 *    #t  - send to current output port and return string
 *    '() - send to current output port, do not return string
 *    string port
 *    file port
 *    *stdout*
 *    *stderr*
 */

struct sink_s {
    union {
        struct {
            uint8_t to_string : 1;
            uint8_t to_current_output_port : 1;
            uint8_t to_string_port : 1;
            uint8_t to_file_port : 1;
        };
        uint8_t data;
    };
} sink;

FILE *ostream;
const char *port_filename;

void _handle_sink(s7_scheme *s7, s7_pointer port)
{
    if (port == s7_f(s7)) { // return string only, no port
        sink.to_string = 1;
        //port = s7_unspecified(s7); /* write to buffer */
        /* return_string = true; */
    }
    else if (port == s7_t(s7)) {
        // send to current-output-port and return string
        // do not write directly to cop: write to buffer then to port
        // port = s7_undefined(s7); // means "to buffer then to current op
        /* return_string = true; */
        sink.to_string = 1;
        sink.to_current_output_port = 1;
    }
    else if (port == s7_nil(s7)) {
        // send to current-output-port and return nothing
        // port = s7_undefined(s7); // means "to buffer then to current op
        sink.to_current_output_port = 1;
    } else if (s7_is_output_port(s7, port)) {
        // file or string port
        /* port_filename = s7_port_filename(s7, port); */
        // no s7_port_file(); (port-file p) returns c-pointer for FILE*
        //FIXME: what does it return for string ports?
        s7_pointer fp = s7_apply_function(s7,
                                          s7_name_to_value(s7, "port-file"),
                                          s7_list(s7, 1, port));
        if (s7_is_c_pointer(fp)) {
            sink.to_file_port = 1;
            ostream = s7_c_pointer(fp);
        } else {
            sink.to_string_port = 1;
        }
    } else {
        s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"),
                 s7_list(s7, 3, s7_make_string(s7, "~S is a ~S, but should be #t, #f, '(), or a port."),
                         port, s7_type_of(s7, port)));
    }
}

/*     else if (port == s7_undefined(s7)) { */
/*         // :port #t - send to current-outp, return str */
/*         // :port () - send to current-outp, do NOT return str */
/*         int rc = mustach_wrap_mem(s7_string(template), */
/*                                   0, // tlength, */
/*                                   &mustach_wrap_itf_scm, */
/*                                   &e, /\* closure, struct tstack_s* *\/ */
/*                                   flags, */
/*                                   &ret, */
/*                                   &size); */
/*         if (rc < 0) { */
/*             log_error("mustach_wrap_mem failure: %s", strerror(errno)); */
/*             return s7_make_integer(s7,rc); */
/*         } else { */
/* #ifdef DEVBUILD */
/*             log_debug("mustach:render wrote %s", ret); */
/* #endif */
/*             s7_pointer s = s7_make_string(s7, ret); */
/*             s7_display(s7, s, s7_current_output_port(s7)); */
/*             if (return_string) { */
/*                 return s; */
/*             } else { */
/*                 return s7_unspecified(s7); */
/*             } */
/*         } */
/*     } */

/*     // (port? port) already verified */
/*     const char *port_filename = s7_port_filename(s7, port); */
/*     (void)port_filename; */
/* #ifdef DEVBUILD */
/*     log_debug("port_filename: %s", port_filename); */
/* #endif */
/*     s7_pointer env = s7_inlet(s7, */
/*                               s7_list(s7, 1, */
/*                                       s7_cons(s7, */
/*                                               s7_make_symbol(s7, "p"), */
/*                                               port))); */
/*     s7_pointer pfile = s7_eval_c_string_with_environment( */
/*                                                          s7, "(port-file p)", */
/*                                                          env); */

/* #ifdef DEVBUILD */
/*     DUMP("port file", pfile); */
/* #endif */
/*     if (s7_c_pointer_type(pfile) == s7_f(s7)) { */
/* #ifdef DEVBUILD */
/*         log_debug("GOT STRING PORT"); */
/* #endif */

/*         int rc = mustach_wrap_mem(s7_string(template), */
/*                                   0, // tlength, */
/*                                   &mustach_wrap_itf_scm, &e, */
/*                                   flags, */
/*                                   &ret, */
/*                                   &size); */
/*         if (rc < 0) { */
/*             log_error("mustach_wrap_mem failure: %s", strerror(errno)); */
/*             return s7_make_integer(s7,rc); */
/*         } else { */
/* #ifdef DEVBUILD */
/*             log_debug("mustach:render wrote %s", ret); */
/* #endif */
/*             s7_display(s7, s7_make_string(s7, ret), port); */
/*             /\* if (return_string) // only true if port = current-output-port *\/ */
/*             /\*     return s7_make_string(s7, ret); *\/ */
/*             /\* else *\/ */
/*             return s7_make_integer(s7, size); */
/*         } */

/*     } else { */
/* #ifdef DEVBUILD */
/*         log_debug("GOT FILE PORT"); */
/* #endif */
/*         /\* (void)data_scheme;          /\\* currently unused *\\/ *\/ */
/*         struct tstack_s e; */
    /* memset(&stack, '\0', sizeof(struct tstack_s)); */
/*         e.root = (s7_pointer)data; */
/*         s7_flush_output_port(s7, s7_current_output_port(s7)); */
/*         int rc = mustach_wrap_file(s7_string(template), 0, // tlength, */
/*                                  &mustach_wrap_itf_scm, &e, */
/*                                  flags, */
/*                                  s7_c_pointer(pfile)); */
/*         (void)rc; */
/* #ifdef DEVBUILD */
/*         log_debug("FILE port RC: %d", rc); */
/* #endif */
/*         /\* cop = s7_current_output_port(s7); *\/ */
/*         /\* os = s7_output_string(s7, cop); *\/ */
/*         /\* const char *os = s7_get_output_string(s7, cop); *\/ */
/*         /\* log_debug("current output str: %s", os); *\/ */
/*         /\* log_debug("UUxxxxxxxxxxxxxxxxx"); *\/ */
/*         /\* (void) os; *\/ */
/*     } */

/*     return s7_make_string(s7, ret); */

/* **************************************************************** */


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
    const char *template_str;
    TRACE_S7_DUMP("t", template);

    if (s7_is_string(template)) {
        template_str = s7_string(template);
    } else {
        s7_pointer e = s7_wrong_type_arg_error(s7,
                                               "mustach_render", // caller
                                               1,
                                               template,
                                               "a template string");
        return e;
    }

    //**** DATA
    s7_pointer data     = s7_cadr(args);

    //**** PORT
    /* #f - return string only (default)
       #t - returns string and also sends to current-output-port
       () - send to current-output-port but do not return string
       else must be file or string port
     */
    s7_pointer port     = s7_caddr(args);

    _handle_sink(s7, port);

    if (sink.data == 0) {
        log_error("Bad sink");  /* FIXME */
    }

    //**** FLAGS are opt-out
    // we default to all extensions except json ptr enabled
    int flags = Mustach_With_AllExtensions;
    (void)flags;
    flags &= ~Mustach_With_JsonPointer;
    //FIXME: enable flag optouts
    s7_pointer flags_optout     = s7_cadddr(args);
#ifdef DEVBUILD
    DUMP("f", flags_optout);
#endif
    (void)flags_optout;

    //FIXME: user-passed flags should REPLACE the default

    /* **** render **** */

    s7_pointer b;
    b = s7_apply_function(s7, s7_name_to_value(s7, "json:map?"),
                                   s7_list(s7, 1, data));
    if (b == s7_t(s7)) {
        // call mustache_json_render
        cJSON *root = (cJSON*)s7_c_object_value(data);
        if (sink.to_file_port) {
            mustache_json_frender(ostream, template_str, 0, root, flags);
        }
        else if (sink.to_string) {
        }
        else if (sink.to_current_output_port) {
        }
        else if (sink.to_string_port) {
        }
        else {
        }

    } else {
        b = s7_apply_function(s7, s7_name_to_value(s7, "toml:map?"),
                              s7_list(s7, 1, data));
        if (b == s7_t(s7)) {
            // call mustache_toml_render
        } else {
            b = s7_apply_function(s7, s7_name_to_value(s7, "map?"),
                                  s7_list(s7, 1, data));
            if (b == s7_t(s7)) {
                // call mustache_scm_render
            } else {
                log_error("bad data");
            }
        }
    }


    const char *rendered;
    (void)rendered;

    // if scm data
    // scm polymorphic - on fn for all sinks
    /* rendered = mustache_scm_render(port, */
    /*                                template, 0, */
    /*                                data, */
    /*                                flags); */

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


/* **************************************************************** */
/*     if (port == s7_f(s7)) { */
/*         // return string only, no port */
/*         port = s7_unspecified(s7); /\* write to buffer *\/ */
/*         /\* return_string = true; *\/ */
/*     } */
/*     else if (port == s7_t(s7)) { */
/* #ifdef DEVBUILD */
/*         log_debug("PORT TRUE"); */
/* #endif */
/*         // send to current-output-port and return string */
/*         // do not write directly to cop: write to buffer then to port */
/*         port = s7_undefined(s7); // means "to buffer then to current op */
/*         return_string = true; */
/*     } */
/*     else if (port == s7_nil(s7)) { */
/* #ifdef DEVBUILD */
/*         log_debug("PORT NIL"); */
/* #endif */
/*         // send to current-output-port and return nothing */
/*         port = s7_undefined(s7); // means "to buffer then to current op */
/*     } else if (s7_is_output_port(s7, port)) { */
/*         // file or string port */
/*     } else { */
/*         s7_pointer e = s7_wrong_type_arg_error(s7, */
/*                                                "mustach_render", // caller */
/*                                                3, */
/*                                                port, */
/*                                                "an output port"); */
/*         return e; */
/*     } */
/* #ifdef DEVBUILD */
/*     DUMP("p", port); */
/* #endif */

