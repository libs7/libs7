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

/* #ifdef DEVBUILD */
/* #include "ansi_colors.h" */
/* #include "debug.h" */
/* #endif */

/* s7_scheme *s7; */

/* ****************************************************************
 * API
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

struct sink_flags_s {
    union {
        struct {
            uint8_t to_string : 1;
            uint8_t to_current_output_port : 1;
            uint8_t to_string_port : 1;
            uint8_t to_file_port : 1;
        };
        uint8_t data;
    };
};

FILE *ostream;
const char *port_filename;

void _handle_sink_flags(s7_scheme *s7,
                        struct sink_flags_s *sink_flags,
                        s7_pointer sink)
{
    log_debug("_handle_sink_flags");
    if (sink == s7_f(s7)) { // return string only, no port
        sink_flags->to_string = 1;
        //port = s7_unspecified(s7); /* write to buffer */
        /* return_string = true; */
    }
    else if (sink == s7_t(s7)) {
        // send to current-output-port and return string
        // do not write directly to cop: write to buffer then to port
        // port = s7_undefined(s7); // means "to buffer then to current op
        /* return_string = true; */
        sink_flags->to_string = 1;
        sink_flags->to_current_output_port = 1;
    }
    else if (sink == s7_nil(s7)) {
        // send to current-output-port and return nothing
        // port = s7_undefined(s7); // means "to buffer then to current op
        sink_flags->to_current_output_port = 1;
    } else if (s7_is_output_port(s7, sink)) {
        // file or string port

        // there is no s7_is_string_port fn, so we go by port-filename
        s7_pointer pfn = s7_apply_function(s7,
                                           s7_name_to_value(s7, "port-filename"),
                                          s7_list(s7, 1, sink));
        TRACE_S7_DUMP("port-filename", pfn);
        if (s7_string_length(pfn) == 0) {
            sink_flags->to_string_port = 1;
        } else {
            sink_flags->to_file_port = 1;
            s7_pointer fp = s7_apply_function(s7,
                              s7_name_to_value(s7, "port-file"),
                              s7_list(s7, 1, sink));
            ostream = s7_c_pointer(fp);
        }
    } else {
        s7_error(s7, s7_make_symbol(s7, "wrong-type-arg"),
                 s7_list(s7, 3, s7_make_string(s7, "~S is a ~S, but should be #t, #f, '(), or a port."),
                         sink, s7_type_of(s7, sink)));
    }
}

/*
 * (mustache:render sink template data flags)
 */
s7_pointer g_mustachios_render(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(g_mustachios_render);
    TRACE_S7_DUMP("args", args);

    /* args: sink, template, data, flags */

    //**** arg 0: SINK ****************
    /* #f - return string only (default)
       #t - returns string and also sends to current-output-port
       () - send to current-output-port but do not return string
       else must be file or string port
     */
    s7_pointer sink     = s7_car(args);
    if ( !(s7_is_boolean(sink)
           || s7_is_output_port(s7, sink)
           || s7_is_null(s7, sink)) ) {
        return(s7_wrong_type_arg_error(s7,
                                       "mustache:render",
                                       1, sink, "a boolean, output port, or '()"));
    }

    struct sink_flags_s sink_flags;
    sink_flags.data = 0;
    _handle_sink_flags(s7, &sink_flags, sink);

    if (sink_flags.data == 0) {
        log_error("Bad sink");  /* FIXME */
    }

    //**** arg 1: TEMPLATE ****************
    s7_pointer template = s7_cadr(args);
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

    //**** arg 2: DATA ****************
    s7_pointer data     = s7_caddr(args);


    //**** arg 3: FLAGS ****************
    // flags are opt-out?
    // we default to all extensions except json ptr enabled
    int flags = Mustach_With_AllExtensions;
    (void)flags;
    flags &= ~Mustach_With_JsonPointer;
    //FIXME: enable flag optouts
    s7_pointer flags_optout     = s7_cadddr(args);
#ifdef DEVBUILD
    /* DUMP("f", flags_optout); */
#endif
    (void)flags_optout;

    //FIXME: user-passed flags should REPLACE the default

    /* ******************** render ******************** */
    s7_pointer b;
    b = s7_apply_function(s7, s7_name_to_value(s7, "json:datum?"),
                                   s7_list(s7, 1, data));
    if (b == s7_t(s7)) {
        // call mustache_json_render
        cJSON *root = (cJSON*)s7_c_object_value(data);
        if (sink_flags.to_file_port) {
            log_debug("SINK: file port");
            mustache_json_frender(ostream, template_str, 0, root, flags);
        }
        else if (sink_flags.to_string) {
            const char * s = mustache_json_render(template_str, 0, root, flags);
            s7_pointer str7 = s7_make_string(s7, s);
            if (sink_flags.to_current_output_port) {
                log_debug("SINK: #t");
                /* s7_pointer cip = s7_current_output_port(s7); */
                s7_display(s7, str7, s7_current_output_port(s7));
                /* s7_format(s7, s7_list(s7, 3, */
                /*                       s7_nil(s7), // write to cip */
                /*                       s7_make_string(s7, "~S~%"), */
                /*                       str7)); */
            } else {
                log_debug("SINK: #f");
            }
            return str7;
        }
        else if (sink_flags.to_current_output_port) {
            log_debug("SINK: '()");
            const char * s = mustache_json_render(template_str, 0, root, flags);
            s7_display(s7, s7_make_string(s7, s),
                       s7_current_output_port(s7));
        }
        else if (sink_flags.to_string_port) {
            /* log_debug("SINK: string port"); */
            const char * s = mustache_json_render(template_str, 0, root, flags);
            s7_display(s7, s7_make_string(s7, s), sink);
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

s7_pointer libmustachios_s7_init(s7_scheme *s7)
{
    TRACE_ENTRY(libmustachios_s7_init);
    TRACE_LOG_DEBUG("libmustachios_s7_init", "");

    /* s7 = _s7; */
    s7_pointer curr_env;
    curr_env = s7_inlet(s7, s7_nil(s7));
    s7_pointer old_shadow = s7_set_shadow_rootlet(s7, curr_env);

    /* s7_define(s7, curr_env, */
    /*           s7_make_symbol(s7, ...), */
    /*           s7_make_function_star(s7, ...)); */

    s7_define_function_star(s7,
                            "mustache:render", g_mustachios_render,
                            "(sink (error 'unset-arg \"sink parameter not set\"))"
                            "(template (error 'unset-arg \"template parameter not set\"))"
                            "(data  (error 'unset-arg \"data parameter not set\"))"
                            "(flags 0)",
                            "(mustach:render port template data flags) port defaults to current output port");

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

