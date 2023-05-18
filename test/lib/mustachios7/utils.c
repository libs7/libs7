/* #include "common.h" */
/* #include "s7_common.h" */
#include "libmustachios7_s7.h"
#include "libs7.h"
#include "trace.h"

s7_pointer make_render_env(s7_scheme *s7, s7_pointer template, s7_pointer data)
{
    return s7_inlet(s7,
                    s7_list(s7, 2,
                            s7_cons(s7,
                                    s7_make_symbol(s7, "t"),
                                    template),
                            s7_cons(s7,
                                    s7_make_symbol(s7, "d"),
                                    data)));
}

s7_pointer data;
s7_int gc_loc;

s7_pointer read_data_string(s7_scheme *s7, char *data_string)
{
    s7_pointer port  = s7_open_input_string(s7, data_string);
    if (!s7_is_input_port(s7, port)) {
        log_error("s7_open_input_string failed for %s", "#{:subject \"world\"}");
    }
    gc_loc = s7_gc_protect(s7, port);
    data   = s7_read(s7, port);
    TRACE_S7_DUMP("data", data);
    /* if (s7_is_hash_table(data)) { */
        data = s7_eval(s7, data, s7_rootlet(s7));
    /* } */
    s7_close_input_port(s7, port);
    s7_gc_unprotect_at(s7, gc_loc);
    return data;
}

static s7_pointer read_json_handler(s7_scheme *s7, s7_pointer args)
{
    TRACE_ENTRY(read_json_handler);
    return s7_nil(s7);
}

s7_pointer read_json(s7_scheme *s7, char *json_str)
{
    /* FIXME: put json:read in a global var */

    /* (catch 'x */
    /*  (lambda () (json:read ...)) */
    /*  (lambda () handler)) */

    s7_pointer rval =  s7_call(s7, s7_name_to_value(s7, "json:read"),
                               s7_list(s7, 1, s7_make_string(s7, json_str)));

    return rval;
    /* s7_call_with_catch(s, s7_t(s), */
    /*                    s7_name_to_value(s, "json:read"), */
    /*                    s7_name_to_value(s, "wd-inner-test-handler")); */

}

s7_pointer apply_render(s7_scheme *s7, s7_pointer template, s7_pointer data)
{
    return s7_call(s7, s7_name_to_value(s7, "mustache:render"),
                   s7_list(s7, 2, template, data));
}

s7_pointer apply_render_port(s7_scheme *s7,
                             s7_pointer template, s7_pointer data,
                             s7_pointer port)
{
    return s7_call(s7, s7_name_to_value(s7, "mustache:render"),
                   s7_list(s7, 4, template, data,
                           s7_make_keyword(s7, "port"), port));
}

