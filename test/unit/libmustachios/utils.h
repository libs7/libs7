#ifndef TEST_S7_COMMON_H
#define TEST_S7_COMMON_H

#include "libs7.h"

s7_pointer make_render_env(s7_scheme *s7, s7_pointer template, s7_pointer data);

s7_pointer read_data_string(s7_scheme *s7, char *data);

s7_pointer read_json(s7_scheme *s7, char *json_str);

s7_pointer apply_render(s7_scheme *s7, s7_pointer template, s7_pointer data);

s7_pointer apply_render_port(s7_scheme *s7,
                             s7_pointer template, s7_pointer data,
                             s7_pointer port);

/* s7_scheme *initialize(int argc, char **argv); */

#endif // TEST_S7_COMMON_H
